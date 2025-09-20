// ============================================================================
// HANGMAN GAME BACKEND SERVER
// ============================================================================
// Main Express server for the Hangman game application.
// Handles API endpoints, database operations, and user authentication.
//
// SETUP INSTRUCTIONS:
// 1. Run 'npm install' to install all dependencies
// 2. Run 'npm run dev' to start the development server with nodemon
// 3. Run 'npm start' to start the production server
// 4. Server will automatically initialize the database on startup
// ============================================================================

const express = require('express');
const cors = require('cors');
const path = require('path');
const bcrypt = require('bcrypt');
const jwt = require('jsonwebtoken');
require('dotenv').config();

// Import database module
const { initializeDatabase, runQuery, getQuery } = require('./db/database');

// Import Prolog bridge for game logic
const {
    selectRandomWord,
    maskWord,
    checkGuess,
    isGameWon,
    isGameLost,
    getAISuggestion,
    healthCheck
} = require('./prologBridge');

// ============================================================================
// SERVER CONFIGURATION
// ============================================================================

const app = express();
const PORT = process.env.PORT || 3001;

// JWT Configuration
const JWT_SECRET = process.env.JWT_SECRET || 'your-super-secret-jwt-key-change-this-in-production';
const JWT_EXPIRES_IN = process.env.JWT_EXPIRES_IN || '24h';
const BCRYPT_ROUNDS = parseInt(process.env.BCRYPT_ROUNDS) || 10;

// ============================================================================
// MIDDLEWARE SETUP
// ============================================================================

// Enable CORS for cross-origin requests
app.use(cors({
    origin: process.env.CLIENT_URL || 'http://localhost:3000',
    credentials: true,
    methods: ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS'],
    allowedHeaders: ['Content-Type', 'Authorization']
}));

// Parse JSON request bodies
app.use(express.json({ limit: '10mb' }));

// Parse URL-encoded request bodies
app.use(express.urlencoded({ extended: true, limit: '10mb' }));

// Serve static files from public directory (if exists)
const publicPath = path.join(__dirname, 'public');
app.use(express.static(publicPath));

// Request logging middleware
app.use((req, res, next) => {
    const timestamp = new Date().toISOString();
    console.log(`[${timestamp}] ${req.method} ${req.path} - ${req.ip}`);
    next();
});

// ============================================================================
// BASIC ROUTES
// ============================================================================

// Root endpoint - API health check
app.get('/', (req, res) => {
    res.json({
        message: 'Welcome to Hangman Game API! 🎮',
        version: '1.0.0',
        status: 'running',
        timestamp: new Date().toISOString(),
        endpoints: {
            health: '/health',
            api: '/api',
            docs: '/api/docs'
        }
    });
});

// Health check endpoint
app.get('/health', (req, res) => {
    res.json({
        status: 'healthy',
        uptime: process.uptime(),
        timestamp: new Date().toISOString(),
        memory: process.memoryUsage(),
        version: process.version
    });
});

// API base endpoint
app.get('/api', (req, res) => {
    res.json({
        message: 'Hangman Game API v1.0.0',
        documentation: 'Authentication endpoints are now available!',
        endpoints: {
            auth: {
                register: 'POST /api/auth/register - Register new user account',
                login: 'POST /api/auth/login - Login and get JWT token',
                profile: 'GET /api/auth/profile - Get user profile (requires auth)',
                logout: 'POST /api/auth/logout - Logout user (requires auth)'
            },
            games: {
                new: 'POST /api/game/new - Start new game (requires auth)',
                guess: 'POST /api/game/guess - Make letter guess (requires auth)',
                status: 'GET /api/game/status/:game_id - Get game status (requires auth)',
                history: 'GET /api/game/history - Get game history (requires auth)',
                hint: 'GET /api/game/hint/:game_id - Get AI hint (requires auth)',
                health: 'GET /api/game/prolog-health - Check Prolog bridge (requires auth)'
            },
            users: {
                stats: 'GET /api/users/stats (coming soon)',
                leaderboard: 'GET /api/users/leaderboard (coming soon)'
            }
        },
        authentication: {
            type: 'JWT Bearer Token',
            header: 'Authorization: Bearer <token>',
            expires: '24h'
        }
    });
});

// ============================================================================
// API ROUTE PLACEHOLDERS
// ============================================================================
// These will be implemented in future tasks

// ============================================================================
// AUTHENTICATION MIDDLEWARE
// ============================================================================

/**
 * Middleware to authenticate JWT tokens from Authorization header
 * Usage: Add as middleware to protected routes
 * Header format: "Authorization: Bearer <token>"
 */
function authenticateToken(req, res, next) {
    // Get token from Authorization header
    const authHeader = req.headers['authorization'];
    const token = authHeader && authHeader.split(' ')[1]; // Bearer TOKEN

    // Check if token exists
    if (!token) {
        return res.status(401).json({
            status: 'error',
            message: 'Access token required. Please provide a valid JWT token in Authorization header.'
        });
    }

    // Verify token
    jwt.verify(token, JWT_SECRET, (err, decoded) => {
        if (err) {
            console.error('JWT verification failed:', err.message);
            return res.status(403).json({
                status: 'error',
                message: 'Invalid or expired token. Please login again.'
            });
        }

        // Add user info to request object for use in route handlers
        req.user = {
            id: decoded.id,
            username: decoded.username
        };

        next(); // Continue to the protected route
    });
}

// ============================================================================
// AUTHENTICATION ROUTES
// ============================================================================

/**
 * POST /api/auth/register
 * Register a new user account
 * 
 * Request Body:
 * {
 *   "username": "string (required, 3-20 characters)",
 *   "password": "string (required, minimum 6 characters)"
 * }
 */
app.post('/api/auth/register', async (req, res) => {
    try {
        const { username, password } = req.body;

        // Input validation
        if (!username || !password) {
            return res.status(400).json({
                status: 'error',
                message: 'Username and password are required'
            });
        }

        // Validate username format
        if (username.length < 3 || username.length > 20) {
            return res.status(400).json({
                status: 'error',
                message: 'Username must be between 3 and 20 characters'
            });
        }

        // Validate username characters (alphanumeric and underscore only)
        if (!/^[a-zA-Z0-9_]+$/.test(username)) {
            return res.status(400).json({
                status: 'error',
                message: 'Username can only contain letters, numbers, and underscores'
            });
        }

        // Validate password strength
        if (password.length < 6) {
            return res.status(400).json({
                status: 'error',
                message: 'Password must be at least 6 characters long'
            });
        }

        // Check if username already exists
        const existingUser = await getQuery(
            'SELECT id FROM users WHERE username = ?',
            [username]
        );

        if (existingUser) {
            return res.status(409).json({
                status: 'error',
                message: 'Username already exists. Please choose a different username.'
            });
        }

        // Hash the password using bcrypt
        console.log(`🔐 Hashing password for user: ${username}`);
        const passwordHash = await bcrypt.hash(password, BCRYPT_ROUNDS);

        // Insert new user into database using prepared statement
        const result = await runQuery(
            'INSERT INTO users (username, password_hash) VALUES (?, ?)',
            [username, passwordHash]
        );

        console.log(`✅ User registered successfully: ${username} (ID: ${result.id})`);

        // Return success response (don't include password hash)
        res.status(201).json({
            status: 'success',
            message: 'User registered successfully',
            data: {
                id: result.id,
                username: username,
                score: 0
            }
        });

    } catch (error) {
        console.error('❌ Registration error:', error.message);
        
        // Handle specific database errors
        if (error.message.includes('UNIQUE constraint failed')) {
            return res.status(409).json({
                status: 'error',
                message: 'Username already exists'
            });
        }

        res.status(500).json({
            status: 'error',
            message: 'Internal server error during registration'
        });
    }
});

/**
 * POST /api/auth/login
 * Authenticate user and return JWT token
 * 
 * Request Body:
 * {
 *   "username": "string (required)",
 *   "password": "string (required)"
 * }
 */
app.post('/api/auth/login', async (req, res) => {
    try {
        const { username, password } = req.body;

        // Input validation
        if (!username || !password) {
            return res.status(400).json({
                status: 'error',
                message: 'Username and password are required'
            });
        }

        // Find user in database by username
        const user = await getQuery(
            'SELECT id, username, password_hash, score, games_played, games_won, games_lost FROM users WHERE username = ?',
            [username]
        );

        if (!user) {
            console.log(`🚫 Login attempt with non-existent username: ${username}`);
            return res.status(401).json({
                status: 'error',
                message: 'Invalid username or password'
            });
        }

        // Compare provided password with stored hash using bcrypt
        console.log(`🔐 Verifying password for user: ${username}`);
        const passwordMatch = await bcrypt.compare(password, user.password_hash);

        if (!passwordMatch) {
            console.log(`🚫 Invalid password attempt for user: ${username}`);
            return res.status(401).json({
                status: 'error',
                message: 'Invalid username or password'
            });
        }

        // Create JWT token containing user information
        const tokenPayload = {
            id: user.id,
            username: user.username
        };

        const token = jwt.sign(tokenPayload, JWT_SECRET, {
            expiresIn: JWT_EXPIRES_IN
        });

        console.log(`✅ User logged in successfully: ${username} (ID: ${user.id})`);

        // Return JWT token and user information
        res.json({
            status: 'success',
            message: 'Login successful',
            data: {
                token: token,
                user: {
                    id: user.id,
                    username: user.username,
                    score: user.score,
                    games_played: user.games_played,
                    games_won: user.games_won,
                    games_lost: user.games_lost
                }
            }
        });

    } catch (error) {
        console.error('❌ Login error:', error.message);
        res.status(500).json({
            status: 'error',
            message: 'Internal server error during login'
        });
    }
});

/**
 * GET /api/auth/profile
 * Get current user profile (protected route)
 * Requires valid JWT token in Authorization header
 */
app.get('/api/auth/profile', authenticateToken, async (req, res) => {
    try {
        // Get user details from database (req.user is set by authenticateToken middleware)
        const user = await getQuery(
            'SELECT id, username, score, games_played, games_won, games_lost, created_at FROM users WHERE id = ?',
            [req.user.id]
        );

        if (!user) {
            return res.status(404).json({
                status: 'error',
                message: 'User not found'
            });
        }

        res.json({
            status: 'success',
            data: {
                user: {
                    id: user.id,
                    username: user.username,
                    score: user.score,
                    games_played: user.games_played,
                    games_won: user.games_won,
                    games_lost: user.games_lost,
                    win_percentage: user.games_played > 0 ? 
                        Math.round((user.games_won / user.games_played) * 100) : 0,
                    member_since: user.created_at
                }
            }
        });

    } catch (error) {
        console.error('❌ Profile fetch error:', error.message);
        res.status(500).json({
            status: 'error',
            message: 'Internal server error while fetching profile'
        });
    }
});

/**
 * POST /api/auth/logout
 * Logout user (client-side token removal)
 * Note: JWT tokens are stateless, so logout is handled client-side
 */
app.post('/api/auth/logout', authenticateToken, (req, res) => {
    console.log(`👋 User logged out: ${req.user.username}`);
    
    res.json({
        status: 'success',
        message: 'Logout successful. Please remove the token from client storage.'
    });
});

// ============================================================================
// GAME API ENDPOINTS
// ============================================================================
// These endpoints handle game creation, moves, and state management
// All endpoints are protected and require valid JWT authentication

/**
 * POST /api/game/new
 * Create a new Hangman game for the authenticated user
 * 
 * Protected Route: Requires valid JWT token
 * 
 * Response:
 * {
 *   "status": "success",
 *   "data": {
 *     "game_id": 1,
 *     "masked_word": "______",
 *     "attempts_left": 7,
 *     "guessed_letters": [],
 *     "incorrect_guesses": []
 *   }
 * }
 */
app.post('/api/game/new', authenticateToken, async (req, res) => {
    try {
        const userId = req.user.id;
        console.log(`🎮 Starting new game for user: ${req.user.username} (ID: ${userId})`);
        
        // Step 1: Use Prolog bridge to select a random word
        const word = await selectRandomWord();
        if (!word) {
            throw new Error('Failed to select a word from Prolog');
        }
        
        console.log(`🎯 Selected word for game: ${word}`);
        
        // Step 2: Create initial masked word (all underscores)
        const maskedWord = await maskWord(word, []);
        
        // Step 3: Insert new game record into database
        const gameResult = await runQuery(
            `INSERT INTO games (
                user_id, word, masked_word, guessed_letters, 
                incorrect_guesses, attempts_left, status
            ) VALUES (?, ?, ?, ?, ?, ?, ?)`,
            [
                userId,
                word,
                maskedWord,
                '', // Empty guessed letters initially
                '', // Empty incorrect guesses initially
                7,  // Default attempts
                'active'
            ]
        );
        
        const gameId = gameResult.id;
        console.log(`✅ New game created with ID: ${gameId}`);
        
        // Step 4: Return initial game state to client
        res.status(201).json({
            status: 'success',
            message: 'New game created successfully',
            data: {
                game_id: gameId,
                masked_word: maskedWord,
                attempts_left: 7,
                guessed_letters: [],
                incorrect_guesses: [],
                word_length: word.length
            }
        });
        
    } catch (error) {
        console.error('❌ Error creating new game:', error.message);
        res.status(500).json({
            status: 'error',
            message: 'Failed to create new game',
            details: error.message
        });
    }
});

/**
 * POST /api/game/guess
 * Make a letter guess in an active game
 * 
 * Protected Route: Requires valid JWT token
 * 
 * Request Body:
 * {
 *   "game_id": 1,
 *   "guess": "a"
 * }
 * 
 * Response:
 * {
 *   "status": "success",
 *   "data": {
 *     "guess_result": "correct|incorrect",
 *     "masked_word": "_a_a_e",
 *     "attempts_left": 6,
 *     "game_status": "active|won|lost",
 *     "score": 100
 *   }
 * }
 */
app.post('/api/game/guess', authenticateToken, async (req, res) => {
    try {
        const userId = req.user.id;
        const { game_id, guess } = req.body;
        
        // Input validation
        if (!game_id || !guess) {
            return res.status(400).json({
                status: 'error',
                message: 'game_id and guess are required'
            });
        }
        
        // Validate guess format (single letter)
        if (typeof guess !== 'string' || guess.length !== 1 || !/^[a-zA-Z]$/.test(guess)) {
            return res.status(400).json({
                status: 'error',
                message: 'Guess must be a single letter'
            });
        }
        
        const normalizedGuess = guess.toLowerCase();
        console.log(`🎲 User ${req.user.username} guessing '${normalizedGuess}' in game ${game_id}`);
        
        // Step 1: Retrieve current game state from database
        const game = await getQuery(
            `SELECT * FROM games WHERE id = ? AND user_id = ? AND status = 'active'`,
            [game_id, userId]
        );
        
        if (!game) {
            return res.status(404).json({
                status: 'error',
                message: 'Active game not found or does not belong to user'
            });
        }
        
        // Parse current guessed letters
        const currentGuessedLetters = game.guessed_letters ? 
            game.guessed_letters.split(',').filter(l => l.length > 0) : [];
        const currentIncorrectGuesses = game.incorrect_guesses ? 
            game.incorrect_guesses.split(',').filter(l => l.length > 0) : [];
        
        // Check if letter was already guessed
        if (currentGuessedLetters.includes(normalizedGuess)) {
            return res.status(400).json({
                status: 'error',
                message: 'Letter already guessed'
            });
        }
        
        // Step 2: Use Prolog bridge to check if guess is correct
        const guessResult = await checkGuess(normalizedGuess, game.word);
        console.log(`🔍 Guess result: ${guessResult}`);
        
        // Step 3: Update game state based on guess result
        let newAttemptsLeft = game.attempts_left;
        let newGuessedLetters = [...currentGuessedLetters, normalizedGuess];
        let newIncorrectGuesses = [...currentIncorrectGuesses];
        
        if (guessResult === 'incorrect') {
            newAttemptsLeft -= 1;
            newIncorrectGuesses.push(normalizedGuess);
        }
        
        // Step 4: Generate new masked word
        const newMaskedWord = await maskWord(game.word, newGuessedLetters);
        
        // Step 5: Check win/loss conditions using Prolog bridge
        const gameWon = await isGameWon(newMaskedWord);
        const gameLost = await isGameLost(newAttemptsLeft);
        
        let gameStatus = 'active';
        let score = 0;
        
        if (gameWon) {
            gameStatus = 'won';
            // Calculate score based on word length and attempts remaining
            score = game.word.length * 10 + newAttemptsLeft * 5;
            console.log(`🎉 Game won! Score: ${score}`);
        } else if (gameLost) {
            gameStatus = 'lost';
            console.log(`💀 Game lost! Word was: ${game.word}`);
        }
        
        // Step 6: Update game in database
        await runQuery(
            `UPDATE games SET 
                masked_word = ?, 
                guessed_letters = ?, 
                incorrect_guesses = ?, 
                attempts_left = ?, 
                status = ?,
                score = ?
            WHERE id = ?`,
            [
                newMaskedWord,
                newGuessedLetters.join(','),
                newIncorrectGuesses.join(','),
                newAttemptsLeft,
                gameStatus,
                score,
                game_id
            ]
        );
        
        // Step 7: Update user score if game is completed
        if (gameStatus === 'won') {
            await runQuery(
                'UPDATE users SET score = score + ? WHERE id = ?',
                [score, userId]
            );
        }
        
        // Step 8: Return updated game state
        const responseData = {
            guess_result: guessResult,
            masked_word: newMaskedWord,
            attempts_left: newAttemptsLeft,
            guessed_letters: newGuessedLetters,
            incorrect_guesses: newIncorrectGuesses,
            game_status: gameStatus
        };
        
        // Add score and word for completed games
        if (gameStatus !== 'active') {
            responseData.score = score;
            responseData.word = game.word;
        }
        
        res.json({
            status: 'success',
            message: `Guess '${normalizedGuess}' is ${guessResult}`,
            data: responseData
        });
        
    } catch (error) {
        console.error('❌ Error processing guess:', error.message);
        res.status(500).json({
            status: 'error',
            message: 'Failed to process guess',
            details: error.message
        });
    }
});

/**
 * GET /api/game/status/:game_id
 * Get current status of a specific game
 * 
 * Protected Route: Requires valid JWT token
 * 
 * Response:
 * {
 *   "status": "success",
 *   "data": {
 *     "game_id": 1,
 *     "masked_word": "_a_a_e",
 *     "attempts_left": 5,
 *     "guessed_letters": ["a", "e"],
 *     "incorrect_guesses": ["x", "z"],
 *     "game_status": "active",
 *     "created_at": "2025-09-20T18:00:00.000Z"
 *   }
 * }
 */
app.get('/api/game/status/:game_id', authenticateToken, async (req, res) => {
    try {
        const userId = req.user.id;
        const gameId = req.params.game_id;
        
        // Validate game_id parameter
        if (!gameId || isNaN(gameId)) {
            return res.status(400).json({
                status: 'error',
                message: 'Invalid game_id parameter'
            });
        }
        
        console.log(`📊 Getting status for game ${gameId} (user: ${req.user.username})`);
        
        // Retrieve game from database
        const game = await getQuery(
            `SELECT 
                id, masked_word, guessed_letters, incorrect_guesses, 
                attempts_left, status, score, started_at, completed_at,
                CASE WHEN status != 'active' THEN word ELSE NULL END as revealed_word
            FROM games 
            WHERE id = ? AND user_id = ?`,
            [gameId, userId]
        );
        
        if (!game) {
            return res.status(404).json({
                status: 'error',
                message: 'Game not found or does not belong to user'
            });
        }
        
        // Parse letter arrays
        const guessedLetters = game.guessed_letters ? 
            game.guessed_letters.split(',').filter(l => l.length > 0) : [];
        const incorrectGuesses = game.incorrect_guesses ? 
            game.incorrect_guesses.split(',').filter(l => l.length > 0) : [];
        
        // Build response data
        const responseData = {
            game_id: game.id,
            masked_word: game.masked_word,
            attempts_left: game.attempts_left,
            guessed_letters: guessedLetters,
            incorrect_guesses: incorrectGuesses,
            game_status: game.status,
            score: game.score || 0,
            started_at: game.started_at,
            word_length: game.masked_word.length
        };
        
        // Add completion data for finished games
        if (game.status !== 'active') {
            responseData.completed_at = game.completed_at;
            responseData.word = game.revealed_word;
        }
        
        res.json({
            status: 'success',
            data: responseData
        });
        
    } catch (error) {
        console.error('❌ Error getting game status:', error.message);
        res.status(500).json({
            status: 'error',
            message: 'Failed to get game status',
            details: error.message
        });
    }
});

/**
 * GET /api/game/history
 * Get game history for the authenticated user
 * 
 * Protected Route: Requires valid JWT token
 * 
 * Query Parameters:
 * - limit: Number of games to return (default: 10, max: 50)
 * - offset: Number of games to skip (default: 0)
 */
app.get('/api/game/history', authenticateToken, async (req, res) => {
    try {
        const userId = req.user.id;
        const limit = Math.min(parseInt(req.query.limit) || 10, 50);
        const offset = parseInt(req.query.offset) || 0;
        
        console.log(`📚 Getting game history for user: ${req.user.username}`);
        
        // Get games with pagination
        const games = await runQuery(
            `SELECT 
                id, word, masked_word, attempts_left, status, score, 
                started_at, completed_at
            FROM games 
            WHERE user_id = ? 
            ORDER BY started_at DESC 
            LIMIT ? OFFSET ?`,
            [userId, limit, offset]
        );
        
        res.json({
            status: 'success',
            data: {
                games: games || [],
                pagination: {
                    limit: limit,
                    offset: offset,
                    returned: (games || []).length
                }
            }
        });
        
    } catch (error) {
        console.error('❌ Error getting game history:', error.message);
        res.status(500).json({
            status: 'error',
            message: 'Failed to get game history',
            details: error.message
        });
    }
});

/**
 * GET /api/game/hint/:game_id
 * Get AI suggestion for next best letter to guess
 * 
 * Protected Route: Requires valid JWT token
 */
app.get('/api/game/hint/:game_id', authenticateToken, async (req, res) => {
    try {
        const userId = req.user.id;
        const gameId = req.params.game_id;
        
        // Get current game state
        const game = await getQuery(
            `SELECT masked_word, guessed_letters FROM games 
            WHERE id = ? AND user_id = ? AND status = 'active'`,
            [gameId, userId]
        );
        
        if (!game) {
            return res.status(404).json({
                status: 'error',
                message: 'Active game not found'
            });
        }
        
        const guessedLetters = game.guessed_letters ? 
            game.guessed_letters.split(',').filter(l => l.length > 0) : [];
        
        // Get AI suggestion
        const suggestion = await getAISuggestion(game.masked_word, guessedLetters);
        
        res.json({
            status: 'success',
            data: {
                suggested_letter: suggestion,
                confidence: 'high' // Could be enhanced with actual confidence from Prolog
            }
        });
        
    } catch (error) {
        console.error('❌ Error getting AI hint:', error.message);
        res.status(500).json({
            status: 'error',
            message: 'Failed to get AI hint',
            details: error.message
        });
    }
});

/**
 * GET /api/game/prolog-health
 * Check Prolog bridge health status
 * 
 * Protected Route: Requires valid JWT token (admin only in production)
 */
app.get('/api/game/prolog-health', authenticateToken, async (req, res) => {
    try {
        const health = await healthCheck();
        res.json({
            status: 'success',
            data: health
        });
    } catch (error) {
        res.status(500).json({
            status: 'error',
            message: 'Health check failed',
            details: error.message
        });
    }
});

// User routes (placeholder)
app.use('/api/users', (req, res, next) => {
    res.status(501).json({
        error: 'User endpoints not yet implemented',
        message: 'Coming in the next development phase',
        requested: `${req.method} ${req.path}`
    });
});

// ============================================================================
// ERROR HANDLING MIDDLEWARE
// ============================================================================

// 404 handler for unknown routes
app.use('*', (req, res) => {
    res.status(404).json({
        error: 'Endpoint not found',
        message: `The requested endpoint ${req.method} ${req.originalUrl} does not exist`,
        availableEndpoints: [
            'GET /',
            'GET /health',
            'GET /api',
            'POST /api/auth/* (coming soon)',
            'GET /api/games/* (coming soon)',
            'GET /api/users/* (coming soon)'
        ]
    });
});

// Global error handler
app.use((err, req, res, next) => {
    console.error('❌ Server Error:', err);
    
    // Don't leak error details in production
    const isDevelopment = process.env.NODE_ENV !== 'production';
    
    res.status(err.status || 500).json({
        error: 'Internal Server Error',
        message: isDevelopment ? err.message : 'Something went wrong',
        ...(isDevelopment && { stack: err.stack })
    });
});

// ============================================================================
// SERVER STARTUP
// ============================================================================

async function startServer() {
    try {
        console.log('🚀 Starting Hangman Game Backend Server...');
        console.log('📦 Node.js version:', process.version);
        console.log('🌍 Environment:', process.env.NODE_ENV || 'development');
        
        // Initialize database
        console.log('🔄 Initializing database...');
        await initializeDatabase();
        
        // Start the server
        const server = app.listen(PORT, () => {
            console.log('✅ Server successfully started!');
            console.log(`🌐 Server running on http://localhost:${PORT}`);
            console.log(`📚 API documentation: http://localhost:${PORT}/api`);
            console.log(`💊 Health check: http://localhost:${PORT}/health`);
            console.log('🎮 Ready to handle Hangman game requests!');
            console.log('─'.repeat(50));
        });

        // Graceful shutdown handling
        const gracefulShutdown = (signal) => {
            console.log(`\n🔄 Received ${signal}, shutting down gracefully...`);
            server.close(() => {
                console.log('✅ Server closed successfully');
                process.exit(0);
            });
        };

        process.on('SIGTERM', () => gracefulShutdown('SIGTERM'));
        process.on('SIGINT', () => gracefulShutdown('SIGINT'));

    } catch (error) {
        console.error('❌ Failed to start server:', error.message);
        console.error('Stack trace:', error.stack);
        process.exit(1);
    }
}

// ============================================================================
// START THE SERVER
// ============================================================================

// Only start server if this file is run directly (not imported)
if (require.main === module) {
    startServer();
}

// Export app for testing purposes
module.exports = app;
