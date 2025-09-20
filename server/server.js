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
                start: 'POST /api/games/start (coming soon)',
                guess: 'POST /api/games/:id/guess (coming soon)',
                status: 'GET /api/games/:id (coming soon)',
                history: 'GET /api/games/history (coming soon)'
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

// Game routes (placeholder)
app.use('/api/games', (req, res, next) => {
    res.status(501).json({
        error: 'Game endpoints not yet implemented',
        message: 'Coming in the next development phase',
        requested: `${req.method} ${req.path}`
    });
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
