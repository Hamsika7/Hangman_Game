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
require('dotenv').config();

// Import database module
const { initializeDatabase } = require('./db/database');

// ============================================================================
// SERVER CONFIGURATION
// ============================================================================

const app = express();
const PORT = process.env.PORT || 3001;

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
        documentation: 'Coming soon...',
        endpoints: {
            auth: {
                register: 'POST /api/auth/register',
                login: 'POST /api/auth/login',
                profile: 'GET /api/auth/profile'
            },
            games: {
                start: 'POST /api/games/start',
                guess: 'POST /api/games/:id/guess',
                status: 'GET /api/games/:id',
                history: 'GET /api/games/history'
            },
            users: {
                stats: 'GET /api/users/stats',
                leaderboard: 'GET /api/users/leaderboard'
            }
        }
    });
});

// ============================================================================
// API ROUTE PLACEHOLDERS
// ============================================================================
// These will be implemented in future tasks

// Authentication routes (placeholder)
app.use('/api/auth', (req, res, next) => {
    res.status(501).json({
        error: 'Authentication endpoints not yet implemented',
        message: 'Coming in the next development phase',
        requested: `${req.method} ${req.path}`
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
