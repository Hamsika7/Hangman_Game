// ============================================================================
// DATABASE CONNECTION AND INITIALIZATION
// ============================================================================
// This module handles SQLite database connection, initialization, and exports
// the database object for use throughout the application.
// ============================================================================

const sqlite3 = require('sqlite3').verbose();
const fs = require('fs');
const path = require('path');

// Database file path
const DB_PATH = path.join(__dirname, 'hangman.db');
const INIT_SQL_PATH = path.join(__dirname, 'init.sql');

// Create database connection
const db = new sqlite3.Database(DB_PATH, (err) => {
    if (err) {
        console.error('❌ Error opening database:', err.message);
        process.exit(1);
    } else {
        console.log('✅ Connected to SQLite database:', DB_PATH);
        
        // Enable foreign key constraints
        db.run('PRAGMA foreign_keys = ON', (err) => {
            if (err) {
                console.error('❌ Error enabling foreign keys:', err.message);
            } else {
                console.log('✅ Foreign key constraints enabled');
            }
        });
    }
});

// ============================================================================
// DATABASE INITIALIZATION
// ============================================================================

/**
 * Initialize database by running the init.sql script
 * This creates all necessary tables if they don't exist
 */
function initializeDatabase() {
    return new Promise((resolve, reject) => {
        // Check if init.sql exists
        if (!fs.existsSync(INIT_SQL_PATH)) {
            const error = `Init SQL file not found: ${INIT_SQL_PATH}`;
            console.error('❌', error);
            return reject(new Error(error));
        }

        // Read and execute init.sql
        const initSQL = fs.readFileSync(INIT_SQL_PATH, 'utf8');
        
        console.log('🔄 Initializing database schema...');
        
        db.exec(initSQL, (err) => {
            if (err) {
                console.error('❌ Error initializing database:', err.message);
                reject(err);
            } else {
                console.log('✅ Database schema initialized successfully');
                resolve();
            }
        });
    });
}

// ============================================================================
// DATABASE UTILITY FUNCTIONS
// ============================================================================

/**
 * Run a query that doesn't return data (INSERT, UPDATE, DELETE)
 * @param {string} sql - SQL query string
 * @param {Array} params - Query parameters
 * @returns {Promise} Promise that resolves with query result
 */
function runQuery(sql, params = []) {
    return new Promise((resolve, reject) => {
        db.run(sql, params, function(err) {
            if (err) {
                console.error('❌ Database run error:', err.message);
                reject(err);
            } else {
                resolve({
                    id: this.lastID,
                    changes: this.changes
                });
            }
        });
    });
}

/**
 * Get a single row from the database
 * @param {string} sql - SQL query string
 * @param {Array} params - Query parameters
 * @returns {Promise} Promise that resolves with single row or undefined
 */
function getQuery(sql, params = []) {
    return new Promise((resolve, reject) => {
        db.get(sql, params, (err, row) => {
            if (err) {
                console.error('❌ Database get error:', err.message);
                reject(err);
            } else {
                resolve(row);
            }
        });
    });
}

/**
 * Get multiple rows from the database
 * @param {string} sql - SQL query string
 * @param {Array} params - Query parameters
 * @returns {Promise} Promise that resolves with array of rows
 */
function allQuery(sql, params = []) {
    return new Promise((resolve, reject) => {
        db.all(sql, params, (err, rows) => {
            if (err) {
                console.error('❌ Database all error:', err.message);
                reject(err);
            } else {
                resolve(rows || []);
            }
        });
    });
}

/**
 * Close database connection gracefully
 */
function closeDatabase() {
    return new Promise((resolve, reject) => {
        db.close((err) => {
            if (err) {
                console.error('❌ Error closing database:', err.message);
                reject(err);
            } else {
                console.log('✅ Database connection closed');
                resolve();
            }
        });
    });
}

// ============================================================================
// GRACEFUL SHUTDOWN HANDLING
// ============================================================================

// Handle process termination
process.on('SIGINT', async () => {
    console.log('\n🔄 Shutting down gracefully...');
    try {
        await closeDatabase();
        process.exit(0);
    } catch (err) {
        console.error('❌ Error during shutdown:', err.message);
        process.exit(1);
    }
});

process.on('SIGTERM', async () => {
    console.log('\n🔄 Received SIGTERM, shutting down gracefully...');
    try {
        await closeDatabase();
        process.exit(0);
    } catch (err) {
        console.error('❌ Error during shutdown:', err.message);
        process.exit(1);
    }
});

// ============================================================================
// EXPORTS
// ============================================================================

module.exports = {
    db,
    initializeDatabase,
    runQuery,
    getQuery,
    allQuery,
    closeDatabase
};
