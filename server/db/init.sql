-- ============================================================================
-- HANGMAN GAME DATABASE SCHEMA
-- ============================================================================
-- This file contains the SQL commands to create all necessary tables for the
-- Hangman game application, including user management and game state tracking.
-- ============================================================================

-- Enable foreign key constraints (SQLite specific)
PRAGMA foreign_keys = ON;

-- ============================================================================
-- USERS TABLE
-- ============================================================================
-- Stores user account information including authentication and scoring data
CREATE TABLE IF NOT EXISTS users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT UNIQUE NOT NULL,
    password_hash TEXT NOT NULL,
    score INTEGER DEFAULT 0,
    games_played INTEGER DEFAULT 0,
    games_won INTEGER DEFAULT 0,
    games_lost INTEGER DEFAULT 0,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Create index on username for faster lookups
CREATE INDEX IF NOT EXISTS idx_users_username ON users(username);

-- ============================================================================
-- GAMES TABLE
-- ============================================================================
-- Stores individual game sessions and their current state
CREATE TABLE IF NOT EXISTS games (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER NOT NULL,
    word TEXT NOT NULL,
    masked_word TEXT NOT NULL,
    guessed_letters TEXT NOT NULL DEFAULT '',
    incorrect_guesses TEXT NOT NULL DEFAULT '',
    attempts_left INTEGER NOT NULL DEFAULT 7,
    status TEXT NOT NULL DEFAULT 'active' CHECK (status IN ('active', 'won', 'lost', 'abandoned')),
    score INTEGER DEFAULT 0,
    started_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    completed_at DATETIME NULL,
    
    -- Foreign key constraint
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

-- Create indexes for better query performance
CREATE INDEX IF NOT EXISTS idx_games_user_id ON games(user_id);
CREATE INDEX IF NOT EXISTS idx_games_status ON games(status);
CREATE INDEX IF NOT EXISTS idx_games_started_at ON games(started_at);

-- ============================================================================
-- GAME STATISTICS VIEW
-- ============================================================================
-- Convenient view for user statistics and leaderboard queries
CREATE VIEW IF NOT EXISTS user_stats AS
SELECT 
    u.id,
    u.username,
    u.score as total_score,
    u.games_played,
    u.games_won,
    u.games_lost,
    CASE 
        WHEN u.games_played > 0 
        THEN ROUND((u.games_won * 100.0) / u.games_played, 2) 
        ELSE 0 
    END as win_percentage,
    u.created_at,
    u.updated_at
FROM users u;

-- ============================================================================
-- TRIGGERS FOR AUTOMATIC UPDATES
-- ============================================================================

-- Trigger to update user stats when a game is completed
CREATE TRIGGER IF NOT EXISTS update_user_stats_on_game_completion
    AFTER UPDATE OF status ON games
    WHEN NEW.status IN ('won', 'lost') AND OLD.status = 'active'
BEGIN
    UPDATE users 
    SET 
        games_played = games_played + 1,
        games_won = games_won + CASE WHEN NEW.status = 'won' THEN 1 ELSE 0 END,
        games_lost = games_lost + CASE WHEN NEW.status = 'lost' THEN 1 ELSE 0 END,
        score = score + COALESCE(NEW.score, 0),
        updated_at = CURRENT_TIMESTAMP
    WHERE id = NEW.user_id;
END;

-- Trigger to update the updated_at timestamp on users table
CREATE TRIGGER IF NOT EXISTS update_users_timestamp
    AFTER UPDATE ON users
    FOR EACH ROW
BEGIN
    UPDATE users SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
END;

-- Trigger to set completed_at when game status changes to completed
CREATE TRIGGER IF NOT EXISTS set_game_completion_time
    AFTER UPDATE OF status ON games
    WHEN NEW.status IN ('won', 'lost', 'abandoned') AND OLD.status = 'active'
BEGIN
    UPDATE games 
    SET completed_at = CURRENT_TIMESTAMP 
    WHERE id = NEW.id;
END;

-- ============================================================================
-- INITIAL DATA (OPTIONAL)
-- ============================================================================

-- Insert a default admin user (password: 'admin123' - change in production!)
-- Password hash for 'admin123' using bcrypt with salt rounds 10
INSERT OR IGNORE INTO users (id, username, password_hash, score) 
VALUES (1, 'admin', '$2b$10$rOCVZyPIm7vjpms7Wz/x4.8kBKXudpJJeJwrALy9dGKBc8zKqF5W.', 0);

-- ============================================================================
-- SAMPLE QUERIES FOR TESTING
-- ============================================================================

-- Uncomment these queries to test the schema after creation:

-- -- Get all users with their statistics
-- -- SELECT * FROM user_stats ORDER BY total_score DESC;

-- -- Get active games for a specific user
-- -- SELECT * FROM games WHERE user_id = 1 AND status = 'active';

-- -- Get leaderboard (top 10 users by score)
-- -- SELECT username, total_score, games_played, win_percentage 
-- -- FROM user_stats 
-- -- ORDER BY total_score DESC 
-- -- LIMIT 10;

-- -- Get recent games for a user
-- -- SELECT g.*, u.username 
-- -- FROM games g 
-- -- JOIN users u ON g.user_id = u.id 
-- -- WHERE g.user_id = 1 
-- -- ORDER BY g.started_at DESC 
-- -- LIMIT 5;

-- ============================================================================
-- SCHEMA VERSION TRACKING
-- ============================================================================

-- Create a simple version table to track schema updates
CREATE TABLE IF NOT EXISTS schema_version (
    version INTEGER PRIMARY KEY,
    applied_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    description TEXT
);

-- Insert current schema version
INSERT OR REPLACE INTO schema_version (version, description) 
VALUES (1, 'Initial schema with users, games tables, and basic triggers');

-- ============================================================================
-- END OF SCHEMA
-- ============================================================================
