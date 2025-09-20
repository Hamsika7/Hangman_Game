// ============================================================================
// HOME COMPONENT - Welcome Page for Hangman Game
// ============================================================================
// This component serves as the landing page for the Hangman game application.
// It provides a welcome message and navigation options for users.
// ============================================================================

import React from 'react';
import { Link } from 'react-router-dom';
import './Home.css';

const Home = () => {
    return (
        <div className="home-container">
            <div className="home-content">
                {/* Game Title and Logo */}
                <div className="game-header">
                    <h1 className="game-title">🎮 Hangman Game</h1>
                    <div className="game-subtitle">
                        <p>Challenge yourself with word guessing!</p>
                        <p>Powered by Prolog AI 🤖</p>
                    </div>
                </div>

                {/* Game Features */}
                <div className="features-section">
                    <h2>Game Features</h2>
                    <div className="features-grid">
                        <div className="feature-card">
                            <span className="feature-icon">🎯</span>
                            <h3>Smart Word Selection</h3>
                            <p>Words chosen by Prolog algorithms</p>
                        </div>
                        <div className="feature-card">
                            <span className="feature-icon">🧠</span>
                            <h3>AI Hints</h3>
                            <p>Get intelligent letter suggestions</p>
                        </div>
                        <div className="feature-card">
                            <span className="feature-icon">📊</span>
                            <h3>Score Tracking</h3>
                            <p>Track your progress and statistics</p>
                        </div>
                        <div className="feature-card">
                            <span className="feature-icon">🏆</span>
                            <h3>Achievements</h3>
                            <p>Unlock rewards as you play</p>
                        </div>
                    </div>
                </div>

                {/* Action Buttons */}
                <div className="action-section">
                    <h2>Ready to Play?</h2>
                    <div className="button-group">
                        <Link to="/login" className="btn btn-primary">
                            Login to Play
                        </Link>
                        <Link to="/register" className="btn btn-secondary">
                            Create Account
                        </Link>
                    </div>
                    <p className="guest-option">
                        <Link to="/demo" className="link-text">
                            Try Demo Mode (No Account Required)
                        </Link>
                    </p>
                </div>

                {/* Game Rules */}
                <div className="rules-section">
                    <h2>How to Play</h2>
                    <div className="rules-list">
                        <div className="rule-item">
                            <span className="rule-number">1</span>
                            <p>A secret word is chosen for you to guess</p>
                        </div>
                        <div className="rule-item">
                            <span className="rule-number">2</span>
                            <p>Guess letters one at a time</p>
                        </div>
                        <div className="rule-item">
                            <span className="rule-number">3</span>
                            <p>You have 7 incorrect guesses before losing</p>
                        </div>
                        <div className="rule-item">
                            <span className="rule-number">4</span>
                            <p>Win by guessing the complete word!</p>
                        </div>
                    </div>
                </div>

                {/* Footer */}
                <div className="home-footer">
                    <p>Built with React + Node.js + Prolog</p>
                    <p>© 2025 Hangman Game Project</p>
                </div>
            </div>
        </div>
    );
};

export default Home;
