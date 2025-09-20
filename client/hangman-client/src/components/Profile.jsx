// ============================================================================
// PROFILE COMPONENT - User Statistics and Profile Information
// ============================================================================
// This component displays the authenticated user's game statistics, profile
// information, and performance metrics by fetching data from the backend API.
// ============================================================================

import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import { isAuthenticated, getCurrentUser, handleAPIError, userAPI } from '../services/api';
import './Profile.css';

const Profile = () => {
    // ========================================================================
    // STATE MANAGEMENT
    // ========================================================================
    
    // User statistics state
    const [userStats, setUserStats] = useState(null);
    const [isLoading, setIsLoading] = useState(true);
    const [error, setError] = useState(null);
    
    // User information
    const [currentUser, setCurrentUser] = useState(null);
    
    // Navigation hook
    const navigate = useNavigate();

    // ========================================================================
    // LIFECYCLE EFFECTS
    // ========================================================================

    useEffect(() => {
        // Check authentication on component mount
        if (!isAuthenticated()) {
            console.log('🔒 User not authenticated, redirecting to login');
            navigate('/login', { 
                state: { 
                    from: '/profile',
                    message: 'Please log in to view your profile.' 
                }
            });
            return;
        }

        // Get current user information
        const user = getCurrentUser();
        setCurrentUser(user);

        // Fetch user statistics
        fetchUserStats();
    }, [navigate]);

    // ========================================================================
    // API FUNCTIONS
    // ========================================================================

    /**
     * Fetch user statistics from the backend API
     * Makes authenticated GET request to /api/users/stats using API service
     */
    const fetchUserStats = async () => {
        setIsLoading(true);
        setError(null);
        
        try {
            console.log('📊 Fetching user statistics...');
            
            // Use the API service for consistent error handling and interceptors
            const response = await userAPI.getStats();
            console.log('✅ User stats fetched:', response);

            if (response.status === 'success') {
                setUserStats(response.data);
            } else {
                throw new Error(response.message || 'Failed to fetch user statistics');
            }
            
        } catch (error) {
            console.error('❌ Failed to fetch user stats:', error);
            const errorMessage = handleAPIError(error);
            setError(errorMessage);
            
            // If token is invalid, redirect to login (handled by API interceptor)
            if (error.response?.status === 401) {
                navigate('/login', {
                    state: {
                        message: 'Your session has expired. Please log in again.'
                    }
                });
            }
        } finally {
            setIsLoading(false);
        }
    };

    /**
     * Refresh user statistics
     */
    const refreshStats = () => {
        fetchUserStats();
    };

    // ========================================================================
    // UTILITY FUNCTIONS
    // ========================================================================

    /**
     * Format date to readable string
     * @param {string} dateString - ISO date string
     * @returns {string} Formatted date
     */
    const formatDate = (dateString) => {
        if (!dateString) return 'Unknown';
        try {
            return new Date(dateString).toLocaleDateString('en-US', {
                year: 'numeric',
                month: 'long',
                day: 'numeric'
            });
        } catch (error) {
            return 'Invalid Date';
        }
    };

    /**
     * Get performance level based on win percentage
     * @param {number} winPercentage - Win percentage
     * @returns {object} Performance level info
     */
    const getPerformanceLevel = (winPercentage) => {
        if (winPercentage >= 80) {
            return { level: 'Expert', emoji: '🏆', color: '#f6ad55' };
        } else if (winPercentage >= 60) {
            return { level: 'Advanced', emoji: '🥈', color: '#68d391' };
        } else if (winPercentage >= 40) {
            return { level: 'Intermediate', emoji: '🥉', color: '#63b3ed' };
        } else if (winPercentage >= 20) {
            return { level: 'Beginner', emoji: '📚', color: '#a78bfa' };
        } else {
            return { level: 'Novice', emoji: '🌱', color: '#fbb6ce' };
        }
    };

    // ========================================================================
    // RENDER LOADING STATE
    // ========================================================================

    if (isLoading) {
        return (
            <div className="profile-container">
                <div className="profile-content">
                    <div className="loading-state">
                        <div className="loading-spinner"></div>
                        <h2>Loading Profile...</h2>
                        <p>Fetching your game statistics</p>
                    </div>
                </div>
            </div>
        );
    }

    // ========================================================================
    // RENDER ERROR STATE
    // ========================================================================

    if (error) {
        return (
            <div className="profile-container">
                <div className="profile-content">
                    <div className="error-state">
                        <div className="error-icon">❌</div>
                        <h2>Failed to Load Profile</h2>
                        <p className="error-message">{error}</p>
                        <div className="error-actions">
                            <button 
                                className="retry-btn"
                                onClick={refreshStats}
                            >
                                🔄 Try Again
                            </button>
                            <button 
                                className="home-btn"
                                onClick={() => navigate('/')}
                            >
                                🏠 Go Home
                            </button>
                        </div>
                    </div>
                </div>
            </div>
        );
    }

    // ========================================================================
    // RENDER MAIN COMPONENT
    // ========================================================================

    const performance = userStats ? getPerformanceLevel(userStats.win_percentage) : null;

    return (
        <div className="profile-container">
            <div className="profile-content">
                {/* Profile Header */}
                <div className="profile-header">
                    <div className="profile-avatar">
                        <span className="avatar-emoji">👤</span>
                    </div>
                    <div className="profile-info">
                        <h1 className="profile-username">{userStats?.username}</h1>
                        <p className="profile-member-since">
                            Member since {formatDate(userStats?.member_since)}
                        </p>
                        {performance && (
                            <div className="performance-badge" style={{ backgroundColor: performance.color }}>
                                <span className="performance-emoji">{performance.emoji}</span>
                                <span className="performance-level">{performance.level} Player</span>
                            </div>
                        )}
                    </div>
                    <div className="profile-actions">
                        <button 
                            className="refresh-btn"
                            onClick={refreshStats}
                            title="Refresh Statistics"
                        >
                            🔄 Refresh
                        </button>
                    </div>
                </div>

                {/* Statistics Grid */}
                <div className="stats-grid">
                    {/* Total Score Card */}
                    <div className="stat-card primary">
                        <div className="stat-icon">🏆</div>
                        <div className="stat-content">
                            <h3 className="stat-value">{userStats?.total_score || 0}</h3>
                            <p className="stat-label">Total Score</p>
                        </div>
                    </div>

                    {/* Games Played Card */}
                    <div className="stat-card">
                        <div className="stat-icon">🎮</div>
                        <div className="stat-content">
                            <h3 className="stat-value">{userStats?.games_played || 0}</h3>
                            <p className="stat-label">Games Played</p>
                        </div>
                    </div>

                    {/* Games Won Card */}
                    <div className="stat-card success">
                        <div className="stat-icon">✅</div>
                        <div className="stat-content">
                            <h3 className="stat-value">{userStats?.games_won || 0}</h3>
                            <p className="stat-label">Games Won</p>
                        </div>
                    </div>

                    {/* Games Lost Card */}
                    <div className="stat-card danger">
                        <div className="stat-icon">❌</div>
                        <div className="stat-content">
                            <h3 className="stat-value">{userStats?.games_lost || 0}</h3>
                            <p className="stat-label">Games Lost</p>
                        </div>
                    </div>

                    {/* Win Percentage Card */}
                    <div className="stat-card info">
                        <div className="stat-icon">📊</div>
                        <div className="stat-content">
                            <h3 className="stat-value">{userStats?.win_percentage?.toFixed(1) || 0}%</h3>
                            <p className="stat-label">Win Rate</p>
                        </div>
                    </div>

                    {/* Average Score Card */}
                    <div className="stat-card warning">
                        <div className="stat-icon">📈</div>
                        <div className="stat-content">
                            <h3 className="stat-value">{userStats?.average_score?.toFixed(1) || 0}</h3>
                            <p className="stat-label">Avg Score</p>
                        </div>
                    </div>

                    {/* Best Game Score Card */}
                    <div className="stat-card special">
                        <div className="stat-icon">🌟</div>
                        <div className="stat-content">
                            <h3 className="stat-value">{userStats?.best_game_score || 0}</h3>
                            <p className="stat-label">Best Score</p>
                        </div>
                    </div>
                </div>

                {/* Performance Summary */}
                <div className="performance-summary">
                    <h2>Performance Summary</h2>
                    <div className="summary-content">
                        <div className="summary-item">
                            <span className="summary-label">Current Level:</span>
                            <span className="summary-value">
                                {performance?.emoji} {performance?.level}
                            </span>
                        </div>
                        <div className="summary-item">
                            <span className="summary-label">Total Experience:</span>
                            <span className="summary-value">
                                {userStats?.games_played || 0} games played
                            </span>
                        </div>
                        <div className="summary-item">
                            <span className="summary-label">Success Rate:</span>
                            <span className="summary-value">
                                {userStats?.win_percentage?.toFixed(1) || 0}% win rate
                            </span>
                        </div>
                        <div className="summary-item">
                            <span className="summary-label">Score Performance:</span>
                            <span className="summary-value">
                                {userStats?.average_score?.toFixed(1) || 0} avg per game
                            </span>
                        </div>
                    </div>
                </div>

                {/* Action Buttons */}
                <div className="profile-actions-bottom">
                    <button 
                        className="action-btn primary"
                        onClick={() => navigate('/game')}
                    >
                        🎮 Play Game
                    </button>
                    <button 
                        className="action-btn secondary"
                        onClick={() => navigate('/leaderboard')}
                    >
                        🏆 View Leaderboard
                    </button>
                    <button 
                        className="action-btn tertiary"
                        onClick={() => navigate('/')}
                    >
                        🏠 Home
                    </button>
                </div>
            </div>
        </div>
    );
};

export default Profile;
