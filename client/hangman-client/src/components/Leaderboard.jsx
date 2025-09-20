// ============================================================================
// LEADERBOARD COMPONENT - Top Players Ranking and Statistics
// ============================================================================
// This component displays the top players leaderboard by fetching data from
// the public API endpoint and presenting it in an interactive table format.
// ============================================================================

import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import { handleAPIError, userAPI } from '../services/api';
import './Leaderboard.css';

const Leaderboard = () => {
    // ========================================================================
    // STATE MANAGEMENT
    // ========================================================================
    
    // Leaderboard data state
    const [leaderboardData, setLeaderboardData] = useState([]);
    const [isLoading, setIsLoading] = useState(true);
    const [error, setError] = useState(null);
    
    // Leaderboard configuration
    const [limit, setLimit] = useState(10);
    const [sortBy, setSortBy] = useState('score');
    const [totalPlayers, setTotalPlayers] = useState(0);
    const [lastUpdated, setLastUpdated] = useState(null);
    
    // Navigation hook
    const navigate = useNavigate();

    // ========================================================================
    // LIFECYCLE EFFECTS
    // ========================================================================

    useEffect(() => {
        // Fetch leaderboard data when component mounts or parameters change
        fetchLeaderboard();
    }, [limit, sortBy]);

    // ========================================================================
    // API FUNCTIONS
    // ========================================================================

    /**
     * Fetch leaderboard data from the public API endpoint
     * Makes GET request to /api/users/leaderboard using API service
     */
    const fetchLeaderboard = async () => {
        setIsLoading(true);
        setError(null);
        
        try {
            console.log(`🏆 Fetching leaderboard - limit: ${limit}, sort: ${sortBy}`);
            
            // Use the API service with query parameters
            const response = await userAPI.getLeaderboard({
                limit: limit.toString(),
                sort: sortBy
            });
            
            console.log('✅ Leaderboard data fetched:', response);

            if (response.status === 'success') {
                setLeaderboardData(response.data.leaderboard);
                setTotalPlayers(response.data.total_players);
                setLastUpdated(response.data.last_updated);
            } else {
                throw new Error(response.message || 'Failed to fetch leaderboard data');
            }
            
        } catch (error) {
            console.error('❌ Failed to fetch leaderboard:', error);
            const errorMessage = handleAPIError(error);
            setError(errorMessage);
        } finally {
            setIsLoading(false);
        }
    };

    /**
     * Refresh leaderboard data
     */
    const refreshLeaderboard = () => {
        fetchLeaderboard();
    };

    /**
     * Handle limit change
     * @param {number} newLimit - New limit value
     */
    const handleLimitChange = (newLimit) => {
        setLimit(newLimit);
    };

    /**
     * Handle sort criteria change
     * @param {string} newSortBy - New sort criteria
     */
    const handleSortChange = (newSortBy) => {
        setSortBy(newSortBy);
    };

    // ========================================================================
    // UTILITY FUNCTIONS
    // ========================================================================

    /**
     * Get rank display with appropriate styling
     * @param {number} rank - Player rank
     * @returns {object} Rank display info
     */
    const getRankDisplay = (rank) => {
        if (rank === 1) {
            return { emoji: '🥇', class: 'rank-gold' };
        } else if (rank === 2) {
            return { emoji: '🥈', class: 'rank-silver' };
        } else if (rank === 3) {
            return { emoji: '🥉', class: 'rank-bronze' };
        } else {
            return { emoji: `#${rank}`, class: 'rank-normal' };
        }
    };

    /**
     * Format date to readable string
     * @param {string} dateString - ISO date string
     * @returns {string} Formatted date
     */
    const formatLastUpdated = (dateString) => {
        if (!dateString) return 'Unknown';
        try {
            return new Date(dateString).toLocaleString('en-US', {
                month: 'short',
                day: 'numeric',
                hour: '2-digit',
                minute: '2-digit'
            });
        } catch (error) {
            return 'Invalid Date';
        }
    };

    // ========================================================================
    // RENDER LOADING STATE
    // ========================================================================

    if (isLoading) {
        return (
            <div className="leaderboard-container">
                <div className="leaderboard-content">
                    <div className="loading-state">
                        <div className="loading-spinner"></div>
                        <h2>Loading Leaderboard...</h2>
                        <p>Fetching top players data</p>
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
            <div className="leaderboard-container">
                <div className="leaderboard-content">
                    <div className="error-state">
                        <div className="error-icon">❌</div>
                        <h2>Failed to Load Leaderboard</h2>
                        <p className="error-message">{error}</p>
                        <div className="error-actions">
                            <button 
                                className="retry-btn"
                                onClick={refreshLeaderboard}
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

    return (
        <div className="leaderboard-container">
            <div className="leaderboard-content">
                {/* Leaderboard Header */}
                <div className="leaderboard-header">
                    <div className="header-info">
                        <h1 className="leaderboard-title">🏆 Leaderboard</h1>
                        <p className="leaderboard-subtitle">
                            Top players competing in Hangman
                        </p>
                        <div className="leaderboard-stats">
                            <span className="stat-item">
                                👥 {totalPlayers} total players
                            </span>
                            <span className="stat-item">
                                🕒 Updated {formatLastUpdated(lastUpdated)}
                            </span>
                        </div>
                    </div>
                    <div className="header-actions">
                        <button 
                            className="refresh-btn"
                            onClick={refreshLeaderboard}
                            title="Refresh Leaderboard"
                        >
                            🔄 Refresh
                        </button>
                    </div>
                </div>

                {/* Leaderboard Controls */}
                <div className="leaderboard-controls">
                    <div className="control-group">
                        <label htmlFor="sort-select">Sort by:</label>
                        <select 
                            id="sort-select"
                            value={sortBy} 
                            onChange={(e) => handleSortChange(e.target.value)}
                            className="control-select"
                        >
                            <option value="score">🏆 Total Score</option>
                            <option value="wins">✅ Games Won</option>
                        </select>
                    </div>
                    
                    <div className="control-group">
                        <label htmlFor="limit-select">Show:</label>
                        <select 
                            id="limit-select"
                            value={limit} 
                            onChange={(e) => handleLimitChange(parseInt(e.target.value))}
                            className="control-select"
                        >
                            <option value={5}>Top 5</option>
                            <option value={10}>Top 10</option>
                            <option value={20}>Top 20</option>
                            <option value={50}>Top 50</option>
                        </select>
                    </div>
                </div>

                {/* Leaderboard Table */}
                <div className="leaderboard-table-container">
                    {leaderboardData.length > 0 ? (
                        <table className="leaderboard-table">
                            <thead>
                                <tr>
                                    <th className="rank-column">Rank</th>
                                    <th className="player-column">Player</th>
                                    <th className="score-column">Score</th>
                                    <th className="games-column">Games</th>
                                    <th className="wins-column">Wins</th>
                                    <th className="winrate-column">Win Rate</th>
                                </tr>
                            </thead>
                            <tbody>
                                {leaderboardData.map((player, index) => {
                                    const rankDisplay = getRankDisplay(player.rank);
                                    return (
                                        <tr key={index} className={`player-row ${rankDisplay.class}`}>
                                            <td className="rank-cell">
                                                <span className="rank-display">
                                                    {rankDisplay.emoji}
                                                </span>
                                            </td>
                                            <td className="player-cell">
                                                <div className="player-info">
                                                    <span className="player-name">
                                                        {player.username}
                                                    </span>
                                                </div>
                                            </td>
                                            <td className="score-cell">
                                                <span className="score-value">
                                                    {player.total_score.toLocaleString()}
                                                </span>
                                            </td>
                                            <td className="games-cell">
                                                <span className="games-value">
                                                    {player.games_played}
                                                </span>
                                            </td>
                                            <td className="wins-cell">
                                                <span className="wins-value">
                                                    {player.games_won}
                                                </span>
                                            </td>
                                            <td className="winrate-cell">
                                                <div className="winrate-display">
                                                    <span className="winrate-value">
                                                        {player.win_percentage.toFixed(1)}%
                                                    </span>
                                                    <div className="winrate-bar">
                                                        <div 
                                                            className="winrate-fill"
                                                            style={{ width: `${player.win_percentage}%` }}
                                                        ></div>
                                                    </div>
                                                </div>
                                            </td>
                                        </tr>
                                    );
                                })}
                            </tbody>
                        </table>
                    ) : (
                        <div className="no-data-message">
                            <div className="no-data-icon">📊</div>
                            <h3>No Players Found</h3>
                            <p>No players have completed any games yet.</p>
                        </div>
                    )}
                </div>

                {/* Leaderboard Footer */}
                <div className="leaderboard-footer">
                    <div className="footer-info">
                        <p>
                            Showing top {Math.min(limit, leaderboardData.length)} of {totalPlayers} players
                        </p>
                        <p className="footer-note">
                            Rankings are updated in real-time based on game results
                        </p>
                    </div>
                    
                    <div className="footer-actions">
                        <button 
                            className="action-btn primary"
                            onClick={() => navigate('/game')}
                        >
                            🎮 Play Game
                        </button>
                        <button 
                            className="action-btn secondary"
                            onClick={() => navigate('/profile')}
                        >
                            👤 My Profile
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
        </div>
    );
};

export default Leaderboard;
