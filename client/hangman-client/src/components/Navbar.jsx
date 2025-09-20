// ============================================================================
// NAVBAR COMPONENT - Navigation Bar with Authentication
// ============================================================================
// This component provides navigation links and handles user authentication state.
// It shows different options based on whether the user is logged in or not.
// ============================================================================

import React, { useState, useEffect } from 'react';
import { Link, useNavigate, useLocation } from 'react-router-dom';
import { authAPI, isAuthenticated, getCurrentUser, clearAuthData } from '../services/api';
import './Navbar.css';

const Navbar = () => {
    // ========================================================================
    // STATE MANAGEMENT
    // ========================================================================
    
    // Authentication state
    const [user, setUser] = useState(null);
    const [isLoggedIn, setIsLoggedIn] = useState(false);
    const [isLoggingOut, setIsLoggingOut] = useState(false);
    
    // Mobile menu state
    const [isMobileMenuOpen, setIsMobileMenuOpen] = useState(false);
    
    // Navigation hooks
    const navigate = useNavigate();
    const location = useLocation();

    // ========================================================================
    // LIFECYCLE EFFECTS
    // ========================================================================

    useEffect(() => {
        // Check authentication status on component mount and route changes
        checkAuthStatus();
    }, [location.pathname]);

    useEffect(() => {
        // Close mobile menu when route changes
        setIsMobileMenuOpen(false);
    }, [location.pathname]);

    // ========================================================================
    // AUTHENTICATION FUNCTIONS
    // ========================================================================

    /**
     * Check current authentication status and update state
     */
    const checkAuthStatus = () => {
        const authenticated = isAuthenticated();
        const currentUser = getCurrentUser();
        
        setIsLoggedIn(authenticated);
        setUser(currentUser);
        
        console.log('🔍 Auth status check:', { authenticated, user: currentUser?.username });
    };

    /**
     * Handle user logout process
     */
    const handleLogout = async () => {
        setIsLoggingOut(true);
        
        try {
            console.log('👋 Logging out user:', user?.username);
            
            // Call logout API (this also clears localStorage)
            await authAPI.logout();
            
            // Update local state
            setIsLoggedIn(false);
            setUser(null);
            
            // Navigate to home page
            navigate('/', { 
                state: { 
                    message: 'You have been logged out successfully.' 
                }
            });
            
            console.log('✅ Logout successful');
            
        } catch (error) {
            console.error('❌ Logout error:', error);
            
            // Even if API call fails, clear local data
            clearAuthData();
            setIsLoggedIn(false);
            setUser(null);
            navigate('/');
        } finally {
            setIsLoggingOut(false);
        }
    };

    /**
     * Toggle mobile menu visibility
     */
    const toggleMobileMenu = () => {
        setIsMobileMenuOpen(!isMobileMenuOpen);
    };

    /**
     * Close mobile menu
     */
    const closeMobileMenu = () => {
        setIsMobileMenuOpen(false);
    };

    // ========================================================================
    // NAVIGATION HELPERS
    // ========================================================================

    /**
     * Check if current route is active
     * @param {string} path - Route path to check
     * @returns {boolean} True if route is active
     */
    const isActiveRoute = (path) => {
        return location.pathname === path;
    };

    /**
     * Get CSS classes for navigation links
     * @param {string} path - Route path
     * @returns {string} CSS class names
     */
    const getLinkClasses = (path) => {
        return `nav-link ${isActiveRoute(path) ? 'active' : ''}`;
    };

    // ========================================================================
    // RENDER COMPONENT
    // ========================================================================

    return (
        <nav className="navbar">
            <div className="nav-container">
                {/* Logo/Brand */}
                <Link to="/" className="nav-brand" onClick={closeMobileMenu}>
                    <span className="brand-icon">🎮</span>
                    <span className="brand-text">Hangman Game</span>
                </Link>

                {/* Desktop Navigation Links */}
                <div className={`nav-menu ${isMobileMenuOpen ? 'active' : ''}`}>
                    {/* Always visible links */}
                    <Link 
                        to="/" 
                        className={getLinkClasses('/')}
                        onClick={closeMobileMenu}
                    >
                        Home
                    </Link>

                    {/* Conditional links based on authentication status */}
                    {isLoggedIn ? (
                        // Authenticated user links
                        <>
                            <Link 
                                to="/game" 
                                className={getLinkClasses('/game')}
                                onClick={closeMobileMenu}
                            >
                                Play Game
                            </Link>
                            <Link 
                                to="/profile" 
                                className={getLinkClasses('/profile')}
                                onClick={closeMobileMenu}
                            >
                                Profile
                            </Link>
                            <Link 
                                to="/leaderboard" 
                                className={getLinkClasses('/leaderboard')}
                                onClick={closeMobileMenu}
                            >
                                Leaderboard
                            </Link>
                        </>
                    ) : (
                        // Guest user links
                        <>
                            <Link 
                                to="/demo" 
                                className={getLinkClasses('/demo')}
                                onClick={closeMobileMenu}
                            >
                                Demo
                            </Link>
                            <Link 
                                to="/login" 
                                className={getLinkClasses('/login')}
                                onClick={closeMobileMenu}
                            >
                                Login
                            </Link>
                            <Link 
                                to="/register" 
                                className={getLinkClasses('/register')}
                                onClick={closeMobileMenu}
                            >
                                Register
                            </Link>
                        </>
                    )}
                </div>

                {/* User Section */}
                <div className="nav-user">
                    {isLoggedIn ? (
                        // Authenticated user section
                        <div className="user-section">
                            <div className="user-info">
                                <span className="user-avatar">👤</span>
                                <span className="user-name">{user?.username}</span>
                                {user?.score !== undefined && (
                                    <span className="user-score">Score: {user.score}</span>
                                )}
                            </div>
                            <button
                                className={`logout-button ${isLoggingOut ? 'loading' : ''}`}
                                onClick={handleLogout}
                                disabled={isLoggingOut}
                            >
                                {isLoggingOut ? (
                                    <>
                                        <span className="spinner-small"></span>
                                        Logging out...
                                    </>
                                ) : (
                                    <>
                                        <span>Logout</span>
                                        <span className="logout-icon">🚪</span>
                                    </>
                                )}
                            </button>
                        </div>
                    ) : (
                        // Guest user section
                        <div className="guest-section">
                            <Link to="/login" className="nav-button nav-button-primary">
                                Login
                            </Link>
                            <Link to="/register" className="nav-button nav-button-secondary">
                                Sign Up
                            </Link>
                        </div>
                    )}
                </div>

                {/* Mobile Menu Toggle */}
                <button
                    className={`mobile-toggle ${isMobileMenuOpen ? 'active' : ''}`}
                    onClick={toggleMobileMenu}
                    aria-label="Toggle navigation menu"
                >
                    <span className="hamburger-line"></span>
                    <span className="hamburger-line"></span>
                    <span className="hamburger-line"></span>
                </button>
            </div>

            {/* Mobile Menu Overlay */}
            {isMobileMenuOpen && (
                <div className="mobile-overlay" onClick={closeMobileMenu}></div>
            )}
        </nav>
    );
};

export default Navbar;
