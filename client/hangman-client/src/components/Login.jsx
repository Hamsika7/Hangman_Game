// ============================================================================
// LOGIN COMPONENT - User Authentication Form
// ============================================================================
// This component handles user login with JWT token management and navigation.
// It stores the JWT token in localStorage and redirects to the game page.
// ============================================================================

import React, { useState, useEffect } from 'react';
import { Link, useNavigate, useLocation } from 'react-router-dom';
import { authAPI, handleAPIError, isAuthenticated } from '../services/api';
import './Auth.css';

const Login = () => {
    // ========================================================================
    // STATE MANAGEMENT
    // ========================================================================
    
    // Form input states using useState hooks
    const [formData, setFormData] = useState({
        username: '',
        password: ''
    });
    
    // UI state management
    const [isLoading, setIsLoading] = useState(false);
    const [message, setMessage] = useState({ type: '', text: '' });
    const [showPassword, setShowPassword] = useState(false);
    const [rememberMe, setRememberMe] = useState(false);
    
    // Navigation and location hooks
    const navigate = useNavigate();
    const location = useLocation();

    // ========================================================================
    // LIFECYCLE EFFECTS
    // ========================================================================

    useEffect(() => {
        // Check if user is already authenticated
        if (isAuthenticated()) {
            console.log('👤 User already authenticated, redirecting to game...');
            navigate('/game');
            return;
        }

        // Check for messages from registration or other pages
        if (location.state?.message) {
            setMessage({
                type: 'success',
                text: location.state.message
            });
            
            // Clear the message from location state
            window.history.replaceState({}, document.title);
        }
    }, [navigate, location.state]);

    // ========================================================================
    // FORM HANDLING FUNCTIONS
    // ========================================================================

    /**
     * Handle input field changes and update state
     * @param {Event} e - Input change event
     */
    const handleInputChange = (e) => {
        const { name, value } = e.target;
        setFormData(prev => ({
            ...prev,
            [name]: value
        }));
        
        // Clear any existing messages when user starts typing
        if (message.text) {
            setMessage({ type: '', text: '' });
        }
    };

    /**
     * Validate form data before submission
     * @returns {Object} Validation result with isValid flag and errors
     */
    const validateForm = () => {
        const errors = [];
        
        // Username validation
        if (!formData.username.trim()) {
            errors.push('Username is required');
        }
        
        // Password validation
        if (!formData.password) {
            errors.push('Password is required');
        }
        
        return {
            isValid: errors.length === 0,
            errors: errors
        };
    };

    /**
     * Handle form submission and API call
     * @param {Event} e - Form submission event
     */
    const handleSubmit = async (e) => {
        // Prevent default form submission behavior
        e.preventDefault();
        
        // Validate form data
        const validation = validateForm();
        if (!validation.isValid) {
            setMessage({
                type: 'error',
                text: validation.errors.join('. ')
            });
            return;
        }
        
        // Set loading state to show spinner/disable form
        setIsLoading(true);
        setMessage({ type: '', text: '' });
        
        try {
            // Make API call to login endpoint
            console.log('🔄 Attempting to login user:', formData.username);
            
            const response = await authAPI.login({
                username: formData.username,
                password: formData.password
            });
            
            console.log('✅ Login successful:', response);
            
            // The authAPI.login function automatically stores the JWT token
            // and user data in localStorage, so we don't need to do it manually
            
            // Show success message briefly
            setMessage({
                type: 'success',
                text: `Welcome back, ${response.data.user.username}!`
            });
            
            // Clear form data for security
            setFormData({
                username: '',
                password: ''
            });
            
            // Navigate to game page after successful login
            console.log('🎮 Redirecting to game page...');
            setTimeout(() => {
                navigate('/game', { 
                    state: { 
                        welcomeMessage: `Welcome back, ${response.data.user.username}!` 
                    }
                });
            }, 1500);
            
        } catch (error) {
            console.error('❌ Login failed:', error);
            
            // Handle API errors with user-friendly messages
            const errorMessage = handleAPIError(error);
            setMessage({
                type: 'error',
                text: errorMessage
            });
            
            // Clear password field on failed login for security
            setFormData(prev => ({
                ...prev,
                password: ''
            }));
        } finally {
            // Always reset loading state
            setIsLoading(false);
        }
    };

    /**
     * Handle demo mode access (no authentication required)
     */
    const handleDemoMode = () => {
        console.log('🎮 Entering demo mode...');
        navigate('/demo');
    };

    // ========================================================================
    // RENDER COMPONENT
    // ========================================================================

    return (
        <div className="auth-container">
            <div className="auth-card">
                {/* Header Section */}
                <div className="auth-header">
                    <h1 className="auth-title">Welcome Back!</h1>
                    <p className="auth-subtitle">
                        Sign in to continue your Hangman adventure
                    </p>
                </div>

                {/* Login Form */}
                <form onSubmit={handleSubmit} className="auth-form">
                    {/* Username Input */}
                    <div className="form-group">
                        <label htmlFor="username" className="form-label">
                            Username
                        </label>
                        <input
                            type="text"
                            id="username"
                            name="username"
                            value={formData.username}
                            onChange={handleInputChange}
                            className="form-input"
                            placeholder="Enter your username"
                            disabled={isLoading}
                            autoComplete="username"
                            autoFocus
                            required
                        />
                    </div>

                    {/* Password Input */}
                    <div className="form-group">
                        <label htmlFor="password" className="form-label">
                            Password
                        </label>
                        <div className="password-input-container">
                            <input
                                type={showPassword ? 'text' : 'password'}
                                id="password"
                                name="password"
                                value={formData.password}
                                onChange={handleInputChange}
                                className="form-input"
                                placeholder="Enter your password"
                                disabled={isLoading}
                                autoComplete="current-password"
                                required
                            />
                            <button
                                type="button"
                                className="password-toggle"
                                onClick={() => setShowPassword(!showPassword)}
                                disabled={isLoading}
                            >
                                {showPassword ? '👁️' : '👁️‍🗨️'}
                            </button>
                        </div>
                    </div>

                    {/* Remember Me Checkbox */}
                    <div className="form-group form-group-checkbox">
                        <label className="checkbox-label">
                            <input
                                type="checkbox"
                                checked={rememberMe}
                                onChange={(e) => setRememberMe(e.target.checked)}
                                disabled={isLoading}
                            />
                            <span className="checkbox-text">Remember me</span>
                        </label>
                        <Link to="/forgot-password" className="forgot-link">
                            Forgot password?
                        </Link>
                    </div>

                    {/* Message Display */}
                    {message.text && (
                        <div className={`message message-${message.type}`}>
                            {message.type === 'success' ? '✅' : '❌'} {message.text}
                        </div>
                    )}

                    {/* Submit Button */}
                    <button
                        type="submit"
                        className={`auth-button ${isLoading ? 'loading' : ''}`}
                        disabled={isLoading}
                    >
                        {isLoading ? (
                            <>
                                <span className="spinner"></span>
                                Signing In...
                            </>
                        ) : (
                            'Sign In'
                        )}
                    </button>
                </form>

                {/* Alternative Options */}
                <div className="auth-divider">
                    <span>or</span>
                </div>

                <button
                    type="button"
                    className="demo-button"
                    onClick={handleDemoMode}
                    disabled={isLoading}
                >
                    🎮 Try Demo Mode
                </button>

                {/* Footer Links */}
                <div className="auth-footer">
                    <p>
                        Don't have an account?{' '}
                        <Link to="/register" className="auth-link">
                            Create one here
                        </Link>
                    </p>
                    <p>
                        <Link to="/" className="auth-link">
                            ← Back to Home
                        </Link>
                    </p>
                </div>
            </div>

            {/* JWT Token Information (Development Only) */}
            {process.env.NODE_ENV === 'development' && (
                <div className="dev-info">
                    <h4>🔧 Development Info</h4>
                    <p><strong>API Endpoint:</strong> POST /api/auth/login</p>
                    <p><strong>Token Storage:</strong> localStorage['hangman_token']</p>
                    <p><strong>User Data:</strong> localStorage['hangman_user']</p>
                </div>
            )}
        </div>
    );
};

export default Login;
