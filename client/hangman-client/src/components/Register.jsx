// ============================================================================
// REGISTER COMPONENT - User Registration Form
// ============================================================================
// This component handles user registration with form validation and API integration.
// It communicates with the Node.js backend to create new user accounts.
// ============================================================================

import React, { useState } from 'react';
import { Link, useNavigate } from 'react-router-dom';
import { authAPI, handleAPIError } from '../services/api';
import './Auth.css';

const Register = () => {
    // ========================================================================
    // STATE MANAGEMENT
    // ========================================================================
    
    // Form input states using useState hooks
    const [formData, setFormData] = useState({
        username: '',
        password: '',
        confirmPassword: ''
    });
    
    // UI state management
    const [isLoading, setIsLoading] = useState(false);
    const [message, setMessage] = useState({ type: '', text: '' });
    const [showPassword, setShowPassword] = useState(false);
    
    // Navigation hook for programmatic routing
    const navigate = useNavigate();

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
        } else if (formData.username.length < 3) {
            errors.push('Username must be at least 3 characters long');
        } else if (formData.username.length > 20) {
            errors.push('Username must be less than 20 characters');
        } else if (!/^[a-zA-Z0-9_]+$/.test(formData.username)) {
            errors.push('Username can only contain letters, numbers, and underscores');
        }
        
        // Password validation
        if (!formData.password) {
            errors.push('Password is required');
        } else if (formData.password.length < 6) {
            errors.push('Password must be at least 6 characters long');
        }
        
        // Confirm password validation
        if (formData.password !== formData.confirmPassword) {
            errors.push('Passwords do not match');
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
            // Make API call to register endpoint
            console.log('🔄 Attempting to register user:', formData.username);
            
            const response = await authAPI.register({
                username: formData.username,
                password: formData.password
            });
            
            console.log('✅ Registration successful:', response);
            
            // Show success message
            setMessage({
                type: 'success',
                text: 'Account created successfully! You can now log in.'
            });
            
            // Clear form data
            setFormData({
                username: '',
                password: '',
                confirmPassword: ''
            });
            
            // Redirect to login page after 2 seconds
            setTimeout(() => {
                navigate('/login', { 
                    state: { 
                        message: 'Registration successful! Please log in with your new account.' 
                    }
                });
            }, 2000);
            
        } catch (error) {
            console.error('❌ Registration failed:', error);
            
            // Handle API errors with user-friendly messages
            const errorMessage = handleAPIError(error);
            setMessage({
                type: 'error',
                text: errorMessage
            });
        } finally {
            // Always reset loading state
            setIsLoading(false);
        }
    };

    // ========================================================================
    // RENDER COMPONENT
    // ========================================================================

    return (
        <div className="auth-container">
            <div className="auth-card">
                {/* Header Section */}
                <div className="auth-header">
                    <h1 className="auth-title">Create Account</h1>
                    <p className="auth-subtitle">
                        Join the Hangman Game community and start playing!
                    </p>
                </div>

                {/* Registration Form */}
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
                            required
                        />
                        <small className="form-hint">
                            3-20 characters, letters, numbers, and underscores only
                        </small>
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
                                autoComplete="new-password"
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
                        <small className="form-hint">
                            At least 6 characters long
                        </small>
                    </div>

                    {/* Confirm Password Input */}
                    <div className="form-group">
                        <label htmlFor="confirmPassword" className="form-label">
                            Confirm Password
                        </label>
                        <input
                            type={showPassword ? 'text' : 'password'}
                            id="confirmPassword"
                            name="confirmPassword"
                            value={formData.confirmPassword}
                            onChange={handleInputChange}
                            className="form-input"
                            placeholder="Confirm your password"
                            disabled={isLoading}
                            autoComplete="new-password"
                            required
                        />
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
                                Creating Account...
                            </>
                        ) : (
                            'Create Account'
                        )}
                    </button>
                </form>

                {/* Footer Links */}
                <div className="auth-footer">
                    <p>
                        Already have an account?{' '}
                        <Link to="/login" className="auth-link">
                            Sign in here
                        </Link>
                    </p>
                    <p>
                        <Link to="/" className="auth-link">
                            ← Back to Home
                        </Link>
                    </p>
                </div>
            </div>
        </div>
    );
};

export default Register;
