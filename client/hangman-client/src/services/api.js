// ============================================================================
// API SERVICE - Backend Communication Layer
// ============================================================================
// This module handles all API calls to the Node.js backend server.
// It includes authentication, game management, and user profile endpoints.
// ============================================================================

import axios from 'axios';

// Base API configuration
const API_BASE_URL = import.meta.env.VITE_API_URL || 'http://localhost:3001';

console.log('🔧 API Configuration:', {
    baseURL: API_BASE_URL,
    environment: import.meta.env.MODE,
    allEnvVars: import.meta.env
});

// Create axios instance with default configuration
const apiClient = axios.create({
    baseURL: API_BASE_URL,
    headers: {
        'Content-Type': 'application/json',
    },
    timeout: 10000, // 10 seconds timeout
    withCredentials: true, // Important for CORS with credentials
});

// ============================================================================
// REQUEST/RESPONSE INTERCEPTORS
// ============================================================================

// Request interceptor to add auth token
apiClient.interceptors.request.use(
    (config) => {
        const token = localStorage.getItem('hangman_token');
        if (token) {
            config.headers.Authorization = `Bearer ${token}`;
        }
        return config;
    },
    (error) => {
        return Promise.reject(error);
    }
);

// Response interceptor for error handling
apiClient.interceptors.response.use(
    (response) => {
        console.log('✅ API Response:', {
            url: response.config.url,
            status: response.status,
            data: response.data
        });
        return response;
    },
    (error) => {
        console.error('❌ API Error:', {
            url: error.config?.url,
            status: error.response?.status,
            message: error.message,
            response: error.response?.data,
            code: error.code
        });
        
        if (error.response?.status === 401) {
            // Token expired or invalid
            localStorage.removeItem('hangman_token');
            localStorage.removeItem('hangman_user');
            window.location.href = '/login';
        }
        return Promise.reject(error);
    }
);

// ============================================================================
// AUTHENTICATION API
// ============================================================================

export const authAPI = {
    /**
     * Register a new user account
     * @param {Object} userData - User registration data
     * @param {string} userData.username - Username
     * @param {string} userData.password - Password
     * @returns {Promise} API response
     */
    register: async (userData) => {
        const response = await apiClient.post('/api/auth/register', userData);
        return response.data;
    },

    /**
     * Login user and get JWT token
     * @param {Object} credentials - Login credentials
     * @param {string} credentials.username - Username
     * @param {string} credentials.password - Password
     * @returns {Promise} API response with token and user data
     */
    login: async (credentials) => {
        const response = await apiClient.post('/api/auth/login', credentials);
        
        // Store token and user data in localStorage
        if (response.data.status === 'success') {
            localStorage.setItem('hangman_token', response.data.data.token);
            localStorage.setItem('hangman_user', JSON.stringify(response.data.data.user));
        }
        
        return response.data;
    },

    /**
     * Get current user profile
     * @returns {Promise} User profile data
     */
    getProfile: async () => {
        const response = await apiClient.get('/api/auth/profile');
        return response.data;
    },

    /**
     * Logout user (client-side token removal)
     * @returns {Promise} Logout confirmation
     */
    logout: async () => {
        try {
            await apiClient.post('/api/auth/logout');
        } catch (error) {
            console.warn('Logout API call failed, proceeding with local cleanup');
        } finally {
            // Always clean up local storage
            localStorage.removeItem('hangman_token');
            localStorage.removeItem('hangman_user');
        }
    },
};

// ============================================================================
// GAME API
// ============================================================================

export const gameAPI = {
    /**
     * Start a new game
     * @returns {Promise} New game data
     */
    startNewGame: async () => {
        const response = await apiClient.post('/api/game/new');
        return response.data;
    },

    /**
     * Make a letter guess
     * @param {Object} guessData - Guess information
     * @param {number} guessData.game_id - Game ID
     * @param {string} guessData.guess - Letter to guess
     * @returns {Promise} Updated game state
     */
    makeGuess: async (guessData) => {
        const response = await apiClient.post('/api/game/guess', guessData);
        return response.data;
    },

    /**
     * Get current game status
     * @param {number} gameId - Game ID
     * @returns {Promise} Game status data
     */
    getGameStatus: async (gameId) => {
        const response = await apiClient.get(`/api/game/status/${gameId}`);
        return response.data;
    },

    /**
     * Get game history for current user
     * @param {Object} params - Query parameters
     * @param {number} params.limit - Number of games to fetch
     * @param {number} params.offset - Offset for pagination
     * @returns {Promise} Game history data
     */
    getGameHistory: async (params = {}) => {
        const queryParams = new URLSearchParams(params).toString();
        const response = await apiClient.get(`/api/game/history?${queryParams}`);
        return response.data;
    },

    /**
     * Get AI hint for current game
     * @param {number} gameId - Game ID
     * @returns {Promise} AI suggestion
     */
    getHint: async (gameId) => {
        const response = await apiClient.get(`/api/game/hint/${gameId}`);
        return response.data;
    },

    /**
     * Check Prolog bridge health
     * @returns {Promise} Health status
     */
    checkHealth: async () => {
        const response = await apiClient.get('/api/game/prolog-health');
        return response.data;
    },
};

// ============================================================================
// USER API
// ============================================================================

export const userAPI = {
    /**
     * Get user statistics
     * @returns {Promise} User stats data
     */
    getStats: async () => {
        const response = await apiClient.get('/api/users/stats');
        return response.data;
    },

    /**
     * Get leaderboard
     * @param {Object} params - Query parameters
     * @param {number} params.limit - Number of users to fetch
     * @returns {Promise} Leaderboard data
     */
    getLeaderboard: async (params = {}) => {
        const queryParams = new URLSearchParams(params).toString();
        const response = await apiClient.get(`/api/users/leaderboard?${queryParams}`);
        return response.data;
    },
};

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

/**
 * Check if user is authenticated
 * @returns {boolean} Authentication status
 */
export const isAuthenticated = () => {
    const token = localStorage.getItem('hangman_token');
    return !!token;
};

/**
 * Get current user data from localStorage
 * @returns {Object|null} User data or null
 */
export const getCurrentUser = () => {
    const userStr = localStorage.getItem('hangman_user');
    return userStr ? JSON.parse(userStr) : null;
};

/**
 * Clear all authentication data
 */
export const clearAuthData = () => {
    localStorage.removeItem('hangman_token');
    localStorage.removeItem('hangman_user');
};

/**
 * Handle API errors with user-friendly messages
 * @param {Error} error - API error
 * @returns {string} User-friendly error message
 */
export const handleAPIError = (error) => {
    if (error.response) {
        // Server responded with error status
        const message = error.response.data?.message || 'An error occurred';
        return message;
    } else if (error.request) {
        // Request made but no response received
        return 'Unable to connect to server. Please check your internet connection.';
    } else {
        // Something else happened
        return 'An unexpected error occurred. Please try again.';
    }
};

// ============================================================================
// EXPORTS
// ============================================================================

export default {
    auth: authAPI,
    game: gameAPI,
    user: userAPI,
    utils: {
        isAuthenticated,
        getCurrentUser,
        clearAuthData,
        handleAPIError,
    },
};
