// ============================================================================
// PROTECTED ROUTE COMPONENT - Authentication Guard
// ============================================================================
// This component protects routes that require authentication by checking
// for a valid JWT token and redirecting unauthenticated users to login.
// ============================================================================

import React from 'react';
import { Navigate, useLocation } from 'react-router-dom';
import { isAuthenticated } from '../services/api';

/**
 * ProtectedRoute component that guards authenticated routes
 * @param {Object} props - Component props
 * @param {React.ReactNode} props.children - Child components to render if authenticated
 * @returns {React.ReactElement} Protected content or redirect to login
 */
const ProtectedRoute = ({ children }) => {
    const location = useLocation();
    
    // Check if user is authenticated
    const authenticated = isAuthenticated();
    
    if (!authenticated) {
        console.log('🔒 Access denied - redirecting to login');
        
        // Redirect to login page with return URL
        return (
            <Navigate 
                to="/login" 
                state={{ 
                    from: location.pathname,
                    message: 'Please log in to access this page.' 
                }} 
                replace 
            />
        );
    }
    
    // User is authenticated, render the protected content
    console.log('✅ Access granted - user authenticated');
    return children;
};

export default ProtectedRoute;
