// ============================================================================
// MAIN APP COMPONENT - Hangman Game Application
// ============================================================================
// This is the root component that handles routing and global app state.
// It sets up the React Router for navigation between different pages and
// includes the navigation bar for consistent user experience.
// ============================================================================

import React from 'react';
import { BrowserRouter as Router, Routes, Route, useLocation } from 'react-router-dom';
import Navbar from './components/Navbar';
import Home from './components/Home';
import Login from './components/Login';
import Register from './components/Register';
import Game from './components/Game';
import Profile from './components/Profile';
import Leaderboard from './components/Leaderboard';
import ProtectedRoute from './components/ProtectedRoute';
import './App.css';

// Component to conditionally render navbar based on route
const AppContent = () => {
  const location = useLocation();
  
  // Routes where navbar should be hidden (full-screen pages)
  const hideNavbarRoutes = ['/'];
  const shouldHideNavbar = hideNavbarRoutes.includes(location.pathname);

  return (
    <div className="app">
      {/* Conditional Navbar - hidden on landing page for full-screen experience */}
      {!shouldHideNavbar && <Navbar />}
      
      {/* Main Application Routes */}
      <Routes>
        {/* Home/Landing Page */}
        <Route path="/" element={<Home />} />
        
        {/* Authentication Routes */}
        <Route path="/login" element={<Login />} />
        <Route path="/register" element={<Register />} />
        
        {/* Game Routes */}
        <Route path="/game" element={
          <ProtectedRoute>
            <Game />
          </ProtectedRoute>
        } />
        <Route path="/demo" element={<div className="coming-soon">🎯 Demo Mode - Coming Soon!</div>} />
        
        {/* Profile and Stats Routes */}
        <Route path="/profile" element={
          <ProtectedRoute>
            <Profile />
          </ProtectedRoute>
        } />
        <Route path="/leaderboard" element={<Leaderboard />} />
        
        {/* Additional Routes */}
        <Route path="/forgot-password" element={<div className="coming-soon">🔐 Password Reset - Coming Soon!</div>} />
        
        {/* 404 Not Found Route */}
        <Route path="*" element={
          <div className="not-found">
            <h1>404 - Page Not Found</h1>
            <p>The page you're looking for doesn't exist.</p>
            <a href="/">Go back to Home</a>
          </div>
        } />
      </Routes>
    </div>
  );
};

function App() {
  return (
    <Router>
      <AppContent />
    </Router>
  );
}

export default App;
