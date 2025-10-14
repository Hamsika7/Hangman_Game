// ============================================================================
// HANGMAN VISUAL COMPONENT - Cartoon-Style Character with Filled Shapes
// ============================================================================
// This component renders a cartoon-style hangman character using filled SVG shapes.
// Features include:
// - Solid gallows structure with thick filled rectangles
// - Cartoon character with clothing and skin tones
// - Dynamic facial expressions based on game progression
// - Smooth animations for part appearance
// ============================================================================

import React from 'react';
import './HangmanVisual.css';

/**
 * Cartoon-Style HangmanVisual Component
 * 
 * Renders a cartoon hangman character with filled shapes and dynamic expressions.
 * Each wrong guess adds a new part with smooth animations.
 * 
 * Design Specifications:
 * - Gallows: Thick dark grey/slate filled rectangles with rounded ends
 * - Character: Filled shapes with clothing and skin tones
 * - Shirt: Pastel blue/purple (#667eea) with rounded corners
 * - Pants/Limbs: Dark slate grey (#4a5568)
 * - Skin: Light skin tone (#f3d6c4)
 * 
 * Visual Progression (7 stages based on wrongGuesses):
 * - Stage 1: Head with neutral/happy expression
 * - Stage 2: Body (shirt)
 * - Stage 3: Left arm
 * - Stage 4: Right arm
 * - Stage 5: Left leg
 * - Stage 6: Right leg
 * - Stage 7: Defeated expression (XX eyes, frown)
 * 
 * Facial Expressions:
 * - 0-3 wrong guesses: Neutral/Happy (dot eyes, slight smile)
 * - 4-5 wrong guesses: Worried/Concerned (larger eyes, neutral mouth)
 * - 6+ wrong guesses: Defeated (XX eyes, frown mouth)
 * 
 * @param {Object} props - Component props
 * @param {number} props.guessesLeft - Number of remaining guesses (0-7)
 * @param {string} props.gameStatus - Game status ('active', 'won', 'lost')
 */
const HangmanVisual = ({ guessesLeft, gameStatus = 'active' }) => {
    // Calculate wrong guesses (0-7)
    const wrongGuesses = 7 - guessesLeft;
    
    // Helper function to determine if a part should be visible
    const shouldShowPart = (stage) => wrongGuesses >= stage;
    
    // Determine facial expression based on wrong guesses
    const getFacialExpression = () => {
        if (wrongGuesses >= 6) return 'defeated'; // 6+ wrong guesses
        if (wrongGuesses >= 4) return 'worried';  // 4-5 wrong guesses
        return 'happy'; // 0-3 wrong guesses
    };
    
    return (
        <div className="hangman-visual-container">
            <svg 
                width="240" 
                height="300" 
                viewBox="0 0 240 300" 
                className="hangman-svg"
                role="img"
                aria-label={`Hangman figure with ${wrongGuesses} parts drawn`}
            >
                {/* Solid Gallows Structure - Always Visible */}
                <g className="gallows">
                    {/* Base Platform - Thick filled rectangle */}
                    <rect 
                        x="10" y="260" 
                        width="180" height="12" 
                        fill="#475569" 
                        rx="6"
                        className="gallows-base"
                    />
                    
                    {/* Vertical Post - Thick filled rectangle */}
                    <rect 
                        x="35" y="20" 
                        width="12" height="250" 
                        fill="#475569" 
                        rx="6"
                        className="gallows-post"
                    />
                    
                    {/* Horizontal Beam - Thick filled rectangle */}
                    <rect 
                        x="35" y="20" 
                        width="100" height="12" 
                        fill="#475569" 
                        rx="6"
                        className="gallows-beam"
                    />
                    
                    {/* Support Brace - Angled filled rectangle */}
                    <polygon 
                        points="47,32 65,20 71,26 53,38" 
                        fill="#475569"
                        className="gallows-brace"
                    />
                    
                    {/* Noose - Simple filled rectangle */}
                    <rect 
                        x="129" y="32" 
                        width="6" height="25" 
                        fill="#475569" 
                        rx="3"
                        className="gallows-noose"
                    />
                </g>

                {/* Cartoon Hangman Character - Matching Reference Image */}
                <g className="hangman-character">
                    {/* Stage 1: Head - Shows after 1 wrong guess */}
                    {shouldShowPart(1) && (
                        <g className="character-head animate-fade-in">
                            {/* Head - Light skin tone filled circle */}
                            <circle 
                                cx="132" 
                                cy="75" 
                                r="18" 
                                fill="#f3d6c4"
                                stroke="#e2b89f"
                                strokeWidth="1"
                                className="head-shape"
                            />
                            
                            {/* Dynamic Facial Expression */}
                            {getFacialExpression() === 'happy' && (
                                <g className="face-happy">
                                    {/* Happy eyes - small dots */}
                                    <circle cx="127" cy="72" r="2" fill="#2d3748" />
                                    <circle cx="137" cy="72" r="2" fill="#2d3748" />
                                    {/* Happy smile */}
                                    <path 
                                        d="M 125 80 Q 132 85 139 80" 
                                        fill="none" 
                                        stroke="#2d3748" 
                                        strokeWidth="1.5"
                                        strokeLinecap="round"
                                    />
                                </g>
                            )}
                            
                            {getFacialExpression() === 'worried' && (
                                <g className="face-worried">
                                    {/* Worried eyes - larger, concerned */}
                                    <ellipse cx="127" cy="72" rx="2.5" ry="3" fill="#2d3748" />
                                    <ellipse cx="137" cy="72" rx="2.5" ry="3" fill="#2d3748" />
                                    {/* Neutral mouth */}
                                    <line x1="128" y1="80" x2="136" y2="80" stroke="#2d3748" strokeWidth="1.5" strokeLinecap="round" />
                                </g>
                            )}
                            
                            {getFacialExpression() === 'defeated' && (
                                <g className="face-defeated">
                                    {/* XX eyes - matching reference image */}
                                    <g className="x-eyes">
                                        <line x1="124" y1="69" x2="130" y2="75" stroke="#2d3748" strokeWidth="2" strokeLinecap="round"/>
                                        <line x1="130" y1="69" x2="124" y2="75" stroke="#2d3748" strokeWidth="2" strokeLinecap="round"/>
                                        <line x1="134" y1="69" x2="140" y2="75" stroke="#2d3748" strokeWidth="2" strokeLinecap="round"/>
                                        <line x1="140" y1="69" x2="134" y2="75" stroke="#2d3748" strokeWidth="2" strokeLinecap="round"/>
                                    </g>
                                    {/* Frown mouth */}
                                    <path 
                                        d="M 125 82 Q 132 78 139 82" 
                                        fill="none" 
                                        stroke="#2d3748" 
                                        strokeWidth="1.5"
                                        strokeLinecap="round"
                                    />
                                </g>
                            )}
                        </g>
                    )}
                    
                    {/* Stage 2: Body (Shirt) - Shows after 2 wrong guesses */}
                    {shouldShowPart(2) && (
                        <g className="character-body animate-fade-in">
                            {/* Shirt - Pastel blue matching reference */}
                            <rect 
                                x="118" y="93" 
                                width="28" height="45" 
                                fill="#667eea"
                                rx="5"
                                className="shirt"
                            />
                            {/* Shirt collar/neckline */}
                            <path 
                                d="M 126 93 Q 132 98 138 93" 
                                fill="#5a67d8"
                                className="collar"
                            />
                        </g>
                    )}
                    
                    {/* Stage 3: Left Arm - Shows after 3 wrong guesses */}
                    {shouldShowPart(3) && (
                        <g className="character-left-arm animate-fade-in">
                            {/* Left sleeve */}
                            <rect 
                                x="100" y="105" 
                                width="18" height="12" 
                                fill="#667eea"
                                rx="6"
                                className="left-sleeve"
                            />
                            {/* Left arm */}
                            <rect 
                                x="92" y="117" 
                                width="10" height="20" 
                                fill="#f3d6c4"
                                rx="5"
                                className="left-arm"
                            />
                            {/* Left hand */}
                            <circle 
                                cx="97" 
                                cy="142" 
                                r="5" 
                                fill="#f3d6c4"
                                className="left-hand"
                            />
                        </g>
                    )}
                    
                    {/* Stage 4: Right Arm - Shows after 4 wrong guesses */}
                    {shouldShowPart(4) && (
                        <g className="character-right-arm animate-fade-in">
                            {/* Right sleeve */}
                            <rect 
                                x="146" y="105" 
                                width="18" height="12" 
                                fill="#667eea"
                                rx="6"
                                className="right-sleeve"
                            />
                            {/* Right arm */}
                            <rect 
                                x="162" y="117" 
                                width="10" height="20" 
                                fill="#f3d6c4"
                                rx="5"
                                className="right-arm"
                            />
                            {/* Right hand */}
                            <circle 
                                cx="167" 
                                cy="142" 
                                r="5" 
                                fill="#f3d6c4"
                                className="right-hand"
                            />
                        </g>
                    )}
                    
                    {/* Stage 5: Left Leg - Shows after 5 wrong guesses */}
                    {shouldShowPart(5) && (
                        <g className="character-left-leg animate-fade-in">
                            {/* Left pants leg */}
                            <rect 
                                x="122" y="138" 
                                width="10" height="30" 
                                fill="#4a5568"
                                rx="5"
                                className="left-pants"
                            />
                            {/* Left shoe */}
                            <ellipse 
                                cx="127" 
                                cy="175" 
                                rx="7" ry="4" 
                                fill="#2d3748"
                                className="left-shoe"
                            />
                        </g>
                    )}
                    
                    {/* Stage 6: Right Leg - Shows after 6 wrong guesses */}
                    {shouldShowPart(6) && (
                        <g className="character-right-leg animate-fade-in">
                            {/* Right pants leg */}
                            <rect 
                                x="132" y="138" 
                                width="10" height="30" 
                                fill="#4a5568"
                                rx="5"
                                className="right-pants"
                            />
                            {/* Right shoe */}
                            <ellipse 
                                cx="137" 
                                cy="175" 
                                rx="7" ry="4" 
                                fill="#2d3748"
                                className="right-shoe"
                            />
                        </g>
                    )}
                </g>

            </svg>
            
            {/* High-Impact Status Badge for Game End */}
            {gameStatus === 'lost' && (
                <div className="status-badge game-over">
                    GAME OVER
                </div>
            )}
            {gameStatus === 'won' && (
                <div className="status-badge victory">
                    VICTORY!
                </div>
            )}
        </div>
    );
};

export default HangmanVisual;
