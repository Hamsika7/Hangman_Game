// ============================================================================
// GAME COMPONENT - Main Hangman Game Interface
// ============================================================================
// This component handles the core game logic, state management, and API integration.
// It provides a complete hangman game experience with visual feedback and scoring.
// ============================================================================

import React, { useState, useEffect } from 'react';
import { useNavigate, useLocation } from 'react-router-dom';
import { gameAPI, handleAPIError, isAuthenticated, getCurrentUser } from '../services/api';
import './Game.css';

const Game = () => {
    // ========================================================================
    // STATE MANAGEMENT
    // ========================================================================
    
    // Game state variables using useState hooks
    const [gameId, setGameId] = useState(null);
    const [maskedWord, setMaskedWord] = useState('');
    const [guessesLeft, setGuessesLeft] = useState(7);
    const [guessedLetters, setGuessedLetters] = useState(new Set());
    const [message, setMessage] = useState('');
    const [actualWord, setActualWord] = useState(''); // For end game display
    const [gameStatus, setGameStatus] = useState('active'); // 'active', 'won', 'lost'
    const [score, setScore] = useState(0);
    
    // UI state management
    const [isLoading, setIsLoading] = useState(false);
    const [isGuessing, setIsGuessing] = useState(false);
    const [showHint, setShowHint] = useState(false);
    const [hintLetter, setHintLetter] = useState('');
    
    // User information
    const [user, setUser] = useState(null);
    
    // Navigation hooks
    const navigate = useNavigate();
    const location = useLocation();

    // ========================================================================
    // LIFECYCLE EFFECTS
    // ========================================================================

    useEffect(() => {
        // Check authentication on component mount
        if (!isAuthenticated()) {
            console.log('🔒 User not authenticated, redirecting to login');
            navigate('/login', { 
                state: { 
                    from: '/game',
                    message: 'Please log in to play the game.' 
                }
            });
            return;
        }

        // Get current user information
        const currentUser = getCurrentUser();
        setUser(currentUser);

        // Check for welcome message from login
        if (location.state?.welcomeMessage) {
            setMessage(location.state.welcomeMessage);
            // Clear the message from location state
            window.history.replaceState({}, document.title);
        }

        // Automatically start a new game when component loads
        startNewGame();
    }, [navigate, location.state]);

    // ========================================================================
    // GAME MANAGEMENT FUNCTIONS
    // ========================================================================

    /**
     * Start a new game by making API call to create new game session
     */
    const startNewGame = async () => {
        setIsLoading(true);
        setMessage('Starting new game...');
        
        try {
            console.log('🎮 Starting new game for user:', user?.username);
            
            // Make authenticated POST request to start new game
            const response = await gameAPI.startNewGame();
            
            console.log('✅ New game started:', response);
            
            // Update component state with game data from API response
            const gameData = response.data;
            
            // IMPORTANT: Save the game_id from API response for future guess requests
            // The server returns 'game_id' not 'id' in the response
            setGameId(gameData.game_id);
            setMaskedWord(gameData.masked_word);
            setGuessesLeft(gameData.attempts_left);
            setGuessedLetters(new Set()); // Reset guessed letters
            setGameStatus('active');
            setScore(gameData.score || 0);
            setMessage('New game started! Guess a letter to begin.');
            setShowHint(false);
            setHintLetter('');
            
            console.log('🎮 Game state updated:', {
                gameId: gameData.game_id,
                maskedWord: gameData.masked_word,
                attemptsLeft: gameData.attempts_left
            });
            
        } catch (error) {
            console.error('❌ Failed to start new game:', error);
            const errorMessage = handleAPIError(error);
            setMessage(`Failed to start game: ${errorMessage}`);
        } finally {
            setIsLoading(false);
        }
    };

    /**
     * Handle user's letter guess with API integration
     * @param {string} letter - The guessed letter
     */
    const handleGuess = async (letter) => {
        // Validate input
        if (!letter || typeof letter !== 'string' || letter.length !== 1) {
            setMessage('Please enter a valid single letter.');
            return;
        }

        // Convert to lowercase for consistency
        letter = letter.toLowerCase();

        // CRITICAL: Check if gameId exists - prevent guessing without an active game
        if (!gameId) {
            setMessage('Please start a new game before making guesses.');
            return;
        }

        // Check if letter already guessed
        if (guessedLetters.has(letter)) {
            setMessage(`You already guessed '${letter.toUpperCase()}'. Try a different letter.`);
            return;
        }

        // Check if game is still active
        if (gameStatus !== 'active') {
            setMessage('Game is over. Start a new game to continue playing.');
            return;
        }

        setIsGuessing(true);
        setMessage(`Checking guess: ${letter.toUpperCase()}...`);
        
        try {
            console.log(`🎯 User guessing '${letter}' in game ${gameId}`);
            
            // IMPORTANT: Prepare request body with gameId and guess letter
            // Both game_id and guess are required by the backend API
            const requestBody = {
                game_id: gameId,  // Current game session ID from state
                guess: letter     // The letter being guessed
            };
            
            console.log('📤 Sending guess request:', requestBody);
            
            // Make authenticated POST request to submit guess
            const response = await gameAPI.makeGuess(requestBody);
            
            console.log('✅ Guess processed:', response);
            
            // Update component state with data from API response
            const gameData = response.data;
            
            // Update masked word display
            setMaskedWord(gameData.masked_word);
            
            // Update remaining attempts
            setGuessesLeft(gameData.attempts_left);
            
            // Add letter to guessed letters set
            setGuessedLetters(prev => new Set([...prev, letter]));
            
            // Update game status and score
            setGameStatus(gameData.status);
            setScore(gameData.score || 0);
            
            // Handle different game outcomes
            if (gameData.status === 'won') {
                setActualWord(gameData.word);
                setMessage(`🎉 Congratulations! You won! The word was "${gameData.word.toUpperCase()}". Score: ${gameData.score}`);
            } else if (gameData.status === 'lost') {
                setActualWord(gameData.word);
                setMessage(`💀 Game over! The word was "${gameData.word.toUpperCase()}". Better luck next time!`);
            } else {
                // Game still active - provide feedback on the guess
                const isCorrect = gameData.correct_guess;
                if (isCorrect) {
                    setMessage(`✅ Great guess! '${letter.toUpperCase()}' is in the word.`);
                } else {
                    setMessage(`❌ Sorry, '${letter.toUpperCase()}' is not in the word. ${gameData.attempts_left} guesses left.`);
                }
            }
            
        } catch (error) {
            console.error('❌ Failed to process guess:', error);
            const errorMessage = handleAPIError(error);
            setMessage(`Failed to process guess: ${errorMessage}`);
        } finally {
            setIsGuessing(false);
        }
    };

    /**
     * Get AI hint for the current game
     */
    const getHint = async () => {
        if (!gameId || gameStatus !== 'active') {
            setMessage('No active game for hints.');
            return;
        }

        setIsLoading(true);
        
        try {
            console.log('💡 Getting AI hint for game:', gameId);
            
            const response = await gameAPI.getHint(gameId);
            const hintData = response.data;
            
            if (hintData.suggestion) {
                setHintLetter(hintData.suggestion.toUpperCase());
                setShowHint(true);
                setMessage(`💡 AI suggests trying the letter: ${hintData.suggestion.toUpperCase()}`);
            } else {
                setMessage('No hints available at this time.');
            }
            
        } catch (error) {
            console.error('❌ Failed to get hint:', error);
            const errorMessage = handleAPIError(error);
            setMessage(`Failed to get hint: ${errorMessage}`);
        } finally {
            setIsLoading(false);
        }
    };

    /**
     * Handle keyboard input for guessing
     * @param {KeyboardEvent} event - Keyboard event
     */
    const handleKeyPress = (event) => {
        if (event.key.match(/[a-zA-Z]/) && event.key.length === 1) {
            handleGuess(event.key.toLowerCase());
        }
    };

    // Add keyboard event listener
    useEffect(() => {
        window.addEventListener('keypress', handleKeyPress);
        return () => {
            window.removeEventListener('keypress', handleKeyPress);
        };
    }, [gameId, guessedLetters, gameStatus]); // Dependencies for handleKeyPress

    // ========================================================================
    // UTILITY FUNCTIONS
    // ========================================================================

    /**
     * Generate alphabet for virtual keyboard
     * @returns {Array} Array of letters A-Z
     */
    const getAlphabet = () => {
        return 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.split('');
    };

    /**
     * Get CSS class for letter button based on guess status
     * @param {string} letter - The letter to check
     * @returns {string} CSS class name
     */
    const getLetterButtonClass = (letter) => {
        const lowerLetter = letter.toLowerCase();
        if (guessedLetters.has(lowerLetter)) {
            // Check if the letter is in the masked word (correct guess)
            if (maskedWord.toLowerCase().includes(lowerLetter)) {
                return 'letter-btn guessed correct';
            } else {
                return 'letter-btn guessed incorrect';
            }
        }
        return 'letter-btn available';
    };

    /**
     * Get hangman visual representation based on remaining guesses
     * @param {number} guessesLeft - Number of guesses remaining
     * @returns {string} Hangman ASCII art or emoji
     */
    const getHangmanVisual = (guessesLeft) => {
        const hangmanStages = [
            '😵', // 0 guesses left - dead
            '😰', // 1 guess left - very worried
            '😨', // 2 guesses left - scared
            '😟', // 3 guesses left - worried
            '😐', // 4 guesses left - neutral
            '🙂', // 5 guesses left - slight smile
            '😊', // 6 guesses left - happy
            '😄'  // 7 guesses left - very happy
        ];
        return hangmanStages[guessesLeft] || '😄';
    };

    // ========================================================================
    // RENDER COMPONENT
    // ========================================================================

    return (
        <div className="game-container">
            <div className="game-content">
                {/* Game Header */}
                <div className="game-header">
                    <h1 className="game-title">🎮 Hangman Game</h1>
                    {user && (
                        <div className="user-info">
                            <span className="username">Playing as: {user.username}</span>
                            <span className="current-score">Score: {score}</span>
                        </div>
                    )}
                </div>

                {/* Game Status Display */}
                <div className="game-status">
                    <div className="hangman-visual">
                        <span className="hangman-emoji">{getHangmanVisual(guessesLeft)}</span>
                        <div className="guesses-info">
                            <span className="guesses-left">Guesses Left: {guessesLeft}</span>
                            <div className="guess-dots">
                                {Array.from({ length: 7 }, (_, i) => (
                                    <span 
                                        key={i} 
                                        className={`guess-dot ${i < (7 - guessesLeft) ? 'used' : 'available'}`}
                                    />
                                ))}
                            </div>
                        </div>
                    </div>
                </div>

                {/* Masked Word Display */}
                <div className="word-display">
                    {gameId && maskedWord ? (
                        <>
                            <div className="masked-word">
                                {maskedWord.split('').map((char, index) => (
                                    <span key={index} className={`word-letter ${char === '_' ? 'hidden' : 'revealed'}`}>
                                        {char === '_' ? '_' : char.toUpperCase()}
                                    </span>
                                ))}
                            </div>
                            {actualWord && gameStatus !== 'active' && (
                                <div className="actual-word">
                                    Word: <span className="word-reveal">{actualWord.toUpperCase()}</span>
                                </div>
                            )}
                        </>
                    ) : (
                        <div className="no-game-message">
                            <span className="no-game-icon">🎮</span>
                            <h3>No Active Game</h3>
                            <p>Click "New Game" to start playing!</p>
                        </div>
                    )}
                </div>

                {/* Message Display */}
                <div className="message-display">
                    <div className={`game-message ${gameStatus === 'won' ? 'win' : gameStatus === 'lost' ? 'lose' : 'active'}`}>
                        {message}
                    </div>
                </div>

                {/* Virtual Keyboard */}
                <div className="virtual-keyboard">
                    <h3>Click a letter or use your keyboard:</h3>
                    <div className="keyboard-grid">
                        {getAlphabet().map(letter => (
                            <button
                                key={letter}
                                className={getLetterButtonClass(letter)}
                                onClick={() => handleGuess(letter.toLowerCase())}
                                disabled={
                                    !gameId ||  // Disable if no active game
                                    guessedLetters.has(letter.toLowerCase()) || 
                                    gameStatus !== 'active' || 
                                    isGuessing || 
                                    isLoading
                                }
                            >
                                {letter}
                            </button>
                        ))}
                    </div>
                </div>

                {/* Guessed Letters Display */}
                <div className="guessed-letters">
                    <h4>Guessed Letters:</h4>
                    <div className="letters-list">
                        {guessedLetters.size > 0 ? (
                            Array.from(guessedLetters).sort().map(letter => (
                                <span 
                                    key={letter} 
                                    className={`guessed-letter ${maskedWord.toLowerCase().includes(letter) ? 'correct' : 'incorrect'}`}
                                >
                                    {letter.toUpperCase()}
                                </span>
                            ))
                        ) : (
                            <span className="no-guesses">No letters guessed yet</span>
                        )}
                    </div>
                </div>

                {/* Game Controls */}
                <div className="game-controls">
                    <button
                        className="control-btn new-game-btn"
                        onClick={startNewGame}
                        disabled={isLoading}
                    >
                        {isLoading ? (
                            <>
                                <span className="spinner"></span>
                                Starting...
                            </>
                        ) : (
                            <>
                                🎮 New Game
                            </>
                        )}
                    </button>

                    {gameStatus === 'active' && (
                        <button
                            className="control-btn hint-btn"
                            onClick={getHint}
                            disabled={isLoading || !gameId}
                        >
                            💡 Get Hint
                        </button>
                    )}

                    {showHint && hintLetter && (
                        <div className="hint-display">
                            <span className="hint-text">AI Suggestion: </span>
                            <span className="hint-letter">{hintLetter}</span>
                        </div>
                    )}
                </div>

                {/* Game Over Actions */}
                {gameStatus !== 'active' && (
                    <div className="game-over-actions">
                        <div className={`game-result ${gameStatus}`}>
                            {gameStatus === 'won' ? (
                                <div className="win-message">
                                    <span className="result-emoji">🎉</span>
                                    <h2>Congratulations!</h2>
                                    <p>You guessed the word correctly!</p>
                                    <p className="final-score">Final Score: {score}</p>
                                </div>
                            ) : (
                                <div className="lose-message">
                                    <span className="result-emoji">💀</span>
                                    <h2>Game Over!</h2>
                                    <p>Better luck next time!</p>
                                    <p className="final-score">Final Score: {score}</p>
                                </div>
                            )}
                        </div>
                    </div>
                )}

                {/* Instructions */}
                <div className="game-instructions">
                    <h4>How to Play:</h4>
                    <ul>
                        <li>🎯 Guess letters to reveal the hidden word</li>
                        <li>⌨️ Use your keyboard or click the virtual buttons</li>
                        <li>💡 Get AI hints when you're stuck</li>
                        <li>🏆 Complete words with fewer guesses for higher scores</li>
                        <li>❌ You have 7 wrong guesses before the game ends</li>
                    </ul>
                </div>
            </div>
        </div>
    );
};

export default Game;
