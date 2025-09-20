// ============================================================================
// PROLOG BRIDGE - Node.js to SWI-Prolog Communication
// ============================================================================
// This module provides a bridge between Node.js and SWI-Prolog, allowing
// the Express server to execute Prolog queries and receive structured results.
// It handles process management, output parsing, and error recovery.
// ============================================================================

const { spawn } = require('child_process');
const path = require('path');
const fs = require('fs');

// ============================================================================
// CONFIGURATION
// ============================================================================

// Prolog executable path (adjust if SWI-Prolog is not in PATH)
const PROLOG_EXECUTABLE = process.env.PROLOG_PATH || 'swipl';

// Path to Prolog files
const PROLOG_BASE_PATH = path.join(__dirname, '..', 'prolog');
const HANGMAN_LOGIC_FILE = path.join(PROLOG_BASE_PATH, 'hangman_logic.pl');
const AI_GUESSER_FILE = path.join(PROLOG_BASE_PATH, 'ai_guesser.pl');

// Query timeout in milliseconds
const QUERY_TIMEOUT = process.env.PROLOG_TIMEOUT || 10000;

// Maximum number of retry attempts
const MAX_RETRIES = 3;

// ============================================================================
// CORE PROLOG QUERY EXECUTION
// ============================================================================

/**
 * Execute a Prolog query and return structured results
 * @param {string} predicate - The Prolog predicate to execute (e.g., 'select_word')
 * @param {Array} args - Arguments for the predicate (optional)
 * @param {Object} options - Additional options for query execution
 * @returns {Promise<Object>} Structured result from Prolog query
 */
async function runPrologQuery(predicate, args = [], options = {}) {
    const startTime = Date.now();
    
    try {
        // Validate inputs
        validateQueryInputs(predicate, args);
        
        // Build the complete Prolog query
        const query = buildPrologQuery(predicate, args, options);
        
        console.log(`🔄 Executing Prolog query: ${query}`);
        
        // Execute the query with retry logic
        const result = await executeWithRetry(query, options);
        
        const executionTime = Date.now() - startTime;
        console.log(`✅ Prolog query completed in ${executionTime}ms`);
        
        return {
            success: true,
            result: result,
            executionTime: executionTime,
            query: query
        };
        
    } catch (error) {
        const executionTime = Date.now() - startTime;
        console.error(`❌ Prolog query failed after ${executionTime}ms:`, error.message);
        
        return {
            success: false,
            error: error.message,
            executionTime: executionTime,
            query: predicate
        };
    }
}

/**
 * Execute Prolog query with retry logic for reliability
 * @param {string} query - Complete Prolog query string
 * @param {Object} options - Query execution options
 * @returns {Promise<any>} Parsed result from Prolog
 */
async function executeWithRetry(query, options, attempt = 1) {
    try {
        return await executePrologQuery(query, options);
    } catch (error) {
        if (attempt < MAX_RETRIES && isRetryableError(error)) {
            console.log(`⚠️ Prolog query failed (attempt ${attempt}/${MAX_RETRIES}), retrying...`);
            await sleep(1000 * attempt); // Exponential backoff
            return executeWithRetry(query, options, attempt + 1);
        }
        throw error;
    }
}

/**
 * Core function to execute Prolog query using child process
 * @param {string} query - Complete Prolog query string
 * @param {Object} options - Query execution options
 * @returns {Promise<any>} Raw output from Prolog process
 */
function executePrologQuery(query, options = {}) {
    return new Promise((resolve, reject) => {
        // Verify Prolog files exist
        if (!fs.existsSync(HANGMAN_LOGIC_FILE)) {
            return reject(new Error(`Prolog file not found: ${HANGMAN_LOGIC_FILE}`));
        }
        
        // Build Prolog command arguments
        const prologArgs = [
            '-q',                           // Quiet mode (no banner)
            '-f', HANGMAN_LOGIC_FILE,      // Load hangman logic
            '-g', query,                   // Execute query
            '-t', 'halt'                   // Terminate after query
        ];
        
        // Add AI guesser if needed
        if (options.useAI) {
            prologArgs.splice(3, 0, '-f', AI_GUESSER_FILE);
        }
        
        console.log(`🚀 Starting Prolog process: ${PROLOG_EXECUTABLE} ${prologArgs.join(' ')}`);
        
        // Spawn Prolog process
        const prologProcess = spawn(PROLOG_EXECUTABLE, prologArgs, {
            stdio: ['pipe', 'pipe', 'pipe'],
            timeout: QUERY_TIMEOUT
        });
        
        let stdout = '';
        let stderr = '';
        let isResolved = false;
        
        // Set up timeout protection
        const timeoutId = setTimeout(() => {
            if (!isResolved) {
                isResolved = true;
                prologProcess.kill('SIGKILL');
                reject(new Error(`Prolog query timeout after ${QUERY_TIMEOUT}ms`));
            }
        }, QUERY_TIMEOUT);
        
        // Collect stdout data
        prologProcess.stdout.on('data', (data) => {
            stdout += data.toString();
        });
        
        // Collect stderr data
        prologProcess.stderr.on('data', (data) => {
            stderr += data.toString();
        });
        
        // Handle process completion
        prologProcess.on('close', (code) => {
            clearTimeout(timeoutId);
            
            if (isResolved) return; // Already handled by timeout
            isResolved = true;
            
            if (code === 0) {
                try {
                    // Parse and clean the output
                    const result = parseOutput(stdout, query);
                    resolve(result);
                } catch (parseError) {
                    reject(new Error(`Output parsing failed: ${parseError.message}`));
                }
            } else {
                const errorMsg = stderr.trim() || `Prolog process exited with code ${code}`;
                reject(new Error(errorMsg));
            }
        });
        
        // Handle process errors
        prologProcess.on('error', (error) => {
            clearTimeout(timeoutId);
            
            if (isResolved) return;
            isResolved = true;
            
            if (error.code === 'ENOENT') {
                reject(new Error('SWI-Prolog not found. Please install SWI-Prolog and ensure it\'s in your PATH.'));
            } else {
                reject(new Error(`Prolog process error: ${error.message}`));
            }
        });
    });
}

// ============================================================================
// QUERY BUILDING AND PARSING
// ============================================================================

/**
 * Build a complete Prolog query string from predicate and arguments
 * @param {string} predicate - Prolog predicate name
 * @param {Array} args - Arguments for the predicate
 * @param {Object} options - Query options
 * @returns {string} Complete Prolog query
 */
function buildPrologQuery(predicate, args, options) {
    // Handle different query types
    if (options.queryType === 'findall') {
        // For queries that need to find all solutions
        const variable = options.variable || 'X';
        const goal = args.length > 0 ? `${predicate}(${args.join(', ')}, ${variable})` : `${predicate}(${variable})`;
        return `findall(${variable}, ${goal}, Results), write_canonical(Results).`;
    }
    
    if (options.queryType === 'check') {
        // For boolean queries (true/false)
        const goal = args.length > 0 ? `${predicate}(${args.join(', ')})` : `${predicate}`;
        return `(${goal} -> write('true') ; write('false')).`;
    }
    
    // Default: single result query
    const argList = args.length > 0 ? `(${args.join(', ')}, Result)` : '(Result)';
    return `${predicate}${argList}, write_canonical(Result).`;
}

/**
 * Parse Prolog output into structured JavaScript data
 * @param {string} output - Raw output from Prolog process
 * @param {string} query - Original query for context
 * @returns {any} Parsed result
 */
function parseOutput(output, query) {
    const cleanOutput = output.trim();
    
    if (!cleanOutput) {
        throw new Error('Empty output from Prolog query');
    }
    
    // Handle different output formats
    if (cleanOutput === 'true' || cleanOutput === 'false') {
        return cleanOutput === 'true';
    }
    
    // Handle atom outputs (words, status messages)
    if (cleanOutput.match(/^[a-z][a-zA-Z0-9_]*$/)) {
        return cleanOutput;
    }
    
    // Handle quoted strings
    if (cleanOutput.startsWith("'") && cleanOutput.endsWith("'")) {
        return cleanOutput.slice(1, -1);
    }
    
    // Handle lists (for findall results)
    if (cleanOutput.startsWith('[') && cleanOutput.endsWith(']')) {
        try {
            // Convert Prolog list format to JSON
            const jsonString = cleanOutput
                .replace(/'/g, '"')           // Convert single quotes to double quotes
                .replace(/([a-zA-Z_][a-zA-Z0-9_]*)/g, '"$1"'); // Quote unquoted atoms
            return JSON.parse(jsonString);
        } catch (error) {
            // If JSON parsing fails, return as string
            return cleanOutput;
        }
    }
    
    // Handle numbers
    if (!isNaN(cleanOutput)) {
        return parseInt(cleanOutput, 10);
    }
    
    // Return as-is for other formats
    return cleanOutput;
}

// ============================================================================
// GAME-SPECIFIC HELPER FUNCTIONS
// ============================================================================

/**
 * Select a random word for a new game
 * @returns {Promise<string>} Selected word
 */
async function selectRandomWord() {
    const result = await runPrologQuery('select_word', []);
    if (result.success) {
        return result.result;
    }
    throw new Error(`Failed to select word: ${result.error}`);
}

/**
 * Create a masked version of a word based on guessed letters
 * @param {string} word - The target word
 * @param {Array<string>} guessedLetters - Letters that have been guessed
 * @returns {Promise<string>} Masked word (e.g., "_a_a_e")
 */
async function maskWord(word, guessedLetters = []) {
    const letterList = `[${guessedLetters.map(l => `'${l}'`).join(', ')}]`;
    const result = await runPrologQuery('mask_word', [`'${word}'`, letterList]);
    
    if (result.success) {
        return result.result;
    }
    throw new Error(`Failed to mask word: ${result.error}`);
}

/**
 * Check if a guessed letter is in the target word
 * @param {string} letter - The guessed letter
 * @param {string} word - The target word
 * @returns {Promise<string>} 'correct' or 'incorrect'
 */
async function checkGuess(letter, word) {
    const result = await runPrologQuery('check_guess', [`'${letter}'`, `'${word}'`]);
    
    if (result.success) {
        return result.result;
    }
    throw new Error(`Failed to check guess: ${result.error}`);
}

/**
 * Check if the game has been won (no underscores in masked word)
 * @param {string} maskedWord - Current masked word state
 * @returns {Promise<boolean>} True if game is won
 */
async function isGameWon(maskedWord) {
    const result = await runPrologQuery('is_game_won', [`'${maskedWord}'`], { queryType: 'check' });
    
    if (result.success) {
        return result.result;
    }
    throw new Error(`Failed to check win condition: ${result.error}`);
}

/**
 * Check if the game has been lost (no attempts remaining)
 * @param {number} attemptsLeft - Number of attempts remaining
 * @returns {Promise<boolean>} True if game is lost
 */
async function isGameLost(attemptsLeft) {
    const result = await runPrologQuery('is_game_lost', [attemptsLeft], { queryType: 'check' });
    
    if (result.success) {
        return result.result;
    }
    throw new Error(`Failed to check lose condition: ${result.error}`);
}

/**
 * Get AI suggestion for next best letter to guess
 * @param {string} maskedWord - Current masked word state
 * @param {Array<string>} guessedLetters - Letters already guessed
 * @returns {Promise<string>} Recommended letter
 */
async function getAISuggestion(maskedWord, guessedLetters = []) {
    const letterList = `[${guessedLetters.map(l => `'${l}'`).join(', ')}]`;
    const result = await runPrologQuery('ai_guess', [`'${maskedWord}'`, letterList], { useAI: true });
    
    if (result.success) {
        return result.result;
    }
    throw new Error(`Failed to get AI suggestion: ${result.error}`);
}

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

/**
 * Validate query inputs before execution
 * @param {string} predicate - Prolog predicate name
 * @param {Array} args - Query arguments
 */
function validateQueryInputs(predicate, args) {
    if (!predicate || typeof predicate !== 'string') {
        throw new Error('Predicate must be a non-empty string');
    }
    
    if (!Array.isArray(args)) {
        throw new Error('Arguments must be an array');
    }
    
    // Check for potentially dangerous characters
    if (predicate.includes(';') || predicate.includes('halt') || predicate.includes('!')) {
        throw new Error('Predicate contains potentially unsafe characters');
    }
}

/**
 * Check if an error is retryable
 * @param {Error} error - The error to check
 * @returns {boolean} True if error is retryable
 */
function isRetryableError(error) {
    const retryableMessages = [
        'timeout',
        'ECONNRESET',
        'EPIPE',
        'process exited with code'
    ];
    
    return retryableMessages.some(msg => 
        error.message.toLowerCase().includes(msg.toLowerCase())
    );
}

/**
 * Sleep for specified milliseconds
 * @param {number} ms - Milliseconds to sleep
 * @returns {Promise<void>}
 */
function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

// ============================================================================
// HEALTH CHECK AND DIAGNOSTICS
// ============================================================================

/**
 * Test Prolog bridge connectivity and basic functionality
 * @returns {Promise<Object>} Health check results
 */
async function healthCheck() {
    const checks = {
        prologExecutable: false,
        hangmanLogicFile: false,
        basicQuery: false,
        aiGuesser: false
    };
    
    try {
        // Check if Prolog executable exists
        await runPrologQuery('true', [], { queryType: 'check' });
        checks.prologExecutable = true;
        
        // Check if hangman logic file is accessible
        checks.hangmanLogicFile = fs.existsSync(HANGMAN_LOGIC_FILE);
        
        // Test basic query
        const wordResult = await selectRandomWord();
        checks.basicQuery = !!wordResult;
        
        // Test AI guesser
        try {
            await getAISuggestion('_a_a_e', ['a', 'e']);
            checks.aiGuesser = true;
        } catch (error) {
            console.log('AI guesser not available:', error.message);
        }
        
    } catch (error) {
        console.error('Health check failed:', error.message);
    }
    
    return {
        healthy: Object.values(checks).every(check => check),
        checks: checks,
        timestamp: new Date().toISOString()
    };
}

// ============================================================================
// EXPORTS
// ============================================================================

module.exports = {
    // Core functions
    runPrologQuery,
    
    // Game-specific helpers
    selectRandomWord,
    maskWord,
    checkGuess,
    isGameWon,
    isGameLost,
    getAISuggestion,
    
    // Utilities
    healthCheck
};
