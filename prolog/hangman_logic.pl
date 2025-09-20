% ============================================================================
% HANGMAN GAME LOGIC - Core Prolog Implementation
% ============================================================================
% This file contains the fundamental game logic for a Hangman word-guessing game.
% It includes word selection, masking, guess validation, and win/lose conditions.
% ============================================================================

% ----------------------------------------------------------------------------
% WORD DATABASE
% ----------------------------------------------------------------------------
% Facts defining the available words for the game.
% Each word is between 5-10 characters as specified.

word(apple).      % 5 characters
word(banana).     % 6 characters  
word(cherry).     % 6 characters
word(dragon).     % 6 characters
word(elephant).   % 8 characters
word(forest).     % 6 characters
word(guitar).     % 6 characters
word(harmony).    % 7 characters
word(island).     % 6 characters
word(jungle).     % 6 characters
word(keyboard).   % 8 characters
word(library).    % 7 characters
word(mountain).   % 8 characters
word(notebook).   % 8 characters
word(ocean).      % 5 characters
word(planet).     % 6 characters
word(rainbow).    % 7 characters
word(sunset).     % 6 characters
word(thunder).    % 7 characters
word(universe).   % 8 characters

% ----------------------------------------------------------------------------
% WORD SELECTION
% ----------------------------------------------------------------------------
% select_word/1: Randomly selects a word from the word database
% Usage: ?- select_word(Word).

select_word(Word) :-
    findall(W, word(W), WordList),
    length(WordList, Length),
    random(0, Length, Index),
    nth0(Index, WordList, Word).

% ----------------------------------------------------------------------------
% WORD MASKING
% ----------------------------------------------------------------------------
% mask_word/3: Creates a masked version of a word based on guessed letters
% mask_word(+Word, +GuessedLetters, -MaskedWord)
% Word: The target word to mask
% GuessedLetters: List of letters that have been guessed
% MaskedWord: The word with unguessed letters replaced by underscores

mask_word(Word, GuessedLetters, MaskedWord) :-
    atom_chars(Word, WordChars),
    mask_chars(WordChars, GuessedLetters, MaskedChars),
    atom_chars(MaskedWord, MaskedChars).

% Helper predicate to mask individual characters
mask_chars([], _, []).
mask_chars([Char|RestChars], GuessedLetters, [MaskedChar|RestMasked]) :-
    (   member(Char, GuessedLetters)
    ->  MaskedChar = Char
    ;   MaskedChar = '_'
    ),
    mask_chars(RestChars, GuessedLetters, RestMasked).

% ----------------------------------------------------------------------------
% GUESS VALIDATION
% ----------------------------------------------------------------------------
% check_guess/3: Determines if a guessed letter is in the target word
% check_guess(+Letter, +Word, -Status)
% Letter: The guessed letter
% Word: The target word
% Status: 'correct' if letter is in word, 'incorrect' otherwise

check_guess(Letter, Word, Status) :-
    atom_chars(Word, WordChars),
    (   member(Letter, WordChars)
    ->  Status = correct
    ;   Status = incorrect
    ).

% ----------------------------------------------------------------------------
% GAME STATE EVALUATION
% ----------------------------------------------------------------------------
% is_game_won/1: Checks if the game has been won (no underscores in masked word)
% is_game_won(+MaskedWord)
% MaskedWord: The current masked version of the word

is_game_won(MaskedWord) :-
    atom_chars(MaskedWord, MaskedChars),
    \+ member('_', MaskedChars).

% is_game_lost/1: Checks if the game has been lost (no attempts remaining)
% is_game_lost(+AttemptsLeft)
% AttemptsLeft: Number of incorrect guesses remaining

is_game_lost(AttemptsLeft) :-
    AttemptsLeft =< 0.

% ----------------------------------------------------------------------------
% UTILITY PREDICATES
% ----------------------------------------------------------------------------
% get_word_length/2: Gets the length of a word
% get_word_length(+Word, -Length)

get_word_length(Word, Length) :-
    atom_chars(Word, Chars),
    length(Chars, Length).

% count_unique_letters/2: Counts unique letters in a word
% count_unique_letters(+Word, -Count)

count_unique_letters(Word, Count) :-
    atom_chars(Word, Chars),
    sort(Chars, UniqueChars),
    length(UniqueChars, Count).

% ----------------------------------------------------------------------------
% EXAMPLE QUERIES AND USAGE
% ----------------------------------------------------------------------------
% Here are example queries you can run in the Prolog console:
%
% 1. Select a random word:
%    ?- select_word(W).
%    W = dragon.
%
% 2. Mask a word with some guessed letters:
%    ?- mask_word(dragon, [d, a], MaskedWord).
%    MaskedWord = 'da___'.
%
% 3. Check if a letter is in a word:
%    ?- check_guess(r, dragon, Status).
%    Status = correct.
%    
%    ?- check_guess(z, dragon, Status).
%    Status = incorrect.
%
% 4. Check if game is won:
%    ?- is_game_won(dragon).
%    true.
%    
%    ?- is_game_won('d_a_o_').
%    false.
%
% 5. Check if game is lost:
%    ?- is_game_lost(0).
%    true.
%    
%    ?- is_game_lost(3).
%    false.
%
% 6. Complete game flow example:
%    ?- select_word(W), 
%       mask_word(W, [a, e], Masked), 
%       check_guess(r, W, Status).
%    W = forest,
%    Masked = '_e__e_',
%    Status = correct.
% ----------------------------------------------------------------------------

% ============================================================================
% MAIN GAME LOGIC - Interactive Hangman Game
% ============================================================================
% This section contains the main game loop and user interface for playing
% an interactive Hangman game. It integrates all the core game predicates
% and provides a complete gaming experience.
% ============================================================================

% ----------------------------------------------------------------------------
% MAIN GAME ENTRY POINT
% ----------------------------------------------------------------------------
% play_game/0: Starts a new Hangman game
% This is the main predicate that users call to begin playing.
% It initializes the game state and starts the game loop.

play_game :-
    % Display welcome message and game instructions
    display_welcome_message,
    
    % Initialize game state
    select_word(Word),
    atom_chars(Word, WordChars),
    length(WordChars, WordLength),
    
    % Create initial masked word (all underscores)
    create_initial_mask(WordLength, InitialMask),
    
    % Set initial game parameters
    RemainingGuesses = 7,
    IncorrectGuesses = [],
    GuessedLetters = [],
    
    % Start the main game loop
    game_loop(Word, InitialMask, GuessedLetters, IncorrectGuesses, RemainingGuesses).

% ----------------------------------------------------------------------------
% GAME LOOP - Core Game Management
% ----------------------------------------------------------------------------
% game_loop/5: Main recursive game loop that manages game state
% game_loop(+Word, +MaskedWord, +GuessedLetters, +IncorrectGuesses, +RemainingGuesses)
%
% Parameters:
%   Word: The secret word to guess
%   MaskedWord: Current masked version showing guessed letters
%   GuessedLetters: List of all letters guessed so far
%   IncorrectGuesses: List of incorrect letter guesses
%   RemainingGuesses: Number of wrong guesses remaining

game_loop(Word, MaskedWord, GuessedLetters, IncorrectGuesses, RemainingGuesses) :-
    % Display current game state
    display_game_state(MaskedWord, IncorrectGuesses, RemainingGuesses),
    
    % Check win condition first
    (   is_game_won(MaskedWord)
    ->  display_win_message(Word)
    
    % Check loss condition
    ;   is_game_lost(RemainingGuesses)
    ->  display_loss_message(Word)
    
    % Continue game - get user input and process guess
    ;   get_user_guess(Guess),
        process_guess(Word, MaskedWord, GuessedLetters, IncorrectGuesses, 
                     RemainingGuesses, Guess, 
                     NewMaskedWord, NewGuessedLetters, NewIncorrectGuesses, 
                     NewRemainingGuesses),
        
        % Recursive call to continue the game loop
        game_loop(Word, NewMaskedWord, NewGuessedLetters, 
                 NewIncorrectGuesses, NewRemainingGuesses)
    ).

% ----------------------------------------------------------------------------
% GUESS PROCESSING
% ----------------------------------------------------------------------------
% process_guess/10: Processes a user's letter guess and updates game state
% This predicate handles the logic for determining if a guess is correct,
% updating the masked word, and managing the guess counters.

process_guess(Word, MaskedWord, GuessedLetters, IncorrectGuesses, RemainingGuesses, 
             Guess, NewMaskedWord, NewGuessedLetters, NewIncorrectGuesses, 
             NewRemainingGuesses) :-
    
    % Check if letter was already guessed
    (   member(Guess, GuessedLetters)
    ->  write('You already guessed that letter! Try again.'), nl,
        NewMaskedWord = MaskedWord,
        NewGuessedLetters = GuessedLetters,
        NewIncorrectGuesses = IncorrectGuesses,
        NewRemainingGuesses = RemainingGuesses
    
    % Process new guess
    ;   % Add guess to guessed letters list
        NewGuessedLetters = [Guess|GuessedLetters],
        
        % Check if guess is correct
        check_guess(Guess, Word, Status),
        
        (   Status = correct
        ->  % Correct guess - update masked word
            write('Good guess! '), write(Guess), write(' is in the word.'), nl,
            mask_word(Word, NewGuessedLetters, NewMaskedWord),
            NewIncorrectGuesses = IncorrectGuesses,
            NewRemainingGuesses = RemainingGuesses
            
        ;   % Incorrect guess - update incorrect list and decrement attempts
            write('Sorry, '), write(Guess), write(' is not in the word.'), nl,
            NewMaskedWord = MaskedWord,
            NewIncorrectGuesses = [Guess|IncorrectGuesses],
            NewRemainingGuesses is RemainingGuesses - 1
        )
    ).

% ----------------------------------------------------------------------------
% USER INPUT HANDLING
% ----------------------------------------------------------------------------
% get_user_guess/1: Prompts user for input and validates the guess
% get_user_guess(-Guess)
% Ensures the input is a single lowercase letter

get_user_guess(Guess) :-
    write('Enter your guess (single letter): '),
    read_line_to_string(user_input, Input),
    
    % Validate and process input
    (   validate_input(Input, ValidatedGuess)
    ->  Guess = ValidatedGuess
    ;   write('Invalid input! Please enter a single letter.'), nl,
        get_user_guess(Guess)  % Recursive call for invalid input
    ).

% validate_input/2: Validates user input is a single letter
% validate_input(+Input, -ValidatedGuess)
validate_input(Input, Guess) :-
    string_length(Input, 1),
    string_chars(Input, [Char]),
    char_type(Char, alpha),
    downcase_atom(Char, Guess).

% ----------------------------------------------------------------------------
% DISPLAY FUNCTIONS - User Interface
% ----------------------------------------------------------------------------
% display_welcome_message/0: Shows game introduction and rules
display_welcome_message :-
    nl,
    write('=============================================='), nl,
    write('         WELCOME TO HANGMAN GAME!'), nl,
    write('=============================================='), nl,
    write('Rules:'), nl,
    write('- Guess letters one at a time'), nl,
    write('- You have 7 incorrect guesses allowed'), nl,
    write('- Win by guessing the complete word'), nl,
    write('- Good luck!'), nl,
    write('=============================================='), nl, nl.

% display_game_state/3: Shows current game status to user
% display_game_state(+MaskedWord, +IncorrectGuesses, +RemainingGuesses)
display_game_state(MaskedWord, IncorrectGuesses, RemainingGuesses) :-
    nl,
    write('Current word: '), write(MaskedWord), nl,
    write('Incorrect guesses: '), write(IncorrectGuesses), nl,
    write('Remaining guesses: '), write(RemainingGuesses), nl,
    draw_hangman(RemainingGuesses),
    nl.

% display_win_message/1: Shows victory message with the word
% display_win_message(+Word)
display_win_message(Word) :-
    nl,
    write('🎉 CONGRATULATIONS! YOU WON! 🎉'), nl,
    write('The word was: '), write(Word), nl,
    write('Great job guessing it correctly!'), nl,
    nl.

% display_loss_message/1: Shows defeat message revealing the word
% display_loss_message(+Word)
display_loss_message(Word) :-
    nl,
    write('💀 GAME OVER - YOU LOST! 💀'), nl,
    write('The word was: '), write(Word), nl,
    write('Better luck next time!'), nl,
    draw_hangman(0),  % Show complete hangman
    nl.

% ----------------------------------------------------------------------------
% UTILITY FUNCTIONS
% ----------------------------------------------------------------------------
% create_initial_mask/2: Creates initial masked word with all underscores
% create_initial_mask(+Length, -MaskedWord)
create_initial_mask(Length, MaskedWord) :-
    length(UnderscoreList, Length),
    maplist(=('_'), UnderscoreList),
    atom_chars(MaskedWord, UnderscoreList).

% draw_hangman/1: Draws ASCII hangman based on remaining guesses
% draw_hangman(+RemainingGuesses)
% Visual representation gets more complete as guesses decrease
draw_hangman(7) :-
    write('  +---+'), nl,
    write('  |   |'), nl,
    write('      |'), nl,
    write('      |'), nl,
    write('      |'), nl,
    write('      |'), nl,
    write('========='), nl.

draw_hangman(6) :-
    write('  +---+'), nl,
    write('  |   |'), nl,
    write('  O   |'), nl,
    write('      |'), nl,
    write('      |'), nl,
    write('      |'), nl,
    write('========='), nl.

draw_hangman(5) :-
    write('  +---+'), nl,
    write('  |   |'), nl,
    write('  O   |'), nl,
    write('  |   |'), nl,
    write('      |'), nl,
    write('      |'), nl,
    write('========='), nl.

draw_hangman(4) :-
    write('  +---+'), nl,
    write('  |   |'), nl,
    write('  O   |'), nl,
    write(' /|   |'), nl,
    write('      |'), nl,
    write('      |'), nl,
    write('========='), nl.

draw_hangman(3) :-
    write('  +---+'), nl,
    write('  |   |'), nl,
    write('  O   |'), nl,
    write(' /|\\  |'), nl,
    write('      |'), nl,
    write('      |'), nl,
    write('========='), nl.

draw_hangman(2) :-
    write('  +---+'), nl,
    write('  |   |'), nl,
    write('  O   |'), nl,
    write(' /|\\  |'), nl,
    write(' /    |'), nl,
    write('      |'), nl,
    write('========='), nl.

draw_hangman(1) :-
    write('  +---+'), nl,
    write('  |   |'), nl,
    write('  O   |'), nl,
    write(' /|\\  |'), nl,
    write(' / \\  |'), nl,
    write('      |'), nl,
    write('========='), nl.

draw_hangman(0) :-
    write('  +---+'), nl,
    write('  |   |'), nl,
    write('  O   |'), nl,
    write(' /|\\  |'), nl,
    write(' / \\  |'), nl,
    write('      |'), nl,
    write('========='), nl,
    write('HANGED!'), nl.

% ----------------------------------------------------------------------------
% GAME VARIANTS AND EXTENSIONS
% ----------------------------------------------------------------------------
% play_ai_demo/0: Demonstration mode where AI plays against itself
% This shows how the AI guesser would play the game automatically
play_ai_demo :-
    write('=== AI DEMONSTRATION MODE ==='), nl,
    select_word(Word),
    atom_chars(Word, WordChars),
    length(WordChars, WordLength),
    create_initial_mask(WordLength, InitialMask),
    write('AI is playing with word: '), write(Word), nl, nl,
    ai_game_loop(Word, InitialMask, [], [], 7).

% ai_game_loop/5: Game loop for AI demonstration
% Similar to regular game loop but uses AI to make guesses
ai_game_loop(Word, MaskedWord, GuessedLetters, IncorrectGuesses, RemainingGuesses) :-
    display_game_state(MaskedWord, IncorrectGuesses, RemainingGuesses),
    
    (   is_game_won(MaskedWord)
    ->  write('AI WINS! The word was: '), write(Word), nl
    
    ;   is_game_lost(RemainingGuesses)
    ->  write('AI LOSES! The word was: '), write(Word), nl
    
    ;   % AI makes a guess using the ai_guesser
        (   consult('ai_guesser.pl'),
            ai_guess(MaskedWord, GuessedLetters, AIGuess)
        ->  write('AI guesses: '), write(AIGuess), nl,
            process_guess(Word, MaskedWord, GuessedLetters, IncorrectGuesses, 
                         RemainingGuesses, AIGuess, 
                         NewMaskedWord, NewGuessedLetters, NewIncorrectGuesses, 
                         NewRemainingGuesses),
            sleep(2),  % Pause for dramatic effect
            ai_game_loop(Word, NewMaskedWord, NewGuessedLetters, 
                        NewIncorrectGuesses, NewRemainingGuesses)
        ;   write('AI cannot make a guess - using fallback'), nl,
            get_user_guess(Guess),
            process_guess(Word, MaskedWord, GuessedLetters, IncorrectGuesses, 
                         RemainingGuesses, Guess, 
                         NewMaskedWord, NewGuessedLetters, NewIncorrectGuesses, 
                         NewRemainingGuesses),
            ai_game_loop(Word, NewMaskedWord, NewGuessedLetters, 
                        NewIncorrectGuesses, NewRemainingGuesses)
        )
    ).

% ----------------------------------------------------------------------------
% EXAMPLE USAGE FOR MAIN GAME
% ----------------------------------------------------------------------------
% To play the game, simply query:
%    ?- play_game.
%
% To watch the AI play:
%    ?- play_ai_demo.
%
% Example game session:
%    ?- play_game.
%    ==============================================
%             WELCOME TO HANGMAN GAME!
%    ==============================================
%    Rules:
%    - Guess letters one at a time
%    - You have 7 incorrect guesses allowed
%    - Win by guessing the complete word
%    - Good luck!
%    ==============================================
%    
%    Current word: ______
%    Incorrect guesses: []
%    Remaining guesses: 7
%    Enter your guess (single letter): e
%    Good guess! e is in the word.
%    
%    Current word: ___e__
%    Incorrect guesses: []
%    Remaining guesses: 7
%    Enter your guess (single letter): a
%    ... (game continues)
% ----------------------------------------------------------------------------