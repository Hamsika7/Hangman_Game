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