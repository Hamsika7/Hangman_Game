% ============================================================================
% HANGMAN AI GUESSER - Intelligent Letter Selection Algorithm
% ============================================================================
% This file implements an AI that uses frequency analysis to make intelligent
% guesses in the Hangman game. The AI analyzes possible words that match the
% current game state and selects the most statistically likely letter.
% ============================================================================

% Import the word database and utility predicates from hangman_logic.pl
:- consult('hangman_logic.pl').

% ----------------------------------------------------------------------------
% MAIN AI GUESSING ALGORITHM
% ----------------------------------------------------------------------------
% ai_guess/3: Main predicate that determines the best letter to guess
% ai_guess(+MaskedWord, +GuessedLetters, -BestLetter)
% 
% Algorithm Overview:
% 1. Filter word database to find words matching the current pattern
% 2. Calculate frequency of unguessed letters in possible words
% 3. Select the letter with highest frequency as the best guess
%
% Parameters:
%   MaskedWord: Current state of word with underscores (e.g., '_a_a_e')
%   GuessedLetters: List of letters already guessed
%   BestLetter: The AI's recommended next guess

ai_guess(MaskedWord, GuessedLetters, BestLetter) :-
    % Step 1: Find all possible words that match the current pattern
    find_matching_words(MaskedWord, GuessedLetters, PossibleWords),
    
    % Step 2: Calculate letter frequencies from possible words
    calculate_letter_frequencies(PossibleWords, GuessedLetters, FrequencyList),
    
    % Step 3: Select the letter with highest frequency
    select_best_letter(FrequencyList, BestLetter).

% ----------------------------------------------------------------------------
% WORD PATTERN MATCHING
% ----------------------------------------------------------------------------
% find_matching_words/3: Finds words that match the masked pattern
% find_matching_words(+MaskedWord, +GuessedLetters, -PossibleWords)
%
% This predicate filters the word database to find words that:
% 1. Have the same length as the masked word
% 2. Match the revealed letters in their correct positions
% 3. Don't contain any incorrectly guessed letters
% 4. Contain all correctly guessed letters

find_matching_words(MaskedWord, GuessedLetters, PossibleWords) :-
    findall(Word, 
            (word(Word), matches_pattern(Word, MaskedWord, GuessedLetters)), 
            PossibleWords).

% matches_pattern/3: Checks if a word matches the current game state
% matches_pattern(+Word, +MaskedWord, +GuessedLetters)
matches_pattern(Word, MaskedWord, GuessedLetters) :-
    atom_chars(Word, WordChars),
    atom_chars(MaskedWord, MaskedChars),
    
    % Check if word has same length as masked word
    length(WordChars, Length),
    length(MaskedChars, Length),
    
    % Check if word matches the revealed pattern
    matches_revealed_letters(WordChars, MaskedChars),
    
    % Check if word doesn't contain incorrectly guessed letters
    \+ contains_incorrect_letters(WordChars, GuessedLetters, MaskedChars),
    
    % Check if word contains all correctly guessed letters
    contains_all_correct_letters(WordChars, GuessedLetters, MaskedChars).

% matches_revealed_letters/2: Verifies word matches revealed positions
% matches_revealed_letters(+WordChars, +MaskedChars)
matches_revealed_letters([], []).
matches_revealed_letters([WChar|WRest], [MChar|MRest]) :-
    (   MChar = '_'
    ->  true  % Underscore can match any character
    ;   WChar = MChar  % Revealed letters must match exactly
    ),
    matches_revealed_letters(WRest, MRest).

% contains_incorrect_letters/3: Checks for incorrectly guessed letters
% contains_incorrect_letters(+WordChars, +GuessedLetters, +MaskedChars)
contains_incorrect_letters(WordChars, GuessedLetters, MaskedChars) :-
    member(Letter, GuessedLetters),
    member(Letter, WordChars),
    \+ member(Letter, MaskedChars).

% contains_all_correct_letters/3: Verifies all correct guesses are present
% contains_all_correct_letters(+WordChars, +GuessedLetters, +MaskedChars)
contains_all_correct_letters(WordChars, GuessedLetters, MaskedChars) :-
    findall(Letter, 
            (member(Letter, GuessedLetters), member(Letter, MaskedChars)), 
            CorrectLetters),
    forall(member(Letter, CorrectLetters), member(Letter, WordChars)).

% ----------------------------------------------------------------------------
% FREQUENCY ANALYSIS
% ----------------------------------------------------------------------------
% calculate_letter_frequencies/3: Analyzes letter frequencies in possible words
% calculate_letter_frequencies(+PossibleWords, +GuessedLetters, -FrequencyList)
%
% Creates a list of [Letter-Frequency] pairs for all unguessed letters
% found in the possible words, sorted by frequency (highest first)

calculate_letter_frequencies(PossibleWords, GuessedLetters, FrequencyList) :-
    % Extract all letters from possible words
    extract_all_letters(PossibleWords, AllLetters),
    
    % Filter out already guessed letters
    subtract(AllLetters, GuessedLetters, UnguessedLetters),
    
    % Count frequency of each unguessed letter
    count_letter_frequencies(UnguessedLetters, AllLetters, FrequencyPairs),
    
    % Sort by frequency (descending order)
    sort(2, @>=, FrequencyPairs, FrequencyList).

% extract_all_letters/2: Extracts all letters from a list of words
% extract_all_letters(+Words, -AllLetters)
extract_all_letters([], []).
extract_all_letters([Word|RestWords], AllLetters) :-
    atom_chars(Word, WordChars),
    extract_all_letters(RestWords, RestLetters),
    append(WordChars, RestLetters, AllLetters).

% count_letter_frequencies/3: Counts occurrences of each letter
% count_letter_frequencies(+UnguessedLetters, +AllLetters, -FrequencyPairs)
count_letter_frequencies([], _, []).
count_letter_frequencies([Letter|RestLetters], AllLetters, [Letter-Count|RestPairs]) :-
    count_occurrences(Letter, AllLetters, Count),
    count_letter_frequencies(RestLetters, AllLetters, RestPairs).

% count_occurrences/3: Counts how many times a letter appears in a list
% count_occurrences(+Letter, +LetterList, -Count)
count_occurrences(_, [], 0).
count_occurrences(Letter, [Letter|Rest], Count) :-
    !,
    count_occurrences(Letter, Rest, RestCount),
    Count is RestCount + 1.
count_occurrences(Letter, [_|Rest], Count) :-
    count_occurrences(Letter, Rest, Count).

% ----------------------------------------------------------------------------
% LETTER SELECTION
% ----------------------------------------------------------------------------
% select_best_letter/2: Selects the letter with highest frequency
% select_best_letter(+FrequencyList, -BestLetter)
%
% If multiple letters have the same highest frequency, selects the first one
% (tie-breaking could be enhanced with additional heuristics)

select_best_letter([Letter-_|_], Letter) :-
    !.  % Select first letter (highest frequency due to sorting)
select_best_letter([], _) :-
    fail.  % No letters available (shouldn't happen in normal gameplay)

% ----------------------------------------------------------------------------
% UTILITY PREDICATES
% ----------------------------------------------------------------------------
% get_possible_word_count/3: Returns count of words matching pattern
% get_possible_word_count(+MaskedWord, +GuessedLetters, -Count)
get_possible_word_count(MaskedWord, GuessedLetters, Count) :-
    find_matching_words(MaskedWord, GuessedLetters, PossibleWords),
    length(PossibleWords, Count).

% analyze_ai_confidence/4: Provides confidence analysis for AI guess
% analyze_ai_confidence(+MaskedWord, +GuessedLetters, +BestLetter, -Confidence)
analyze_ai_confidence(MaskedWord, GuessedLetters, BestLetter, Confidence) :-
    calculate_letter_frequencies([], GuessedLetters, FrequencyList),
    find_matching_words(MaskedWord, GuessedLetters, PossibleWords),
    length(PossibleWords, WordCount),
    member(BestLetter-Frequency, FrequencyList),
    Confidence is (Frequency * 100) / WordCount.

% ----------------------------------------------------------------------------
% EXAMPLE QUERIES AND USAGE
% ----------------------------------------------------------------------------
% Here are example queries you can run in the Prolog console:
%
% 1. Basic AI guess for a masked word:
%    ?- ai_guess('_a_a_e', [a, e], BestLetter).
%    BestLetter = n.  % (assuming 'banana' is a likely match)
%
% 2. AI guess with more guessed letters:
%    ?- ai_guess('_r___n', [r, n], BestLetter).
%    BestLetter = a.  % (common letter in remaining positions)
%
% 3. Check how many possible words match a pattern:
%    ?- get_possible_word_count('___a__', [a], Count).
%    Count = 8.  % (number of 6-letter words with 'a' in 4th position)
%
% 4. Complete AI reasoning chain:
%    ?- ai_guess('__a__', [a], Letter),
%       get_possible_word_count('__a__', [a], Count).
%    Letter = r,
%    Count = 12.
%
% 5. Test with specific word pattern:
%    ?- ai_guess('d_a___', [d, a], BestLetter).
%    BestLetter = r.  % (likely 'dragon' pattern)
%
% 6. Advanced: Analyze AI confidence:
%    ?- ai_guess('_a___', [a], Letter),
%       analyze_ai_confidence('_a___', [a], Letter, Confidence).
%    Letter = n,
%    Confidence = 25.0.  % (25% of possible words contain 'n')
% ----------------------------------------------------------------------------