--
-- Copyright © 2015, Patrick M. Niedzielski.
-- This file is part of fowlie-mga
--
-- fowlie-mga is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published
-- by the Free Software Foundation, either version 3 of the License,
-- or (at your option) any later version.
--
-- fowlie-mga is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with fowlie-mga.  If not, see <http://www.gnu.org/licenses/>.
--

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiWayIf    #-}

module Main( main ) where

import System.Environment
import Data.Maybe
import Data.Char
import Data.List
import Data.List.Unicode
import Prelude.Unicode
import Control.Applicative
import Control.Monad.Unicode


--------------------------------------------------------------------------------
--                                                       0. TABLE OF CONTENTS --
--------------------------------------------------------------------------------


{-

This program is divided into the following sections:

0. TABLE OF CONTENTS  This section
1. PROGRAM STRUCTURE  The application main function
2. UTILITIES          Generic utilities used throughout the program
3. TOKENIZATION       Tokenization functionality
4. FEATURES           Selectional and Licensing feature types
5. LEXICON            Lexical items, with associated features
6. GRAMMAR            A lexicon and a set of start symbols
7. PARSING            CKY parser for MGs
8. RECOGNITION        Recognizer for MGs
9. USER-FACING        Helpful functions for the user to use

-}


--------------------------------------------------------------------------------
--                                                       1. PROGRAM STRUCTURE --
--------------------------------------------------------------------------------


-- | 'main' is the entry point of the program.  It reads each of its
-- command line arguments, parses each according to the grammar, and
-- prints the results to standard out.  If there are no command line
-- arguments, then each line of standard in is used instead.
main ∷ IO ()
main = getInputs ≫= putStr ∘ unlines ∘ fmap (show ∘ parse grammar')
  where
    -- Return command line arguments if there are any, and stdin lines
    -- otherwise.
    getInputs = do
      args  ← getArgs
      stdin ← getLines
      return $ selectNotNull args stdin
    -- Return the first argument unless it is the empty list, in which
    -- case return the second argument.
    selectNotNull [] xs = xs
    selectNotNull xs _  = xs
    -- Return a lazy list of lines from standard in.
    getLines = getContents ≫= return ∘ lines


--------------------------------------------------------------------------------
--                                                               2. UTILITIES --
--------------------------------------------------------------------------------


-- | 'ℕ' is the natural numbers.  For efficiency, this is just an alias
-- for ℤ, and its only purpose in life is to make it a little clearer
-- that we don't want negative numbers.
type ℕ = Int

-- | 'ε' is the empty string.
ε ∷ String
ε = ""

-- | 'nullToNothing' maps an empty list to Nothing and a nonempty list
-- to Just that list.
nullToNothing ∷ [α] → Maybe [α]
nullToNothing [] = Nothing
nullToNothing x  = Just x

-- | 'stabilize' returns the cycle of a transformation 't' that has a
-- single value cycle when applied to 'initial'.  Precondition: 't'^𝓃
-- applied to 'initial' describes an orbit over a subset of α with a
-- cycle that contains a single value.
stablize ∷ Eq α ⇒ (α → α) → α → α
stablize f initial
  | initial' ≡ initial = initial
  | otherwise          = stablize f initial'
  where initial' = f initial

-- | 'modifyFirst' takes in a list, a predicate, and a transformation,
-- and returns a new list with the first element to match the
-- predicate modified by the given transformation.
modifyFirst ∷ (α → Bool) → (α → α) → [α] → [α]
modifyFirst _ _  [] = []
modifyFirst p f (x : xs)
  | p x       = f x : xs
  | otherwise = x  : modifyFirst p f xs


--------------------------------------------------------------------------------
--                                                            3. TOKENIZATION --
--------------------------------------------------------------------------------


-- | 'Token' is the orthographic representation of a lexical item.  It
-- is normalized to all lowercase characters, and has no internal
-- whitespace.
type Token = String

-- | 'tokenize' takes an input string and returns a list of its
-- tokens.
tokenize ∷ String → [Token]
tokenize = words ∘ fmap toLower  -- normalize to lowercase and then
                                 -- split on space


--------------------------------------------------------------------------------
--                                                                4. FEATURES --
--------------------------------------------------------------------------------


-- | A 'Selectional' feature is one that drives Merge.  A categorial
-- feature 𝐹 and a selector feature =𝐹 can Merge to form a larger
-- tree.
type Selectional = String

-- | A 'Licensing' feature is one that drives Move.  A licensor
-- feature +𝐹 and a licensee feature -𝐹 are checked by a Move to
-- reorder the constituents in the tree.
type Licensing   = String

-- | A 'Feature' is either a selectional feature or a licensing
-- feature.
data Feature = Category Selectional
             | Selector Selectional
             | Licensor Licensing
             | Licensee Licensing
             deriving( Eq )

instance Show Feature where
  show (Category f) =       f
  show (Selector f) = "=" ⧺ f
  show (Licensor f) = "+" ⧺ f
  show (Licensee f) = "-" ⧺ f


--------------------------------------------------------------------------------
--                                                                 5. LEXICON --
--------------------------------------------------------------------------------


-- | A 'LexicalItem' is a pairing of an orthographic representation
-- that appears in the surface string and a list of features that must
-- be checked in a valid parse.
data LexicalItem = LexicalItem( Token, [Feature] )
                 deriving( Eq )

instance Show LexicalItem where
  show (LexicalItem( token, f )) = token ⧺ "∷" ⧺ featureString
    where featureString = concat $ show <$> f

-- | 'featuresOf' returns the list of features associated with a
-- lexical item.
featuresOf ∷ LexicalItem → [Feature]
featuresOf (LexicalItem( _, f )) = f

-- | 'tokenOf' returns the surface form associated with a lexical
-- item.
tokenOf ∷ LexicalItem → Token
tokenOf (LexicalItem( token, _ )) = token

-- | A 'Lexicon' is simply a list of lexical entries, with no
-- duplcates.
type Lexicon = [LexicalItem]


--------------------------------------------------------------------------------
--                                                                 6. GRAMMAR --
--------------------------------------------------------------------------------


-- | A 'Grammar' is just a lexicon and a set of start symbols in MG.
type Grammar = ([Selectional], Lexicon)

-- | 'lexicon' returns a grammar's lexicon
lexicon ∷ Grammar → Lexicon
lexicon (_, l) = l

-- | 'startSymbols' returns a grammar's set of start symbols.
startSymbols ∷ Grammar → [Selectional]
startSymbols (s, _) = s

-- | 'findInLexicon' returns a list of all lexical items with the
-- given surface form.  Because we allow homophony in our lexicon,
-- this has to return a list.
findInLexicon ∷ Token → Grammar → [LexicalItem]
findInLexicon t = filter (\x → tokenOf x ≡ t) ∘ lexicon

-- | 'emptyItems' returns a list of all lexical items with no surface
-- form (i.e., such that the surface form is ε).
emptyItems ∷ Grammar → [LexicalItem]
emptyItems = findInLexicon ε

-- | 'grammar' is a test grammar.
grammar ∷ Grammar
grammar =
  (["T"],
   [ LexicalItem( "marie",   [Category "D", Licensee "nom"]                )
   , LexicalItem( "pierre",  [Category "D"]                                )
   , LexicalItem( "praises", [Selector "D", Selector "D", Category "V"]    )
   , LexicalItem( ε,         [Selector "V", Licensor "nom", Category "T"]  )
   ])
grammar' =
  (["T"],
   [ LexicalItem( "john",  [Category "D",   Licensee "epp"]               )
   , LexicalItem( "will",  [Selector "V",   Licensor "epp", Category "T"] )
   , LexicalItem( "see",   [Selector "D",   Selector "D",   Category "V"] )
   , LexicalItem( "the",   [Selector "Num", Category "D"]                 )
   , LexicalItem( "movie", [Category "N"]                                 )
   , LexicalItem( ε,       [Selector "T", Category "C"]                   )
   , LexicalItem( ε,       [Selector "N", Category "Num"]                 )
   ])


--------------------------------------------------------------------------------
--                                                                 7. PARSING --
--------------------------------------------------------------------------------

-- | Each chain has a 'Type' that is either 'Lexical' or 'Derived'.
-- 'Lexical' chains consist only of lexical items, whereas 'Derived'
-- chains are the result of a merge or move operation.
data Type = Lexical | Derived
          deriving( Eq, Ord )

instance Show Type where
  show Lexical = "∷"
  show Derived = ":"

-- | A 'Chain' is a contiguous forest (represented as a start and end
-- pair in the chart), a type, and a set of features.
data Chain = Chain( (ℕ, ℕ), Type, [Feature] )
           deriving( Eq )

instance Show Chain where
  show (Chain( pos, t, fs) ) =
    show pos ⧺ show t ⧺ (foldl (⧺) ε $ show <$> fs)

-- | An 'Expression' is a non-empty list of chains.  An invalid
-- expression (formed by calling 'merge' or 'move' on arguments not
-- within its definition space) is represented by an empty list.
data Expression = Expression [Chain]
                deriving( Eq )

instance Show Expression where
  show (Expression chains) = "  " ++ (intercalate ";" $ show <$> chains)


-- | Give a lexical item and a span, 'lexicalToExpr' yields a
-- lexical expression with the same features.
lexicalToExpr ∷ (ℕ, ℕ) → LexicalItem → Expression
lexicalToExpr pos l = Expression [chain]
  where chain = Chain( pos, Lexical, features )
        LexicalItem( _, features ) = l

{-

  CHART PARSING

-}

-- | The 'Agenda' holds all items we want to use in an inference.
type Agenda = [Expression]

-- | The 'Chart' holds all conclusions we know to be true at a given
-- parse state, based on the inference rules of the grammar.
type Chart = [Expression]

-- | 'ParseState' is the combination of the Agenda and the Chart.
type ParseState = (Agenda, Chart)

-- | The 'initialParseState' contains all axioms we can derive from
-- the grammar and the string of tokens.
initialParseState ∷ Grammar → [Token] → ParseState
initialParseState g tokens = (agenda, c)
  where
    -- Initially, the agenda and the chart are the same.
    agenda            = concat emptyAxioms ⧺ concat lexicalAxioms
    c                 = concat emptyAxioms ⧺ concat lexicalAxioms
    -- To compute this, we find all lexical items and null entries in
    -- the string.
    inputSize         = length tokens
    -- emptyAxioms is all ε lexical items
    emptyAxioms       = emptyAxiomsAt             <$> [0..inputSize]
    emptyAxiomsAt n   = lexicalToExpr (n, n)      <$> emptyItems g
    -- lexicalAxioms is all lexical items in the input
    lexicalAxioms     = lexicalAxiomsAt           <$> [0..inputSize-1]
    lexicalAxiomsAt n = lexicalToExpr (n, succ n) <$> lexicals n
    lexicals n        = findInLexicon (tokens !! n) g

-- | 'inferAll' reduces a ParseState until no more inferences can be
-- made.
inferAll ∷ ParseState → Chart
inferAll initialState = c'
  where parseStates   = iterate inferStep initialState
        Just ([], c') = find (null ∘ fst) parseStates

-- | 'inferStep' reduces a ParseState by pulling one item of the
-- agenda and applying all inferences it can using that item.
inferStep ∷ ParseState → ParseState
inferStep ([], c)            = ([], c)
inferStep (item : agenda, c) = (agenda', c')
  where
    Just (_, agenda', c') = find (\(x, _, _) → x ≡ Nothing) inferences
    inferences            = iterate inferOn initialInference
    initialInference      = (Just item, agenda, c)
    inferOn (Nothing, agenda, c)   = (Nothing, agenda, c)
    inferOn (Just item, agenda, c) = (infer item c, agenda', c')
      where
        canInfer  = isJust   $ infer item c
        inference = fromJust $ infer item c
        agenda'   = if canInfer then inference : agenda else agenda
        c'        = if canInfer then inference : c      else c

-- | 'inferOne' returns a list of conclusions we can draw by applying
-- inference rules on a given expression with a current state of
-- knowledge.  This list may contain known conclusions.
inferOne ∷ Expression → Chart → [Expression]
inferOne item c = conclusions
  where
    -- Try inference rules:
    -- Move operates only on one expression
    moveConclusions    = [move item]
    -- Merges operate on two expressions
    merge1Conclusions  = merge1 item <$> c
    merge2Conclusions  = merge2 item <$> c
    merge3Conclusions  = merge3 item <$> c
    conclusions        = catMaybes (moveConclusions   ⧺
                                    merge1Conclusions ⧺
                                    merge2Conclusions ⧺
                                    merge3Conclusions)

-- | 'infer' returns either an unknown conclusion drawn from the given
-- expression and the current state of knowledge, or 'Nothing'.
infer ∷ Expression → Chart → Maybe Expression
infer item c = conclusion
  where
    conclusion = listToMaybe $ filter (not ∘ (∈ c)) $ inferOne item c

{-

   MERGE

-}

-- | 'merge' takes a left-hand chart entry and a right-hand chart
-- entry and attempts to merge them according to the three MG merge
-- functions.  If the merge fails, we return an empty list.

-- merge lexical entry with something else
merge1 ∷ Expression → Expression → Maybe Expression
merge1 (Expression( Chain( (s,  m), Lexical, Selector f  : γ  ) : [] ))
       (Expression( Chain( (m', e),       _, Category f' : [] ) : αs ))
  | m ≡ m' ∧ f ≡ f'   = Just (Expression newExpr)
  | otherwise         = Nothing
  where newChain = Chain( (s, e), Derived, γ )
        newExpr  = [newChain] ⧺ αs
merge1 (Expression( Chain( (m', e),       _, Category f' : [] ) : αs ))
       (Expression( Chain( (s,  m), Lexical, Selector f  : γ  ) : [] ))
  | m ≡ m' ∧ f ≡ f'   = Just (Expression newExpr)
  | otherwise         = Nothing
  where newChain = Chain( (s, e), Derived, γ )
        newExpr  = [newChain] ⧺ αs
merge1 _ _            = Nothing

-- merge something with derived entry
merge2 ∷ Expression → Expression → Maybe Expression
merge2 (Expression( Chain( (s,  m),       _, Category f  : [] ) : βs ))
       (Expression( Chain( (m', e), Derived, Selector f' : γ  ) : αs ))
  | m ≡ m' ∧ f ≡ f'   = Just (Expression newExpr)
  | otherwise         = Nothing
  where newChain = Chain( (s, e), Derived, γ )
        newExpr  = [newChain] ⧺ αs ⧺ βs
merge2 (Expression( Chain( (m', e), Derived, Selector f' : γ  ) : αs ))
       (Expression( Chain( (s,  m),       _, Category f  : [] ) : βs ))
  | m ≡ m' ∧ f ≡ f'   = Just (Expression newExpr)
  | otherwise         = Nothing
  where newChain = Chain( (s, e), Derived, γ )
        newExpr  = [newChain] ⧺ αs ⧺ βs
merge2 _ _ = Nothing

-- merge two non-contiguous things with further requirements
merge3 ∷ Expression → Expression → Maybe Expression
merge3 (Expression( Chain( pos ,           _,   Category f  : γ : γs ) : βs ))
       (Expression( Chain( pos',           _,   Selector f' : δ : δs ) : αs ))
  | f ≡ f'    = Just (Expression newExpr)
  | otherwise = Nothing 
  where newChain  = Chain( pos , Derived , γ : γs )
        newChain' = Chain( pos', Derived, δ : δs )
        newExpr   = [newChain'] ⧺ αs ⧺ [newChain] ⧺ βs
merge3 (Expression( Chain( pos',           _,   Selector f' : δ : δs ) : αs ))
       (Expression( Chain( pos ,           _,   Category f  : γ : γs ) : βs ))
  | f ≡ f'    = Just (Expression newExpr)
  | otherwise = Nothing
  where newChain  = Chain( pos , Derived , γ : γs )
        newChain' = Chain( pos', Derived, δ : δs )
        newExpr   = [newChain'] ⧺ αs ⧺ [newChain] ⧺ βs
merge3 _ _ = Nothing

{-

   MOVE

-}

-- | 'move' takes an expression and attempts to check licensing
-- features in each chain.
move ∷ Expression → Maybe Expression
-- We need at least two chains to move.
move (Expression( c : [] )) = Nothing
-- move1:
move (Expression( c@(Chain( (m, e), Derived, Licensor f : γ ) : chains) ))
  | any onlyLicensedBy chains = Just( Expression( [newChain] ⧺ αs ) )
  | otherwise                 = Nothing
  where
    onlyLicensedBy (Chain( (s, m'), Derived, Licensee f' : [] ))
                          = m ≡ m' ∧ f ≡ f'
    onlyLicensedBy _      = False
    mover                 = fromJust $ find onlyLicensedBy chains
    Chain( (s, _), _, _ ) = mover
    newChain              = Chain( (s, e), Derived, γ )
    αs                    = delete mover chains
-- move2:
move (Expression( c@(Chain( pos, t, Licensor f : γ ) : chains) ))
  | any licensedBy chains =
      Just( Expression( Chain( pos, t, γ ) : featureCheck chains ) )
  | otherwise             = Nothing
  where
    featureCheck = modifyFirst licensedBy checkFeature
    licensedBy   (Chain(    _,  _, Licensee f' : _ )) = f ≡ f'
    licensedBy   _                                    = False
    checkFeature (Chain( pos', t', Licensee f' : δ )) =
      Chain( pos', t', δ )
-- otherwise, don't move
move _ = Nothing

{-

   PARSING

-}

fullChart ∷ Grammar -> [Token] → Chart
fullChart g tokens = inferAll initialState
  where initialState = initialParseState g tokens

partialParseTokens ∷ Grammar → [Token] → Maybe [Expression]
partialParseTokens g tokens =
  nullToNothing results
  where
    chart      = fullChart g tokens
    endState (Expression[ Chain( (start', end'), _, [Category _] ) ])
               = start' ≡ start ∧ end' ≡ end
    endState _ = False
    results    = filter endState chart
    start      = 0
    end        = length tokens

parseTokens ∷ Grammar → [Token] → Maybe [Expression]
parseTokens g tokens =
     partialParseTokens g tokens
  ≫= nullToNothing ∘ filter isValidParse
    where isValidParse (Expression e) = isStartSymbol g e

isStartSymbol ∷ Grammar → [Chain] → Bool
isStartSymbol g [Chain (_, _, [Category f])] = f ∈ startSymbols g
isStartSymbol _ _                            = False


--------------------------------------------------------------------------------
--                                                             8. RECOGNITION --
--------------------------------------------------------------------------------


-- | 'recognizeTokens' takes a list of tokens and returns whether the list
-- of tokens can be generated by the language.
recognizeTokens ∷ Grammar → [Token] → Bool
recognizeTokens g = isJust ∘ parseTokens g


--------------------------------------------------------------------------------
--                                                            9. USER-FACING  --
--------------------------------------------------------------------------------


parse        g = parseTokens        g ∘ tokenize
partialParse g = partialParseTokens g ∘ tokenize
recognize    g = recognizeTokens    g ∘ tokenize
