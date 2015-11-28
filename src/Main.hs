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
import Data.Matrix
import Data.Char
import Data.List
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
main = getInputs ≫= putStr ∘ unlines ∘ fmap (show ∘ parse)
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
  show (Category f) = f
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
  show (LexicalItem( token, f )) = token ⧺ " ∷ " ⧺ featureString
    where featureString = unwords $ fmap show f

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
  (["C"],
   [ LexicalItem( "fox",     [Category "N"]                                )
   , LexicalItem( "the",     [Selector "N", Category "D"]                  )
   , LexicalItem( "marie",   [Category "D", Licensee "case"]               )
   , LexicalItem( "pierre",  [Category "D"]                                )
   , LexicalItem( "praises", [Selector "D", Selector "D", Category "V"]    )
   , LexicalItem( ε,         [Selector "V", Licensor "case", Category "T"] )
   , LexicalItem( ε,         [Selector "T", Category "C"]                  )
   ])


--------------------------------------------------------------------------------
--                                                                 7. PARSING --
--------------------------------------------------------------------------------

-- | Each chain has a 'Type' that is either 'Lexical' or 'Derived'.
-- 'Lexical' chains consist only of lexical items, whereas 'Derived'
-- chains are the result of a merge or move operation.
data Type = Lexical | Derived
          deriving( Eq, Ord, Show )

-- | A 'Chain' is a contiguous forest (represented as a start and end
-- pair in the chart), a type, and a set of features.
data Chain = Chain( (ℕ, ℕ), Type, [Feature] )
           deriving( Eq )

instance Show Chain where
  show (Chain( pos, t, fs) ) =
    show pos ⧺ showNoCoords (Chain (pos, t, fs))

showNoCoords ∷ Chain → String
showNoCoords (Chain( _, Lexical, fs )) = foldl (⧺) "𝓁 " $ fmap show fs
showNoCoords (Chain( _, Derived, fs )) = foldl (⧺) ε    $ fmap show fs

-- | An 'Expression' is a non-empty list of chains.  An invalid
-- expression (formed by calling 'merge' or 'move' on arguments not
-- within its definition space) is represented by an empty list.
type Expression = [Chain]

data ChartEntry = ChartEntry Expression
                deriving( Eq )

instance Show ChartEntry where
  show (ChartEntry( [] ))     = ""
  show (ChartEntry( x : xs )) = showNoCoords x ⧺ show' xs
    where show' [] = ε
          show' xs = " " ⧺ (unwords $ fmap show xs)

-- | 'merge' Takes a left-hand chart entry and a right-hand chart
-- entry and attempts to merge them according to the three MG merge
-- functions.  If the merge fails, we return an empty list.
merge ∷ Expression → Expression → Expression

-- merge1: merge lexical entry with something else
merge ( Chain( (s, _),   Lexical,   Selector f  : γ  ) : [] )
      ( Chain( (_, e),         _,   Category f' : [] ) : αs )
  | f ≡ f'   = newExpr
  | otherwise = []
  where newChain = Chain( (s, e), Derived, γ )
        newExpr  = [newChain] ⧺ αs

-- merge2: merge something with derived entry
merge ( Chain( (s, _),         _,   Category f  : [] ) : βs )
      ( Chain( (_, e),   Derived,   Selector f' : γ  ) : αs )
  | f ≡ f'   = newExpr
  | otherwise = []
  where newChain = Chain( (s, e), Derived, γ )
        newExpr = [newChain] ⧺ αs ⧺ βs

-- merge3: merge two things with further requirements
merge ( Chain( pos ,          t ,   Category f  : γ  ) : βs )
      ( Chain( pos',          t',   Selector f' : δ  ) : αs )
  | f ≡ f'   = newExpr
  | otherwise = []
  where newChain  = Chain( pos', t', δ )
        newChain' = Chain( pos , t , γ )
        newExpr   = [newChain] ⧺ αs ⧺ [newChain'] ⧺ βs
merge ( Chain( pos',          t',   Selector f' : δ  ) : αs )
      ( Chain( pos ,          t ,   Category f  : γ  ) : βs )
  | f ≡ f'   = newExpr
  | otherwise = []
  where newChain  = Chain( pos', t', δ )
        newChain' = Chain( pos , t , γ )
        newExpr   = [newChain] ⧺ αs ⧺ [newChain'] ⧺ βs

-- otherwise, don't merge
merge _ _ = []

partialParseTokens ∷ [Token] → Maybe [ChartEntry]
partialParseTokens input =
  nullToNothing $ ChartEntry <$> parseResults
    where parseResults = cky g input start end
          start        = 0
          end          = length input
          g            = grammar

parseTokens ∷ [Token] → Maybe [ChartEntry]
parseTokens input =
     partialParseTokens input
  ≫= nullToNothing ∘ filter (\(ChartEntry x) → isValidParse x)
    where isValidParse = isStartSymbol g
          g            = grammar

isStartSymbol ∷ Grammar → Expression → Bool
isStartSymbol g [Chain (_, _, [Category f])] = f ∈ startSymbols g
isStartSymbol _ _                            = False

cky ∷ Grammar → [Token] → ℕ → ℕ → [Expression]
cky g input start end
  | invalidCell     = []
  | emptyCatCell    = emptyItemEntries
  | lexicalCell     = lexicalItemEntries
  | otherwise       = stablize (nub ∘ mergeWithEmpties) constituents
  where
    -- What type of cell is (start, end)?
    invalidCell  = end < start      -- Unused cell
    emptyCatCell = start ≡ end      -- ε entries
    lexicalCell  = end - start ≡ 1  -- Lexical entries
    -- Convert lexical items to chart entries
    lexicalToChart l    = [Chain ((start, end), Lexical, featuresOf l)]
    -- All empty items.
    emptyItemEntries   = lexicalToChart <$> emptyItems g
    -- All lexical items that are homophonous with token.  This allows
    -- us to capture lexical ambiguity.
    lexicalItemEntries = fmap lexicalToChart lexicalItems
    lexicalItems       = findInLexicon token g
    token              = input !! start
    -- Finds all possible constituents.
    constituents       =
      do
        midpoint ← [(start + 1) .. (end - 1)]
        lhs      ← cky g input start midpoint
        rhs      ← cky g input midpoint end
        return $ merge lhs rhs
    mergeWithEmpties items =
      items ⧺ do
        lhs      ← cky g input start start
        rhs      ← items
        return $ merge lhs rhs

fullChart ∷ Grammar -> [Token] → Matrix [ChartEntry]
fullChart g input = fmap (fmap ChartEntry) $ fromLists $ do
  x ← [0..end]
  return $
    do
      y ← [0..end]
      return $ filter (not ∘ null) $ cky g input x y
        where end = length input


--------------------------------------------------------------------------------
--                                                             8. RECOGNITION --
--------------------------------------------------------------------------------


-- | 'recognize' takes a list of tokens and returns whether the list
-- of tokens can be generated by the language.
recognizeTokens ∷ [Token] → Bool
recognizeTokens = isJust ∘ parseTokens


--------------------------------------------------------------------------------
--                                                            9. USER-FACING  --
--------------------------------------------------------------------------------


parse        = parseTokens        ∘ tokenize
partialParse = partialParseTokens ∘ tokenize
recognize    = recognizeTokens    ∘ tokenize
