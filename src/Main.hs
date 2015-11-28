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
import Control.Monad.Unicode


--------------------------------------------------------------------------------
--                                                          PROGRAM STRUCTURE --
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
--                                                                  UTILITIES --
--------------------------------------------------------------------------------


-- | 'ℕ' is the natural numbers.  For efficiency, this is just an alias
-- for ℤ, and its only purpose in life is to make it a little clearer
-- that we don't want negative numbers.
type ℕ = Int

-- | 'ε' is the empty string.
ε = ""

-- | 'nullToNothing' maps an empty list to Nothing and a nonempty list
-- to Just that list.
nullToNothing ∷ [α] → Maybe [α]
nullToNothing [] = Nothing
nullToNothing x  = Just x


--------------------------------------------------------------------------------
--                                                               TOKENIZATION --
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
--                                                                   FEATURES --
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

-- | 'selects' returns whether an atom with a given list of
-- features can merge with an atom with another list of features.
-- This is only true when the head of the lefthand features list is a
-- selector for the head of the righthand features list.
selects ∷ [Feature] → [Feature] → Bool
selects ((Selector f):_) ((Category f'):_) = f ≡ f'
selects _ _                                = False

-- | 'satisfyMerge' removes the first feature from a list of features.
-- Precondition: this feature set selects another feature set in a
-- Merge operation.
satisfyMerge ∷ [Feature] → [Feature]
satisfyMerge = tail


--------------------------------------------------------------------------------
--                                                                    LEXICON --
--------------------------------------------------------------------------------


-- | A 'LexicalItem' is a pairing of an orthographic representation
-- that appears in the surface string and a list of features that must
-- be checked in a valid parse.
data LexicalItem = LexicalItem (Token, [Feature])
                 deriving( Eq )

instance Show LexicalItem where
  show (LexicalItem (token, features)) = token ⧺ " ∷ " ⧺ featureString
    where featureString = unwords $ fmap show features

-- | 'featuresOf' returns the list of features associated with a
-- lexical item.
featuresOf ∷ LexicalItem → [Feature]
featuresOf (LexicalItem (_, features)) = features

-- | 'tokenOf' returns the surface form associated with a lexical
-- item.
tokenOf ∷ LexicalItem → Token
tokenOf (LexicalItem (token, _)) = token

-- | A 'Lexicon' is simply a list of lexical entries, with no
-- duplcates.
type Lexicon = [LexicalItem]


--------------------------------------------------------------------------------
--                                                                    GRAMMAR --
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
emptyItems = findInLexicon ""

-- | 'grammar' is a test grammar.
grammar ∷ Grammar
grammar =
  (["V"],
   [ LexicalItem( "fox",     [Category "N"]                             )
   , LexicalItem( "the",     [Selector "N", Category "D"]               )
   , LexicalItem( "marie",   [Category "D"]                             )
   , LexicalItem( "pierre",  [Category "D"]                             )
   , LexicalItem( "praises", [Selector "D", Selector "D", Category "V"] )
   ])


--------------------------------------------------------------------------------
--                                                                    PARSING --
--------------------------------------------------------------------------------


data ChartEntry = LexicalEntry [Feature]
                | MergeEntry (ℕ, [Feature])
                deriving( Show, Eq )

entryFeatures ∷ ChartEntry → [Feature]
entryFeatures (LexicalEntry f)    = f
entryFeatures (MergeEntry (_, f)) = f

partialParseTokens ∷ [Token] → Maybe [ChartEntry]
partialParseTokens input =
  nullToNothing parseResults
    where parseResults = cky g input start end
          start        = 0
          end          = length input
          g            = grammar

parseTokens ∷ [Token] → Maybe [ChartEntry]
parseTokens input =
     partialParseTokens input
  ≫= nullToNothing ∘ filter isValidParse
    where isValidParse = isStartSymbol g
          g            = grammar

isStartSymbol ∷ Grammar → ChartEntry → Bool
isStartSymbol g (LexicalEntry     [Category f])  = f ∈ startSymbols g
isStartSymbol g (MergeEntry   (_, [Category f])) = f ∈ startSymbols g
isStartSymbol _ _                                = False

cky ∷ Grammar → [Token] → ℕ → ℕ → [ChartEntry]
cky g input start end
  | invalidCell     = []
  | emptyCatCell    = emptyItemEntries
  | lexicalCell     = lexicalItemEntries
  | otherwise       = nub $ findConstituents
  where
    -- What type of cell is (start, end)?
    invalidCell  = end < start      -- Unused cell
    emptyCatCell = start ≡ end      -- ε entries
    lexicalCell  = end - start ≡ 1  -- Lexical entries
    -- Convert lexical items to chart entries
    lexicalToChart     = LexicalEntry ∘ featuresOf
    -- All empty items.
    emptyItemEntries   = fmap lexicalToChart $ emptyItems g
    -- All lexical items that are homophonous with token.  This allows
    -- us to capture lexical ambiguity.
    lexicalItemEntries = fmap lexicalToChart lexicalItems
    lexicalItems       = findInLexicon token g
    token              = input !! start
    -- Finds all possible constituents.
    findConstituents = do
      midpoint ← [(start + 1) .. (end - 1)]
      lhs ← cky g input start midpoint
      rhs ← cky g input midpoint end
      tryMergeComplement midpoint lhs rhs ⧺ tryMergeSpecifier midpoint lhs rhs
    -- Try to satisfy a Merge by merging in a complement.  We can only
    -- do this if the lhs is a lexical entry and selects the rhs.
    tryMergeComplement midpoint (LexicalEntry lhsFeatures) rhs
      | lhsFeatures `selects` (entryFeatures rhs) =
          [MergeEntry (midpoint, satisfyMerge lhsFeatures)]
      | otherwise = []
    tryMergeComplement _ _ _ = []
    -- Try to satisfy a Merge by merging in a specifier.  We can only
    -- do this if the rhs is not a lexical entry and selects the lhs.
    tryMergeSpecifier midpoint lhs (MergeEntry (_, rhsFeatures))
      | rhsFeatures `selects` (entryFeatures lhs) =
          [MergeEntry (midpoint, satisfyMerge rhsFeatures)]
      | otherwise = []
    tryMergeSpecifier _ _ _ = []

full_chart ∷ Grammar -> [Token] → Matrix [ChartEntry]
full_chart g input = fromLists $ do
  x ← [0..end]
  return $
    do
      y ← [0..end]
      return $ cky g input x y
        where end = length input


--------------------------------------------------------------------------------
--                                                                RECOGNITION --
--------------------------------------------------------------------------------


-- | 'recognize' takes a list of tokens and returns whether the list
-- of tokens can be generated by the language.
recognizeTokens ∷ [Token] → Bool
recognizeTokens = isJust ∘ parseTokens


--------------------------------------------------------------------------------
--                                                               USER-FACING  --
--------------------------------------------------------------------------------


parse        = parseTokens        ∘ tokenize
partialParse = partialParseTokens ∘ tokenize
recognize    = recognizeTokens    ∘ tokenize
