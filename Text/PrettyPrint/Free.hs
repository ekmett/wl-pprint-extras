{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Free
-- Copyright   :  Edward Kmett (c) 2011,
--                Daan Leijen (c) 2000
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Pretty print module based on Daan Leijen's implementation of Philip Wadler's 
-- \"prettier printer\"
--
-- @
--      \"A prettier printer\"
--      Draft paper, April 1997, revised March 1998.
--      <http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf>
-- @
--
-- This is an implementation of the pretty printing combinators
-- described by Philip Wadler (1997). In their bare essence, the
-- combinators of Wadler are not expressive enough to describe some
-- commonly occurring layouts. The PPrint library adds new primitives
-- to describe these layouts and works well in practice.
--
-- The library is based on a single way to concatenate documents,
-- which is associative and has both a left and right unit.  This
-- simple design leads to an efficient and short implementation. The
-- simplicity is reflected in the predictable behaviour of the
-- combinators which make them easy to use in practice.
--
-- A thorough description of the primitive combinators and their
-- implementation can be found in Philip Wadler's paper
-- (1997). Additions and the main differences with his original paper
-- are:
--
-- * The nil document is called empty.
--
-- * The operator '</>' is used
-- for soft line breaks.
--
-- * There are three new primitives: 'align', 'fill' and
-- 'fillBreak'. These are very useful in practice.
--
-- * Lots of other useful combinators, like 'fillSep' and 'list'.
--
-- * There are two renderers, 'renderPretty' for pretty printing and
-- 'renderCompact' for compact output. The pretty printing algorithm
-- also uses a ribbon-width now for even prettier output.
--
-- * There are two displayers, 'displayS' for strings and 'displayIO' for
-- file based output.
--
-- * There is a 'Pretty' class.
--
-- * The implementation uses optimised representations and strictness
-- annotations.
--
-- * A type argument has been added and embedded 'effects' can be seen in
-- the SimpleDoc type.
-- 
-- 

-----------------------------------------------------------
module Text.PrettyPrint.Free (
  -- * Documents
    Doc, putDoc, hPutDoc

  -- * Basic combinators
  , char, text, nest, line, linebreak, group, softline
  , softbreak

  -- * Alignment
  --
  -- The combinators in this section can not be described by Wadler's
  -- original combinators. They align their output relative to the
  -- current output position - in contrast to @nest@ which always
  -- aligns to the current nesting level. This deprives these
  -- combinators from being \`optimal\'. In practice however they
  -- prove to be very useful. The combinators in this section should
  -- be used with care, since they are more expensive than the other
  -- combinators. For example, @align@ shouldn't be used to pretty
  -- print all top-level declarations of a language, but using @hang@
  -- for let expressions is fine.
  , align, hang, indent, encloseSep, list, tupled, semiBraces

  -- * Operators
  , (<+>), above, (</>), aboveBreak, (<//>)

  -- * List combinators
  , hsep, vsep, fillSep, sep, hcat, vcat, fillCat, cat, punctuate

  -- * Fillers
  , fill, fillBreak

  -- * Bracketing combinators
  , enclose, squotes, dquotes, parens, angles, braces, brackets

  -- * Character documents
  , lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket
  , squote, dquote, semi, colon, comma, space, dot, backslash, equals

  -- * Pretty class
  , Pretty(..)

  -- * Rendering
  , SimpleDoc(..), renderPretty, renderCompact
  , displayS, displayIO

  -- * Undocumented

  , column, nesting, width, columns

  -- * Re-exported standard functions
  , empty, (<>)
  ) where

import Text.PrettyPrint.Free.Internal
