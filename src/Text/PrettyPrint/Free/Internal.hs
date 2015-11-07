-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Free.Internal
-- Copyright   :  Google, Inc. (c) 2013,
--                Edward Kmett (c) 2011,
--                Daan Leijen  (c) 2000
--
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
-- * There are two display routines, 'displayS' for strings and 'displayIO'
-- for file based output.
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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------
module Text.PrettyPrint.Free.Internal (
  -- * Documents
    Doc(..), putDoc, hPutDoc

  -- * Basic combinators
  , char, text, nest, line, linebreak, group, softline
  , softbreak, hardline, flatAlt, flatten

  -- * Annotations
  , annotate, sdocAE, sdocScanAnn

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
  , SimpleDoc(..), renderPretty, renderCompact, renderSmart
  , displayS, displayIO, displayDecorated

  -- * Undocumented

  , column, nesting, width, columns, ribbon

  , docLeafyRec

  -- * Re-exported standard functions
  , empty, (<>)
  ) where

import Data.String
import Data.Foldable hiding (fold)
import Data.Traversable
import Data.Bifunctor
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Functor.Plus
import Data.Int
import Data.Word
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString.Lazy.UTF8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.List.NonEmpty (NonEmpty)
import Numeric.Natural (Natural)
import Control.Applicative
import Control.Monad
import Data.Sequence (Seq)
import Data.Semigroup
import System.IO (Handle,hPutStr,stdout)
import Prelude hiding (foldr1)

infixr 5 </>,<//>,`above`,`aboveBreak`
infixr 6 <+>

-----------------------------------------------------------
-- list, tupled and semiBraces pretty print a list of
-- documents either horizontally or vertically aligned.
-----------------------------------------------------------

-- | The document @(list xs)@ comma separates the documents @xs@ and
-- encloses them in square brackets. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All comma separators are put in front of the elements.
list :: Foldable f => f (Doc a e) -> Doc a e
list = encloseSep lbracket rbracket comma

-- | The document @(tupled xs)@ comma separates the documents @xs@ and
-- encloses them in parenthesis. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All comma separators are put in front of the elements.
tupled :: Foldable f => f (Doc a e) -> Doc a e
tupled = encloseSep lparen rparen comma

(<+>) :: Doc a e -> Doc a e -> Doc a e
x <+> y = x <> space <> y

-- | The document @(semiBraces xs)@ separates the documents @xs@ with
-- semi colons and encloses them in braces. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All semi colons are put in front of the elements.
semiBraces :: Foldable f => f (Doc a e) -> Doc a e
semiBraces = encloseSep lbrace rbrace semi

-- | The document @(encloseSep l r sep xs)@ concatenates the documents
-- @xs@ separated by @sep@ and encloses the resulting document by @l@
-- and @r@. The documents are rendered horizontally if that fits the
-- page. Otherwise they are aligned vertically. All separators are put
-- in front of the elements. For example, the combinator 'list' can be
-- defined with @encloseSep@:
--
-- > list xs = encloseSep lbracket rbracket comma xs
-- > test    = text "list" <+> (list (map int [10,200,3000]))
--
-- Which is layed out with a page width of 20 as:
--
-- @
-- list [10, 200, 3000]
-- @
--
-- But when the page width is 15, it is layed out as:
--
-- @
-- list [ 10
--      , 200
--      , 3000 ]
-- @
encloseSep :: Foldable f => Doc a e -> Doc a e -> Doc a e -> f (Doc a e) -> Doc a e
encloseSep left right sp ds0
    = case toList ds0 of
        []  -> left <> right
        [d] -> left <> d <> right
        ds  -> group $ align $ left'
                 <> (vcat (zipWith (<>) (empty : repeat (sp <> space)) ds))
                 <> right'
          where left'  = left <> flatAlt space empty
                right' = flatAlt space empty <> right


-----------------------------------------------------------
-- punctuate p [d1,d2,...,dn] => [d1 <> p,d2 <> p, ... ,dn]
-----------------------------------------------------------


-- | @(punctuate p xs)@ concatenates all documents in @xs@ with
-- document @p@ except for the last document.
--
-- > someText = map text ["words","in","a","tuple"]
-- > test     = parens (align (cat (punctuate comma someText)))
--
-- This is layed out on a page width of 20 as:
--
-- @
-- (words,in,a,tuple)
-- @
--
-- But when the page width is 15, it is layed out as:
--
-- @
-- (words,
--  in,
--  a,
--  tuple)
-- @
--
-- (If you want put the commas in front of their elements instead of
-- at the end, you should use 'tupled' or, in general, 'encloseSep'.)
punctuate :: Traversable f => Doc a e -> f (Doc a e) -> f (Doc a e)
punctuate p xs = snd $ mapAccumL (\(d:ds) _ -> (ds, if null ds then d else (d <> p))) (toList xs) xs

-----------------------------------------------------------
-- high-level combinators
-----------------------------------------------------------


-- | The document @(sep xs)@ concatenates all documents @xs@ either
-- horizontally with @(\<+\>)@, if it fits the page, or vertically with
-- @above@.
--
-- > sep xs  = group (vsep xs)
sep :: Foldable f => f (Doc a e) -> Doc a e
sep = group . vsep

-- | The document @(fillSep xs)@ concatenates documents @xs@
-- horizontally with @(\<+\>)@ as long as its fits the page, then
-- inserts a @line@ and continues doing that for all documents in
-- @xs@.
--
-- > fillSep xs  = foldr (</>) empty xs
fillSep :: Foldable f => f (Doc a e) -> Doc a e
fillSep = fold (</>)

-- | The document @(hsep xs)@ concatenates all documents @xs@
-- horizontally with @(\<+\>)@.
hsep :: Foldable f => f (Doc a e) -> Doc a e
hsep = fold (<+>)

-- | The document @(vsep xs)@ concatenates all documents @xs@
-- vertically with @above@. If a 'group' undoes the line breaks
-- inserted by @vsep@, all documents are separated with a space.
--
-- > someText = map text (words ("text to lay out"))
-- >
-- > test     = text "some" <+> vsep someText
--
-- This is layed out as:
--
-- @
-- some text
-- to
-- lay
-- out
-- @
--
-- The 'align' combinator can be used to align the documents under
-- their first element
--
-- > test = text "some" <+> align (vsep someText)
--
-- Which is printed as:
--
-- @
-- some text
--      to
--      lay
--      out
-- @
vsep :: Foldable f => f (Doc a e) -> Doc a e
vsep = fold above

-- | The document @(cat xs)@ concatenates all documents @xs@ either
-- horizontally with @(\<\>)@, if it fits the page, or vertically with
-- @aboveBreak@.
--
-- > cat xs  = group (vcat xs)
cat :: Foldable f => f (Doc a e) -> Doc a e
cat = group . vcat

-- | The document @(fillCat xs)@ concatenates documents @xs@
-- horizontally with @(\<\>)@ as long as its fits the page, then inserts
-- a @linebreak@ and continues doing that for all documents in @xs@.
--
-- > fillCat xs  = foldr (<//>) empty xs
fillCat :: Foldable f => f (Doc a e) -> Doc a e
fillCat = fold (<//>)

-- | The document @(hcat xs)@ concatenates all documents @xs@
-- horizontally with @(\<\>)@.
hcat :: Foldable f => f (Doc a e) -> Doc a e
hcat = fold (<>)

-- | The document @(vcat xs)@ concatenates all documents @xs@
-- vertically with @aboveBreak@. If a 'group' undoes the line breaks
-- inserted by @vcat@, all documents are directly concatenated.
vcat :: Foldable f => f (Doc a e) -> Doc a e
vcat = fold aboveBreak

fold :: Foldable f => (Doc a e -> Doc a e -> Doc a e) -> f (Doc a e) -> Doc a e
fold f xs = case toList xs of
  [] -> empty
  _  -> foldr1 f xs

instance Semigroup (Doc a e) where
  -- | The document @(x \<\> y)@ concatenates document @x@ and document
  -- @y@. It is an associative operation having 'empty' as a left and
  -- right unit.  (infixl 6)
  (<>) = Cat

instance Monoid (Doc a e) where
  mappend = Cat
  mempty = empty
  mconcat = hcat

-- | The document @(x \<\/\> y)@ concatenates document @x@ and @y@ with a
-- 'softline' in between. This effectively puts @x@ and @y@ either
-- next to each other (with a @space@ in between) or underneath each
-- other. (infixr 5)
(</>) :: Doc a e -> Doc a e -> Doc a e
x </> y = x <> softline <> y

-- | The document @(x \<\/\/\> y)@ concatenates document @x@ and @y@ with
-- a 'softbreak' in between. This effectively puts @x@ and @y@ either
-- right next to each other or underneath each other. (infixr 5)
(<//>) :: Doc a e -> Doc a e -> Doc a e
x <//> y        = x <> softbreak <> y

-- | The document @above x y@ concatenates document @x@ and @y@ with a
-- 'line' in between. (infixr 5)
above :: Doc a e -> Doc a e -> Doc a e
above x y         = x <> line <> y

-- | The document @aboveBreak x y@ concatenates document @x@ and @y@ with
-- a @linebreak@ in between. (infixr 5)
aboveBreak :: Doc a e -> Doc a e -> Doc a e
aboveBreak x y        = x <> linebreak <> y

-- | The document @softline@ behaves like 'space' if the resulting
-- output fits the page, otherwise it behaves like 'line'.
--
-- > softline = group line
softline :: Doc a e
softline = group line

-- | The document @softbreak@ behaves like 'empty' if the resulting
-- output fits the page, otherwise it behaves like 'line'.
--
-- > softbreak  = group linebreak
softbreak :: Doc a e
softbreak = group linebreak

-- | Document @(squotes x)@ encloses document @x@ with single quotes
-- \"'\".
squotes :: Doc a e -> Doc a e
squotes = enclose squote squote

-- | Document @(dquotes x)@ encloses document @x@ with double quotes
-- '\"'.
dquotes :: Doc a e -> Doc a e
dquotes = enclose dquote dquote

-- | Document @(braces x)@ encloses document @x@ in braces, \"{\" and
-- \"}\".
braces :: Doc a e -> Doc a e
braces = enclose lbrace rbrace

-- | Document @(parens x)@ encloses document @x@ in parenthesis, \"(\"
-- and \")\".
parens :: Doc a e -> Doc a e
parens = enclose lparen rparen

-- | Document @(angles x)@ encloses document @x@ in angles, \"\<\" and
-- \"\>\".
angles :: Doc a e -> Doc a e
angles = enclose langle rangle

-- | Document @(brackets x)@ encloses document @x@ in square brackets,
-- \"[\" and \"]\".
brackets :: Doc a e -> Doc a e
brackets = enclose lbracket rbracket

-- | The document @(enclose l r x)@ encloses document @x@ between
-- documents @l@ and @r@ using @(\<\>)@.
--
-- > enclose l r x   = l <> x <> r
enclose :: Doc a e -> Doc a e -> Doc a e -> Doc a e
enclose l r x = l <> x <> r

-- | The document @lparen@ contains a left parenthesis, \"(\".
lparen :: Doc a e
lparen = char '('

-- | The document @rparen@ contains a right parenthesis, \")\".
rparen :: Doc a e
rparen = char ')'

-- | The document @langle@ contains a left angle, \"\<\".
langle :: Doc a e
langle = char '<'

-- | The document @rangle@ contains a right angle, \">\".
rangle :: Doc a e
rangle = char '>'

-- | The document @lbrace@ contains a left brace, \"{\".
lbrace :: Doc a e
lbrace = char '{'

-- | The document @rbrace@ contains a right brace, \"}\".
rbrace :: Doc a e
rbrace = char '}'

-- | The document @lbracket@ contains a left square bracket, \"[\".
lbracket :: Doc a e
lbracket = char '['

-- | The document @rbracket@ contains a right square bracket, \"]\".
rbracket :: Doc a e
rbracket = char ']'

-- | The document @squote@ contains a single quote, \"'\".
squote :: Doc a e
squote = char '\''

-- | The document @dquote@ contains a double quote, '\"'.
dquote :: Doc a e
dquote = char '"'

-- | The document @semi@ contains a semi colon, \";\".
semi :: Doc a e
semi = char ';'

-- | The document @colon@ contains a colon, \":\".
colon :: Doc a e
colon = char ':'

-- | The document @comma@ contains a comma, \",\".
comma :: Doc a e
comma = char ','

-- | The document @space@ contains a single space, \" \".
--
-- > x <+> y   = x <> space <> y
space :: Doc a e
space = char ' '

-- | The document @dot@ contains a single dot, \".\".
dot :: Doc a e
dot = char '.'

-- | The document @backslash@ contains a back slash, \"\\\".
backslash :: Doc a e
backslash = char '\\'

-- | The document @equals@ contains an equal sign, \"=\".
equals :: Doc a e
equals = char '='

instance IsString (Doc a e) where
  fromString = pretty

-----------------------------------------------------------
-- overloading "pretty"
-----------------------------------------------------------

-- | The member @prettyList@ is only used to define the @instance Pretty
-- a => Pretty [a]@. In normal circumstances only the @pretty@ function
-- is used.
class Pretty a where
  pretty     :: a   -> Doc an e
  prettyList :: [a] -> Doc an e
  prettyList = list . map pretty

instance Pretty a => Pretty [a] where
  pretty = prettyList

instance Pretty (Doc a' e') where
  pretty = docLeafyRec (\_ -> Empty) (\_ -> id)

instance Pretty B.ByteString where
  pretty = pretty . B.toString

instance Pretty BL.ByteString where
  pretty = pretty . BL.toString

instance Pretty T.Text where
  pretty = pretty . T.encodeUtf8

instance Pretty TL.Text where
  pretty = pretty . TL.encodeUtf8

instance Pretty () where
  pretty () = text "()"

instance Pretty Bool where
  pretty = text . show

instance Pretty Char where
  pretty = char
  prettyList "" = empty
  prettyList ('\n':s) = line <> prettyList s
  prettyList s = case span (/='\n') s of
             (xs,ys) -> text xs <> prettyList ys

instance Pretty a => Pretty (Seq a) where
  pretty = prettyList . toList

instance Pretty a => Pretty (NonEmpty a) where
  pretty = prettyList . toList

instance Pretty Int where
  pretty = text . show

instance Pretty Int8 where
  pretty = text . show

instance Pretty Int16 where
  pretty = text . show

instance Pretty Int32 where
  pretty = text . show

instance Pretty Int64 where
  pretty = text . show

instance Pretty Word where
  pretty = text . show

instance Pretty Word8 where
  pretty = text . show

instance Pretty Word16 where
  pretty = text . show

instance Pretty Word32 where
  pretty = text . show

instance Pretty Word64 where
  pretty = text . show

instance Pretty Integer where
  pretty = text . show

instance Pretty Natural where
  pretty = text . show

instance Pretty Float where
  pretty = text . show

instance Pretty Double where
  pretty = text . show

instance (Pretty a,Pretty b) => Pretty (a,b) where
  pretty (x,y) = tupled [pretty x, pretty y]

instance (Pretty a,Pretty b,Pretty c) => Pretty (a,b,c) where
  pretty (x,y,z)= tupled [pretty x, pretty y, pretty z]

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = empty
  pretty (Just x) = pretty x

instance Pretty Rational where
  pretty = text . show

-----------------------------------------------------------
-- semi primitive: fill and fillBreak
-----------------------------------------------------------

-- | The document @(fillBreak i x)@ first renders document @x@. It
-- then appends @space@s until the width is equal to @i@. If the
-- width of @x@ is already larger than @i@, the nesting level is
-- increased by @i@ and a @line@ is appended. When we redefine @ptype@
-- in the previous example to use @fillBreak@, we get a useful
-- variation of the previous output:
--
-- > ptype (name,tp)
-- >        = fillBreak 6 (text name) <+> text "::" <+> text tp
--
-- The output will now be:
--
-- @
-- let empty  :: Doc a e
--     nest   :: Int -> Doc a e -> Doc a e
--     linebreak
--            :: Doc a e
-- @
fillBreak :: Int -> Doc a e -> Doc a e
fillBreak f x   = width x $ \w ->
                  if (w > f) then nest f linebreak
                             else text (spaces (f - w))


-- | The document @(fill i x)@ renders document @x@. It then appends
-- @space@s until the width is equal to @i@. If the width of @x@ is
-- already larger, nothing is appended. This combinator is quite
-- useful in practice to output a list of bindings. The following
-- example demonstrates this.
--
-- > types  = [("empty","Doc a e")
-- >          ,("nest","Int -> Doc a e -> Doc a e")
-- >          ,("linebreak","Doc a e")]
-- >
-- > ptype (name,tp)
-- >        = fill 6 (text name) <+> text "::" <+> text tp
-- >
-- > test   = text "let" <+> align (vcat (map ptype types))
--
-- Which is layed out as:
--
-- @
-- let empty  :: Doc a e
--     nest   :: Int -> Doc a e -> Doc a e
--     linebreak :: Doc a e
-- @
fill :: Int -> Doc a e -> Doc a e
fill f d = width d $ \w ->
                     if (w >= f)
                     then empty
                     else text (spaces (f - w))

width :: Doc a e -> (Int -> Doc a e) -> Doc a e
width d f = column (\k1 -> d <> column (\k2 -> f (k2 - k1)))


-----------------------------------------------------------
-- semi primitive: Alignment and indentation
-----------------------------------------------------------

-- | The document @(indent i x)@ indents document @x@ with @i@ spaces.
--
-- > test  = indent 4 (fillSep (map text
-- >         (words "the indent combinator indents these words !")))
--
-- Which lays out with a page width of 20 as:
--
-- @
--     the indent
--     combinator
--     indents these
--     words !
-- @
indent :: Int -> Doc a e -> Doc a e
indent i d = hang i (text (spaces i) <> d)

-- | The hang combinator implements hanging indentation. The document
-- @(hang i x)@ renders document @x@ with a nesting level set to the
-- current column plus @i@. The following example uses hanging
-- indentation for some text:
--
-- > test  = hang 4 (fillSep (map text
-- >         (words "the hang combinator indents these words !")))
--
-- Which lays out on a page with a width of 20 characters as:
--
-- @
-- the hang combinator
--     indents these
--     words !
-- @
--
-- The @hang@ combinator is implemented as:
--
-- > hang i x  = align (nest i x)
hang :: Int -> Doc a e -> Doc a e
hang i d = align (nest i d)

-- | The document @(align x)@ renders document @x@ with the nesting
-- level set to the current column. It is used for example to
-- implement 'hang'.
--
-- As an example, we will put a document right above another one,
-- regardless of the current nesting level:
--
-- > x $$ y  = align (above x y)
--
-- > test    = text "hi" <+> (text "nice" $$ text "world")
--
-- which will be layed out as:
--
-- @
-- hi nice
--    world
-- @
align :: Doc a e -> Doc a e
align d = column $ \k ->
         nesting $ \i -> nest (k - i) d   --nesting might be negative :-)

-----------------------------------------------------------
-- Primitives
-----------------------------------------------------------

-- | The abstract data type @Doc@ represents pretty documents.
--
-- @Doc@ is an instance of the 'Show' class. @(show doc)@ pretty
-- prints document @doc@ with a page width of 100 characters and a
-- ribbon width of 40 characters.
--
-- > show (text "hello" `above` text "world")
--
-- Which would return the string \"hello\\nworld\", i.e.
--
-- @
-- hello
-- world
-- @
data Doc a e
  = Fail
  | Empty
  | Char {-# UNPACK #-} !Char       -- invariant: char is not '\n'
  | Text {-# UNPACK #-} !Int String -- invariant: text doesn't contain '\n'
  | Line
  | FlatAlt (Doc a e) (Doc a e)         -- Render the first doc, but when flattened, render the second.
  | Cat (Doc a e) (Doc a e)
  | Nest {-# UNPACK #-} !Int (Doc a e)
  | Union (Doc a e) (Doc a e) -- invariant: first lines of first doc longer than the first lines of the second doc
  | Effect e
  | Annotate a (Doc a e)
  | Column  (Int -> Doc a e)
  | Nesting (Int -> Doc a e)
  | Columns (Maybe Int -> Doc a e)
  | Ribbon  (Maybe Int -> Doc a e)

docLeafyRec :: (e -> Doc a' e')                -- ^ Effect
            -> (a -> Doc a' e' -> Doc a' e')   -- ^ Annotate
            -> Doc a e -> Doc a' e'
docLeafyRec ef an = go
 where
  go Fail           = Fail
  go Empty          = Empty
  go (Char x)       = Char x
  go (Text i s)     = Text i s
  go Line           = Line
  go (FlatAlt l r)  = FlatAlt (go l) (go r)
  go (Cat l r)      = Cat (go l) (go r)
  go (Nest i d)     = Nest i (go d)
  go (Union l r)    = Union (go l) (go r)
  go (Effect x)     = ef x
  go (Annotate a d) = an a (go d)
  go (Column f)     = Column (go . f)
  go (Nesting k)    = Nesting (go . k)
  go (Columns k)    = Columns (go . k)
  go (Ribbon k)     = Ribbon (go . k)

instance Functor (Doc a) where
  fmap f = docLeafyRec (Effect . f) Annotate

instance Bifunctor Doc where
  bimap f g = docLeafyRec (Effect . g) (Annotate . f)

instance Apply (Doc a) where
  (<.>) = ap

instance Applicative (Doc a) where
  pure = Effect
  (<*>) = ap

annotate :: a -> Doc a e -> Doc a e
annotate = Annotate

instance Bind (Doc a) where
  (>>-) = (>>=)

instance Monad (Doc a) where
  return = pure
  d >>= k = docLeafyRec (\e -> k e) Annotate d
  fail _ = empty

instance Alt (Doc a) where
  (<!>) = (<>)

instance Plus (Doc a) where
  zero = empty

instance Alternative (Doc a) where
  (<|>) = (<>)
  -- | The empty document is, indeed, empty. Although @empty@ has no
  -- content, it does have a \'height\' of 1 and behaves exactly like
  -- @(text \"\")@ (and is therefore not a unit of @above@).
  empty = Empty

instance MonadPlus (Doc a) where
  mplus = (<>)
  mzero = empty

-- | The data type @SimpleDoc@ represents rendered documents and is
-- used by the display functions.
--
-- The @Int@ in @SText@ contains the length of the string. The @Int@
-- in @SLine@ contains the indentation for that line. The library
-- provides two default display functions 'displayS' and
-- 'displayIO'. You can provide your own display function by writing a
-- function from a @SimpleDoc@ to your own output format.
data SimpleDoc a e
  = SFail
  | SEmpty
  | SChar {-# UNPACK #-} !Char (SimpleDoc a e)
  | SText {-# UNPACK #-} !Int String (SimpleDoc a e)
  | SLine {-# UNPACK #-} !Int (SimpleDoc a e)
  | SEffect e (SimpleDoc a e)
  | SPushAnn a (SimpleDoc a e)
  | SPopAnn  a (SimpleDoc a e)

instance Functor (SimpleDoc a) where
  fmap _ SFail = SFail
  fmap _ SEmpty = SEmpty
  fmap f (SChar c d) = SChar c (fmap f d)
  fmap f (SText i s d) = SText i s (fmap f d)
  fmap f (SLine i d) = SLine i (fmap f d)
  fmap f (SEffect e d) = SEffect (f e) (fmap f d)
  fmap f (SPushAnn a d) = SPushAnn a (fmap f d)
  fmap f (SPopAnn  a d) = SPopAnn  a (fmap f d)

instance Foldable (SimpleDoc a) where
  foldMap _ SFail = mempty
  foldMap _ SEmpty = mempty
  foldMap f (SChar _ d) = foldMap f d
  foldMap f (SText _ _ d) = foldMap f d
  foldMap f (SLine _ d) = foldMap f d
  foldMap f (SEffect e d) = f e `mappend` foldMap f d
  foldMap f (SPushAnn _ d) = foldMap f d
  foldMap f (SPopAnn  _ d) = foldMap f d

instance Traversable (SimpleDoc a) where
  traverse _ SFail = pure SFail
  traverse _ SEmpty = pure SEmpty
  traverse f (SChar c d) = SChar c <$> traverse f d
  traverse f (SText i s d) = SText i s <$> traverse f d
  traverse f (SLine i d) = SLine i <$> traverse f d
  traverse f (SEffect e d) = SEffect <$> f e <*> traverse f d
  traverse f (SPushAnn a d) = SPushAnn a <$> traverse f d
  traverse f (SPopAnn  a d) = SPopAnn  a <$> traverse f d

-- | The document @(char c)@ contains the literal character @c@. The
-- character shouldn't be a newline (@'\n'@), the function 'line'
-- should be used for line breaks.
char :: Char -> Doc a e
char '\n' = line
char c = Char c

-- | The document @(text s)@ contains the literal string @s@. The
-- string shouldn't contain any newline (@'\n'@) characters. If the
-- string contains newline characters, the function 'string' should be
-- used.
text :: String -> Doc a e
text "" = Empty
text s  = Text (length s) s

-- | The @line@ document advances to the next line and indents to the
-- current nesting level. Document @line@ behaves like @(text \" \")@
-- if the line break is undone by 'group'.
line :: Doc a e
line = FlatAlt Line space

-- | The @linebreak@ document advances to the next line and indents to
-- the current nesting level. Document @linebreak@ behaves like
-- 'empty' if the line break is undone by 'group'.
linebreak :: Doc a e
linebreak = FlatAlt Line empty

-- | A linebreak that can not be flattened; it is guaranteed to be
-- rendered as a newline.
hardline :: Doc a e
hardline = Line

-- | The document @(nest i x)@ renders document @x@ with the current
-- indentation level increased by i (See also 'hang', 'align' and
-- 'indent').
--
-- > nest 2 (text "hello" `above` text "world") `above` text "!"
--
-- outputs as:
--
-- @
-- hello
--   world
-- !
-- @
nest :: Int -> Doc a e -> Doc a e
nest = Nest

column, nesting :: (Int -> Doc a e) -> Doc a e
column = Column
nesting = Nesting

columns :: (Maybe Int -> Doc a e) -> Doc a e
columns = Columns

ribbon :: (Maybe Int -> Doc a e) -> Doc a e
ribbon = Ribbon

-- | The @group@ combinator is used to specify alternative
-- layouts. The document @(group x)@ undoes all line breaks in
-- document @x@. The resulting line is added to the current line if
-- that fits the page. Otherwise, the document @x@ is rendered without
-- any changes.
group :: Doc a e -> Doc a e
group x = Union (flatten x) x

-- | @flatAlt@ creates a document that changes when flattened; normally
-- it is rendered as the first argument, but when flattened is rendered
-- as the second.
flatAlt :: Doc a e -> Doc a e -> Doc a e
flatAlt = FlatAlt

flatten :: Doc a e -> Doc a e
flatten (FlatAlt _ y)   = y
flatten (Cat x y)       = Cat (flatten x) (flatten y)
flatten (Nest i x)      = Nest i (flatten x)
flatten  Line           = Fail
flatten (Union x _)     = flatten x
flatten (Column f)      = Column (flatten . f)
flatten (Nesting f)     = Nesting (flatten . f)
flatten (Columns f)     = Columns (flatten . f)
flatten (Ribbon f)      = Ribbon (flatten . f)
flatten other           = other                     --Empty,Char,Text

-----------------------------------------------------------
-- Renderers
-----------------------------------------------------------

-----------------------------------------------------------
-- renderPretty: the default pretty printing algorithm
-----------------------------------------------------------

-- list of indentation/document pairs; saves an indirection over [(Int,Doc)]
data Docs a e = Nil
          | Cons {-# UNPACK #-} !Int (Doc a e) (Docs a e)

-- | This is the default pretty printer which is used by 'show',
-- 'putDoc' and 'hPutDoc'. @(renderPretty ribbonfrac width x)@ renders
-- document @x@ with a page width of @width@ and a ribbon width of
-- @(ribbonfrac * width)@ characters. The ribbon width is the maximal
-- amount of non-indentation characters on a line. The parameter
-- @ribbonfrac@ should be between @0.0@ and @1.0@. If it is lower or
-- higher, the ribbon width will be 0 or @width@ respectively.
renderPretty :: Float -> Int -> Doc a e -> SimpleDoc a e
renderPretty = renderFits nicest1

-- | A slightly smarter rendering algorithm with more lookahead. It provides
-- provide earlier breaking on deeply nested structures.
-- For example, consider this python-ish pseudocode:
-- @fun(fun(fun(fun(fun([abcdefg, abcdefg])))))@
-- If we put a softbreak (+ nesting 2) after each open parenthesis, and align
-- the elements of the list to match the opening brackets, this will render with
-- @renderPretty@ and a page width of 20c as:
-- @
-- fun(fun(fun(fun(fun([
--                     | abcdef,
--                     | abcdef,
--                     ]
--   )))))             |
-- @
-- Where the 20c. boundary has been marked with |. Because @renderPretty@ only
-- uses one-line lookahead, it sees that the first line fits, and is stuck
-- putting the second and third lines after the 20c mark. In contrast,
-- @renderSmart@ will continue to check the potential document up to the end of
-- the indentation level. Thus, it will format the document as:
--
-- @
-- fun(                |
--   fun(              |
--     fun(            |
--       fun(          |
--         fun([       |
--               abcdef,
--               abcdef,
--             ]       |
--   )))))             |
-- @
-- Which fits within the 20c. mark.
-- In addition, @renderSmart@ uses this lookahead to minimize the number of
-- lines printed, leading to more compact and visually appealing output.
-- Consider this example using the same syntax as above:
-- @aaaaaaaaaaa([abc, def, ghi])@
-- When rendered with @renderPretty@ and a page width of 20c, we get:
-- @
-- aaaaaaaaaaa([ abc
--             , def
--             , ghi ])
-- @
-- Whereas when rendered with @renderSmart@ and a page width of 20c, we get:
-- @
-- aaaaaaaaaaa(
--   [abc, def, ghi])
-- @
renderSmart :: Int -> Doc a e -> SimpleDoc a e
renderSmart = renderFits nicestR 1.0

renderFits :: (Int -> Int -> Int -> Int -> SimpleDoc a e -> SimpleDoc a e
               -> SimpleDoc a e)
              -> Float -> Int -> Doc a e -> SimpleDoc a e
renderFits nicest rfrac w x
    = best 0 0 SEmpty (Cons 0 x Nil)
    where
      -- r :: the ribbon width in characters
      r  = max 0 (min w (round (fromIntegral w * rfrac)))

      -- best :: n = indentation of current line
      --         k = current column
      --        (ie. (k >= n) && (k - n == count of inserted characters)
      best _ _ z Nil            = z
      best n k z (Cons i d ds) =
        case d of
          Fail          -> SFail
          Empty         -> best n k z ds
          Char c        -> let k' = k+1 in seq k' (SChar c (best n k' z ds))
          Text l s      -> let k' = k+l in seq k' (SText l s (best n k' z ds))
          Line          -> SLine i (best i i z ds)
          FlatAlt l _   -> best n k z (Cons i l ds)
          Cat x' y      -> best n k z (Cons i x' (Cons i y ds))
          Nest j x'     -> let i' = i+j in seq i' (best n k z (Cons i' x' ds))
          Effect e      -> SEffect e (best n k z ds)
          Annotate a d' -> SPushAnn a (best n k (SPopAnn a $ best n k z ds) (Cons i d' Nil))
          Union p q     -> nicest n k w r (best n k z (Cons i p ds))
                                          (best n k z (Cons i q ds))
          Column f      -> best n k z (Cons i (f k) ds)
          Nesting f     -> best n k z (Cons i (f i) ds)
          Columns f     -> best n k z (Cons i (f $ Just w) ds)
          Ribbon f      -> best n k z (Cons i (f $ Just r) ds)

-- @nicest1@ compares the first lines of the two documents.
-- n = nesting, k = column, p = pagewidth
nicest1 :: Int -> Int -> Int -> Int -> SimpleDoc a e -> SimpleDoc a e -> SimpleDoc a e
nicest1 n k p r x' y | fits (min n k) wid x' = x'
                     | otherwise = y
  where wid = min (p - k) (r - k + n)
        fits _ w _        | w < 0 = False
        fits _ _ SFail            = False
        fits _ _ SEmpty           = True
        fits m w (SChar _ x)      = fits m (w - 1) x
        fits m w (SText l _ x)    = fits m (w - l) x
        fits _ _ (SLine _ _)      = True
        fits m w (SEffect _ x)    = fits m w x
        fits m w (SPushAnn _ x)   = fits m w x
        fits m w (SPopAnn  _ x)   = fits m w x

-- @nicestR@ compares the initial lines of the two documents that are nested at
-- least as deep as the current nesting level. If the initial lines of both
-- documents fit within the page width, the document that takes fewer lines is
-- prefered, with preference toward the first.
nicestR :: Int -> Int -> Int -> Int -> SimpleDoc a e -> SimpleDoc a e -> SimpleDoc a e
nicestR n k p r x' y =
  if fits (min n k) wid x' <= fits (min n k) wid y then x' else y
  where wid = min (p - k) (r - k + n)
        inf = 1.0/0 :: Double
        -- @fits@ has a little more lookahead: assuming that nesting roughly
        -- corresponds to syntactic depth, @fitsR@ checks that not only the
        -- current line fits, but the entire syntactic structure being formatted
        -- at this level of indentation fits. If we were to remove the second
        -- case for @SLine@, we would check that not only the current structure
        -- fits, but also the rest of the document, which would be slightly more
        -- intelligent but would have exponential runtime (and is prohibitively
        -- expensive in practice).
        -- m = minimum nesting level to fit in
        -- w = the width in which to fit the first line
        fits _ w _           | w < 0     = inf
        fits _ _ SFail                   = inf
        fits _ _ SEmpty                  = 0
        fits m w (SChar _ x)             = fits m (w - 1) x
        fits m w (SText l _ x)           = fits m (w - l) x
        fits m _ (SLine i x) | m < i     = 1 + fits m (p - i) x
                             | otherwise = 0
        fits m w (SEffect _ x)           = fits m w x
        fits m w (SPushAnn _ x)          = fits m w x
        fits m w (SPopAnn  _ x)          = fits m w x


-----------------------------------------------------------
-- renderCompact: renders documents without indentation
--  fast and fewer characters output, good for machines
-----------------------------------------------------------


-- | @(renderCompact x)@ renders document @x@ without adding any
-- indentation. Since no \'pretty\' printing is involved, this
-- renderer is very fast. The resulting output contains fewer
-- characters than a pretty printed version and can be used for output
-- that is read by other programs.
renderCompact :: Doc a e -> SimpleDoc a e
renderCompact x
    = scan SEmpty 0 [x]
    where
      scan z _ []     = z
      scan z k (d:ds) =
        case d of
          Fail          -> SFail
          Empty         -> scan z k ds
          Char c        -> let k' = k+1 in seq k' (SChar c (scan z k' ds))
          Text l s      -> let k' = k+l in seq k' (SText l s (scan z k' ds))
          Effect e      -> SEffect e (scan z k ds)
          Annotate a d' -> SPushAnn a (scan (SPopAnn a $ scan z k ds) k [d'])
          Line          -> SLine 0 (scan z 0 ds)
          FlatAlt y _   -> scan z k (y:ds)
          Cat y z'      -> scan z k (y:z':ds)
          Nest _ y      -> scan z k (y:ds)
          Union _ y     -> scan z k (y:ds)
          Column f      -> scan z k (f k:ds)
          Nesting f     -> scan z k (f 0:ds)
          Columns f     -> scan z k (f Nothing:ds)
          Ribbon  f     -> scan z k (f Nothing:ds)

-----------------------------------------------------------
-- Displayers:  displayS and displayIO
-----------------------------------------------------------


sdocAE :: (r -> e -> SimpleDoc a' e' -> SimpleDoc a' e') -- ^ SEffect processor
       -> (r -> a -> r)                                  -- ^ SPushAnn state merge
       -> (r -> SimpleDoc a' e' -> SimpleDoc a' e')      -- ^ SPushAnn processor
       -> (r -> SimpleDoc a' e' -> SimpleDoc a' e')      -- ^ SPopAnn processor
       -> r                                              -- ^ Initial state
       -> SimpleDoc a e -> SimpleDoc a' e'
sdocAE ef arf adf apf r0 = go [] r0
 where
  go _      _ SFail          = SFail
  go _      _ SEmpty         = SEmpty
  go rs     r (SChar c x)    = SChar c   (go rs r x)
  go rs     r (SText l s x)  = SText l s (go rs r x)
  go rs     r (SLine i x)    = SLine i   (go rs r x)
  go rs     r (SEffect e x)  = ef r e (go rs r x)
  go rs     r (SPushAnn a x) = let r' = arf r a in adf r (go (r:rs) r' x)
  go []     _ (SPopAnn _ x)  = apf r0 (go [] r0 x)
  go (r:rs) _ (SPopAnn _ x)  = apf r  (go rs r  x)

sdocScanAnn :: (r -> e -> e')
            -> (r -> a -> r)
            -> r
            -> SimpleDoc a e
            -> SimpleDoc r e'
sdocScanAnn ef af r0 = sdocAE (\r e d -> SEffect (ef r e) d) af SPushAnn SPopAnn r0

-- | Display a rendered document.
--
-- This function takes a means of starting an annotated region, a means of ending it,
-- and a means of displaying a string, with effects @f@ to display or compute the output @o@.
displayDecorated :: (Applicative f, Monoid o)
                 => (a -> f o)        -- ^ How to start an annotated region
                 -> (a -> f o)        -- ^ How to end an annotated region
                 -> (e -> f o)        -- ^ How to display a point effect
                 -> (String -> f o)   -- ^ How to display a string (from document or whitespace)
                 -> SimpleDoc a e
                 -> f o
displayDecorated start stop eff string = go
 where
  go SFail          = error $ "@SFail@ can not appear uncaught in a " ++
                              "rendered @SimpleDoc@"
  go SEmpty         = pure mempty
  go (SChar c x)    = string (pure c) <++> go x
  go (SText _ s x)  = string s <++> go x
  go (SLine i x)    = string ('\n':indentation i) <++> go x
  go (SEffect e x)  = eff   e <++> go x
  go (SPushAnn a x) = start a <++> go x
  go (SPopAnn  a x) = stop  a <++> go x

  (<++>) = liftA2 mappend

-- | @(displayIO handle simpleDoc)@ writes @simpleDoc@ to the file
-- handle @handle@, discarding all effects and annotations. This function
-- is used for example by 'hPutDoc':
--
-- > hPutDoc handle doc  = displayIO handle (renderPretty 0.4 100 doc)
displayIO :: Handle -> SimpleDoc a e -> IO ()
displayIO handle = displayDecorated cpu cpu cpu (hPutStr handle)
 where cpu = const (pure ())

-- | @(displayS simpleDoc)@ takes the output @simpleDoc@ from a
-- rendering function and transforms it to a 'ShowS' type (for use in
-- the 'Show' class).  Along the way, all effects and annotations are
-- discarded.
--
-- > showWidth :: Int -> Doc -> String
-- > showWidth w x   = displayS (renderPretty 0.4 w x) ""
displayS :: SimpleDoc a e -> ShowS
displayS = displayDecorated ci ci ci showString
 where ci = const id

-----------------------------------------------------------
-- default pretty printers: show, putDoc and hPutDoc
-----------------------------------------------------------
instance Show (Doc a e) where
  showsPrec _ doc = displayS (renderPretty 0.4 80 doc)

-- | The action @(putDoc doc)@ pretty prints document @doc@ to the
-- standard output, with a page width of 100 characters and a ribbon
-- width of 40 characters.
--
-- > main :: IO ()
-- > main = do{ putDoc (text "hello" <+> text "world") }
--
-- Which would output
--
-- @
-- hello world
-- @
putDoc :: Doc a e -> IO ()
putDoc doc = hPutDoc stdout doc

-- | @(hPutDoc handle doc)@ pretty prints document @doc@ to the file
-- handle @handle@ with a page width of 100 characters and a ribbon
-- width of 40 characters.
--
-- > main = do{ handle <- openFile "MyFile" WriteMode
-- >          ; hPutDoc handle (vcat (map text
-- >                            ["vertical","text"]))
-- >          ; hClose handle
-- >          }
hPutDoc :: Handle -> Doc a e -> IO ()
hPutDoc handle doc = displayIO handle (renderPretty 0.4 100 doc)

-----------------------------------------------------------
-- insert spaces
-- "indentation" used to insert tabs but tabs seem to cause
-- more trouble than they solve :-)
-----------------------------------------------------------
spaces :: Int -> String
spaces n | n <= 0    = ""
         | otherwise = replicate n ' '

indentation :: Int -> String
indentation n = spaces n

--indentation n   | n >= 8    = '\t' : indentation (n-8)
--                | otherwise = spaces n

--  LocalWords:  PPrint combinators Wadler Wadler's encloseSep
