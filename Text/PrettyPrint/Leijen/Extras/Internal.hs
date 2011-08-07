{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Leijen.Extras.Internal
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
module Text.PrettyPrint.Leijen.Extras.Internal (
  -- * Documents
    Doc(..), putDoc, hPutDoc

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

  -- * Primitive type documents
  , string, int, integer, float, double, rational

  -- * Pretty class
  , Pretty(..)

  -- * Rendering
  , SimpleDoc(..), renderPretty, renderCompact
  , displayS, displayIO

  -- * Undocumented
  , bool

  , column, nesting, width, columns, ribbon

  -- * Re-exported standard functions
  , empty, (<>)
  ) where

import Data.String
import Data.Foldable hiding (fold)
import Data.Traversable
import Data.Monoid
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Functor.Plus
import Control.Applicative
import Control.Monad
import Data.Semigroup
import System.IO (Handle,hPutStr,hPutChar,stdout)
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
list :: Foldable f => f (Doc e) -> Doc e
list = encloseSep lbracket rbracket comma

-- | The document @(tupled xs)@ comma separates the documents @xs@ and
-- encloses them in parenthesis. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All comma separators are put in front of the elements.
tupled :: Foldable f => f (Doc e) -> Doc e 
tupled = encloseSep lparen rparen comma

(<+>) :: Doc e -> Doc e -> Doc e
x <+> y = x <> space <> y

-- | The document @(semiBraces xs)@ separates the documents @xs@ with
-- semi colons and encloses them in braces. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All semi colons are put in front of the elements.
semiBraces :: Foldable f => f (Doc e) -> Doc e
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
-- list [10,200,3000]
-- @
--
-- But when the page width is 15, it is layed out as:
--
-- @
-- list [10
--      ,200
--      ,3000]
-- @
encloseSep :: Foldable f => Doc e -> Doc e -> Doc e -> f (Doc e) -> Doc e
encloseSep left right sp ds0
    = case toList ds0 of
        []  -> left <> right
        [d] -> left <> d <> right
        ds  -> align (cat (zipWith (<>) (left : repeat sp) ds) <> right) 


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
punctuate :: Traversable f => Doc e -> f (Doc e) -> f (Doc e)
punctuate p xs = snd $ mapAccumL (\(d:ds) _ -> (ds, if null ds then d else (d <> p))) (toList xs) xs

-----------------------------------------------------------
-- high-level combinators
-----------------------------------------------------------


-- | The document @(sep xs)@ concatenates all documents @xs@ either
-- horizontally with @(\<+\>)@, if it fits the page, or vertically with
-- @above@.
--
-- > sep xs  = group (vsep xs)
sep :: Foldable f => f (Doc e) -> Doc e
sep = group . vsep

-- | The document @(fillSep xs)@ concatenates documents @xs@
-- horizontally with @(\<+\>)@ as long as its fits the page, than
-- inserts a @line@ and continues doing that for all documents in
-- @xs@.
--
-- > fillSep xs  = foldr (\<\/\>) empty xs
fillSep :: Foldable f => f (Doc e) -> Doc e
fillSep = fold (</>)

-- | The document @(hsep xs)@ concatenates all documents @xs@
-- horizontally with @(\<+\>)@.
hsep :: Foldable f => f (Doc e) -> Doc e
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
vsep :: Foldable f => f (Doc e) -> Doc e
vsep = fold above

-- | The document @(cat xs)@ concatenates all documents @xs@ either
-- horizontally with @(\<\>)@, if it fits the page, or vertically with
-- @aboveBreak@.
--
-- > cat xs  = group (vcat xs)
cat :: Foldable f => f (Doc e) -> Doc e
cat = group . vcat

-- | The document @(fillCat xs)@ concatenates documents @xs@
-- horizontally with @(\<\>)@ as long as its fits the page, than inserts
-- a @linebreak@ and continues doing that for all documents in @xs@.
--
-- > fillCat xs  = foldr (\<\/\/\>) empty xs
fillCat :: Foldable f => f (Doc e) -> Doc e
fillCat = fold (<//>)

-- | The document @(hcat xs)@ concatenates all documents @xs@
-- horizontally with @(\<\>)@.
hcat :: Foldable f => f (Doc e) -> Doc e
hcat = fold (<>)

-- | The document @(vcat xs)@ concatenates all documents @xs@
-- vertically with @aboveBreak@. If a 'group' undoes the line breaks
-- inserted by @vcat@, all documents are directly concatenated.
vcat :: Foldable f => f (Doc e) -> Doc e
vcat = fold aboveBreak

fold :: Foldable f => (Doc e -> Doc e -> Doc e) -> f (Doc e) -> Doc e
fold f xs = case toList xs of 
  [] -> empty
  _  -> foldr1 f xs

instance Semigroup (Doc e) where
  -- | The document @(x \<\> y)@ concatenates document @x@ and document
  -- @y@. It is an associative operation having 'empty' as a left and
  -- right unit.  (infixl 6)
  (<>) = Cat

instance Monoid (Doc e) where
  mappend = Cat
  mempty = empty

-- | The document @(x \<\/\> y)@ concatenates document @x@ and @y@ with a
-- 'softline' in between. This effectively puts @x@ and @y@ either
-- next to each other (with a @space@ in between) or underneath each
-- other. (infixr 5)
(</>) :: Doc e -> Doc e -> Doc e
x </> y         = x <> softline <> y

-- | The document @(x \<\/\/\> y)@ concatenates document @x@ and @y@ with
-- a 'softbreak' in between. This effectively puts @x@ and @y@ either
-- right next to each other or underneath each other. (infixr 5)
(<//>) :: Doc e -> Doc e -> Doc e
x <//> y        = x <> softbreak <> y

-- | The document @above x y@ concatenates document @x@ and @y@ with a
-- 'line' in between. (infixr 5)
above :: Doc e -> Doc e -> Doc e
above x y         = x <> line <> y

-- | The document @aboveBreak x y@ concatenates document @x@ and @y@ with
-- a @linebreak@ in between. (infixr 5)
aboveBreak :: Doc e -> Doc e -> Doc e
aboveBreak x y        = x <> linebreak <> y

-- | The document @softline@ behaves like 'space' if the resulting
-- output fits the page, otherwise it behaves like 'line'.
--
-- > softline = group line
softline :: Doc e
softline = group line

-- | The document @softbreak@ behaves like 'empty' if the resulting
-- output fits the page, otherwise it behaves like 'line'.
--
-- > softbreak  = group linebreak
softbreak :: Doc e
softbreak = group linebreak

-- | Document @(squotes x)@ encloses document @x@ with single quotes
-- \"'\".
squotes :: Doc e -> Doc e
squotes = enclose squote squote

-- | Document @(dquotes x)@ encloses document @x@ with double quotes
-- '\"'.
dquotes :: Doc e -> Doc e
dquotes = enclose dquote dquote

-- | Document @(braces x)@ encloses document @x@ in braces, \"{\" and
-- \"}\".
braces :: Doc e -> Doc e
braces = enclose lbrace rbrace

-- | Document @(parens x)@ encloses document @x@ in parenthesis, \"(\"
-- and \")\".
parens :: Doc e -> Doc e
parens = enclose lparen rparen

-- | Document @(angles x)@ encloses document @x@ in angles, \"\<\" and
-- \"\>\".
angles :: Doc e -> Doc e
angles = enclose langle rangle

-- | Document @(brackets x)@ encloses document @x@ in square brackets,
-- \"[\" and \"]\".
brackets :: Doc e -> Doc e
brackets = enclose lbracket rbracket

-- | The document @(enclose l r x)@ encloses document @x@ between
-- documents @l@ and @r@ using @(\<\>)@.
--
-- > enclose l r x   = l <> x <> r
enclose :: Doc e -> Doc e -> Doc e -> Doc e
enclose l r x = l <> x <> r

-- | The document @lparen@ contains a left parenthesis, \"(\".
lparen :: Doc e
lparen = char '('
-- | The document @rparen@ contains a right parenthesis, \")\".
rparen :: Doc e
rparen = char ')'
-- | The document @langle@ contains a left angle, \"\<\".
langle :: Doc e
langle = char '<'
-- | The document @rangle@ contains a right angle, \">\".
rangle :: Doc e
rangle = char '>'
-- | The document @lbrace@ contains a left brace, \"{\".
lbrace :: Doc e
lbrace = char '{'
-- | The document @rbrace@ contains a right brace, \"}\".
rbrace :: Doc e
rbrace = char '}'
-- | The document @lbracket@ contains a left square bracket, \"[\".
lbracket :: Doc e
lbracket = char '['
-- | The document @rbracket@ contains a right square bracket, \"]\".
rbracket :: Doc e
rbracket = char ']'


-- | The document @squote@ contains a single quote, \"'\".
squote :: Doc e
squote = char '\''
-- | The document @dquote@ contains a double quote, '\"'.
dquote :: Doc e
dquote = char '"'
-- | The document @semi@ contains a semi colon, \";\".
semi :: Doc e
semi = char ';'
-- | The document @colon@ contains a colon, \":\".
colon :: Doc e
colon = char ':'
-- | The document @comma@ contains a comma, \",\".
comma :: Doc e
comma = char ','
-- | The document @space@ contains a single space, \" \".
--
-- > x <+> y   = x <> space <> y
space :: Doc e 
space = char ' '
-- | The document @dot@ contains a single dot, \".\".
dot :: Doc e
dot = char '.'
-- | The document @backslash@ contains a back slash, \"\\\".
backslash :: Doc e
backslash = char '\\'
-- | The document @equals@ contains an equal sign, \"=\".
equals :: Doc e
equals = char '='

instance IsString (Doc e) where
  fromString = string

-----------------------------------------------------------
-- Combinators for prelude types
-----------------------------------------------------------

-- string is like "text" but replaces '\n' by "line"

-- | The document @(string s)@ concatenates all characters in @s@
-- using @line@ for newline characters and @char@ for all other
-- characters. It is used instead of 'text' whenever the text contains
-- newline characters.
string :: String -> Doc e
string "" = empty
string ('\n':s) = line <> string s
string s = case (span (/='\n') s) of
             (xs,ys) -> text xs <> string ys

bool :: Bool -> Doc e
bool = text . show

-- | The document @(int i)@ shows the literal integer @i@ using
-- 'text'.
int :: Int -> Doc e
int = text . show

-- | The document @(integer i)@ shows the literal integer @i@ using
-- 'text'.
integer :: Integer -> Doc e
integer = text . show

-- | The document @(float f)@ shows the literal float @f@ using
-- 'text'.
float :: Float -> Doc e
float = text . show

-- | The document @(double d)@ shows the literal double @d@ using
-- 'text'.
double :: Double -> Doc e
double = text . show

-- | The document @(rational r)@ shows the literal rational @r@ using
-- 'text'.
rational :: Rational -> Doc e
rational = text . show

-----------------------------------------------------------
-- overloading "pretty"
-----------------------------------------------------------

-- | The member @prettyList@ is only used to define the @instance Pretty
-- a => Pretty [a]@. In normal circumstances only the @pretty@ function
-- is used.
class Pretty a where
  pretty     :: a   -> Doc e
  prettyList :: [a] -> Doc e
  prettyList = list . map pretty

instance Pretty a => Pretty [a] where
  pretty = prettyList

-- instance Pretty (Doc ()) where
--   pretty = id

instance Pretty () where
  pretty () = text "()"

instance Pretty Bool where
  pretty = bool

instance Pretty Char where
  pretty = char
  prettyList s = string s

instance Pretty Int where
  pretty = int

instance Pretty Integer where
  pretty = integer

instance Pretty Float where
  pretty = float

instance Pretty Double where
  pretty = double

--instance Pretty Rational where
--  pretty = rational

instance (Pretty a,Pretty b) => Pretty (a,b) where
  pretty (x,y)  = tupled [pretty x, pretty y]

instance (Pretty a,Pretty b,Pretty c) => Pretty (a,b,c) where
  pretty (x,y,z)= tupled [pretty x, pretty y, pretty z]

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing        = empty
  pretty (Just x)       = pretty x

-----------------------------------------------------------
-- semi primitive: fill and fillBreak
-----------------------------------------------------------

-- | The document @(fillBreak i x)@ first renders document @x@. It
-- than appends @space@s until the width is equal to @i@. If the
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
-- let empty  :: Doc e
--     nest   :: Int -> Doc e -> Doc e 
--     linebreak
--            :: Doc e
-- @
fillBreak :: Int -> Doc e -> Doc e
fillBreak f x   = width x $ \w ->
                  if (w > f) then nest f linebreak
                             else text (spaces (f - w))


-- | The document @(fill i x)@ renders document @x@. It than appends
-- @space@s until the width is equal to @i@. If the width of @x@ is
-- already larger, nothing is appended. This combinator is quite
-- useful in practice to output a list of bindings. The following
-- example demonstrates this.
--
-- > types  = [("empty","Doc e")
-- >          ,("nest","Int -> Doc e -> Doc e")
-- >          ,("linebreak","Doc e")]
-- >
-- > ptype (name,tp)
-- >        = fill 6 (text name) <+> text "::" <+> text tp
-- >
-- > test   = text "let" <+> align (vcat (map ptype types))
--
-- Which is layed out as:
--
-- @
-- let empty  :: Doc e
--     nest   :: Int -> Doc e -> Doc e
--     linebreak :: Doc e
-- @
fill :: Int -> Doc e -> Doc e
fill f d = width d $ \w ->
                     if (w >= f) 
                     then empty
                     else text (spaces (f - w))

width :: Doc e -> (Int -> Doc e) -> Doc e
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
indent :: Int -> Doc e -> Doc e
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
hang :: Int -> Doc e -> Doc e
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
align :: Doc e -> Doc e
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
data Doc e
  = Empty
  | Char {-# UNPACK #-} !Char       -- invariant: char is not '\n'
  | Text {-# UNPACK #-} !Int String -- invariant: text doesn't contain '\n'
  | Line !Bool                      -- True <=> when undone by group, do not insert a space
  | Cat (Doc e) (Doc e)
  | Nest {-# UNPACK #-} !Int (Doc e)
  | Union (Doc e) (Doc e) -- invariant: first lines of first doc longer than the first lines of the second doc
  | Effect e
  | Column  (Int -> Doc e)
  | Nesting (Int -> Doc e)
  | Columns (Int -> Doc e) 
  | Ribbon  (Int -> Doc e) 

instance Functor Doc where
  fmap _ Empty = Empty
  fmap _ (Char c) = Char c
  fmap _ (Line b) = Line b
  fmap _ (Text i s) = Text i s
  fmap f (Cat l r) = Cat (fmap f l) (fmap f r)
  fmap f (Nest i d) = Nest i (fmap f d)
  fmap f (Union l r) = Union (fmap f l) (fmap f r)
  fmap f (Effect e) = Effect (f e)
  fmap f (Column k) = Column (fmap f . k)
  fmap f (Nesting k) = Nesting (fmap f . k)
  fmap f (Columns k) = Columns (fmap f . k)
  fmap f (Ribbon k) = Columns (fmap f . k)

instance Apply Doc where
  (<.>) = ap

instance Applicative Doc where
  pure = Effect
  (<*>) = ap

instance Bind Doc where
  (>>-) = (>>=)

instance Monad Doc where
  return = Effect
  Empty >>= _ = Empty
  Char c >>= _ = Char c
  Text i s >>= _ = Text i s
  Line b >>= _ = Line b
  Cat l r >>= k = Cat (l >>= k) (r >>= k)
  Nest i d >>= k = Nest i (d >>= k)
  Union l r >>= k = Union (l >>= k) (r >>= k)
  Effect e >>= k = k e
  Column f >>= k = Column (f >=> k)
  Nesting f >>= k = Nesting (f >=> k)
  Columns f >>= k = Columns (f >=> k)
  Ribbon f >>= k = Ribbon (f >=> k)
  fail _ = empty

instance Alt Doc where
  (<!>) = (<>)

instance Plus Doc where
  zero = empty

instance Alternative Doc where
  (<|>) = (<>)
  -- | The empty document is, indeed, empty. Although @empty@ has no
  -- content, it does have a \'height\' of 1 and behaves exactly like
  -- @(text \"\")@ (and is therefore not a unit of @above@).
  empty = Empty

instance MonadPlus Doc where
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
data SimpleDoc e 
  = SEmpty
  | SChar {-# UNPACK #-} !Char (SimpleDoc e)
  | SText {-# UNPACK #-} !Int String (SimpleDoc e)
  | SLine {-# UNPACK #-} !Int (SimpleDoc e)
  | SEffect e (SimpleDoc e)

instance Functor SimpleDoc where
  fmap _ SEmpty = SEmpty
  fmap f (SChar c d) = SChar c (fmap f d)
  fmap f (SText i s d) = SText i s (fmap f d)
  fmap f (SLine i d) = SLine i (fmap f d)
  fmap f (SEffect e d) = SEffect (f e) (fmap f d)

instance Foldable SimpleDoc where
  foldMap _ SEmpty = mempty
  foldMap f (SChar _ d) = foldMap f d
  foldMap f (SText _ _ d) = foldMap f d
  foldMap f (SLine _ d) = foldMap f d
  foldMap f (SEffect e d) = f e `mappend` foldMap f d

instance Traversable SimpleDoc where
  traverse _ SEmpty = pure SEmpty
  traverse f (SChar c d) = SChar c <$> traverse f d
  traverse f (SText i s d) = SText i s <$> traverse f d
  traverse f (SLine i d) = SLine i <$> traverse f d
  traverse f (SEffect e d) = SEffect <$> f e <*> traverse f d

-- | The document @(char c)@ contains the literal character @c@. The
-- character shouldn't be a newline (@'\n'@), the function 'line'
-- should be used for line breaks.
char :: Char -> Doc e
char '\n' = line
char c = Char c

-- | The document @(text s)@ contains the literal string @s@. The
-- string shouldn't contain any newline (@'\n'@) characters. If the
-- string contains newline characters, the function 'string' should be
-- used.
text :: String -> Doc e
text "" = Empty
text s  = Text (length s) s

-- | The @line@ document advances to the next line and indents to the
-- current nesting level. Document @line@ behaves like @(text \" \")@
-- if the line break is undone by 'group'.
line :: Doc e
line = Line False

-- | The @linebreak@ document advances to the next line and indents to
-- the current nesting level. Document @linebreak@ behaves like
-- 'empty' if the line break is undone by 'group'.
linebreak :: Doc e
linebreak = Line True

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
nest :: Int -> Doc e -> Doc e
nest = Nest

column, nesting :: (Int -> Doc e) -> Doc e
column = Column
nesting = Nesting

columns :: (Int -> Doc e) -> Doc e
columns = Columns

ribbon :: (Int -> Doc e) -> Doc e
ribbon = Ribbon

-- | The @group@ combinator is used to specify alternative
-- layouts. The document @(group x)@ undoes all line breaks in
-- document @x@. The resulting line is added to the current line if
-- that fits the page. Otherwise, the document @x@ is rendered without
-- any changes.
group :: Doc e -> Doc e
group x = Union (flatten x) x

flatten :: Doc e -> Doc e
flatten (Cat x y)       = Cat (flatten x) (flatten y)
flatten (Nest i x)      = Nest i (flatten x)
flatten (Line brk)      = if brk then Empty else Text 1 " "
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
data Docs e = Nil
          | Cons {-# UNPACK #-} !Int (Doc e) (Docs e)

-- | This is the default pretty printer which is used by 'show',
-- 'putDoc' and 'hPutDoc'. @(renderPretty ribbonfrac width x)@ renders
-- document @x@ with a page width of @width@ and a ribbon width of
-- @(ribbonfrac * width)@ characters. The ribbon width is the maximal
-- amount of non-indentation characters on a line. The parameter
-- @ribbonfrac@ should be between @0.0@ and @1.0@. If it is lower or
-- higher, the ribbon width will be 0 or @width@ respectively.
renderPretty :: Float -> Int -> Doc e -> SimpleDoc e
renderPretty rfrac w x
    = best 0 0 (Cons 0 x Nil)
    where
      -- r :: the ribbon width in characters
      r  = max 0 (min w (round (fromIntegral w * rfrac)))

      -- best :: n = indentation of current line
      --         k = current column
      --        (ie. (k >= n) && (k - n == count of inserted characters)
      best _ _ Nil      = SEmpty
      best n k (Cons i d ds)
        = case d of
            Empty       -> best n k ds
            Char c      -> let k' = k+1 in seq k' (SChar c (best n k' ds))
            Text l s    -> let k' = k+l in seq k' (SText l s (best n k' ds))
            Line _      -> SLine i (best i i ds)
            Cat x' y     -> best n k (Cons i x' (Cons i y ds))
            Nest j x'    -> let i' = i+j in seq i' (best n k (Cons i' x' ds))
            Effect e    -> SEffect e (best n k ds)
            Union x' y   -> nicest n k (best n k (Cons i x' ds))
                                       (best n k (Cons i y ds))

            Column f    -> best n k (Cons i (f k) ds)
            Nesting f   -> best n k (Cons i (f i) ds)
            Columns f   -> best n k (Cons i (f w) ds)
            Ribbon f    -> best n k (Cons i (f r) ds)

      --nicest :: r = ribbon width, w = page width,
      --          n = indentation of current line, k = current column
      --          x and y, the (simple) documents to chose from.
      --          precondition: first lines of x are longer than the first lines of y.
      nicest n k x' y | fits wid x' = x'
                      | otherwise  = y
                        where
                          wid = min (w - k) (r - k + n)


fits :: Int -> SimpleDoc e -> Bool
fits w _ | w < 0     = False
fits _ SEmpty        = True
fits w (SChar _ x)   = fits (w - 1) x
fits w (SText l _ x) = fits (w - l) x
fits _ (SLine _ _)   = True
fits w (SEffect _ x) = fits w x


-----------------------------------------------------------
-- renderCompact: renders documents without indentation
--  fast and fewer characters output, good for machines
-----------------------------------------------------------


-- | @(renderCompact x)@ renders document @x@ without adding any
-- indentation. Since no \'pretty\' printing is involved, this
-- renderer is very fast. The resulting output contains fewer
-- characters than a pretty printed version and can be used for output
-- that is read by other programs.
renderCompact :: Doc e -> SimpleDoc e
renderCompact x
    = scan 0 [x]
    where
      scan _ []     = SEmpty
      scan k (d:ds) = case d of
                        Empty       -> scan k ds
                        Char c      -> let k' = k+1 in seq k' (SChar c (scan k' ds))
                        Text l s    -> let k' = k+l in seq k' (SText l s (scan k' ds))
                        Effect e    -> SEffect e (scan k ds)
                        Line _      -> SLine 0 (scan 0 ds)
                        Cat y z     -> scan k (y:z:ds)
                        Nest _ y    -> scan k (y:ds)
                        Union _ y   -> scan k (y:ds)
                        Column f    -> scan k (f k:ds)
                        Nesting f   -> scan k (f 0:ds)
                        Columns f   -> scan k (f 80:ds)
                        Ribbon  f   -> scan k (f 80:ds)

-----------------------------------------------------------
-- Displayers:  displayS and displayIO
-----------------------------------------------------------


-- | @(displayS simpleDoc)@ takes the output @simpleDoc@ from a
-- rendering function and transforms it to a 'ShowS' type (for use in
-- the 'Show' class).
--
-- > showWidth :: Int -> Doc -> String
-- > showWidth w x   = displayS (renderPretty 0.4 w x) ""
displayS :: SimpleDoc e -> ShowS
displayS SEmpty             = id
displayS (SChar c x)        = showChar c . displayS x
displayS (SText _ s x)      = showString s . displayS x
displayS (SLine i x)        = showString ('\n':indentation i) . displayS x
displayS (SEffect _ t)      = displayS t

-- | @(displayIO handle simpleDoc)@ writes @simpleDoc@ to the file
-- handle @handle@. This function is used for example by 'hPutDoc':
--
-- > hPutDoc handle doc  = displayIO handle (renderPretty 0.4 100 doc)
displayIO :: Handle -> SimpleDoc e -> IO ()
displayIO handle = go where
  go SEmpty        = return ()
  go (SChar c x)   = hPutChar handle c >> go x
  go (SText _ s x) = hPutStr handle s >> go x
  go (SLine i x)   = hPutStr handle ('\n':indentation i) >> go x
  go (SEffect _ t) = go t

-----------------------------------------------------------
-- default pretty printers: show, putDoc and hPutDoc
-----------------------------------------------------------
instance Show (Doc e) where
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
putDoc :: Doc e -> IO ()
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
hPutDoc :: Handle -> Doc e -> IO ()
hPutDoc handle doc = displayIO handle (renderPretty 0.4 80 doc)

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
