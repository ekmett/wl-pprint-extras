module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Text.PrettyPrint.Free.Internal

main :: IO ()
main = defaultMain [
         testGroup "Tests for each data constructor" conTests
       , testGroup "Tests for some combinators" codeTests
       , testGroup "Tests for the formatting algorithms" formatTests
       ]

conTests :: [Test]
conTests = [
    testCase "Empty tests"   emptyTests
  , testCase "Char tests"    charTests
  , testCase "Text tests"    textTests
  , testCase "Line tests"    textTests
  , testCase "FlatAlt tests" flatAltTests
  , testCase "Cat tests"     catTests
  , testCase "Nest tests"    nestTests
  , testCase "Union tests"   unionTests
  , testCase "Column tests"  columnTests
  , testCase "Nesting tests" nestingTests
  , testCase "Columns tests" columnsTests
  , testCase "Ribbon tests"  ribbonTests
  ]

------------------------------------------------------------
-- We test the @Doc@ constructors.

assertPretty :: Int -> String -> String -> Doc e -> Assertion
assertPretty w desc str doc = assertEqual (desc ++ " (pretty)") str
                                $ displayS (renderPretty 1.0 w doc) ""

assertSmart :: Int -> String -> String -> Doc e -> Assertion
assertSmart w desc str doc = assertEqual (desc ++ " (smart)") str
                                $ displayS (renderSmart w doc) ""

assertRender :: Int -> String -> String -> Doc e -> Assertion
assertRender w desc str doc = do assertPretty w desc str doc
                                 assertSmart w desc str doc

emptyTests :: Assertion
emptyTests = assertRender 80 "Empty test 1" "" empty

charTests :: Assertion
charTests = assertRender 80 "Char test 1" "a" (char 'a')

textTests :: Assertion
textTests = assertRender 80 "Text test 1" "text..." (text "text...")

lineTests :: Assertion
lineTests = assertRender 80 "Line test 1" "\n" hardline

flatAltTests :: Assertion
flatAltTests = do assertRender 80 "FlatAlt test 1" "x"
                    $ flatAlt (text "x") (text "y")
                  assertRender 80 "FlatAlt test 2" "y"
                    $ flatten $ flatAlt (text "x") (text "y")

catTests :: Assertion
catTests = do assertRender 80 "Cat test 1" "some code"
                $ text "some" <> space <> text "code"

nestTests :: Assertion
nestTests = do assertRender 80 "Nest test 1" "foo bar"
                 $ text "foo" <+> nest 2 (text "bar")
               assertRender 80 "Nest test 2" "foo\n  bar"
                 $ text "foo" <> nest 2 (line <> text "bar")

unionTests :: Assertion
unionTests = do assertRender 80 "Union test 1" "foo bar"
                  $ text "foo" </> text "bar"
                assertRender 5 "Union test 2" "foo\nbar"
                  $ text "foo" </> text "bar"

columnTests :: Assertion
columnTests = do assertRender 80 "Column test 1" "foo 4"
                   $ text "foo" <+> (column $ \c -> pretty c)

nestingTests :: Assertion
nestingTests = do assertRender 80 "Nesting test 1" "foo 2"
                    $ text "foo" <+> (nest 2 $ nesting $ \n -> pretty n)

columnsTests :: Assertion
columnsTests = do assertRender 21 "Columns test 1" "foo 21"
                    $ text "foo" <+> (nest 2 $ columns $ \w -> pretty w)

ribbonTests :: Assertion
ribbonTests = do assertEqual "Ribbon test 1" "foo 32"
                   $ show (text "foo" <+> (ribbon $ \r -> pretty r))

------------------------------------------------------------
-- We test some combinators.

codeTests :: [Test]
codeTests = [
    testCase "@list@ tests"   listTests
  , testCase "@tupled@ tests" tupledTests
  ]

listTests :: Assertion
listTests = do assertRender 80 "@list@ test 1" "[1, 2, 3]"
                 $ pretty ([1, 2, 3] :: [Int])
               assertRender 5 "@list@ test 2" "[ 1\n, 2\n, 3 ]"
                 $ pretty ([1, 2, 3] :: [Int])

tupledTests :: Assertion
tupledTests = do assertRender 80 "@tupled@ test 1" "(1, True, a)"
                   $ pretty (1 :: Int, True, 'a')
                 assertRender 5 "@tupled@ test 2" "( 1\n, True\n, a )"
                   $ pretty (1 :: Int, True, 'a')

------------------------------------------------------------
-- We test some corner cases of the formatting algorithms on a prototypical
-- syntax for a scripting language (e.g. Python).

formatTests :: [Test]
formatTests = [
    testCase "@renderPretty@ test" renderPrettyTest
  , testCase "@renderSmart@ test"  renderSmartTest
  ]

data Syn = Call String [Syn]    -- name(syn, ..., syn)
         | Plus Syn Syn  -- syn + syn
         | List [Syn]    -- [syn, ..., syn]
         | Name String

instance Pretty Syn where
  pretty (Call n s) =
    text (n ++ "(") <> nest 2 (softbreak <> align body) <//> rparen
    where body = hcat $ punctuate (comma <> softline) (map pretty s)
  pretty (Plus s t) = nest 2 (pretty s) </> text "+" </> nest 2 (pretty t)
  pretty (List l) = list (map pretty l)
  pretty (Name n) = text n

deep :: Int -> Syn
deep 0 = List (map Name ["abc", "abcdef", "abcdef"])
deep i = Call "fun" [deep (i - 1)]

branch :: Int -> Syn
branch 0 = List (map Name ["abc", "abc"])
branch i | i < 3 = Call "fun" [branch (i - 1), branch (i - 1)]
         | otherwise = Call "fun" [branch (i - 1)]

wide :: Syn
wide = Call "aaaaaaaaaaa" [List $ map Name ["abc", "def", "ghi"]]

-- @renderPretty@ has only one line of lookahead, so it can not fit the
-- entire document within the pagewidth (20c), only the first line.
renderPrettyTest :: Assertion
renderPrettyTest = do assertPretty 20 "@renderPretty@ test 1" (concat [
                          "fun(fun(fun(fun(fun(\n"
                        , "                  [ abc\n"
                        , "                  , abcdef\n"
                        , "                  , abcdef ]\n"
                        , "                ))))\n"
                        , ")" ])
                        $ pretty (deep 5)
                      assertPretty 20 "@renderPretty@ test 2" (concat [
                          "fun(fun(fun([ abc\n"
                        , "            , abc ],\n"
                        , "            [ abc\n"
                        , "            , abc ]\n"
                        , "        ), fun([ abc\n"
                        , "               , abc ],\n"
                        , "               [ abc\n"
                        , "               , abc ]\n"
                        , "        )))" ])
                        $ pretty (branch 3)
                      assertPretty 20 "@renderPretty@ test 3" (concat [
                          "aaaaaaaaaaa([ abc\n"
                        , "            , def\n"
                        , "            , ghi ])" ])
                        $ pretty wide

-- @renderSmart@ has more sophisiticated lookahead, so it fits the entire
-- structure within the pagewidth (20c).
renderSmartTest :: Assertion
renderSmartTest = do assertSmart 20 "@renderSmart@ test 1" (concat [
                         "fun(\n"
                       , "  fun(\n"
                       , "    fun(\n"
                       , "      fun(\n"
                       , "        fun(\n"
                       , "          [ abc\n"
                       , "          , abcdef\n"
                       , "          , abcdef ]\n"
                       , "        )))))" ])
                       $ pretty (deep 5)
                     assertSmart 20 "@renderSmart@ test 2" (concat [
                         "fun(\n"
                       , "  fun(\n"
                       , "    fun(\n"
                       , "      fun(\n"
                       , "        fun([ abc\n"
                       , "            , abc ],\n"
                       , "            [ abc\n"
                       , "            , abc ]\n"
                       , "        ),\n"
                       , "        fun([ abc\n"
                       , "            , abc ],\n"
                       , "            [ abc\n"
                       , "            , abc ])\n"
                       , "      ))))" ])
                       $ pretty (branch 5)
                     assertSmart 20 "@renderSmart@ test 3" (concat [
                         "aaaaaaaaaaa(\n"
                       , "  [abc, def, ghi])" ])
                       $ pretty wide
