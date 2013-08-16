module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Text.PrettyPrint.Free

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
  ]

------------------------------------------------------------
-- We test the @Doc@ constructors.

assertPretty :: Int -> String -> String -> Doc e -> Assertion
assertPretty w desc str doc = assertEqual (desc ++ " (pretty)") str
                                $ displayS (renderPretty 1.0 w doc) ""

assertSmart :: Int -> String -> String -> Doc e -> Assertion
assertSmart w desc str doc = assertEqual (desc ++ " (smart)") str
                                $ displayS (renderSmart 1.0 w doc) ""

assertRender :: Int -> String -> String -> Doc e -> Assertion
assertRender w desc str doc =
  do assertPretty w desc str doc
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

-- TODO(sjindel): Add tests for columns and ribbon.

------------------------------------------------------------
-- We test some combinators.

codeTests :: [Test]
codeTests = [
    testCase "@list tests"   listTests
  , testCase "@tupled tests" tupledTests
  ]

listTests :: Assertion
listTests = do assertRender 80 "@list test 1" "[1, 2, 3]"
                 $ pretty ([1, 2, 3] :: [Int])
               assertRender 5 "@list test 2" "[ 1\n, 2\n, 3 ]"
                 $ pretty ([1, 2, 3] :: [Int])

tupledTests :: Assertion
tupledTests = do assertRender 80 "@tupled test 1" "(1, True, a)"
                   $ pretty (1 :: Int, True, 'a')
                 assertRender 5 "@tupled test 2" "( 1\n, True\n, a )"
                   $ pretty (1 :: Int, True, 'a')

------------------------------------------------------------
-- We test some corner cases of the formatting algorithms.

formatTests :: [Test]
formatTests = [
    testCase "@renderPretty test" renderPrettyTest
  , testCase "@renderSmart test"  renderSmartTest
  ]

deep :: Int -> Doc e
deep 0 = pretty ["abc", "abcdef", "abcdef"]
deep i = text "fun(" <> nest 2 (softbreak <> align (deep (i - 1)))

renderPrettyTest :: Assertion
renderPrettyTest = do assertPretty 20 "@renderPretty test 1" (concat [
                         "fun(fun(fun(fun(fun(\n"
                        , "                  [ abc\n"
                        , "                  , abcdef\n"
                        , "                  , abcdef ]" ])
                        $ deep 5

renderSmartTest :: Assertion
renderSmartTest = do assertSmart 20 "@renderPretty test 1" (concat [
                          "fun(\n"
                       , "  fun(\n"
                       , "    fun(\n"
                       , "      fun(\n"
                       , "        fun(\n"
                       , "          [ abc\n"
                       , "          , abcdef\n"
                       , "          , abcdef ]" ])
                       $ deep 5
