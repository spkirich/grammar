import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck

import qualified Data.Set as Set

import Grammar
import Grammar.Definition
import Grammar.Normalization

instance (Arbitrary t, Arbitrary n) => Arbitrary (Production t n) where

  -- | An arbitrary production
  arbitrary = Production <$> arbitrary <*> arbitrary

instance (Arbitrary t, Ord t, Arbitrary n, Ord n) => Arbitrary (Grammar t n) where

  -- | An arbitrary grammar
  arbitrary = Grammar <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = do

  describe "rhsTerminals" $ do

    prop "handles epsilon-productions" $ \lhs -> let
        p = Production lhs [] :: Production Int Int
      in null $ rhsTerminals p

    prop "handles productions with no nonterminals on the right-hand side" $ \lhs rhs -> let
        p = Production lhs [Left t | t <- rhs] :: Production Int Int
      in rhsTerminals p == Set.fromList rhs

    prop "handles productions with no terminals on the right-hand side" $ \lhs rhs -> let
        p = Production lhs [Right n | n <- rhs] :: Production Int Int
      in null $ rhsTerminals p

    it "handles productions with arbitrary symbols on the right-hand side" $ let
        p = Production 0 [Left (-1), Right 2, Left (-3), Right 4, Left (-5)] :: Production Int Int
      in rhsTerminals p == Set.fromList [-1, -3, -5]

  describe "rhsNonterminals" $ do

    prop "handles epsilon-productions" $ \lhs -> let
        p = Production lhs [] :: Production Int Int
      in null $ rhsNonterminals p

    prop "handles productions with no nonterminals on the right-hand side" $ \lhs rhs -> let
        p = Production lhs [Left t | t <- rhs] :: Production Int Int
      in null $ rhsNonterminals p

    prop "handles productions with no terminals on the right-hand side" $ \lhs rhs -> let
        p = Production lhs [Right n | n <- rhs] :: Production Int Int
      in rhsNonterminals p == Set.fromList rhs

    it "handles productions with arbitrary symbols on the right-hand side" $ let
        p = Production 0 [Left (-1), Right 2, Left (-3), Right 4, Left (-5)] :: Production Int Int
      in rhsNonterminals p == Set.fromList [2, 4]

  describe "eliminateNonGeneratingNonterminals" $ do

    prop "is effective" $ \g -> let
        h = eliminateNonGeneratingNonterminals g :: Grammar Int Int
      in nonterminals h == generatingNonterminals g

    prop "keeps trivially generating nonterminals" $ \t n -> let
        g = Grammar (Set.singleton t) (Set.singleton n) (Set.singleton $ Production n [Left t]) n
        h = eliminateNonGeneratingNonterminals g :: Grammar Int Int
      in nonterminals h == nonterminals g

    prop "eliminates trivially non-generating nonterminals" $ \n -> let
        g = Grammar Set.empty (Set.singleton n) (Set.singleton $ Production n [Right n]) n
        h = eliminateNonGeneratingNonterminals g :: Grammar Int Int
      in null $ nonterminals h

    it "handles non-trivial cases" $ let
        g = Grammar
          { terminals    = Set.fromList [-1, -2, -3]
          , nonterminals = Set.fromList [0, 1, 2, 3]

          , productions = Set.fromList
              [ Production 0 []
              , Production 0 [Right 1]
              , Production 0 [Right 0, Right 0]
              , Production 0 [Right 2, Right 3]
              , Production 0 [Left (-1), Right 0, Left (-2)]
              , Production 0 [Left (-3), Right 2, Left (-3)]
              , Production 1 [Left (-1), Right 0, Left (-2)]
              , Production 2 [Left (-3), Right 3, Left (-3)]
              , Production 3 [Left (-1), Right 2, Left (-2)]
              ]

          , startNonterminal = 0
          } :: Grammar Int Int
        h = eliminateNonGeneratingNonterminals g
      in nonterminals h == Set.fromList [0, 1]

main :: IO ()
main = hspec spec
