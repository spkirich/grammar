import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck

import Data.Set (Set)
import qualified Data.Set as Set

import Grammar
import Grammar.Definition
import Grammar.Normalization

instance (Arbitrary t, Arbitrary n) => Arbitrary (Production t n) where

  arbitrary = Production <$> arbitrary <*> arbitrary

instance (Arbitrary t, Ord t, Arbitrary n, Ord n) => Arbitrary (Grammar t n) where

  arbitrary = do

    ts <- listOf  arbitrary
    ns <- listOf1 arbitrary

    ps <- listOf $ do

      lhs <- elements ns

      rhs <- listOf . oneof $ (Right <$> elements ns)
        : [Left <$> elements ts | not $ null ts]

      pure $ Production lhs rhs

    s <- elements ns

    pure $ Grammar (Set.fromList ts) (Set.fromList ns) (Set.fromList ps) s

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

    prop "handles productions with arbitrary symbols on the right-hand side" $ \a b c s t -> let
        p = Production 0 [Left a, Right s, Left b, Right t, Left c] :: Production Int Int
      in rhsTerminals p == Set.fromList [a, b, c]

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

    prop "handles productions with arbitrary symbols on the right-hand side" $ \a b c s t -> let
        p = Production 0 [Left a, Right s, Left b, Right t, Left c] :: Production Int Int
      in rhsNonterminals p == Set.fromList [s, t]

  describe "eliminateNonterminals" $ do

    prop "handles an empty set of nonterminals" $ \g -> let
        h = eliminateNonterminals Set.empty g :: Grammar Int Int
      in nonterminals h == nonterminals g && productions h == productions g

    prop "handles the entire set of the grammar nonterminals" $ \g -> let
        h = eliminateNonterminals (nonterminals g) g :: Grammar Int Int
      in null (nonterminals h) && null (productions h)

  describe "reachableNonterminals" $ do

    prop "includes the start nonterminal" $ \g ->
      Set.member (startNonterminal g) $ reachableNonterminals
        (g :: Grammar Int Int)

    prop "includes trivially reachable nonterminals" $ \s n -> let
        g = Grammar Set.empty (Set.fromList [s, n])
          (Set.singleton (Production s [Right n])) s :: Grammar Int Int
      in reachableNonterminals g == Set.fromList [s, n]

    prop "does not include unreachable nonterminals" $ \s n -> let
        g = Grammar Set.empty (Set.fromList [s, n])
          Set.empty s :: Grammar Int Int
      in reachableNonterminals g == Set.singleton s

  describe "unreachableNonterminals" $ do

    prop "complements reachableNonterminals" $ \g -> let
        uns = unreachableNonterminals g
        rns =   reachableNonterminals g
      in Set.union uns rns == nonterminals
        (g :: Grammar Int Int)

  describe "generatingNonterminals" $ do

    prop "includes trivially generating nonterminals" $ \t s -> let
        g = Grammar (Set.singleton t) (Set.singleton s)
          (Set.fromList [Production s [Left t]]) s :: Grammar Int Int
      in generatingNonterminals g == Set.singleton s

    prop "does not include non-generating nonterminals" $ \s -> let
        g = Grammar Set.empty (Set.singleton s)
          Set.empty s :: Grammar Int Int
      in null $ generatingNonterminals g

  describe "nonGeneratingNonterminals" $ do

    prop "complements generatingNonterminals" $ \g -> let
        nns = nonGeneratingNonterminals g
        gns =    generatingNonterminals g
      in Set.union gns nns == nonterminals
        (g :: Grammar Int Int)

main :: IO ()
main = hspec spec
