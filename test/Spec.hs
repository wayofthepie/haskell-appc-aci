{-# LANGUAGE OverloadedStrings #-}

import ACI.Image
import Data.Attoparsec.Text
import Data.Maybe
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main =
    defaultMain . localOption (QuickCheckVerbose True) $
    testGroup "ACI Image Property Tests" [manifestTestGroup]

-- | Run a parser.
run
    :: Parser a -> T.Text -> Maybe a
run p input =
    case parseOnly p input of
        Right x -> Just x
        Left _ -> Nothing

-----------------------------------------------------------------------------
-- Manifest parser property tests.
-----------------------------------------------------------------------------
manifestTestGroup
    :: TestTree
manifestTestGroup =
    testGroup
        "manifest parser tests"
        [ testProperty "Valid App Container identifier spec" prop_ac_id_spec
        , testCase "Valid App Container kind spec" testAcKindSpec
        ]

-----------------------------------------------------------------------------
-- AC kind tests
acKindStrings
    :: [T.Text]
acKindStrings = ["ImageManifest", "PodManifest"]

testAcKindSpec :: IO ()
testAcKindSpec =
    assert (isJust $ foldl1 (>>) $ fmap (run acKindParser) acKindStrings)

----------------------------------------------------------------------------
-- AC identifier tests
data AllowedACId = AllowedACId T.Text ACIdentifier deriving (Eq,Show)

instance Arbitrary AllowedACId where
    arbitrary =
        genAcId >>=
        \txt ->
             return $ AllowedACId txt (ACIdentifier txt)

genAcIdInitChar :: Gen (Char, Char)
genAcIdInitChar = do
    c1 <- elements $ ['a' .. 'z'] ++ ['0' .. '9']
    c2 <- elements $ ['a' .. 'z'] ++ ['0' .. '9']
    return (c1, c2)

genAcId :: Gen T.Text
genAcId = do
    (initc,endc) <- genAcIdInitChar
    surroundable <- T.pack <$> listOf1 (elements "-a-z0-9._/~")
    return $ surround initc endc surroundable
  where
    surround ic ec str = T.cons ic $ T.snoc str ec

-- | Verifies the AC identifier parser follows the AC identifier specifications outlined here
-- <https://github.com/appc/spec/blob/master/spec/types.md#ac-identifier-type>
prop_ac_id_spec
    :: AllowedACId -> Bool
prop_ac_id_spec (AllowedACId txt acid) = run acIdParser txt == Just acid
