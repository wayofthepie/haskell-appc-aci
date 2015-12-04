{-# LANGUAGE OverloadedStrings #-}
import           Data.Attoparsec.Text
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text                    as T
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.Basic
import           Test.Tasty.Options
import           Test.Tasty.QuickCheck

import           ACI.Image

import           Debug.Trace

main :: IO ()
main = defaultMain . localOption (QuickCheckVerbose True) $
    testGroup "ACI Image Property Tests"
        [ manifestTestGroup
        ]


-- | Run a parser.
run :: Parser a -> T.Text -> Maybe a
run p input = case parseOnly p input of
    Right x -> Just x
    Left _  -> Nothing

-------------------------------------------------------------------------------
-- Manifest parser property tests.
-------------------------------------------------------------------------------
manifestTestGroup :: TestTree
manifestTestGroup = testGroup "manifest parser tests"
   [ testProperty "Valid App Container identifier spec" prop_ac_id_spec
   , testCase "Valid App Container kind spec" test_ac_kind_spec
   ]


-------------------------------------------------------------------------------
-- | AC kind tests
acKindStrings :: [T.Text]
acKindStrings = ["ImageManifest", "PodManifest"]

test_ac_kind_spec :: IO ()
test_ac_kind_spec = assert (isJust $ foldl1 (>>) $ fmap ( run acKindParser ) acKindStrings)

-------------------------------------------------------------------------------
-- | AC identifier tests
data AllowedACId = AllowedACId T.Text ACIdentifier deriving (Eq, Show)

instance Arbitrary AllowedACId where
    arbitrary = genAcId >>= \txt -> pure $ AllowedACId txt (ACIdentifier txt)

genAcIdInitChar :: Gen Char
genAcIdInitChar = elements $ ['a'..'z'] ++ ['0'..'9']

genAcId :: Gen T.Text
genAcId = T.pack <$> ( genAcIdInitChar >>=
    \c -> (++) [c] <$> listOf1 (elements "-a-z0-9._/~"))

-- | Verifies the AC identifier parser follows the AC identifier
-- specifications outlined here
-- <https://github.com/appc/spec/blob/master/spec/types.md#ac-identifier-type>
prop_ac_id_spec :: AllowedACId -> Bool
prop_ac_id_spec (AllowedACId txt acid) = run acidParser txt == Just acid

