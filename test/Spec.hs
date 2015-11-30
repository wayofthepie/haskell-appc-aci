{-# LANGUAGE
    OverloadedStrings
    #-}
import Data.Attoparsec.Text
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.QuickCheck

import ACI.Image

main :: IO ()
main = defaultMain $ testGroup "ACI Image Property Tests"
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
   [ testProperty "valid acid spec" prop_ac_id_spec ]


data AllowedACId = AllowedACId T.Text ACIdentifier deriving (Eq, Show)

instance Arbitrary AllowedACId where
    arbitrary = genAcId >>= \txt -> pure $ AllowedACId txt (ACIdentifier txt)

genAcIdInitChar :: Gen Char
genAcIdInitChar = elements $ ['a'..'z'] ++ ['0'..'9']

genAcId :: Gen T.Text
genAcId = T.pack <$> ( genAcIdInitChar >>=
    \c -> ((++) [c]) <$> elements ["-a-z0-9._/~"] )

-- | Verifies the AC identifier parser follows the AC identifier
-- specifications outlined here
-- <https://github.com/appc/spec/blob/master/spec/types.md#ac-identifier-type>
prop_ac_id_spec :: AllowedACId -> Bool
prop_ac_id_spec (AllowedACId txt acid) = run acidParser txt == Just acid

