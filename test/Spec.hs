{-# LANGUAGE OverloadedStrings #-}

import           ACI.Image
import           Data.Attoparsec.Text
import           Data.Maybe
import           Data.Monoid
import qualified Data.SemVer           as SemVer
import qualified Data.Text             as T
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

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
        , testProperty "Valid App Container version spec" prop_ac_version_spec
        ]

-----------------------------------------------------------------------------
-- AC kind tests
acKindStrings
    :: [T.Text]
acKindStrings = ["ImageManifest", "PodManifest"]

testAcKindSpec :: IO ()
testAcKindSpec =
    assert (isJust $ foldl1 (>>) $ fmap (run acKindParser) acKindStrings)

------------------------------------------------------------------------------
-- AC version tests
-- The parser for the AC version is just the parser from the semver package so
-- it's probably overkill to test it, but just in case this changes/breaks lets
-- test it.
data AllowedVersion =
    AllowedVersion T.Text
                   SemVer.Version
    deriving (Eq,Show)

instance Arbitrary AllowedVersion where
    arbitrary =
        genSemVer >>=
        \sver ->
             return $ AllowedVersion (SemVer.toText sver) sver

-- Generate valid semver's.
genSemVer :: Gen SemVer.Version
genSemVer = do
    let c2txt = T.pack . flip (:) []
    major <- elements [0 .. 9]
    minor <- elements [0 .. 9]
    patch <- elements [0 .. 9]
    maybePreRel <- mconcat . fmap c2txt <$> listOf allowedSymbols
    maybeBuildMeta <- mconcat . fmap c2txt <$> listOf allowedSymbols
    return $ consSemVer major minor patch maybePreRel maybeBuildMeta
  where
    -- Symbols allowed in pre-release versions and build metadata. May only be
    -- ascii alphanumerics and a hyphen.
    allowedSymbols =
        elements $ ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['-']
    consSemVer ma mi patch preRel buildMeta =
        SemVer.version
            ma
            mi
            patch
            (maybeToList $ toId preRel)
            (maybeToList $ toId buildMeta)
      where
        toId txt
          | T.null txt = Nothing
          | otherwise = SemVer.textual txt

-- | Verifies the AC Version parser follows the SemVer specification.
prop_ac_version_spec :: AllowedVersion -> Bool
prop_ac_version_spec (AllowedVersion txt sver) =
    run acVersionParser txt == Just sver

----------------------------------------------------------------------------
-- AC identifier tests
data AllowedACId =
    AllowedACId T.Text
                ACIdentifier
    deriving (Eq,Show)

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
prop_ac_id_spec (AllowedACId txt acid) = run acidParser txt == Just acid
