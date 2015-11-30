
import ACI.Image

main :: IO ()
main = putStrLn "Test suite not yet implemented"

-------------------------------------------------------------------------------
-- tests for createImage
-------------------------------------------------------------------------------

-- <https://github.com/appc/spec/blob/master/spec/aci.md#image-manifest-schema>
newtype AllowedManifest = AllowedManifest
    {
    } deriving (Eq,Show)

instance
