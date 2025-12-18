module Main (main) where

import qualified Data.OpenFeature.Api as OpenFeature
import qualified Data.OpenFeature.Client as OpenFeature
import qualified Data.OpenFeature.FeatureProvider as OpenFeature
import qualified Data.OpenFeature.EvaluationContext as OpenFeature
import qualified Data.OpenFeature.SuperpositionProvider as Superposition
import Data.Text
import GHC.Conc.IO (threadDelay)
import qualified Network.URI as URI
import qualified Data.Aeson as Aeson

expectJust :: Maybe a -> a
expectJust (Just a) = a
expectJust _ = undefined

expectRight :: Either b a -> a
expectRight (Right a) = a
expectRight _ = undefined

main :: IO ()
main = do
  let options =
        Superposition.defaultProviderOptions
          { Superposition.orgId = pack "orgid162145664241766405",
            Superposition.workspaceId = pack "mjos",
            Superposition.endpoint = expectJust $ URI.parseURI "http://localhost:8080",
            Superposition.refreshOptions = Superposition.Poll 10,
            Superposition.logLevel = Superposition.LevelDebug
          }
  provider <- expectRight <$> Superposition.newSuperpositionProvider options
  !_ <- OpenFeature.initialize provider OpenFeature.defaultContext
  -- wait a few seconds...
  threadDelay 4000000
  OpenFeature.setDefaultProvider provider
  client <- OpenFeature.createClient
  readData client
  pure ()
  
readData :: OpenFeature.Client -> IO ()
readData client = do
    threadDelay 10000000
    v <- expectRight <$> OpenFeature.getStringValue client (pack "hyperos_manifest") mempty
    print v
    let context = OpenFeature.withCustomField "os" (Aeson.toJSON ("android"::String)) OpenFeature.defaultContext
    v1 <- expectRight <$> OpenFeature.getStringValue client (pack "hyperos_manifest") (Just context)
    print v1
    _ <- readData client
    pure ()
