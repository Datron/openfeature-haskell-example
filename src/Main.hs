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
expectRight (Left err) = undefined

main :: IO ()
main = do
  let options =
        Superposition.defaultProviderOptions
          { Superposition.orgId = pack "orgid162145664241766405",
            Superposition.workspaceId = pack "test1",
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
  v <- expectRight <$> OpenFeature.getBoolValue client (pack "k1") mempty
  putStrLn $ show v
  let context = OpenFeature.withCustomField "name" (Aeson.toJSON ("john"::String)) (OpenFeature.defaultContext)
  v1 <- expectRight <$> OpenFeature.getBoolValue client (pack "k1") (Just context)
  putStrLn $ show v1
  pure ()
