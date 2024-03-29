{-# LANGUAGE DeriveGeneric #-}

module Test.Tyche (visualize, visualizeResult) where

import Data.Aeson
  ( Options (fieldLabelModifier, sumEncoding),
    SumEncoding (UntaggedValue),
    ToJSON (toEncoding),
    defaultOptions,
    encode,
    genericToEncoding,
  )
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.IORef (IORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import GHC.IORef (newIORef, writeIORef)
import System.Directory (createDirectoryIfMissing)
import Test.QuickCheck (Property, Testable, ioProperty)
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Property (Callback (PostTest), CallbackKind (NotCounterexample), Result (labels, ok, reason, testCase), callback)
import Text.Read (readMaybe)

data FeatureData
  = IntData Int
  | StringData String
  deriving (Generic, Show)

instance ToJSON FeatureData where
  toEncoding =
    genericToEncoding
      ( defaultOptions
          { fieldLabelModifier = drop 1,
            sumEncoding = UntaggedValue
          }
      )

data InfoLine = InfoLine
  { _info_type :: String,
    _info_run_start :: Double,
    _info_property :: String,
    _info_title :: String,
    _info_content :: String
  }
  deriving (Generic, Show)

instance ToJSON InfoLine where
  toEncoding = genericToEncoding (defaultOptions {fieldLabelModifier = drop 6})

data TestCaseLine = TestCaseLine
  { _tc_type :: String,
    _tc_run_start :: Double,
    _tc_property :: String,
    _tc_status :: String,
    _tc_status_reason :: String,
    _tc_representation :: String,
    _tc_features :: Map String FeatureData,
    _tc_coverage :: Maybe ()
  }
  deriving (Generic, Show)

instance ToJSON TestCaseLine where
  toEncoding = genericToEncoding (defaultOptions {fieldLabelModifier = drop 4})

{-# NOINLINE startTimeRef #-}
startTimeRef :: IORef (Maybe Double)
startTimeRef = unsafePerformIO (newIORef Nothing)

getTime :: IO Double
getTime = do
  cached <- readIORef startTimeRef
  case cached of
    Just t -> return t
    Nothing -> do
      time <- fromRational . toRational <$> getPOSIXTime
      writeIORef startTimeRef (Just time)
      return time

writeLine :: (ToJSON a) => String -> a -> IO ()
writeLine propName x = do
  createDirectoryIfMissing True ".quickcheck/observations"
  BSL.appendFile (".quickcheck/observations/" ++ propName ++ ".jsonl") . flip BSL.snoc '\n' . encode $ x

visualizeResult :: String -> QuickCheck.Result -> IO ()
visualizeResult propName res = do
  runStart <- getTime
  writeLine propName $
    InfoLine
      { _info_type = "info",
        _info_run_start = runStart,
        _info_property = propName,
        _info_title = "QuickCheck Result",
        _info_content = show res
      }

visualize :: (Testable prop) => String -> prop -> Property
visualize propName p = ioProperty $ do
  runStart <- getTime
  return $
    callback
      ( PostTest
          NotCounterexample
          ( \_ res ->
              writeLine propName $
                TestCaseLine
                  { _tc_type = "test_case",
                    _tc_run_start = runStart,
                    _tc_property = propName,
                    _tc_status = case ok res of
                      Nothing -> "gave_up"
                      Just True -> "passed"
                      Just False -> "failed",
                    _tc_status_reason = reason res,
                    _tc_representation = case testCase res of [x] -> x; xs -> show xs,
                    _tc_features = M.fromList (map parseLabel (labels res)),
                    _tc_coverage = Nothing
                  }
          )
      )
      p
  where
    parseLabel :: String -> (String, FeatureData)
    parseLabel l
      | ':' `elem` l =
          (takeWhile (/= ':') l, parseData (drop 1 (dropWhile (/= ':') l)))
      where
        parseData s = case readMaybe s of
          Just n -> IntData n
          Nothing -> StringData s
    parseLabel l = (l, StringData "")
