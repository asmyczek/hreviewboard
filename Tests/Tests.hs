-----------------------------------------------------------------------------
-- | 
-- Main test module
--
-- Execute 'runhaskell Tests/Tests.hs'
-- from reviewboard dir
--
-----------------------------------------------------------------------------

module Main where

import Test.HUnit
import Text.JSON
import Data.Ratio
import Control.Monad
import System.Environment
import ReviewBoard.Api
import ReviewBoard.Core
import qualified ReviewBoard.Response as R
import Tests.TestsWithServer
import Data.Maybe

-- | Main test runner
--
main = do
    -- Run default test
    runTestTT defaultTests

    -- Get ReviewBoard server credentials
    -- for server depended tests
    url    <- getEnv "RB_URL"
    user   <- getEnv "RB_USER"
    passwd <- getEnv "RB_PASSWD"

    -- Check if server is available
    avail <- serverAvailable url user passwd
    
    -- and run tests
    runServerTest avail $ apiServerTests url user passwd
    return ()

-- | Review board test suite
--
defaultTests = TestList
    [ TestLabel "Test rrFieldMap"               rrFieldMapTest
    , TestLabel "Test JSON util function"       jsUtilTest
    , TestLabel "Test response DSL"             responseDSLTest
    ]

-- ---------------------------------------------------------------------------
-- API tests

-- Test rrFiledMap coverage for RRField 
-- TODO: do we really need a print test?
rrFieldMapTest = TestCase ( do
    mapM (print . show) [(minBound::RRField)..maxBound]
    return ()
    )

-- ---------------------------------------------------------------------------
-- Tests JSon utils

-- Helper function takes json value from result
toJson :: Result (JSObject JSValue) -> JSValue
toJson (Ok v)    = JSObject v
toJson (Error s) = error s

-- Json test object
testObject :: JSValue
testObject = toJson $ decode "{ \"stat\" : \"fail\", \"err\" : { \"msg\" : \"test message\", \"code\" : 100 } }"

statusErr :: JSValue
statusErr = testObject

statusOk :: JSValue
statusOk = toJson $ decode "{ \"stat\" : \"ok\" }"

-- Test value parsing
-- TODO: old test, add more for new dsl
jsUtilTest = TestCase ( do
    assertEqual "Empty path"    (Just testObject) (R.js4path [] testObject)
    assertEqual "One level"     (Just . JSString $ toJSString "fail") (R.js4name "stat" testObject)
    assertEqual "No path match" Nothing (R.js4name "no" testObject)
    assertEqual "Two levels"    (Just $ JSRational (100%1)) (R.js4path ["err", "code"] testObject)
    assertEqual "One more"      (Just . JSString $ toJSString "test message") (R.js4path ["err", "msg"] testObject)
    assertEqual "No path match" Nothing (R.js4name "noerr" testObject)
    assertEqual "js2v test 1"   (100::Integer) (R.js2v $ fromJust $ R.js4path ["err", "code"] testObject)
    assertEqual "js2v test 2"   "fail" (R.js2v $ fromJust $ R.js4name "stat" testObject)
    )

-- ---------------------------------------------------------------------------
-- JSon response DLS tests

dslObject :: JSValue
dslObject = toJson $ decode "{ \"review_request\" : { \"id\" : 123 } }"

responseDSLTest = TestCase ( do
    assertEqual "rb_review_request DSL" (toJson $ decode "{ \"id\" : 123 }") (R.review_request dslObject)
    assertEqual "rb_id DSL" 123 $ (R.id . R.review_request) dslObject
  )

-- ---------------------------------------------------------------------------
-- Server test utils

-- Perform a login to check if server is available
serverAvailable :: String -> String -> String -> IO Bool
serverAvailable url user passwd = do
    execRBAction url user passwd testLogin
    where
      testLogin :: RBAction Bool
      testLogin = return True

runServerTest True  test = runTestTT test >> return ()
runServerTest False _    = error "Server not available! Skipping tests..."

