-----------------------------------------------------------------------------
-- | 
-- This tests require ReviewBoard server
--
-- POC test with server. More to come...
--
-- TODO: Add negative tests
--
-----------------------------------------------------------------------------
module Tests.TestsWithServer (

  apiServerTests

  ) where

import Test.HUnit
import ReviewBoard.Api
import qualified ReviewBoard.Response as R
import Control.Monad.Trans
import Control.Monad.Error
import System.Environment

apiServerTests url user passwd = TestList
    [ TestLabel "Test repositoryList"       $ runServerTest url user passwd repositoryListAction

    -- User tests
    ,  TestLabel "Test userList"            $ runServerTest url user passwd (userListAction user)
    ,  TestLabel "Test userList search"     $ runServerTest url user passwd (userListSearchAction user)

    -- Review request tests
    , TestLabel "Test create/delete rr"     $ runServerTest url user passwd (createDeleteRR user)
    , TestLabel "Test set rr fields"        $ runServerTest url user passwd (setFieldsRR user)

    -- Test http method
    , TestLabel "Test http requests"        $ runServerTest url user passwd (testHttpRequests user)
    ]

-- Test repositoryList
repositoryListAction :: RBAction ()
repositoryListAction = do
    setErrorHandler error 
    r <- repositoryList >>= return . assertOkStatus
    assertTrue "Repository list empty" ((length . R.repositories $ r) > 0)
    return ()

-- ---------------------------------------------------------------------------
-- User API tests

-- Test userList
userListAction :: String -> RBAction ()
userListAction user = do
    setErrorHandler error 
    r <- userList Nothing >>= return . assertOkStatus
    let users = R.users r
    assertTrue "User list empty" (length users > 0)
    assertTrue "Login user does not exist" ((length . filter (==user) . map R.username) users > 0)
    return ()

-- Test userList search
userListSearchAction :: String -> RBAction ()
userListSearchAction user = do
    setErrorHandler error 
    vr <- userList (Just user) >>= return . assertOkStatus
    assertTrue "User list is not empty" (length (R.users vr) == 1)
    assertTrue "No login user found" (((R.username . (!!0) . R.users) vr) == user)
    ir <- userList (Just "not_a_user_2345234") >>= return . assertOkStatus
    assertTrue "User list is not empty" (length (R.users ir) == 0)
    return ()

-- ---------------------------------------------------------------------------
-- Revire request tests

-- Plain create/delete review request
createDeleteRR :: String -> RBAction ()
createDeleteRR user = do
    setErrorHandler error 
    id <- createRR user
    deleteRR id
    return ()

-- Set fields to RR tests
setFieldsRR :: String -> RBAction ()
setFieldsRR user = do
    setErrorHandler error 
    id <- createRR user
    
    -- Single fields
    let summary = "Test summary"
    sf <- reviewRequestSetField id SUMMARY summary >>= return . assertOkStatus
    assertTrue "Summary not set" (R.summary sf == summary)

    -- Multiple fields
    let description = "Test description"
        branch      = "branches/test"
    sfs <- reviewRequestSet id [(DESCRIPTION, description), (BRANCH, branch)] >>= return . assertOkStatus
    assertTrue "Description not set"    ((R.description . R.draft) sfs == description)
    assertTrue "Branch not set"         ((R.branch . R.draft) sfs == branch)

    deleteRR id
    return ()

-- Create review request
createRR :: String -> RBAction Integer
createRR user = do
    rep <- liftIO $ getEnv "RB_REPOSITORY"
    rr <- reviewRequestNew rep Nothing >>= return . assertOkStatus
    -- Check some response values
    assertTrue "Review request not created" ((R.id . R.review_request) rr > 0)
    assertTrue "Status not pending"         ((R.status . R.review_request) rr == "pending")
    assertTrue "Not the submitting user"    ((R.username . R.submitter . R.review_request) rr == user)
    return $ (R.id . R.review_request) rr

-- Delete review request
deleteRR :: Integer -> RBAction ()
deleteRR id = do
    reviewRequestDelete id >>= return . assertOkStatus
    ls <- reviewRequestListAll Nothing >>= return . assertOkStatus
    let ids = map R.id $ (R.review_requests) ls
    assertTrue "Request not deleted" (null $ filter (==id) ids)
    return ()


-- ---------------------------------------------------------------------------
-- Http method tests

testHttpRequests :: String -> RBAction ()
testHttpRequests user = do
    rsp <- httpGet "" []
    case rsp of
        RBok r  -> liftIO . print . R.body $ r
        RBerr e -> throwError e
    return ()

-- ---------------------------------------------------------------------------
-- Util function

-- | General test action runner
--
runServerTest :: String -> String -> String -> RBAction () -> Test
runServerTest url user passwd action = TestCase ( execRBAction url user passwd action )

-- | Assert for RBAction
--
assertTrue :: String -> Bool -> RBAction()
assertTrue s True = return ()
assertTrue s False = throwError $ "Assertion: " ++ s

-- | Get JSValue from response, ensures request was successful
--
assertOkStatus (RBok r)  = r
assertOkStatus (RBerr e) = error e

