-----------------------------------------------------------------------------
-- |
-- Module      :  Api.hs
--
-- Maintainer  :  adam.smyczek@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- ReviewBoard API
--
-- This module provides the basic ReviewBoard API calls.
-- All calls are executed inside the 'RBAction' monad that represents 
-- a session to the ReviewBoard server. A login to the server is performed
-- in the 'RBAction' run method 'runRBAction'.
--
-- All actions return the 'RBResponse' object that can be a 'RBok' with 
-- the response 'JSValue' or 'RBerr' containing the error message and
-- the encoded response, if received. Errors are handled in 
-- two ways:
--
-- * Network errors, for example connection errors throw an exception.
--
-- * Response errors resulting in for example invalid request parameters are
--   handled using the 'rbErrHandler' (by default print to stdin).
--
-- For API details see ReviewBoard project page <http://code.google.com/p/reviewboard/>
--
-----------------------------------------------------------------------------

--  TODO: find a generic way to build API calls based on path

module ReviewBoard.Api (

    -- Modules
    module ReviewBoard.Core,
    module ReviewBoard.Browser,
    module ReviewBoard.Request,

    -- * API calls
    
    -- ** Users and groups
    userList,
    groupList,
    groupStar,
    groupUnstar,

    -- ** Review request
    reviewRequest,
    reviewRequestByChangenum,
    reviewRequestNew,
    reviewRequestDelete,
    reviewRequestSet,
    reviewRequestSetField,
    reviewRequestSaveDraft,
    reviewRequestDiscardDraft,
    reviewRequestStar,
    reviewRequestUnstar,
    reviewRequestDiffNew,
    reviewRequestScreenshotNew,
    reviewRequestListAll,
    reviewRequestListToGroup,
    reviewRequestListToUser,
    reviewRequestListFromUser,

    -- ** Review
    reviewAll,
    reviewSaveDraft,
    reviewDeleteDraft,
    reviewPublishDraft,

    -- ** Others
    repositoryList,

    -- * Util functions
    execRBAction,

    -- * Example
    -- $example1

    ) where

import Prelude hiding (all)
import ReviewBoard.Core
import ReviewBoard.Browser
import ReviewBoard.Request
import Network.URI
import Network.HTTP hiding (user)
import qualified Network.Browser as NB
import Control.Monad.Error

-- ---------------------------------------------------------------------------
-- User handling API calls

-- | Search for a user or list all users if user is Nothing
--
userList :: Maybe String -> RBAction RBResponse
userList (Just u) =  apiGet (users Nothing) [textField "query" u]
userList Nothing  =  apiGet (users Nothing) []

-- | Search for a group or list all group if Nothing
--
groupList :: Maybe String -> RBAction RBResponse
groupList (Just g) =  apiGet (groups Nothing) [textField "query" g]
groupList Nothing  =  apiGet (groups Nothing) []

-- | Star group for group name
--
groupStar :: String -> RBAction RBResponse
groupStar g = apiGet (groups (Just g) . star) []

-- | Unstar group for group name
--
groupUnstar :: String -> RBAction RBResponse
groupUnstar g = apiGet (groups (Just g) . unstar) []

-- ---------------------------------------------------------------------------
-- Review request API calls

-- | Create new review request using the provided repository path and an optional
-- submit_as user. The returned response contains the @id@ of the new created
-- review request that can be accessed using 'rrId' helper function.
--
reviewRequestNew :: String -> Maybe String -> RBAction RBResponse
reviewRequestNew p (Just u) = apiPost (reviewrequests Nothing . new) $ toFormVar [("repository_path", p), ("submit_as", u)]
reviewRequestNew p Nothing  = apiPost (reviewrequests Nothing . new) [textField "repository_path" p]

-- | Delete review request with request @id@.
--
reviewRequestDelete :: Integer -> RBAction RBResponse
reviewRequestDelete id = apiPost (reviewrequests (Just id) . delete) []

-- | Get review request by @id@.
--
reviewRequest :: Integer -> RBAction RBResponse
reviewRequest id = apiPost (reviewrequests (Just id)) []

-- | Get review request by repository @id@ and changenum @id@
--
reviewRequestByChangenum :: Integer -> Integer -> RBAction RBResponse
reviewRequestByChangenum rId cId = apiPost (reviewrequests Nothing . repository rId . changenum cId) []

-- | Discard review request draft for @id@.
--
reviewRequestSaveDraft :: Integer -> RBAction RBResponse
reviewRequestSaveDraft id = apiPost (reviewrequests (Just id) . draft . save) []

-- | Save review request draft whith @id@.
--
reviewRequestDiscardDraft :: Integer -> RBAction RBResponse
reviewRequestDiscardDraft id = apiPost (reviewrequests (Just id) . draft . discard) []

-- | Set fields to review request draft with @id@.
--
reviewRequestSet :: Integer -> [(RRField, String)] -> RBAction RBResponse
reviewRequestSet id fs = apiPost (reviewrequests (Just id) . draft . set Nothing) (map (\(f, v) -> textField (show f) v) fs)

-- | Set one field for review request draft with @id@.
--
reviewRequestSetField :: Integer -> RRField -> String -> RBAction RBResponse
reviewRequestSetField id f v = apiPost (reviewrequests (Just id) . draft . set (Just f)) $ [textField "value" v]

-- | Star review request for id
--
reviewRequestStar :: Integer -> RBAction RBResponse
reviewRequestStar id = apiGet (reviewrequests (Just id) . star) []

-- | Star review request for id
--
reviewRequestUnstar :: Integer -> RBAction RBResponse
reviewRequestUnstar id = apiGet (reviewrequests (Just id) . unstar) []

-- | Add a new diff to a review request with @id@, file path and the basedir parameter.
--
reviewRequestDiffNew :: Integer -> String -> FilePath -> RBAction RBResponse
reviewRequestDiffNew id bd fp = 
    apiPost (reviewrequests (Just id) . diff . new) [fileUpload "path" fp "text/plain", textField "basedir" bd]

-- | Add a new screenshot with @file path@ to a review request with @id@
--
reviewRequestScreenshotNew :: Integer -> FilePath -> RBAction RBResponse
reviewRequestScreenshotNew id fp =
    apiPost (reviewrequests (Just id) . screenshot . new) [fileUpload "path" fp ((contentType . extension) fp)]
    where
        extension = reverse . takeWhile (/= '.') . reverse
        contentType "png"   = "image/png"
        contentType "gif"   = "image/gif"
        contentType "jpg"   = "image/jpeg"
        contentType "jpeg"  = "image/jpeg"
        contentType _       = "text/plain" -- fallback

-- | List all review requests with an optional status
--
reviewRequestListAll :: Maybe String -> RBAction RBResponse
reviewRequestListAll (Just s) = apiGet (reviewrequests Nothing . all) [textField (show STATUS) s]
reviewRequestListAll Nothing  = apiGet (reviewrequests Nothing . all) []

-- | List review request assigned to a group with an optional status
--
reviewRequestListToGroup :: String -> Maybe String -> RBAction RBResponse
reviewRequestListToGroup g (Just s) = apiGet (reviewrequests Nothing . to . group (Just g)) [textField (show STATUS) s]
reviewRequestListToGroup g Nothing  = apiGet (reviewrequests Nothing . to . group (Just g)) []

-- | List review request assigned to a user, directly or not with an optional status
--
reviewRequestListToUser :: String -> Bool -> Maybe String -> RBAction RBResponse
reviewRequestListToUser u True  (Just s) = apiGet (rr2u u . directly) [textField (show STATUS) s]
reviewRequestListToUser u True  Nothing  = apiGet (rr2u u . directly) []
reviewRequestListToUser u False (Just s) = apiGet (rr2u u) [textField (show STATUS) s]
reviewRequestListToUser u False Nothing  = apiGet (rr2u u) []
rr2u u = reviewrequests Nothing . to . user (Just u)

-- | List review request from a user with an optional status
--
reviewRequestListFromUser :: String  -> Maybe String -> RBAction RBResponse
reviewRequestListFromUser u (Just s) = apiGet (reviewrequests Nothing . from . user (Just u)) [textField (show STATUS) s]
reviewRequestListFromUser u Nothing  = apiGet (reviewrequests Nothing . from . user (Just u)) []

-- ---------------------------------------------------------------------------
-- Review API calls

-- | List all reviews for review request @id@
--
reviewAll :: Integer -> RBAction RBResponse
reviewAll id = apiGet (reviewrequests (Just id) . reviews Nothing) []

-- | Publish review request draft for id
--
reviewPublishDraft :: Integer -> RBAction RBResponse
reviewPublishDraft id = apiPost (reviewrequests (Just id) . reviews Nothing . draft . publish) [checkBox "shipit" False]

-- | Save review draft for review request @id@
--
reviewSaveDraft :: Integer -> RBAction RBResponse
reviewSaveDraft id = apiPost (reviewrequests (Just id) . reviews Nothing . draft . save) []

-- | Delete review draft for review request @id@
--
reviewDeleteDraft :: Integer -> RBAction RBResponse
reviewDeleteDraft id = apiPost (reviewrequests (Just id) . reviews Nothing . draft . delete) []

-- ---------------------------------------------------------------------------
-- Other API calls

-- | List repositories
--
repositoryList :: RBAction RBResponse
repositoryList =  apiGet repositories []

-- ---------------------------------------------------------------------------
-- API uitl functions

-- | Execute a ReviewBoard action using the provided URL, user
-- and password.
--
execRBAction :: String -> String -> String -> (RBAction a) -> IO a
execRBAction url user password action = do
    r <- runRBAction url user password action
    either error return $ fst r

{- $example1

The following RBAction creates a new review request draft, sets some fields
and uploads a diff file:

>    import ReviewBoard.Api
>    import qualified ReviewBoard.Response as R

>    newRRAction :: RBAction ()
>    newRRAction = do
>        rsp <- reviewRequestNew "repository" Nothing
>        case rsp of
>            RBok r -> do
>                let id = R.id . R.review_request $ r
>                reviewRequestsSetField id TARGET_PEOPLE "reviewers"
>                reviewRequestsSetField id DESCRIPTION "Request description"
>                reviewRequestsDiffNew  id "basedir" "diffFileName"
>                reviewRequestSaveDraft id
>                liftIO $ print "Done."
>            RBerr s -> throwError s

To run this action, execute:

>   execRBAction "url" "user" "password" newRRAction

-}

