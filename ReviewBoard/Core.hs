{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Core.hs
--
-- Maintainer  :  adam.smyczek@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The core module implements the base types and functions of the bindings.
--
-----------------------------------------------------------------------------

module ReviewBoard.Core (

    -- * RB action monad
    RBAction,
    runRBAction,
    liftBA,
    runRequest,
    RBRequestType(..),

    -- * RB state 
    RBState(..),
    setErrorHandler,
    setDebugHTTP,

    -- * Response type
    RBResponse(..),
    responseToEither,

    -- * Utils
    mkApiURI,
    mkHttpURI

    ) where

import Network.URI
import Network.HTTP
import qualified Network.Browser as NB
import ReviewBoard.Browser
import qualified ReviewBoard.Response as R
import Control.Monad.Error
import Control.Monad.State
import Data.Maybe
import Text.JSON

-- ---------------------------------------------------------------------------
-- Review board action monad

-- | The action monad, a state with error handler.
--
-- 'RBAction' represents one ReviewBoard session that handles multiple API calls.
-- The RBAction runner 'runRBAction' performs a login into the ReviewBoard server 
-- and initializes the session.  All session related parameters are stored in 
-- the 'RBState' of the action.
--
-- Errors are handled in two ways:
--
-- * Network related error are immediately thrown using ErrorT throwError.
--
-- * ReviewRequest response errors are handled using the error handler defined 
-- in 'RBState' (default print).
--
newtype RBAction a = RBAction
    { exec :: ErrorT String (StateT RBState NB.BrowserAction) a }
    deriving (Functor, Monad, MonadState RBState)

instance MonadIO RBAction where
  liftIO = RBAction . lift . lift . NB.ioAction

instance MonadError String RBAction where
    throwError = RBAction . throwError
    l `catchError` h = RBAction $ exec l `catchError` (exec . h)

-- | Run for 'RBAction', performs a login using provided URL, user 
-- and password parameters and executes the action. When login fails 
-- 'runRBAction' returns immediately with an error.
--
runRBAction :: String -> String -> String -> RBAction a -> IO (Either String a, RBState)
runRBAction url u p a = NB.browse . runStateT (runErrorT (exec init)) $ initState url u
    where init = setDebugHTTP False >> login u p >> a

-- ---------------------------------------------------------------------------
-- State type and handler functions

-- | RB action state containing session related information.
--
data RBState = RBState
    { rbUrl        :: String          -- ^ ReviewBoard server URL
    , rbUser       :: String          -- ^ Logged in user
    , rbSessionId  :: Maybe NB.Cookie -- ^ Session id cookie retrieve from a successful login
    , rbErrHandler :: String -> IO () -- ^ Error handler, for example error or print
    }

-- | Default state initialization including server URL and user.
--
initState :: String -> String -> RBState
initState url user = RBState 
    { rbUrl        = url
    , rbUser       = user
    , rbSessionId  = Nothing
    , rbErrHandler = print }

-- | Session id setter
--
setSessionId :: NB.Cookie -> RBAction ()
setSessionId sid = get >>= \s -> put s { rbSessionId = Just sid } 

-- | Set error handler used for ReviewBoard error responses.
--
setErrorHandler :: (String -> IO ()) -> RBAction ()
setErrorHandler eh = get >>= \s -> put s { rbErrHandler = eh } 

-- ---------------------------------------------------------------------------
-- Response types

-- | Type of the request, Web API or default HTTP
--
data RBRequestType
    = API
    | HTTP
    deriving Show

-- | Response type return by every API function
--
data RBResponse
    = RBok  JSValue -- ^ Successful response, contains JSON response object
    | RBerr String  -- ^ Response error including error message including 
                    --   encoded response
    deriving Eq

instance Show RBResponse where
   show (RBok r)    = "Ok: " ++ encode r
   show (RBerr e)   = "Error: " ++ e

-- | Convenient response converter
--
responseToEither :: RBResponse -> Either String JSValue
responseToEither (RBok r)   = Right r
responseToEither (RBerr s)  = Left s

-- ---------------------------------------------------------------------------
-- Request and response handling

-- | The request runner, generates request from provided 'Form' parameter,
-- executes the requests and handles the response using the handler function.
--
runRequest :: RBRequestType -> Form -> (RBResponse -> RBAction a) -> RBAction a

-- API request runner
runRequest rt form f = do
    s <- get

    -- Execute request
    (u, r) <- liftBA $ do
        attachSID $ rbSessionId s
        formToRequest form >>= NB.request

    -- Check response status
    case rspCode r of
        (2,0,0) -> respond rt r s
        c       -> throwError $ rspReason r ++ " (Code: " ++ show c ++ ")"
    where
        -- Add session id to request, if exists
        attachSID (Just sid) = NB.setCookies [sid]
        attachSID _          = return ()

        -- Respond based on request type
        respond API r s  = case (decode . rspBody) r of
                  Ok rsp  -> mkApiResponse rsp >>= handle f (rbErrHandler s)
                  Error e -> throwError e
        respond HTTP r s = mkHttpResponse r >>= handle f (rbErrHandler s)

        -- Run handler on response
        handle :: (RBResponse -> RBAction a) -> (String -> IO ()) -> RBResponse -> RBAction a
        handle f _  o@(RBok r)  = f o
        handle f eh o@(RBerr e) = liftIO (eh e) >> f o

-- | Login action updates session id cookie from successful login response
--
login :: String -> String -> RBAction RBResponse
login user password = do
    s   <- get
    uri <- mkApiURI "accounts/login/"
    let form = Form POST uri [textField "username" user, textField "password" password]
    runRequest API form setSessionCookie
    where
        setSessionCookie rsp = liftBA NB.getCookies >>= setCookie >> return rsp
        setCookie []       = throwError "No session cookie received!"
        setCookie (c:cs)   = setSessionId c

-- | Create API request RBResponse
--
mkApiResponse :: JSValue -> RBAction RBResponse
mkApiResponse v = do
    stat <- (return $ R.stat v) `catchError` (\_ -> return "")
    case stat of
        "ok"   -> return $ RBok v
        "fail" -> do
            err <- (return $ (R.msg . R.err)  v) `catchError` (\_ -> return "No error message received")
            return $ RBerr (err ++ " (" ++ encode v ++ ")")
        _      -> return $ RBerr "Invalid response, not status received"

-- | Create Http request RBResponse
-- The successful response returns a JSObject of the from:
-- { head : [
--              { name = "header name"
--                value = "header value" }
--          ],
--   body : "body content" }
--
mkHttpResponse :: Response -> RBAction RBResponse
mkHttpResponse r = return $ RBok . JSObject . toJSObject $
      [ ("head", mkHead (rspHeaders r))
      , ("body", mkBody (rspBody r)) ]
    where
        mkHead = JSArray . map (\(Header n v) -> JSObject . toJSObject $
            [ ("name", JSString . toJSString . show $ n)
            , ("value", JSString . toJSString $ v)])
        mkBody = JSString . toJSString

-- ---------------------------------------------------------------------------
-- Util functions

-- | Convenient lift for BrowserActions
--
liftBA :: NB.BrowserAction a -> RBAction a
liftBA = RBAction . lift . lift

-- | Create ReviewBoard specific URI for a Web API call URL.
--
mkApiURI :: String -> RBAction URI
mkApiURI apiUrl = mkURI ("/api/json/" ++ apiUrl)

-- | Create ReviewBoard specific URI for direct HTTP request.
--
mkHttpURI = mkURI

-- | General URI maker
--
mkURI :: String -> RBAction URI
mkURI url = do
    s <- get
    case parseURI (rbUrl s ++ url) of
        Just u  -> return u
        Nothing -> throwError $ "Invalid url: " ++ url

-- | Enable/disable debug output for Browser module
--
setDebugHTTP :: Bool -> RBAction ()
setDebugHTTP True  = liftBA $ NB.setOutHandler putStrLn
setDebugHTTP False = liftBA $ NB.setOutHandler (\_ -> return())

