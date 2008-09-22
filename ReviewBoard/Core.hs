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
    runRequest,
    defaultResponseHandler,

    -- * RB state 
    RBState(..),
    setErrorHandler,

    -- * Form fields
    RBRequestType(..),
    RBRequestMethod(..),
    Form(..),
    FormVar(..),
    textField,
    checkBox,
    fileUpload,

    -- * Response types
    RBResponse(..),
    responseToEither,

    -- * Common functions
    logout,

    ) where

import ReviewBoard.Response
import Control.Monad.Error
import Control.Monad.State
import Data.Maybe
import Text.JSON
import Network.Curl
import qualified Data.Map as M

-- ---------------------------------------------------------------------------
-- Review board action monad

-- | The action monad, a state with error handler.
--
-- 'RBAction' represents one ReviewBoard session that handles multiple API calls.
-- The RBAction runner 'runRBAction' performs a login to the ReviewBoard server 
-- and initializes the session.  All session related parameters are stored in 
-- the 'RBState' of the action.
--
-- Errors are handled in two ways:
--
-- * Network related errors are immediately thrown using ErrorT throwError.
--
-- * ReviewBoard response errors are handled using the error handler defined 
-- in 'RBState' (default handler is print).
--
newtype RBAction a = RBAction
    { exec :: ErrorT String (StateT RBState IO) a }
    deriving (Functor, Monad, MonadIO, MonadState RBState, MonadError String)

-- | Run for 'RBAction', performs a login using provided URL, user 
-- and password parameters and executes the action. When login fails 
-- 'runRBAction' returns immediately with an error.
--
runRBAction :: String -> String -> String -> RBAction a -> IO (Either String a, RBState)
runRBAction url u p a = runStateT (runErrorT (exec init)) $ initState url u
    where init = login u p >> a

-- ---------------------------------------------------------------------------
-- State type and handler functions

-- | RB action state contains session related information.
--
data RBState = RBState
    { rbUrl        :: String          -- ^ ReviewBoard server URL
    , rbUser       :: String          -- ^ Logged in user
    , rbSessionId  :: Maybe String    -- ^ Session id cookie retrieve from a successful login
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
setSessionId :: Maybe String -> RBAction ()
setSessionId sid = get >>= \s -> put s { rbSessionId = sid } 

-- | Set error handler used for ReviewBoard error responses.
--
setErrorHandler :: (String -> IO ()) -> RBAction ()
setErrorHandler eh = get >>= \s -> put s { rbErrHandler = eh } 

-- ---------------------------------------------------------------------------
-- Request types

-- | Type of the request
--
data RBRequestType
    = API               -- ^ Default Json API request 
    | HTTP              -- ^ HTTP request currently used to publish a review request
    deriving Show

-- | HTTP request method
--
data RBRequestMethod
    = GET               -- ^ GET
    | POST              -- ^ and POST
    deriving Show

-- | Typed form variable
--
data FormVar = FormVar
    { fvName :: String          -- ^ variable name
    , fvValue :: FormVarValue   -- ^ and content type
    } deriving Show
 
-- | Form content value types
--
data FormVarValue
    = TextField String              -- ^ A text field with String value
    | FileUpload FilePath String    -- ^ File upload referencing the file path and file name
    | CheckBox Bool                 -- ^ Boolean field, a check box
    deriving Show
 
-- | Form type
--
data Form = Form 
    { requestType   :: RBRequestType    -- ^ the request type API or HTTP
    , requestMethod :: RBRequestMethod  -- ^ http method, GET or POST
    , apiURLString  :: URLString        -- ^ API sub-url e.g. 'repositories'
    , formVars      :: [FormVar] }      -- ^ Form fields
 
-- | Create text field variable
--
textField :: String -> String -> FormVar
textField n v = FormVar n $ TextField v
 
-- | Create checkbox variable
--
checkBox :: String -> Bool -> FormVar
checkBox n v = FormVar n $ CheckBox v
 
-- | Create file upload variable
--
fileUpload :: String -> FilePath -> String -> FormVar
fileUpload n p t = FormVar n $ FileUpload p t

-- ---------------------------------------------------------------------------
-- Response types

-- | Response type return by every API function
--
data RBResponse
    = RBok  JSValue -- ^ Successful response returns the JSON response object
    | RBerr String  -- ^ Response error
    deriving Eq

instance Show RBResponse where
   show (RBok r)    = "Ok: " ++ encode r
   show (RBerr e)   = "Error: " ++ e

-- | Convenient response to either converter
--
responseToEither :: RBResponse -> Either String JSValue
responseToEither (RBok r)   = Right r
responseToEither (RBerr s)  = Left s

-- ---------------------------------------------------------------------------
-- Request and response handling

-- | The request runner, generates request from provided 'Form' parameters,
-- executes the requests and handles the response using the handler function.
--
runRequest :: Form -> (Form -> CurlResponse -> RBAction a) -> RBAction a
runRequest form respHandler = do
    sid <- liftM rbSessionId get
    rt  <- requestType form
    rm  <- requestMethod form

    -- Execute request
    r <- liftIO $ initialize >>= \ h -> do
            setopt  h (CurlVerbose False)
            setopt  h rt
            setopts h rm
            setopt  h (CurlHttpPost (map toHttpPost $ formVars form))
            when (isJust sid) $ setopt h (CurlCookie . fromJust $ sid) >> return ()
            perform_with_response h

    -- Check response status
    case respCurlCode r of
        CurlOK -> respHandler form r
        c      -> throwError $ respStatusLine r ++ " (Code: " ++ show c ++ ")"
    where
        -- Request type to curl option
        requestType (Form API _ url _) = mkApiURL url >>= return . CurlURL
        requestType (Form HTTP _ url _) = mkHttpURL url >>= return . CurlURL  

        -- Request method to curl options
        requestMethod (Form _ POST url _) = return method_POST
        requestMethod (Form _ GET  url _) = return method_GET

        -- HttpPost content
        toHttpPost (FormVar name (TextField value))       = multiformString name value
        toHttpPost (FormVar name (CheckBox bool))         = multiformString name (show bool)
        toHttpPost (FormVar name (FileUpload path ctype)) = HttpPost 
            { postName      = name
            , content       = ContentFile path
            , contentType   = Just ctype
            , extraHeaders  = []
            , showName      = Nothing } 

-- | Default response handler returns the RBResponse type
--
defaultResponseHandler :: Form -> CurlResponse -> RBAction RBResponse
defaultResponseHandler (Form API  _ _ _) resp = decodeResp resp >>= mkApiResponse
defaultResponseHandler (Form HTTP _ _ _) resp = mkHttpResponse resp

-- | Login action updates session id cookie from successful login response
--
login :: String -> String -> RBAction RBResponse
login user password = do
    let form = Form API POST "accounts/login/" [textField "username" user, textField "password" password]
    runRequest form withCookie
    where
        withCookie _ resp = (maybe noCookieErr (respond resp)) . getCookie . respHeaders $ resp
        respond resp sc   = setSessionId (Just sc) >> decodeResp resp >>= mkApiResponse
        noCookieErr       = throwError "No session cookie received!"
        getCookie hs      = (M.lookup "Set-Cookie" . M.fromList $ hs) :: Maybe String

-- | Logout and remove session cookie
--
logout :: RBAction RBResponse
logout = do
    let form = Form API POST "accounts/logout/" []
    runRequest form withCookie
    where
        withCookie _ resp = setSessionId Nothing >> decodeResp resp >>= mkApiResponse

-- | Decode response body
--
decodeResp = (either throwError return) . resultToEither . decode . respBody

-- | Create response for an API request
--
mkApiResponse :: JSValue -> RBAction RBResponse
mkApiResponse v = do
    erh <- liftM rbErrHandler get
    stat <- (return $ stat v) `catchError` (\_ -> return "")
    apiResp stat >>= handle erh
    where
        apiResp "ok"   = return $ RBok v
        apiResp "fail" = do
            err <- (return $ (msg . err)  v) `catchError` (\_ -> return "No error message received")
            return $ RBerr (err ++ " (" ++ encode v ++ ")")
        apiResp _      = return $ RBerr "Invalid response, not status received"

        -- Run handler on response
        handle :: (String -> IO ()) -> RBResponse -> RBAction RBResponse
        handle _ o@(RBok r) = return o
        handle eh o@(RBerr e) = liftIO (eh e) >> return o


-- | Create response for an HTTP request
-- The successful response returns a JSObject of the from:
-- { head : [
--              { name = "header name"
--                value = "header value" }
--          ],
--   body : "body content" }
--
mkHttpResponse :: CurlResponse -> RBAction RBResponse
mkHttpResponse r = return $ RBok . JSObject . toJSObject $
      [ ("head", mkHead (respHeaders r))
      , ("body", mkBody (respBody r)) ]
    where
        mkHead = JSArray . map (\(n, v) -> JSObject . toJSObject $
            [ ("name", JSString . toJSString . show $ n)
            , ("value", JSString . toJSString $ v)])
        mkBody = JSString . toJSString

-- ---------------------------------------------------------------------------
-- Util functions

-- | Create ReviewBoard specific URI for a Web API call URL.
--
mkApiURL :: String -> RBAction URLString
mkApiURL apiUrl = mkURL ("/api/json/" ++ apiUrl)

-- | Create ReviewBoard specific URI for direct HTTP request.
--
mkHttpURL = mkURL

-- | General URI maker.
--
mkURL :: String -> RBAction URLString
mkURL url = liftM ((++url) . rbUrl) get

