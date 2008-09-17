-----------------------------------------------------------------------------
-- |
-- Module      :  Browser.hs
--
-- Maintainer  :  adam.smyczek@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- ReviewBoard.Browser extends Network.Browser module
-- with support for @multipart/form-data@ content type.
--
-- The package contains typed 'Form' and form variables 'FormVar'.
-- The content encryption type is automatically chosen based on the
-- type of the 'FormVar'. Currently @multipart/form-data@
-- encryption is used only if 'Form' contains a 'fileUpload' variable. 
-- Otherwise the request falls back to the default Network.Browser 
-- encryption type.
--
-----------------------------------------------------------------------------

module ReviewBoard.Browser (

    Form(..),
    FormVar,
    formToRequest,

    textField,
    checkBox,
    fileUpload,

    toMap,
    toFormVar,

    ) where

import qualified Network.Browser as NB
import Network.HTTP
import Network.URI
import System.Random

-- | Form to request for typed form variables,
-- same as 'formToRequest' in Network.Browser module.
--
formToRequest :: Form -> NB.BrowserAction Request
formToRequest (Form m u vs) 
    -- Use multipart/form-data content type when 
    -- the form contains at least one FileUpload variable
    | containFileUpload vs = do
        bnd  <- NB.ioAction mkBoundary
        body <- NB.ioAction $ encMultipartVars bnd vs
        return Request 
            { rqMethod  = POST
            , rqHeaders =
                [ Header HdrContentType $ "multipart/form-data; boundary=" ++ bnd,
                  Header HdrContentLength (show . length $ body) ]
            , rqBody    = body
            , rqURI     = u }

    -- Otherwise fall back to Network.Browser formToRequest
    | otherwise = return $ NB.formToRequest (NB.Form m u $ toMap vs)

    where
        -- Check if contains contain file upload
        containFileUpload = or. map (isFU . fvValue)
        isFU (FileUpload _ _) = True
        isFU _                = False

        -- Create random boundary string
        mkBoundary = do
            rand <- randomRIO (100000000000 :: Integer, 999999999999)
            return $ "--------------------" ++ show rand

-- | Encode form variables and append finish boundary.
--
encMultipartVars :: String -> [FormVar] -> IO String
encMultipartVars bnd vs = do
    vars <- mapM (encVar bnd) vs
    return $ concat [concat vars, "--", bnd, "--", crlf]

-- | Encode variable separated to header and content.
--
encVar :: String -> FormVar -> IO String
encVar bnd fv = do
    vs <- encContent . fvValue $ fv
    return $ concat [ "--", bnd, crlf
                    , concatMap show $ encHeader (fvName fv) (fvValue fv), crlf
                    , vs, crlf ]

-- | Encode headers 
encHeader :: String -> FormVarValue -> [Header]
encHeader n (FileUpload f t) =
    [ Header hdrContentDisposition ("form-data; name=\"" ++ n ++ "\"; filename=\"" ++ f ++ "\"")
    , Header HdrContentType t ]
encHeader n _ = [Header hdrContentDisposition ("form-data; name=\"" ++ n ++ "\"")]

-- | Encode content
--
encContent :: FormVarValue -> IO String
encContent (TextField v)     = return v
encContent (CheckBox True)   = return "true"
encContent (CheckBox False)  = return "false"
encContent (FileUpload f t) = readFile f >>= return

hdrContentDisposition :: HeaderName
hdrContentDisposition = HdrCustom "Content-disposition"

crlf :: String
crlf = "\r\n"

-- ---------------------------------------------------------------------------
-- Types

-- | Typed form variable
--
data FormVar = FormVar 
    { fvName  :: String         -- ^ variable name
    , fvValue :: FormVarValue   -- ^ and content 
    } deriving Show

-- | Form content value types
--
data FormVarValue
    = TextField  String
    | FileUpload FilePath String
    | CheckBox   Bool
    deriving Show

-- | Typed form
--
data Form = Form RequestMethod URI [FormVar]

-- ---------------------------------------------------------------------------
-- Util functions

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

-- | Convert [FormVar] to Network.Browser FormVar, a (String, String) map
--
toMap :: [FormVar] -> [(String, String)]
toMap = map (\fv -> (fvName fv, toNBValue . fvValue $ fv))

-- | Opposite to toNBFormVar
-- Converts a String tuple to 'FormVar'
--
toFormVar :: [(String, String)] -> [FormVar]
toFormVar = map (\(n, v) -> textField n v)

-- | Convert Form value to Network.Browser strings
--
toNBValue :: FormVarValue -> String
toNBValue (TextField v)    = v
toNBValue (FileUpload f t) = f
toNBValue (CheckBox True)  = "true"
toNBValue (CheckBox False) = "false"

