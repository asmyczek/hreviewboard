#!/usr/bin/env runhaskell
-----------------------------------------------------------------------------
-- |
-- Module      :  rbpatch.hs
--
-- Maintainer  :  adam.smyczek@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A command line tool to patch local repository from a review request diff
--
-- Sample usage:
-- > rbpatch -r <review request id>
--
-- Same as for @mkrr@ most of the required/optional command line parameters
-- may be pre-defined in @~/.rbpatchrc@ config file. The supported parameters
-- are:
--
-- * url            - ReviewBoard server URL
--
-- * user           - User name
-- 
-- * password       - Password
--
-- * localbasedir   - Path to the repository root
--
-- Run @rbpatch --help@ for details.
--
-----------------------------------------------------------------------------
module Main (main) where

import System.Environment
import System.Console.GetOpt
import System.Directory
import Data.List
import Data.Maybe
import Data.Char
import ReviewBoard.Api
import qualified ReviewBoard.Response as R
import Control.Monad.Error
import System.Process
import System.IO
import System.Exit

-- | Main
--
main :: IO ()
main = do
    -- Parse and validate command line options
    (opts, _) <- getArgs >>= parseOpts
    validateRequired opts

    -- Create the patch action
    let action = patchAction (fromJust . optRRId $ opts) 
                             (optBasedir opts)
                             (optPrintOnly opts) 

    -- ... and run it
    execRBAction (fromJust . optUrl $ opts) 
                 (fromJust . optUser $ opts) 
                 (fromJust . optPassword $ opts) 
                 action

-- | The patch action
--
patchAction :: String -> Maybe String -> Bool -> RBAction ()
patchAction id basedir po = do
    rsp <- httpGet ("/r/" ++ id ++ "/diff/raw/") []
    case rsp of
        RBok r  -> liftIO . (patch po basedir) . R.body $ r
        RBerr e -> throwError e
    where
        patch True  _         = putStr
        patch False Nothing   = doPatch ["-p0"]
        patch False (Just bd) = doPatch ["-d " ++ bd, "-p0"]

-- | Execute patch command
--
doPatch :: [String] -> String -> IO ()
doPatch args p = do
    pf <- findPatch
    (inp, out, err, pid) <- runInteractiveProcess pf args Nothing Nothing
    hPutStr inp p
    hClose inp
    result <- hGetContents out
    errors <- hGetContents err
    when (result == result) $ return ()
    when (errors == errors) $ return ()
    hClose out
    hClose err
    waitForProcess pid
    when (not . null $ result) $ putStr (result ++ "\nDone.\n")
    when (not . null $ errors) $ putStr errors
    return () 
   
-- ---------------------------------------------------------------------------
-- Option handling

-- | Parse options
--
parseOpts :: [String] -> IO (Options, [String])
parseOpts argv = do
    dos <- defaultOpts
    case getOpt Permute options argv of
        (o, n, []) ->  foldM (flip id) dos o >>= \os -> return (os, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))

-- | rbpatch help header
--
header = "Usage: rbpatch [OPTION...]"

-- | Option set
--
data Options = Options
    { optUrl        :: Maybe String
    , optUser       :: Maybe String
    , optPassword   :: Maybe String
    , optRRId       :: Maybe String
    , optBasedir    :: Maybe String
    , optPrintOnly  :: Bool
    } deriving Show

-- | Option descriptor list
--
options :: [OptDescr (Options -> IO Options)]
options =
   [ Option ['u'] []       (ReqArg (\ u opts -> return opts { optUrl = Just u }) "URL") "URL to ReviewBoard server"
   , Option ['U'] []       (ReqArg (\ u opts -> return opts { optUser = Just u }) "USER") "USER name"
   , Option ['P'] []       (ReqArg (\ p opts -> return opts { optPassword = Just p }) "PASSWORD") "User PASSWORD"
   , Option ['b'] []       (ReqArg (\ b opts -> return opts { optBasedir = Just b }) "BASEDIR") "Local base dir"
   , Option ['r'] []       (ReqArg (\ r opts -> return opts { optRRId = Just  r }) "REVIEW_REQUEST_ID") "Review request id"
   , Option ['p'] []       (NoArg  (\   opts -> return opts { optPrintOnly = True })) "Print only, do not apply the patch"
   , Option ['h'] ["help"] (NoArg  (\   opts -> putStr (usageInfo header options) >> exitWith ExitSuccess)) "Print this help"
   ]

-- | Load default options from configuration file
--
defaultOpts :: IO Options
defaultOpts = do
    hd <- getHomeDirectory
    cf <- readFile $ hd ++ "/.rbpatchrc"
    return $ foldl parseOpt initOpts $ lines cf
    where
        parseOpt os l | isPrefixOf "url"            l = os { optUrl        = Just $ parseValue l }
                      | isPrefixOf "user"           l = os { optUser       = Just $ parseValue l }
                      | isPrefixOf "password"       l = os { optPassword   = Just $ parseValue l }
                      | isPrefixOf "localbasedir"   l = os { optBasedir    = Just $ parseValue l }
                      | otherwise                     = os
        parseValue :: String -> String
        parseValue = takeWhile (not . isSeparator) . dropWhile isSeparator . drop 1 . dropWhile (/= '=')

-- | Initial option set
--
initOpts :: Options
initOpts = Options
    { optUrl        = Nothing
    , optUser       = Nothing
    , optPassword   = Nothing
    , optRRId       = Nothing
    , optBasedir    = Nothing
    , optPrintOnly  = False }

-- | Validate required arguments
--
validateRequired :: Options -> IO ()
validateRequired opts = do
    -- TODO: this is imperative, improve this
    when (isNothing . optUrl $ opts)        $ error $ "URL required!" ++ help
    when (isNothing . optUser $ opts)       $ error $ "USER required!" ++ help
    when (isNothing . optPassword $ opts)   $ error $ "PASSWORD required!" ++ help
    when (isNothing . optRRId $ opts)       $ error $ "REVIEW_REQUEST_ID required!" ++ help
    where
        help = " Try rbpatch --help"

-- ---------------------------------------------------------------------------
-- Patch command utils

-- | Common patch locations
--
patchCmds = 
    [ "/usr/bin/patch"
    , "/usr/local/bin/patch"
    , "/opt/bin/sendmail"
    , "/opt/local/bin/sendmail"
    ] :: [String]


-- | Find patch command and throw error if patch 
-- is not installed in one of patchCmds locations
-- 
findPatch :: IO String
findPatch = do
    pf <- foldM fExist "" patchCmds
    if null pf then error "No patch file found!"
               else return pf
    where
        fExist r f | (not . null) r = return r
                   | otherwise      = do
                        e <- doesFileExist f 
                        return $ if e then f else ""

