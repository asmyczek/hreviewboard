#!/usr/bin/env runhaskell
-----------------------------------------------------------------------------
-- |
-- Module      :  mkrr.hs
--
-- Maintainer  :  adam.smyczek@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- @mkrr@ is a sample application demonstrating the usage of ReviewBoard bindings.
-- Using @mkrr@ new review requests can be submitted as easy as executing:
--
-- > svn diff | mkrr -r [reviewers]
--
-- from the local repository copy. Most of the required/optional parameters
-- may be pre-defined in @~/.mkrrrc@ config file. The supported parameters are:
--
-- * url        - ReviewBoard server URL
--
-- * user       - User name
-- 
-- * password   - Password
--
-- * repository - Repository path, e.g. <http://svn.mycompany.com/svn/>
--
-- * basedir    - Diff base dir, e.g. @/trunk@ in case @svn diff@ was executed 
-- in @/trunk@ directory
--
-- * publish    - Publish immediately
-- 
-- Run @mkrr --help@ and see ReviewBoard documentation at
-- <http://code.google.com/p/reviewboard/wiki/ReviewBoardAPI>.
--
-----------------------------------------------------------------------------

module Main (main) where

import System.Environment
import System.Console.GetOpt
import System.IO
import System.Directory
import Control.Exception(finally)
import Data.List
import Data.Maybe
import Data.Char
import ReviewBoard.Api
import Control.Monad.Error
import qualified ReviewBoard.Response as R
import System.Exit

-- | Main
--
main :: IO ()
main = do
    -- Parse and validate command line options
    (opts, _) <- getArgs >>= parseOpts
    validateRequired opts

    -- Create the review request action ...
    let action = mkrrAction (fromJust . optRepository $ opts)
                            (fromJust . optBasedir $ opts)
                            (fromJust . optReviewers $ opts)
                            (optDescription opts)
                            (optSummary opts)
                            (optScreenshot opts)
                            (optPublish opts)

    -- ... and run it
    execAction (fromJust . optUrl $ opts) 
               (fromJust . optUser $ opts) 
               (fromJust . optPassword $ opts) 
               action

-- | Run new review request action
--
execAction :: String -> String -> String -> (String -> RBAction ()) -> IO ()
execAction url user pass action = do
    -- Get diff from stdin
    diff <- hGetContents stdin

    -- Write diff to temp file
    td         <- catch (getTemporaryDirectory) (\_ -> return ".")
    (tfn, tfh) <- openTempFile td "rbDiff.diff"
    mapM (hPutStrLn tfh) $ lines diff
    hClose tfh

    -- Run action and delete temp file
    finally (execRBAction url user pass $ action tfn)
            (do removeFile tfn)

-- | New review request action
--
mkrrAction :: String -> String -> String -> Maybe String -> Maybe String -> Maybe String -> Bool -> String -> RBAction ()
mkrrAction rep basedir revs dsc sum ss pub tfn = do
    -- Create new review request and get the id
    rsp <- reviewRequestNew rep Nothing
    case rsp of
        RBok r -> do
            let id = R.id . R.review_request $ r

            -- Set some fields
            reviewRequestSetField id TARGET_PEOPLE revs
            setOptional id DESCRIPTION dsc 
            setOptional id SUMMARY sum

            -- Upload the diff
            reviewRequestDiffNew id basedir tfn

            -- Upload screenshot if provided
            when (isJust ss) (reviewRequestScreenshotNew id (fromJust ss) >> return ())

            -- And store the review request draft
            reviewRequestSaveDraft id

            -- Publish if enabled
            when (pub) (httpGet ("/r/" ++ show id ++ "/publish/") [] >> return ())

            liftIO $ print "Done."

        RBerr e -> throwError e

    where
        setOptional id f (Just v) = reviewRequestSetField id f v >>= \_ -> return ()
        setOptional id f Nothing  = return()

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

-- | Usage info header
--
header = "Usage: mkrr [OPTION...]"

-- | Option set
--
data Options = Options
    { optUrl         :: Maybe String
    , optUser        :: Maybe String
    , optPassword    :: Maybe String
    , optRepository  :: Maybe String
    , optBasedir     :: Maybe String
    , optReviewers   :: Maybe String
    , optDescription :: Maybe String
    , optSummary     :: Maybe String
    , optScreenshot  :: Maybe String
    , optPublish     :: Bool
    } deriving Show

-- | Option descriptor list
--
options :: [OptDescr (Options -> IO Options)]
options =
   [ Option ['u'] []       (ReqArg (\ u opts -> return opts { optUrl = Just u }) "URL") "URL to ReviewBoard server"
   , Option ['U'] []       (ReqArg (\ u opts -> return opts { optUser = Just u }) "USER") "USER name"
   , Option ['P'] []       (ReqArg (\ p opts -> return opts { optPassword = Just p }) "PASSWORD") "User PASSWORD"
   , Option ['R'] []       (ReqArg (\ r opts -> return opts { optRepository = Just r }) "REROSITORY") "Repository path"
   , Option ['b'] []       (ReqArg (\ r opts -> return opts { optBasedir = Just r }) "BASEDIR") "Diff base dir"
   , Option ['r'] []       (ReqArg (\ r opts -> return opts { optReviewers = Just r }) "REVIEWERS") "Comma separated REVIEWERS list"
   , Option ['d'] []       (ReqArg (\ r opts -> return opts { optDescription = Just r }) "DESCRIPTION") "Optional request description"
   , Option ['s'] []       (ReqArg (\ s opts -> return opts { optSummary = Just s }) "SUMMARY") "Optional request summary"
   , Option ['S'] []       (ReqArg (\ f opts -> return opts { optScreenshot = Just f }) "SCREENSHOT") "attach SCREENSHOT"
   , Option ['l'] []       (NoArg  (\   opts -> return opts { optPublish = True })) "Publish review"
   , Option ['h'] ["help"] (NoArg  (\   opts -> putStr (usageInfo header options) >> exitWith ExitSuccess)) "Print this help"
   ]

-- | Load default options from configuration file
--
defaultOpts :: IO Options
defaultOpts = do
    hd <- getHomeDirectory
    cf <- readFile $ hd ++ "/.mkrrrc"
    return $ foldl parseOpt initOpts $ lines cf
    where
        parseOpt os l | isPrefixOf "url"        l = os { optUrl        = Just $ parseValue l }
                      | isPrefixOf "user"       l = os { optUser       = Just $ parseValue l }
                      | isPrefixOf "password"   l = os { optPassword   = Just $ parseValue l }
                      | isPrefixOf "repository" l = os { optRepository = Just $ parseValue l }
                      | isPrefixOf "basedir"    l = os { optBasedir    = Just $ parseValue l }
                      | isPrefixOf "publish"    l = os { optPublish    = read $ parseValue l }
                      | otherwise                 = os
        parseValue :: String -> String
        parseValue = takeWhile (not . isSeparator) . dropWhile isSeparator . drop 1 . dropWhile (/= '=')

-- | Initial option set
--
initOpts :: Options
initOpts = Options
    { optUrl         = Nothing
    , optUser        = Nothing
    , optPassword    = Nothing
    , optRepository  = Nothing
    , optBasedir     = Nothing
    , optReviewers   = Nothing
    , optDescription = Nothing
    , optSummary     = Nothing
    , optScreenshot  = Nothing
    , optPublish     = False }

-- | Validate required options
--
validateRequired :: Options -> IO ()
validateRequired opts = do
    -- TODO: this is imperative, improve this
    when (isNothing . optUrl $ opts)        $ error $ "URL required!" ++ help
    when (isNothing . optUser $ opts)       $ error $ "USER required!" ++ help
    when (isNothing . optPassword $ opts)   $ error $ "PASSWORD required!" ++ help
    when (isNothing . optRepository $ opts) $ error $ "REPOSITORY required!" ++ help
    when (isNothing . optBasedir $ opts)    $ error $ "BASEDIR required!" ++ help
    when (isNothing . optReviewers $ opts)  $ error $ "REVIEWERS required!" ++ help
    where
        help = " Try mkrr --help"

