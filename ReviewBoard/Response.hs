-----------------------------------------------------------------------------
-- |
-- Module      :  Response.hs
--
-- Maintainer  :  adam.smyczek@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This is an optional module that provides additional functionality
-- to simplify handling of ReviewBoard responses.
--
-----------------------------------------------------------------------------

module ReviewBoard.Response (

    -- * JSON response utils
    js4name,
    js4path,
    js4spath,
    js2v,

    -- * Basic Response DSL
 
    -- | The DSL provides a function for most ReviewBoard JSObject members
    -- that directly returns the value of the member. The function name is equivalent
    -- to the name of the member element, for example values of a response:
    --
    -- > { "stat": "fail", 
    -- >   "err": {
    -- >     "msg": "You are not logged in", 
    -- >     "code": 103
    -- >   }
    -- > }
    -- 
    -- may be accessed as following:
    --
    -- > (msg . err) response
    -- > -- returns 'You are not logged in' :: String
    --
    -- > (code . err) response
    -- > -- returns 103 :: Integer
    -- 
    -- If the entry name represented by the function does not exist,
    -- an error is thrown.
    --
    -- The current function list is build by screen scraping ReviewBoard
    -- source code, so it's likely that some elements are missing. 
    -- The missing function can be added using 'mkrb' function. 
    -- Please drop me an email if you find one and I will include 
    -- this in the next version.
    --
    -- This approach for handling responses may change if I find a way
    -- to generate the DSL methods directly from ReviewBoard code.
    --
    mkrb,

    id,
    stat,
    err,
    msg,
    code,
    last_updated,
    summary,
    description,
    bugs_closed,
    branch,
    target_groups,
    target_people,
    public,
    name,
    timestamp,
    timesince,
    text,
    draft,
    username,
    first_name,
    last_name,
    fullname,
    email,
    repository,
    repositories,
    display_name,
    mailing_list,
    url,
    submitter,
    time_added,
    status,
    changenum,
    review_request,
    review_requests,
    testing_done,
    user,
    users,
    ship_it,
    body_top,
    body_bottom,
    comments,
    path,
    tool,
    filediff,
    interfilediff,
    first_line,
    last_line,
    num_lines,
    caption,
    title,
    image_url,
    screenshot,
    x,
    y,
    w,
    h,
    diffset,
    source_file,
    dest_file,
    source_revision,
    dest_detail,
    revision,
    head, 
    body

    ) where

import Prelude hiding (id, head)
import Text.JSON

-- ---------------------------------------------------------------------------
-- Some JSON helpers

-- | Get value for name from a JSObject or Nothing
-- if JSValue is not a JSObject
--
js4name :: String -> JSValue -> Maybe JSValue
js4name n (JSObject o) = findValue n (fromJSObject o)
js4name n _            = Nothing

-- | Get JSValue for name path, for example
-- for JSON object '{ \"obj1\" : { \"str\" : \"test\" } }'
-- js4path [\"obj1\", \"str\"] returns Just 'test'
--
js4path :: [String] -> JSValue -> Maybe JSValue
js4path []     v = Just v
js4path (x:xs) v = maybe Nothing (js4path xs) $ js4name x v

-- | Get JSValue for string path of the form
-- @reviewrequests.5.delete@. Dots inside a name
-- are not supported.
--
js4spath :: String -> JSValue -> Maybe JSValue
js4spath = js4path . split
    where
        split s | xs == []  = [x]
                | otherwise = x : split (tail xs)
                where (x, xs) = span (/='.') s

-- | Find value in object map
--
findValue :: String -> [(String, JSValue)] -> Maybe JSValue
findValue s []          = Nothing
findValue s ((n, v):xs) | s == n    = Just v
                        | otherwise = findValue s xs

-- | Extract value from JSValue or throw error
--
js2v :: (JSON a) => JSValue -> a
js2v v = case readJSON v of
    Ok a    -> a
    Error s -> error s

-- ---------------------------------------------------------------------------
-- Minimalistic DSL for parsing JSON response values

-- | Constructor for DSL functions
--
mkrb :: (JSON a) => String -> (JSValue -> a)
mkrb s = \v -> (js2v . fromJustWithError  s) (js4name s v)
    where
      fromJustWithError e Nothing  = error $ "Invalid response attribute " ++ e
      fromJustWithError _ (Just v) = v

-- TODO: the dsl should be generated from ReviewBoard source

-- Common attributes
id               = (mkrb "id")               :: JSValue -> Integer
last_updated     = (mkrb "last_updated")     :: JSValue -> String
summary          = (mkrb "summary")          :: JSValue -> String
description      = (mkrb "description")      :: JSValue -> String
bugs_closed      = (mkrb "bugs_closed")      :: JSValue -> JSValue
branch           = (mkrb "branch")           :: JSValue -> String
target_groups    = (mkrb "target_groups")    :: JSValue -> JSValue
target_people    = (mkrb "target_people")    :: JSValue -> JSValue
public           = (mkrb "public")           :: JSValue -> Bool
name             = (mkrb "name")             :: JSValue -> String
timestamp        = (mkrb "timestamp")        :: JSValue -> JSValue
timesince        = (mkrb "timesince")        :: JSValue -> String
text             = (mkrb "text")             :: JSValue -> String
draft            = (mkrb "draft")            :: JSValue -> JSValue
repository       = (mkrb "repository")       :: JSValue -> JSValue
repositories     = (mkrb "repositories")     :: JSValue -> [JSValue]

-- Attribute of status object
stat             = (mkrb "stat")             :: JSValue -> String
err              = (mkrb "err")              :: JSValue -> JSValue
msg              = (mkrb "msg")              :: JSValue -> String
code             = (mkrb "code")             :: JSValue -> String

-- Attributes of Group object
display_name     = (mkrb "display_name")     :: JSValue -> String
mailing_list     = (mkrb "mailing_list")     :: JSValue -> String
url              = (mkrb "url")              :: JSValue -> String

-- Attributes of User object
username         = (mkrb "username")         :: JSValue -> String
first_name       = (mkrb "first_name")       :: JSValue -> String
last_name        = (mkrb "last_name")        :: JSValue -> String
fullname         = (mkrb "fullname")         :: JSValue -> String
email            = (mkrb "email")            :: JSValue -> String

-- Attributes of ReviewRequest object
submitter        = (mkrb "submitter")        :: JSValue -> JSValue
time_added       = (mkrb "time_added")       :: JSValue -> String
status           = (mkrb "status")           :: JSValue -> String
changenum        = (mkrb "changenum")        :: JSValue -> Integer

-- Attributes of ReviewRequestDraft object
review_request   = (mkrb "review_request")   :: JSValue -> JSValue
review_requests  = (mkrb "review_requests")  :: JSValue -> [JSValue]
testing_done     = (mkrb "testing_done")     :: JSValue -> String

-- Attributes of Review
user             = (mkrb "user")             :: JSValue -> JSValue
users            = (mkrb "users")            :: JSValue -> [JSValue]
ship_it          = (mkrb "ship_it")          :: JSValue -> Bool
body_top         = (mkrb "body_top")         :: JSValue -> String
body_bottom      = (mkrb "body_bottom")      :: JSValue -> String
comments         = (mkrb "comments")         :: JSValue -> JSValue

-- Attributes of Repository object
path             = (mkrb "path")             :: JSValue -> String
tool             = (mkrb "tool")             :: JSValue -> String

-- Attributes of Comment object
filediff         = (mkrb "filediff")         :: JSValue -> JSValue
interfilediff    = (mkrb "interfilediff")    :: JSValue -> JSValue
first_line       = (mkrb "first_line")       :: JSValue -> Integer
last_line        = (mkrb "last_line")        :: JSValue -> Integer
num_lines        = (mkrb "num_lines")        :: JSValue -> Integer

-- Attributes of Comment object
caption          = (mkrb "caption")          :: JSValue -> String
title            = (mkrb "title")            :: JSValue -> String
image_url        = (mkrb "image_url")        :: JSValue -> String

-- Attributes of ScreenshotComment object
screenshot         = (mkrb "screenshot")     :: JSValue -> JSValue
x                  = (mkrb "x")              :: JSValue -> Integer
y                  = (mkrb "y")              :: JSValue -> Integer
w                  = (mkrb "w")              :: JSValue -> Integer
h                  = (mkrb "h")              :: JSValue -> Integer

-- Attributes of FileDiff object
diffset            = (mkrb "diffset")        :: JSValue -> JSValue
source_file        = (mkrb "source_file")    :: JSValue -> String
dest_file          = (mkrb "dest_file")      :: JSValue -> String
source_revision    = (mkrb "source_revision"):: JSValue -> String
dest_detail        = (mkrb "dest_detail")    :: JSValue -> String

-- Attributes of DiffSet object
revision           = (mkrb "revision")       :: JSValue -> Integer

-- Attributes of http response


head               = (mkrb "head")          :: JSValue -> [JSValue]
body               = (mkrb "body")          :: JSValue -> String

