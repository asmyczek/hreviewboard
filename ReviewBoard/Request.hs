-----------------------------------------------------------------------------
-- |
-- Module      :  Request.hs
--
-- Maintainer  :  adam.smyczek@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides util functions to build ReviewBoard requests.
-- The current implementation is maybe an overkill for a project of this size, 
-- but it provides some constants for URL path elements and definitely simplifies 
-- building API calls.
--
-- The request format is @method (path1 . path2 . path3) [form var]@
-- , for example the API GET call to
-- @\/api\/json\/reviewrequests\/5\/delete\/@ can be executed by calling
--
-- > apiGet (reviewrequests (Just 5) . delete) []
--
-- The supported methods are 'apiGet', 'apiPost', 'httpGet' and 'httpPost'.
-- Http methods may be used to perform direct requests to ReviewBoard web UI
-- that are not supported by the API. As an example see @rbpatch@ command
-- line tool in examples. Http methods, same as API methods return 'RBResponse'
-- object with a JSValue result of the form:
--
-- > { "head": [
-- >     { "name" : <header name>,
-- >       "value": <header value> }
-- >   ]
-- >   "body" : <content>
-- > }
--
-- The current approach to handle requests may change if I find a way
-- to automatically generate API calls from ReviewBoard code.
--
-----------------------------------------------------------------------------
module ReviewBoard.Request (
    -- * URL path elements
    new,
    delete,
    save,
    discard,
    publish,
    update_from_changenum,
    star,
    unstar,
    all,
    to,
    from,
    count,
    directly,
    repositories,
    reviewrequest,
    comments,
    replies,
    draft,
    diff,
    screenshot,
    reviewrequests,
    reviews,
    groups,
    group,
    users,
    user,
    set,
    file,
    line,
    repository,
    changenum,

    -- * Types
    RRField(..),

    -- * Request methods
    apiGet,
    apiPost,
    httpGet,
    httpPost,

    -- * Util types and functions
    UrlPath,
    mkup,
    mkpup

    ) where

import Prelude hiding (all)
import ReviewBoard.Browser
import ReviewBoard.Core
import Network.HTTP hiding (user)
import Control.Monad.Trans
import qualified Control.Monad.State as S
import Data.Maybe
import Network.URI

-- ---------------------------------------------------------------------------
-- URL path elements

-- Constant definitions (UrlPath -> UrlPath)
new                     = mkup "new"
delete                  = mkup "delete"
save                    = mkup "save"
discard                 = mkup "discard"
publish                 = mkup "publish"
update_from_changenum   = mkup "repositories"

star                    = mkup "star"
unstar                  = mkup "unstar"

all                     = mkup "all"
to                      = mkup "to"
from                    = mkup "from"
count                   = mkup "count"
directly                = mkup "directly"

repositories            = mkup "repositories"
reviewrequest           = mkup "reviewrequest"
comments                = mkup "comments"
replies                 = mkup "replies"
draft                   = mkup "draft"
diff                    = mkup "diff"
screenshot              = mkup "screenshot"

-- Path elements with optional parameter
reviewrequests          = mkpup "reviewrequests" :: (Maybe Integer -> UrlPath -> UrlPath)
reviews                 = mkpup "reviews"        :: (Maybe Integer -> UrlPath -> UrlPath)
groups                  = mkpup "groups"         :: (Maybe String  -> UrlPath -> UrlPath)
group                   = mkpup "group"          :: (Maybe String  -> UrlPath -> UrlPath)
users                   = mkpup "users"          :: (Maybe String  -> UrlPath -> UrlPath)
user                    = mkpup "user"           :: (Maybe String  -> UrlPath -> UrlPath)
set                     = mkpup "set"            :: (Maybe RRField -> UrlPath -> UrlPath)
file                    = mkpup "file"           :: (Maybe Integer -> UrlPath -> UrlPath)
line                    = mkpup "line"           :: (Maybe Integer -> UrlPath -> UrlPath)

-- Other definitions
repository id   = mkup "repository" . mkup (show id)
changenum n     = mkup "changenum " . mkup (show n)

-- ---------------------------------------------------------------------------
-- Types

-- | Review request field type.
--
data RRField 
    = STATUS
    | PUBLIC
    | SUMMARY
    | DESCRIPTION
    | TESTING_DONE
    | BUGS_CLOSED
    | BRANCH
    | TARGET_GROUPS
    | TARGET_PEOPLE
    deriving (Eq, Enum, Bounded)

-- | Request field to name map.
--
rrFieldMap :: [(RRField, String)]
rrFieldMap =
    [ (STATUS,          "status")
    , (PUBLIC,          "public")
    , (SUMMARY,         "summary")
    , (DESCRIPTION,     "description")
    , (TESTING_DONE,    "testing_done")
    , (BUGS_CLOSED,     "bugs_closed")
    , (BRANCH,          "branch")
    , (TARGET_GROUPS,   "target_groups")
    , (TARGET_PEOPLE,   "target_people") ]

instance Show RRField where
    show = fromJust . flip lookup rrFieldMap

-- ---------------------------------------------------------------------------
-- Request functions

-- | API GET request method
--
apiGet :: (UrlPath -> UrlPath) -> [FormVar] -> RBAction RBResponse
apiGet u vs = mkApiURI (u "") >>= rbRequest API GET vs

-- | API POST request method
--
apiPost :: (UrlPath -> UrlPath) -> [FormVar] -> RBAction RBResponse
apiPost u vs = mkApiURI (u "") >>= rbRequest API POST vs

-- | Fall back to default http request for the case an action is not supported 
-- by the ReviewBoard WebAPI (HTTP GET)
--
httpGet :: String -> [FormVar] -> RBAction RBResponse
httpGet u vs = mkHttpURI u >>= rbRequest HTTP GET vs

-- | Same as 'httpGet' for HTTP POST requests
--
httpPost :: String -> [FormVar] -> RBAction RBResponse
httpPost u vs = mkHttpURI u >>= rbRequest HTTP POST vs

-- | Internal generalized request runner
--
rbRequest :: RBRequestType -> RequestMethod -> [FormVar] -> URI -> RBAction RBResponse
rbRequest rt rm vs u = do
    let form = Form rm u vs
    runRequest rt form return

-- ---------------------------------------------------------------------------
-- Util types and functions

-- | Synonym for URL path element
--
type UrlPath = String

-- | (MaKe UrlPath) Default URL element path maker
--
mkup :: String -> (UrlPath -> UrlPath)
mkup s = ((s ++ "/") ++)

-- | Make path element with a parameter of type a e.g.
-- reviewrequests (Just 5) => \"reviewrequests\/5\/\"
--
mkpup :: Show a => String -> (Maybe a -> UrlPath -> UrlPath)
mkpup s = \p u -> maybe (noparam u) (flip param u) p
    where
       noparam = mkup s
       param i = mkup s . mkup (show i)

