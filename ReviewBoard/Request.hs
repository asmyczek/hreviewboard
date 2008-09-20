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
    user,
    users,
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
import ReviewBoard.Core
import Control.Monad.Trans
import qualified Control.Monad.State as S
import Data.Maybe

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
users                   = mkup "users"

-- Path elements with optional parameter
reviewrequests          = mkpup "reviewrequests" :: (Maybe Integer -> UrlPath -> UrlPath)
reviews                 = mkpup "reviews"        :: (Maybe Integer -> UrlPath -> UrlPath)
groups                  = mkpup "groups"         :: (Maybe String  -> UrlPath -> UrlPath)
group                   = mkpup "group"          :: (Maybe String  -> UrlPath -> UrlPath)
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
apiGet p fs = rbRequest API GET (p "") fs

-- | API POST request method
--
apiPost :: (UrlPath -> UrlPath) -> [FormVar] -> RBAction RBResponse
apiPost p fs = rbRequest API POST (p "") fs

-- | Fall back to default http request for the case an action is not supported 
-- by the ReviewBoard WebAPI (HTTP GET)
--
httpGet :: String -> [FormVar] -> RBAction RBResponse
httpGet = rbRequest HTTP GET

-- | Same as 'httpGet' for HTTP POST requests
--
httpPost :: String -> [FormVar] -> RBAction RBResponse
httpPost = rbRequest HTTP POST

-- | Internal generalized request runner
--
rbRequest :: RBRequestType -> RBRequestMethod -> String -> [FormVar] -> RBAction RBResponse
rbRequest rt rm u vs = do
    let form = Form rt rm u vs
    runRequest form defaultResponseHandler

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
mkpup :: Show a => String -> (Maybe a -> String -> String)
mkpup s = \p u -> maybe (noparam u) (flip param u) p
    where
       noparam = mkup s
       param i = mkup s . mkup (show i)

-- ---------------------------------------------------------------------------
-- ReviewBoard API call list:
--
-- accounts/login/$
-- accounts/logout/$
-- repositories/$
-- repositories/(?P<repository_id>[0-9]+)/info/$
-- users/$
-- groups/$
-- groups/(?P<group_name>[A-Za-z0-9_-]+)/star/$
-- groups/(?P<group_name>[A-Za-z0-9_-]+)/unstar/$
-- reviewrequests/all/$
-- reviewrequests/all/count/$
-- reviewrequests/to/group/(?P<group_name>[A-Za-z0-9_-]+)/$
-- reviewrequests/to/group/(?P<group_name>[A-Za-z0-9_-]+)/count/$
-- reviewrequests/to/user/(?P<username>[A-Za-z0-9_-]+)/$
-- reviewrequests/to/user/(?P<username>[A-Za-z0-9_-]+)/count/$
-- reviewrequests/to/user/(?P<username>[A-Za-z0-9_-]+)/directly/$
-- reviewrequests/to/user/(?P<username>[A-Za-z0-9_-]+)/directly/count/$
-- reviewrequests/from/user/(?P<username>[A-Za-z0-9_-]+)/$
-- reviewrequests/from/user/(?P<username>[A-Za-z0-9_-]+)/count/$
-- reviewrequests/new/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/$
-- reviewrequests/repository/(?P<repository_id>[0-9]+)/changenum/(?P<changenum>[0-9]+)/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/star/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/unstar/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/delete/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/update_from_changenum/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/draft/save/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/draft/discard/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/draft/set/(?P<field_name>[A-Za-z0-9_-]+)/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/draft/set/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/reviews/draft/save/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/reviews/draft/publish/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/reviews/draft/delete/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/reviews/draft/comments/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/reviews/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/reviews/count/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/reviews/(?P<review_id>[0-9]+)/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/reviews/(?P<review_id>[0-9]+)/comments/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/reviews/(?P<review_id>[0-9]+)/comments/count/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/reviews/(?P<review_id>[0-9]+)/replies/draft/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/reviews/(?P<review_id>[0-9]+)/replies/draft/save/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/reviews/(?P<review_id>[0-9]+)/replies/draft/discard/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/reviews/(?P<review_id>[0-9]+)/replies/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/reviews/(?P<review_id>[0-9]+)/replies/count/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/diff/new/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/screenshot/new/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/diff/(?P<diff_revision>[0-9]+)/file/(?P<filediff_id>[0-9]+)/line/(?P<line>[0-9]+)/comments/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/diff/(?P<diff_revision>[0-9]+)-(?P<interdiff_revision>[0-9]+)/file/(?P<filediff_id>[0-9]+)-(?P<interfilediff_id>[0-9]+)/line/(?P<line>[0-9]+)/comments/$
-- reviewrequests/(?P<review_request_id>[0-9]+)/s/(?P<screenshot_id>[0-9]+)/comments/(?P<w>[0-9]+)x(?P<h>[0-9]+)\+(?P<x>[0-9]+)\+(?P<y>[0-9]+)/$

