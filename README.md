database-prepare
================

A pair of haskell libraries to use SQLite or Postgresql to prepare and
introspect a query, treating a query as a foreign language to which a call is
being made, rather than as code to be generated. It has as its goals:

  1. to ensure that the queries are syntactically valid
  2. to ensure that the queries match the purported schema
  3. to allow any query written by any convenient process can be incorporated
     without change regardless of style
  4. to be totally compatible with legacy database schemas, regardless of
     naming convention. In particular, column names which are not valid 
     Haskell identifiers should be available, and it doesn't derive constraints
     on queries based on the shape of types, but rather it derives 
     constraints on types based on the shape of queries.
  5. to some degree, to provide convenient schema management tools

Points 1-3 are probably desirable by anyone. Point 4 can make it a bit wordy
and, so far, means it doesn't yet have a stable interface. Point 5 is only 
there to the extent that schema migration is necessary in order to compile
code like this.

database-prepare-sqlite
=======================

I wrote this because I wanted a better interface to sqlite than I was getting
from sqlite-simple. It started out as a way to get an experience like
data-fileembed for some typescript code I was writing. But if you know that
some code is sqlite code, it makes sense to verify it at compile-time rather
than waiting for runtime to error.

My perspective is that the SQLite types are correct even when they are
inconsistent, and therefore it is a matter for application code to convert them
to their own format. But the library is compatible with sqlite-simple whose
ToField and FromField can easily be used. 

Sqlite has several different syntaxes for query parameters:

  * ?123 anonymous numbered parameters
  * ? anonymous implicitly numbered parameters
  * :AAAA colon-named parameters
  * @AAAA at-named parameters
  * $AAAA dollars-named parameters (which can contain colons and parentheses)

Moreover, column names can be arbitrary. Therefore, the most convenient 
approach is not to assume any particular mapping between sqlite names and 
Haskell names and instead to require the string to be written out. The 
interface looks something this; for the full versions, see 
database-prepare-sqlite/examples.

```haskell
-- defines:
--    QueryAllParams :: Type
--    QueryAllResult :: Type
--    queryAll :: Connection -> QueryAllParams -> IO QueryAllResult
$(embedSqlite "test-data/schema" "test-data/query/basic-named.sql" "queryAll")

-- supplied by the author
data User = User {uId :: Int, name :: Text, email :: Text, created :: UTCTime}
  deriving (Show, Eq)

data QueryParams = QueryParams {qpEmail :: Maybe Text, qpId :: Maybe Int}
  deriving (Show, Eq)

-- helpers defined by the author
qp2params :: QueryParams -> QueryAllParams
qp2params QueryParams {..} = QueryAllParams (Label @"$email" .== toField qpEmail .+ Label @"$id" .== toField qpId .+ Data.Row.Records.empty)

result2User :: QueryAllResult -> Ok User
result2User (QueryAllResult result) = do
  uId <- fromField $ result .! #id
  name <- fromField $ result .! #name
  email <- fromField $ result .! #email
  created <- fromField $ result .! #created_at

  pure User {..}

-- convenient interface defined by the user
basicNamed :: Connection -> QueryParams -> IO (Ok [User])
basicNamed conn = fmap (mapM result2User) . queryAll conn . qp2params
```

Note that this uses the row-types package which is abandoned but works. For this reason,
the package is regarded as experimental.

database-prepare-postgresql
===========================

There are various other similar libraries in Haskell to
database-prepare-postgresql. I wrote this because I was frustrated with:

  1. beam, because it was entirely awkward, too abstract and too imposing
  2. postgresql-simple, because it doesn't genuinely support prepared 
     queries
  3. hasql, because it had an almost-sql template haskell interface
  4. everything, because m

and because I enjoyed using database-prepare-sqlite. Since then, another
hasql option has become available which is similar to this one.

Postgresql supports only numbered parameters. This makes it clearly less
convenient for an approach like this. On the plus side, it is more precisely
typed, so as long as your types/queries tend to change in a way that breaks the
mapping, you'll get the advantage of a type error.

database-prepare-postgresql requires you to set up a database server. The 
examples do this when each example is separately generated; in usage, I have
set one up with nix and I load the connection string from environmental
variables.

Relevant section of database-prepare-postgresql/example/BasicParams.hs and 
database-prepare-postgresql/example/MappedTypes.hs:

```haskell
data User = User
  { uId :: Int32
  , uName :: Text
  , uEmail :: Text
  , uCreatedAt :: UTCTime
  } deriving (Show, GHC.Generics.Generic)

data UserSearch = UserSearch
  { usId :: Int32
  , usEmail :: Text
  } deriving (Show, GHC.Generics.Generic)

$(deriveMappingType ''User)
$(deriveMappingType ''UserSearch)

userResultMap :: UserFieldNames
userResultMap = UserFieldNames
  { uId = "id"
  , uName = "name"
  , uEmail = "email"
  , uCreatedAt = "created_at"
  }

userSearchParamMap :: UserSearchFieldNames
userSearchParamMap = UserSearchFieldNames
  { usId = "$1"
  , usEmail = "$2"
  }

searchUsers :: Connection -> UserSearch -> IO (Either PostgresError [User])
searchUsers = $(do
    conn <- establishConnection
    embedPostgres conn "test-data/query/basic-named.sql"
      (Just (''UserSearch, fieldPairs userSearchParamMap))
      (Just (''User, fieldPairs userResultMap))
  )
```
