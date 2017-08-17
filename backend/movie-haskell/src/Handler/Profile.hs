{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Profile where

import           Import
import           Network.API.TheMovieDB  as TMDB
import           Text.Printf             (printf)
import           Yesod.Auth.GoogleEmail2
import           Yesod.Form.Bootstrap3


-- This datatype serves no other purpose than wrapping a search-query
-- in the searchForm
data SearchQuery = SearchQuery
          {query :: Text}
           deriving Show

-- Applicative Search Form
searchForm :: AForm Handler SearchQuery
searchForm = SearchQuery
            <$> areq textField (bfs ("Search for a Movie" :: Text))  Nothing


getProfileR :: Handler Html
getProfileR = do
    token <- getUserAccessToken
    -- TODO: Fehlerbehandlung bei Nothing. Exception?
    -- GET USER NAME FROM TOKEN
    let Just (justToken) = token
    manager <- newManager
    person <- getPerson manager justToken
    let Just(justPerson) = person
    let userName = case (personDisplayName justPerson) of
                    Nothing   -> "User"
                    Just name -> name

    -- GET RECOMMONDATIONS FROM DB
    maid <- maybeAuthId
    let Just(usid) = maid
    --- TEST CODE: Relation zwischen Movie und aktuellem User --
    someMovie <- runDB $ selectFirst [MovieTmdbId ==. "1234"] []
    let Just(justSomeMovie) = someMovie
    back <- runDB $  insert $ Movie_User usid (entityKey justSomeMovie)
    -- TMDB-Ids for this user's recommendation are now stored here
    movieIds <-  runDB $ selectList [Movie_UserUserId ==. usid] []
    -- END TEST CODE --



    -- Render the Search Form
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm searchForm

    defaultLayout $ do
        setTitle "Profile"
        $(widgetFile "profile")


-- Post Handler for Search Form:
-- Inspects the query and redirects to the Results Page with the Query
-- as HTTP GET Parameter.
-- The real searching on TMDB is done by the result Handler. (see Result.hs)
postProfileR :: Handler Html
postProfileR = do
    ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm searchForm
    case res of
      FormSuccess theQuery -> do
              print theQuery
              redirect $ ResultR (query theQuery)
      _ -> error "Error while searching"


-- | Search for movies with a query string.
searchAndListMovies :: Text -> TheMovieDB [TMDB.Movie]
searchAndListMovies query = do
  movies <- searchMovies query
  return movies

  --liftIO $ mapM_ printMovieHeader movies

printMovieHeader :: TMDB.Movie -> IO ()
printMovieHeader m =
  --Import.print (movieTitle m)
  Import.print ("TEST")
  --printf "%8d: %s (%s)\n" (movieID m) (unpack $ movieTitle m) year
  --where year = case movieReleaseDate m of
                 --Just d  -> formatTime defaultTimeLocale "%Y" d
                 --Nothing -> "----"

--getRecommendations :: UserId ->IO ()
--getRecommendations usid = do
--    movieIds <- liftIO $ runDB $ selectList [Movie_UserUserId ==. usid] []
--    Import.print "Test"
