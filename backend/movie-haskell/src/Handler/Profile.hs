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
import           Text.Read
import Data.List as List


----------------- GENERATING FORMS ------

-- This datatype serves no other purpose than wrapping a search-query
-- in the searchForm
data SearchQuery = SearchQuery
          {query :: Text}
           deriving Show

-- Applicative Search Form
searchForm :: AForm Handler SearchQuery
searchForm = SearchQuery
            <$> areq textField (bfs ("Search for a Movie" :: Text))  Nothing




----------- HTTP HANDLERS ----------------------
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
    --someMovie <- runDB $ selectFirst [MovieTmdbId ==. "1234"] []
    --let Just(justSomeMovie) = someMovie
    --back <- runDB $  insert $ Movie_User usid (entityKey justSomeMovie)
    -- TMDB-Ids for this user's recommendation are now stored here
    movieRelationEntities <-  runDB $ selectList [Movie_UserUserId ==. usid] []

    -- END TEST CODE --

    -- List Comprehension to extract the IDs from the collected Movie_Entites
    let reccMovieIds = [movie_UserMovieId x | Entity someId x <- movieRelationEntities]

    -- Fetch the Movie Entities for the extracted IDs
    reccMovies <- runDB $ selectList [MovieId <-. reccMovieIds ] []

    -- Match the Database Movie Objects up with TMDB Objects to present
    -- further information (TODO)
    -- read $ Data.Text.unpack fortyTwo :: Int

    --mapM :: Monad m => (a -> m b) -> t a -> m (t b)
    test <- liftIO $ return $ mapM toTMDBMovie reccMovies


    --let tmdbMovies1 = [liftIO ( toTMDBMovie currentEnt) :: TMDB.Movie | Entity _ currentEnt <-  reccMovies]





    -- Load TMDB Config to construct Image-URLS:
    eitherConfig <- liftIO $ runTheMovieDB key config
    let Right(theconfig) = eitherConfig


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
      _ -> redirect $ HomeR



------------- HELPER FUNCTIONS ------------------

-- Kompiliert:
toTMDBMovie :: Entity Import.Movie -> IO TMDB.Movie
toTMDBMovie x = do
          let (Entity _ currentEnt) = x
          let intID =  read (unpack (movieTmdbId currentEnt)) :: Int
          movie <- runTheMovieDB key (fetchMovie intID)
          let Right bla = movie
          return bla


--mapM :: Monad m => (a -> m b) -> t a -> m (t b)




--- Couldn't match expected type `TMDB.Movie' with actual type `IO ()'
--toTMDBMovie :: Import.Movie -> IO()
--toTMDBMovie currentEnt = do
--          let intID =  read (unpack (movieTmdbId currentEnt)) :: Int
--          movie <- runTheMovieDB key (fetchMovie intID)
--          let Right bla = movie
--          bla

--toTMDBMovie currentEnt =
--          let
--                intID =  read (unpack (movieTmdbId currentEnt)) :: Int
--                movie = liftIO $ runTheMovieDB key (fetchMovie intID)
--          in movie



-- | Search for movies with a query string.
searchAndListMovies :: Text -> TheMovieDB [TMDB.Movie]
searchAndListMovies query = do
  movies <- searchMovies query
  return movies


  --getRecommendations :: UserId ->IO ()
  --getRecommendations usid = do
  --    movieIds <- liftIO $ runDB $ selectList [Movie_UserUserId ==. usid] []
  --    Import.print "Test"
