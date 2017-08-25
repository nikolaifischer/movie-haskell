{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Profile where

import           Import
import           Network.API.TheMovieDB  as TMDB
import           Yesod.Auth.GoogleEmail2
import           Yesod.Form.Bootstrap3
import           Text.Read
import Data.List as List
import           System.IO.Unsafe
import Data.Map as Maps


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

    -- TMDB-Ids for this user's recommendation are now stored here
    movieRelationEntities <-  runDB $ selectList [Movie_UserUserId ==. usid] []

    -- List Comprehension to extract the IDs from the collected Movie_Entites
    let reccMovieIds = [movie_UserMovieId x | Entity someId x <- movieRelationEntities]

    -- Fetch the Movie Entities for the extracted IDs
    moviesFromDB <- runDB $ selectList [MovieId <-. reccMovieIds, MovieWatched ==. False ] []

    -- Fetch TMDB Objects corresponding to the Movie Entities
    recommended_movies <- liftIO $ mapM toTMDBMovie moviesFromDB


    let tupelList = [ (i,j) |  i <- recommended_movies , Entity _ j <- moviesFromDB] :: [ (TMDB.Movie , Import.Movie)]
    let movieDict = Maps.fromList tupelList -- :: Map TMDB.Movie Import.Movie


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


-- Maps a Movie Entity from the local Database to a Movie Entity in the TMDB API Wrapper  :
toTMDBMovie :: Entity Import.Movie -> IO TMDB.Movie
toTMDBMovie x = do
          let (Entity _ currentEnt) = x
          let intID =  read (unpack (movieTmdbId currentEnt)) :: Int
          movie <- runTheMovieDB key (fetchMovie intID)
          let val = case movie of
                      Left _ -> error "Error while mapping the Movie"
                      Right smth -> smth
          --let Right val = movie
          return val


-- | Search for movies with a query string.
searchAndListMovies :: Text -> TheMovieDB [TMDB.Movie]
searchAndListMovies query = do
  movies <- searchMovies query
  return movies



instance Ord TMDB.Movie where
  movie1 `compare` movie2 = (movieID movie1) `compare` (movieID movie2)
