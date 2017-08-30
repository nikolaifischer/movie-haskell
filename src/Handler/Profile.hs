{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Profile where

import           Import
import           Network.API.TheMovieDB  as TMDB
import           Yesod.Form.Bootstrap3
import           Text.Read
import Data.List as List
import Data.Map as Maps


----------------- GENERATING FORMS ------

-- This datatype serves no other purpose than wrapping a search-query
-- in the searchForm
newtype SearchQuery = SearchQuery{query :: Text}
                     deriving Show

-- Applicative Search Form
searchForm :: AForm Handler SearchQuery
searchForm = SearchQuery
            <$> areq textField (bfs ("Recommend a Movie to a Friend" :: Text))  Nothing


----------- HTTP HANDLERS ----------------------
getProfileR :: Handler Html
getProfileR = do

    -- GET RECOMMONDATIONS FROM DB
    maid <- maybeAuthId
    let Just usid = maid

    -- TMDB-Ids for this user's recommendation are now stored here
    movieRelationEntities <-  runDB $ selectList [Movie_UserUserId ==. usid] []

    -- List Comprehension to extract the IDs from the collected Movie_Entites
    let reccMovieIds = [movie_UserMovieId x | Entity _ x <- movieRelationEntities]

    -- Fetch the Movie Entities for the extracted IDs
    moviesFromDB <- runDB $ selectList [MovieId <-. reccMovieIds, MovieWatched ==. False ] []

    -- Fetch TMDB Objects corresponding to the Movie Entities
    recommended_movies_with_duplicates <- liftIO $ mapM toTMDBMovie moviesFromDB


    let recommended_movies = removeDuplicateMovies recommended_movies_with_duplicates

    let tupelList = [ (i,j) |  i <- recommended_movies_with_duplicates , Entity _ j <- moviesFromDB,  pack (show (movieID i)) == movieTmdbId j] :: [ (TMDB.Movie , Import.Movie)]

    let movieDict = Maps.fromList tupelList -- :: Map TMDB.Movie Import.Movie


    -- Load TMDB Config to construct Image-URLS:
    eitherConfig <- liftIO $ runTheMovieDB key config
    let Right theconfig = eitherConfig


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
    ((res, _), _) <- runFormPost $ renderBootstrap3 BootstrapBasicForm searchForm
    case res of
      FormSuccess theQuery -> redirect $ ResultR (query theQuery)
      _ -> redirect HomeR



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


removeDuplicateMovies :: [TMDB.Movie] -> [TMDB.Movie]
removeDuplicateMovies = remover []
    where remover seen [] = seen
          remover seen (x:xs)
              | x `List.elem` seen = remover seen xs
              | otherwise = remover (seen List.++ [x]) xs


-- This instance is needed to construct dictionaries for the movie objects
instance Ord TMDB.Movie where
  movie1 `compare` movie2 =  movieID movie1 `compare` movieID movie2
