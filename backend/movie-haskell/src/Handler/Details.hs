{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Details where

import Import
import Yesod.Auth.GoogleEmail2
import Network.API.TheMovieDB as TMDB
import Text.Printf (printf)
import Data.List as List
import Data.Time.Calendar as Calendar

getDetailsR :: Int -> Handler Html
getDetailsR movieID = do

    token <- getUserAccessToken
    -- TODO: Fehlerbehandlung bei Nothing. Exception?
    -- GET USER NAME FROM TOKEN
    let Just (justToken) = token
    manager <- newManager
    person <- getPerson manager justToken
    let Just(justPerson) = person
    let userName = case (personDisplayName justPerson) of
                    Nothing -> "User"
                    Just name -> name
    maid <- maybeAuthId
    let Just(usid) = maid



    -- Load TMDB Config to construct Image-URLS:
    eitherConfig <- liftIO $ runTheMovieDB key config
    let Right(theconfig) = eitherConfig

    result <- liftIO $ runTheMovieDB key (fetchMovie movieID)

    let themovie = case result of
                Left err -> error "Movie not found in DB"
                Right something -> something


    defaultLayout $ do
        setTitle "Details"
        $(widgetFile "details")



-- | Search for movies with a query string.
searchAndListMovies :: Text -> TheMovieDB [TMDB.Movie]
searchAndListMovies query = do
  movies <- searchMovies query
  return movies

  --liftIO $ mapM_ printMovieHeader movies


-- Small Helper function to return the release date of a movie as a String
-- or return "Not Released" if it's not yet or was never released
findRelease :: TMDB.Movie -> String
findRelease movie = thedate where
  thedate = case movieReleaseDate movie of
            Nothing -> "Not Released"
            Just (date) -> Calendar.showGregorian date


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
