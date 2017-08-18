{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Result where

import Import
import Yesod.Auth.GoogleEmail2
import Network.API.TheMovieDB as TMDB
import Text.Printf (printf)
import Data.List as List

getResultR :: Text -> Handler Html
getResultR query = do

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
    --let Just (test) = personDisplayName justPerson
    --let bs = searchAndListMovies (pack "Star Wars")

    -- GET RECOMMONDATIONS FROM DB
    maid <- maybeAuthId
    let Just(usid) = maid
    --- TEST CODE: Relation zwischen Movie und aktuellem User --
    --someMovie <- runDB $ selectFirst [MovieTmdbId ==. "1234"] []
    --let Just(justSomeMovie) = someMovie
    --back <- runDB $  Import.insert $ Movie_User usid (entityKey justSomeMovie)
    -- END TEST CODE --
    movieIds <-  runDB $ selectList [Movie_UserUserId ==. usid] []
    print "The Movies"
    print movieIds

    -- Load TMDB Config to construct Image-URLS:
    eitherConfig <- liftIO $ runTheMovieDB key config
    let Right(theconfig) = eitherConfig

    result <- liftIO $ runTheMovieDB key $ searchAndListMovies (query)

    let list = case result of
                Left err -> []
                Right array -> array


    defaultLayout $ do
        setTitle "Results"
        $(widgetFile "result")



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
