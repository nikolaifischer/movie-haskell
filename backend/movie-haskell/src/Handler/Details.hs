{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes           #-}
module Handler.Details where

import           Data.List               as List
import           Data.Time.Calendar      as Calendar
import           Import
import           Network.API.TheMovieDB  as TMDB
import           Yesod.Auth.GoogleEmail2
import           Yesod.Form.Bootstrap3



successMessage :: String
successMessage = "Your Recommendation was saved!"

----------------- GENERATING FORMS ------

-- This datatype serves no other purpose than wrapping a search-query
-- in the searchForm
data UserQuery = UserQuery
          {email :: Text}
           deriving Show

-- Applicative Form for Recommendations
recommendationForm :: AForm Handler UserQuery
recommendationForm = UserQuery
            <$> areq textField (bfs ("Insert E-Mail of another User" :: Text))  Nothing



getDetailsR :: Int -> Handler Html
getDetailsR theID = do


    maid <- maybeAuthId
    let Just(usid) = maid



    -- Load TMDB Config to construct Image-URLS:
    eitherConfig <- liftIO $ runTheMovieDB key config
    let Right(theconfig) = eitherConfig

    result <- liftIO $ runTheMovieDB key (fetchMovie theID)

    let themovie = case result of
                Left err        -> error "Movie not found in DB"
                Right something -> something



    mmsg <- getMessage


    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm recommendationForm

    defaultLayout $ do
        setTitle "Details"
        $(widgetFile "details")



postDetailsR :: ItemID -> Handler Html
postDetailsR tmdbident = do
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm recommendationForm
  case res of
    FormSuccess userQuery -> do

          -- Get the User to whom this movie should be recommended:
            let mail = email userQuery
            maybeUser <-  runDB $ selectFirst [UserIdent ==. mail] []
            let user =  case maybeUser of
                        Nothing -> error "TODO: User not found error"
                        Just (Entity theid therest) -> theid

          -- Get the logged in User

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

            themovieId <-  runDB $ Import.insert $ Import.Movie (pack (show tmdbident)) False userName

            let entity = Movie_User user themovieId
            entId <- runDB $ Import.insert entity

            setMessage $ toHtml $ successMessage
            redirect $ DetailsR tmdbident
    --_ -> defaultLayout $(widgetFile "details")
    _ -> redirect $ HomeR


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
            Nothing     -> "Not Released"
            Just (date) -> Calendar.showGregorian date
