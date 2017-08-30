{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes           #-}
module Handler.Details where

import           Data.List  as List
import           Data.Time.Calendar      as Calendar
import           Import
import           Network.API.TheMovieDB  as TMDB
import           Yesod.Auth.GoogleEmail2
import           Yesod.Form.Bootstrap3



successMessage :: String
successMessage = "Your Recommendation was saved!"

errorMessage :: Text
errorMessage = "The owner of this E-Mail Address does not have an account on 'Watch That!'. Why don't you invite him or her?"

----------------- GENERATING FORM ------

-- This datatype serves no other purpose than wrapping a reccomendation-query
-- in the recommendationForm
newtype UserQuery = UserQuery{email :: Text}
                   deriving Show

-- Applicative Form for Recommendations
recommendationForm ::  AForm Handler UserQuery
recommendationForm  = UserQuery
            <$> areq userField  (bfs ("Enter the E-Mail of a User" :: Text))  Nothing
          where
            -- Do some Input Validation: Is there an account for this E-Mail Address?
            userField = checkM checkUser textField
            checkUser :: Text -> Handler (Either Text Text)
            checkUser input = do
              maybeUser <- runDB $ selectFirst [UserIdent ==. input] []
              return $ case maybeUser of
                Nothing  -> Left ("This User does not have an Account on this platform" :: Text)
                (Just _ ) -> Right input

-----------------------

---------GET HANDLER--------------
getDetailsR :: Int -> Handler Html
getDetailsR theID = do

    maid <- maybeAuthId
    let Just usid = maid

    -- Load TMDB Config to construct Image-URLS:
    eitherConfig <- liftIO $ runTheMovieDB key config
    let Right theconfig = eitherConfig

    result <- liftIO $ runTheMovieDB key (fetchMovie theID)

    let themovie = case result of
                Left _        -> error "Movie not found in DB"
                Right something -> something

    mmsg <- getMessage

    -- Checks if the movie for this details page was
    -- recommended to the current User
    -- If it was, a "Mark as Watched" button is displayed
    --------------------------------------
    -- Identify all Movie Objects corresponding to the current TMDB ID
    movieEntities <- runDB $ selectList [ MovieTmdbId ==. pack (show theID), MovieWatched ==. False] []
    let movieIds = [someId | Entity someId _ <- movieEntities]
    let movieobjs = [obj | Entity _ obj <- movieEntities]

    -- Check if there is an Entry for the User ID and one of the Movie Objects
    -- in the Movie - User DB Table.
    hits <- runDB $ selectList [Movie_UserMovieId <-. movieIds, Movie_UserUserId ==. usid] []
    -- Set the Flag accordingly
    let isRecommended =  not (List.null hits)  -- means the same as  length hits > 0

    ----------------------------------------

    -- Render Form and Page
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm recommendationForm
    defaultLayout $ do
        setTitle "Details"
        $(widgetFile "details")


------ POST HANDLER -------
postDetailsR :: ItemID -> Handler Html
postDetailsR tmdbident = do
  ((res, _), _) <- runFormPost $ renderBootstrap3 BootstrapBasicForm recommendationForm
  watchedButtonPressed <- runInputPost $ iopt boolField "watchedFlag"
  case res of
    FormSuccess userQuery -> do
          -- Get the User to whom this movie should be recommended:
            let mail = email userQuery
            maybeUser <-  runDB $ selectFirst [UserIdent ==. mail] []
            let user =  case maybeUser of
                        Nothing -> error "Something went wrong with the Input Validation"
                        Just (Entity theid _) -> theid

          -- Get the logged in User

            token <- getUserAccessToken
            -- TODO: Fehlerbehandlung bei Nothing. Exception?
            -- GET USER NAME FROM TOKEN
            let Just justToken = token
            manager <- newManager
            person <- getPerson manager justToken
            let Just justPerson = person
            let userName = fromMaybe "User" (personDisplayName justPerson)

            -- Insert a new Movie Entity in the DB:
            themovieId <-  runDB $ Import.insert $ Import.Movie (pack (show tmdbident)) False userName

            let entity = Movie_User user themovieId
            _ <- runDB $ Import.insert entity

            setMessage $ toHtml successMessage

            redirect $ DetailsR tmdbident

  -- The User did not use the recommendationForm.
  -- Check if the "watched" Button was pressed
    _ ->  case watchedButtonPressed of
            Just True -> do
              -- Watched Button was pressed
              -- Set the watched flag in the DB:

              --  Identify the current user
              maid <- maybeAuthId
              let Just loggedInId = maid

              -- Get the movies belonging to this user
              movieIdsTmp <- runDB $ selectList [Movie_UserUserId ==. loggedInId] []
              let movieIds = [movie_UserMovieId x | Entity _ x <- movieIdsTmp]

              -- Identify the Movies corresponding to the ID of this detail page
              let tmdbidentText = pack $ show tmdbident
              -- Set the watched flag for all of these movies to "True"
              _ <- runDB $ updateWhere [MovieTmdbId ==. tmdbidentText, MovieId <-. movieIds] [MovieWatched =. True]

              redirect ProfileR

          -- Both Forms failed. The E-Mail which was input by the user is not in the DB.
            _ -> do
                setMessage $ toHtml errorMessage
                redirect $ DetailsR tmdbident


-- Small Helper function to return the release date of a movie as a String
-- or return "Not Released" if it's not yet or was never released
findRelease :: TMDB.Movie -> String
findRelease movie = thedate where
  thedate = case movieReleaseDate movie of
            Nothing     -> "Not Released"
            Just date -> Calendar.showGregorian date
