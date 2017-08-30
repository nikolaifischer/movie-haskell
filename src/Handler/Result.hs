{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Result where

import Import
import Network.API.TheMovieDB as TMDB
import Data.List as List

getResultR :: Text -> Handler Html
getResultR query = do

    -- GET RECOMMONDATIONS FROM DB
    -- Load TMDB Config to construct Image-URLS:
    eitherConfig <- liftIO $ runTheMovieDB key config
    let Right theconfig = eitherConfig

    result <- liftIO $ runTheMovieDB key $ searchMovies query

    let list = case result of
                Left _ -> []
                Right smth -> smth

    defaultLayout $ do
        setTitle "Results"
        $(widgetFile "result")
