module Handler.Comment where

import Import

postCommentR :: Handler Value
postCommentR = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    comment <- (requireJsonBody :: Handler Comment)

    -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
    maybeCurrentUserId <- maybeAuthId
    let comment' = comment { commentUserId = maybeCurrentUserId}
    -- TEST CODE START
    let movie = Movie (pack "1234") False
    theid <- runDB $ insertEntity movie
    movies <- runDB $ selectList [MovieWatched ==. True] []
    liftIO $ Import.print movies
    -- TEST CODE STOP

    insertedComment <- runDB $ insertEntity comment'

    returnJson insertedComment
