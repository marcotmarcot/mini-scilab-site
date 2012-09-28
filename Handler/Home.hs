{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

-- base
import Control.Exception (evaluate)
import System.Timeout (timeout)
import Control.Monad (when)

-- time
import Data.Time (getCurrentTime, utcToZonedTime, TimeZone (TimeZone))

-- text
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- deepseq
import Control.DeepSeq (force)

-- scilab
import Scilab.Interpreter

-- mini-scilab-site
import Import
import Exercises

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing

getSubmitR :: Handler RepHtml
getSubmitR = defaultLayout $(widgetFile "submit")

postSubmitR :: Handler RepHtml
postSubmitR
  = do
    student <- runInputPost $ ireq textField "student"
    exercise <- T.unpack <$> runInputPost (ireq textField "exercise")
    code <- runInputPost $ ireq textField "code"
    result <- liftIO $ checkExercises (io $ read exercise) code
    when result $ liftIO $ success student exercise
    let
      message = if result then "Correto!" else "Erro!" :: Text
      -- doits = show doit
    defaultLayout $(widgetFile "posted")

checkExercises :: ExerciseIO -> Text -> IO Bool
checkExercises (rea_inp, exp_out) code
  = do
    result <- timeout 100000 $ evaluate $ force $ interpret rea_inp code
    return
      $ case result of
        Nothing -> False
        Just (lef_inp, rea_out) -> lef_inp == [] && rea_out == exp_out

success :: T.Text -> String -> IO ()
success student exercise
  = do
    time
      <- T.pack
        <$> show
        <$> utcToZonedTime (TimeZone (-180) False "BRT")
        <$> getCurrentTime
    T.appendFile ("/home/marcot/mini-scilab-site/" ++ exercise)
      $ time <> ": " <> student <> "\n"
