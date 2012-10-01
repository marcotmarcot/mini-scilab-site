{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

-- base
import qualified Control.Exception as E
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

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $(widgetFile "submit")

postHomeR :: Handler RepHtml
postHomeR
  = do
    student <- runInputPost $ ireq textField "student"
    exercise <- T.unpack <$> runInputPost (ireq textField "exercise")
    code <- runInputPost $ ireq textField "code"
    result <- liftIO $ checkExercises (io exercise) code
    when result $ liftIO $ success student exercise
    let message = if result then "Correto!" else "Erro!" :: Text
    defaultLayout $(widgetFile "posted")

checkExercises :: ExerciseIO -> Text -> IO Bool
checkExercises (rea_inp, exp_out) code
  = do
    result
      <- timeout 100000
          (E.evaluate
            $ force
            $ interpret rea_inp code)
        `E.catch` \e
          -> T.appendFile
              "/home/marcot/public_html/mini-scilab-site/log"
              (T.unlines [code, T.pack $ show (e :: E.SomeException)])
            >> return Nothing
    case result of
      Just (lef_inp, rea_out)
        -> if lef_inp == [] && rea_out == exp_out
           then return True
           else T.appendFile
              "/home/marcot/public_html/mini-scilab-site/log"
              (T.unlines [code, T.pack $ show (lef_inp, rea_out, exp_out)])
              >> return False
      _ -> return False

success :: T.Text -> String -> IO ()
success student exercise
  = do
    time
      <- T.pack
        <$> show
        <$> utcToZonedTime (TimeZone (-180) False "BRT")
        <$> getCurrentTime
    T.appendFile ("/home/marcot/public_html/mini-scilab-site/" ++ exercise)
      $ time <> ": " <> student <> "\n"
