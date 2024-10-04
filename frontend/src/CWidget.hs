module CWidget where

import JSDOM
import qualified JSDOM.Generated.NonElementParentNode as DOM
import qualified JSDOM.Generated.Element as DOM
import JSDOM.Generated.Storage (getItem, removeItem, setItem)
import JSDOM.Types (FromJSString, Storage, ToJSString, JSM, liftJSM)
import JSDOM.Generated.Window (getLocalStorage,getNavigator)
import JSDOM.Generated.Navigator (vibrate_)

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class (liftIO)
import System.Random (randomRIO)

import Reflex.Dom.Core 
  ( text, el, divClass, elAttr', blank, sample
  , (=:), leftmost, elDynAttr' , domEvent
  , current, prerender_, prerender 
  , DomBuilder
  , PostBuild, Event, EventName(Click), MonadHold ,Dynamic
  , Prerender
  )

mkHidden :: Bool -> Map.Map T.Text T.Text
mkHidden False = "hidden" =: ""
mkHidden True = mempty

elSpace :: DomBuilder t m => m ()
elSpace = el "p" $ text " "

evElButton :: DomBuilder t m => T.Text -> T.Text -> m (Event t ())
evElButton c s = do
  (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: c) $ text s
  return $ domEvent Click e

evElButtonH ::
  ( DomBuilder t m
  , PostBuild t m
  ) => Dynamic t Bool -> T.Text -> T.Text -> m (Event t ())
evElButtonH dyB c s = do
  (e, _) <- elDynAttr' "button" dyBHide $ text s
  return $ domEvent Click e
  where mkHidden' False = "hidden" =: "" <> otherAttr
        mkHidden' True = otherAttr
        dyBHide = mkHidden' <$> dyB
        otherAttr = "type" =: "button" <> "class" =: c

evElNumberPad :: DomBuilder t m => Int -> m (Event t T.Text)
evElNumberPad i = do
  divClass "gr" $ do
    evts <- mapM (\n -> (toText n <$) <$> evElNumberButton (toText n)) [1..i]
    return $ leftmost evts 
  where
    evElNumberButton = evElButton "pad" 
    toText = T.pack . show


elRandom :: (DomBuilder t m, MonadHold t m, Prerender t m) => m Int
elRandom = do
  dyRand <- prerender (return (0::Int)) $ liftIO $ randomRIO (0,4)
  sample (current dyRand)



elTextScroll :: (DomBuilder t m, Prerender t m) => m ()
elTextScroll = prerender_ blank $ do
  doc <- currentDocumentUnchecked
  scrollText <- DOM.getElementById doc ("wkText" :: String)
  case scrollText of
    Just scrT -> DOM.scrollBy scrT (-20) 0 
    Nothing -> return ()
  
elVibration :: (DomBuilder t m, Prerender t m) => m ()
elVibration = prerender_ blank $ do
  win <- currentWindowUnchecked
  nav <- getNavigator win
  vibrate_ nav 50
  
getLocalStorageUnchecked :: JSM Storage
getLocalStorageUnchecked = currentWindowUnchecked >>= getLocalStorage

save :: ToJSString a => T.Text -> a -> JSM ()
save key val = getLocalStorageUnchecked >>= \ls -> setItem ls key val

load :: FromJSString a => T.Text -> JSM (Maybe a)
load key = getLocalStorageUnchecked >>= flip getItem key

clear :: T.Text -> JSM ()
clear key = getLocalStorageUnchecked >>= flip removeItem key

saveState :: (DomBuilder t m, Prerender t m) => T.Text -> m ()
saveState sd = prerender_ blank $ liftJSM $ save "gameState" sd 

loadState :: (DomBuilder t m, Prerender t m) =>  m (Dynamic t (Maybe T.Text)) 
loadState  = prerender (return Nothing) $ liftJSM $ load "gameState" 
  
