module App where

import qualified JSDOM.Generated.CanvasRenderingContext2D as C 
import qualified JSDOM.Generated.HTMLImageElement as I
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Fix (MonadFix)

import qualified Data.Text as T

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Linear.V2 (V2(..))

import Reflex.Dom.Core 
  ( text, el, blank, leftmost, accumDyn, dynText  
  , tickLossyFromPostBuildTime, widgetHold_ , divClass
  , DomBuilder, Prerender, PerformEvent, TriggerEvent
  , PostBuild, MonadHold, Performable, Event
  )

import Define
import Canvas (drawToCanvas)
import CWidget (evElButton)

appMain :: 
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , MonadIO (Performable m)
  , PerformEvent t m
  , PostBuild t m
  , Prerender t m
  , TriggerEvent t m
  ) => C.CanvasRenderingContext2D -> [I.HTMLImageElement] -> Game -> m ()
appMain cx ies ga = mdo
  evTime <- tickLossyFromPostBuildTime 0.2 
  let evGTick = GTick <$ evTime
  evBtList <- evGButtons 
  let evG = leftmost (evBtList<>[evGTick])
  dyGa <- accumDyn updateGame ga evG
  let dyChrs = _chrs <$> dyGa
  widgetHold_ blank (drawToCanvas cx ies dyGa <$ evTime)
  divClass "status" $ dynText (T.pack . show <$> dyChrs)
  
evGButtons :: (DomBuilder t m) => m [Event t GEvent]
evGButtons = do
  _ <- el "div" $ text ""
  evGUp <- evElButton "pad3" "↑" <&> (<$) GUp
  _ <- el "div" $ text ""
  evGDir <- mapM (evElButton "pad") ["←","●","→"] <&> 
                                        zipWith (<$) [GLeft,GOk,GRight]
  _ <- el "div" $ text ""
  evGDown <- evElButton "pad3" "↓" <&> (<$) GDown
  evGSub <- evElButton "pad2" "□" <&> (<$) GSub
  return $ [evGUp,evGSub]<>evGDir<>[evGDown]


updateGame :: Game -> GEvent -> Game
updateGame gm ev = 
  let chrs = _chrs gm
      ply@(Ch _ _ pMov pDir pPos) = fromMaybe nullChara $ getChByName "player" chrs
   in case ev of
    GTick -> 
      let nply = if pMov then Ch 0 "player" pMov pDir (pPos + dirToDelta pDir) 
                     else ply
          nchrs = updateChByName "player" nply chrs
       in gm {_cnt=_cnt gm + 1, _chrs=nchrs}
    GOk -> gm
    GSub -> gm
    dirEv -> 
      let keyDir = (\d -> if d==NoDir then pDir else d) $ inpToDir dirEv
          isSameDir = pDir == keyDir
          nply = if isSameDir then if pMov then Ch 0 "player" False pDir pPos 
                                           else Ch 0 "player" True pDir pPos
                              else Ch 0 "player" False keyDir pPos
          nchrs = updateChByName "player" nply chrs
       in gm{_chrs=nchrs}

nullChara :: Chara
nullChara = Ch 0 T.empty False NoDir (V2 0 0)

inpToDir :: GEvent -> ChDir
inpToDir p = case p of
  GRight -> East; GUp -> North; GLeft -> West; GDown -> South; _ -> NoDir

dirToDelta :: ChDir -> Pos
dirToDelta dr = case dr of
  East -> V2 5 0; North -> V2 0 (-5); West -> V2 (-5) 0; South -> V2 0 5 
  NoDir -> V2 0 0

getChByName :: ChName -> [Chara] -> Maybe Chara 
getChByName _ [] = Nothing 
getChByName chnm (ch@(Ch _ nm _ _ _):xs) = 
  if chnm==nm then Just ch else getChByName chnm xs

updateChByName :: ChName -> Chara -> [Chara] -> [Chara]
updateChByName _ _ [] = []
updateChByName chnm chr (ch@(Ch _ nm _ _ _):xs) =
  if chnm==nm then chr:xs else ch:updateChByName chnm chr xs 
