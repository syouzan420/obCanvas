module Init where

import JSDOM (currentDocumentUnchecked)
import JSDOM.Types (JSString,CanvasRenderingContext2D(..)
                   ,unRenderingContext,HTMLImageElement(..)
                   ,HTMLCanvasElement(..))
import JSDOM.Generated.NonElementParentNode (getElementByIdUnchecked)
import JSDOM.Generated.HTMLCanvasElement (getContextUnchecked) 
import JSDOM.Generated.Element(unElement) 

import qualified Data.Text as T

import Linear.V2(V2(..))

import Reflex.Dom.Core
  ( DomBuilder, Prerender, prerender_, blank) 

import Define
import App (appMain)

newGame :: Game
newGame = Game {_cnt=0, _chrs=[Ch 0 "player" False South (V2 0 0)]}

chImgNum :: Int
chImgNum = 8

setUpCanvas :: 
  ( DomBuilder t m
  , Prerender t m
  ) => m ()
setUpCanvas = prerender_ blank $ do
  doc <- currentDocumentUnchecked
  canvas <- getElementByIdUnchecked doc ("canvas" :: T.Text)
  let canvasElement = HTMLCanvasElement (unElement canvas)
  c <- getContextUnchecked canvasElement ("2d"::String) ([] :: [JSString])
  let cx = CanvasRenderingContext2D (unRenderingContext c)
  imgs <- mapM (\nm -> getElementByIdUnchecked doc ("ch"<>show nm)) 
                                                        [(0::Int)..(chImgNum-1)] 
  let ies = map (HTMLImageElement . unElement) imgs
  appMain cx ies newGame

