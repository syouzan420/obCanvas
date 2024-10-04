module Canvas where

import qualified JSDOM.Generated.HTMLImageElement as I
import qualified JSDOM.Generated.CanvasRenderingContext2D as C 

import qualified Data.Text as T
import qualified Data.Map as Map

import Linear.V2 (V2(..))

import Reflex.Dom.Core
  ( DomBuilder, Prerender , Dynamic
  , prerender_, blank, sample, current, (=:))

import Obelisk.Generated.Static (static)

import Define


drawToCanvas :: (DomBuilder t m, Prerender t m) => 
    C.CanvasRenderingContext2D -> [I.HTMLImageElement] -> Dynamic t Game -> m ()
drawToCanvas cx ies dg = prerender_ blank $ do
  gm <- (sample . current) dg 
  let cnt = _cnt gm 
  let chrs = _chrs gm
  let chdf = mod cnt 2
  C.clearRect cx 0 0 400 400
  mapM_ (\(Ch chn _ _ cdir (V2 cpx cpy)) -> 
     C.drawImage cx (ies!!(chn*8+fromEnum cdir*2+chdf))
                             (fromIntegral cpx) (fromIntegral cpy)) chrs 
  C.setFont cx ("bold 23px serif"::T.Text)
  C.setFillStyle cx ("cyan"::String)
  C.fillText cx ("キャラクターの"::T.Text) 0 60 (Just 100) 
  C.strokeText cx ("キャラクターの"::T.Text) 0 60 (Just 100) 
  C.fillText cx ("絵が"::T.Text) 30 80 (Just 80) 
  C.strokeText cx ("絵が"::T.Text) 30 80 (Just 80) 
  C.fillText cx ("更新される"::T.Text) 40 100 (Just 80) 
  C.strokeText cx ("更新される"::T.Text) 40 100 (Just 80) 
  C.fillText cx ((T.pack . show) cnt) 40 120 (Just 80) 


charaSrc :: [Map.Map T.Text T.Text]
charaSrc = ["src" =: $(static "CharaImage/ch0.gif")
           ,"src" =: $(static "CharaImage/ch1.gif")
           ,"src" =: $(static "CharaImage/ch2.gif")
           ,"src" =: $(static "CharaImage/ch3.gif")
           ,"src" =: $(static "CharaImage/ch4.gif")
           ,"src" =: $(static "CharaImage/ch5.gif")
           ,"src" =: $(static "CharaImage/ch6.gif")
           ,"src" =: $(static "CharaImage/ch7.gif")
           ]
