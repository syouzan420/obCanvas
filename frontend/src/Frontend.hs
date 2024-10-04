module Frontend where

import Common.Route (FrontendRoute (..))
import qualified Data.Text as T
import Obelisk.Frontend (Frontend (..))
import Obelisk.Route (R)
import Obelisk.Generated.Static (static)

import Reflex.Dom.Core 
  ( text, el, elAttr, blank , (=:)
  , DomBuilder, Prerender )

import Init (setUpCanvas,chImgNum) 
import Canvas (charaSrc)

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = frontendHead 
  , _frontend_body = frontendBody 
  }

frontendHead :: DomBuilder t m => m ()
frontendHead = do
  el "title" $ text "Canvas"
  elAttr
    "meta"
    ( "name" =: "viewport"
        <> "contents" =: "width=device-width, initial-scale=1.0"
    )
    blank

  elAttr
    "link"
    ("href" =: $(static "main.css")
      <> "type" =: "text/css"
      <> "rel" =: "stylesheet")
    blank

frontendBody :: 
  ( DomBuilder t m
  , Prerender t m
  ) => m ()
frontendBody = do 
  elAttr "canvas" ("id" =: "canvas" <> "style" =: "background-color: #008080" 
                <> "width" =: "400" <> "height" =: "400") blank
  elAttr "div" ("style" =: "display:none;") $
    mapM_ (\nm -> elAttr "img" ("id"=:("ch"<>(T.pack . show) nm) <> "width"=:"32"
                  <> "height"=:"32" <> (charaSrc!!nm)) blank) 
                                            [(0::Int)..(chImgNum-1)]
  setUpCanvas

