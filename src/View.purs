module View 
 
where

import VirtualDOM.VTree
import VirtualDOM
import qualified Data.Array as A
import Data.Maybe.Unsafe
import Control.Monad.Eff
import DOM
import Data.Function
import qualified Signal as S
import qualified Signal.Channel as S
import Data.Maybe
import Data.Foldable

import Optic.Lens


foreign import windowOnLoad """
  function windowOnLoad(callback) {
    return function() {
      window.onload = function() {
        callback();
      };
    };
  }
""" :: forall e e2. Eff e Unit -> Eff (dom :: DOM | e2) Unit

foreign import bodyAppend """
  function bodyAppend(node) {
    return function() {
      document.body.appendChild(node);;
    };
  }
""" :: forall e. Node -> Eff (dom :: DOM | e) Unit


foreign import foldpEP """
  function foldpEP (constant, upd, seed, sig) {
    return function () {
      var acc = seed;
      var out = constant(acc);
      sig.subscribe(function(val) {
        acc = upd(val)(acc)();
        out.set(acc);
      });
      return out;
    };
  }""" :: forall a b c e. Fn4 (c -> S.Signal c) (a -> b -> Eff e b) b (S.Signal a) (Eff e (S.Signal b))

foldpE = runFn4 foldpEP S.constant

viewRender :: forall e. VTree -> S.Signal VTree -> Eff (dom::DOM | e) Unit
viewRender init svt = do
  bodyAppend n
  void $ foldpE updateDOM {n:n,t:init} svt
  where
    n = createElement init
    updateDOM t' {n:n,t:t} = do
      n' <- patch (diff t t') n
      return {n:n',t:t'}

foreign import data Callback :: *

foreign import callback """
  var callback = function(cfn) {
    return function(a) {
      console.log('running cb');
      cfn(a)();
    };
  };""" :: forall a e. (a -> Eff (|e) Unit) -> Callback


class Display a where
  display :: a -> VTree

instance vtreeDisplay :: Display VTree where
  display = id

p s = vnode "p" {} [vtext s]


newtype PuzzlerViewSpec = PuzzlerViewSpec
  { id :: String
  , title :: String
  , board :: GridViewSpec
  , pieces :: ComponentsContainerViewSpec GridViewSpec
  , instructions :: ComponentsContainerViewSpec VTree
  --, buttons :: ComponentsContainerViewSpec
  }

_PuzzlerViewSpec = lens (\(PuzzlerViewSpec a) -> a) (\_ n -> PuzzlerViewSpec n)
_id = lens (\o -> o.id) (\o x -> o{id = x})
title = lens (\o -> o.title) (\o x -> o{title = x})
board = lens (\o -> o.board) (\o x -> o{board = x})
pieces = lens (\o -> o.pieces) (\o x -> o{pieces = x})

puzzlerInit :: VTree
puzzlerInit = vtext "Loading..."

puzzlerView :: PuzzlerViewSpec -> VTree
puzzlerView (PuzzlerViewSpec spec) =
  vnode "div" {id:spec.id} 
    [ vnode "div" {attributes:{"class":"header"}, id:spec.id ++ "-title"} 
      [ vtext spec.title ]
    , gridView spec.board
    , componentsContainerView spec.pieces
    , componentsContainerView spec.instructions
    --, componentsContainerView spec.buttons
    ]

instance puzzlerDisplay :: Display PuzzlerViewSpec where
  display = puzzlerView




newtype GridViewSpec = GridViewSpec
  { id :: String
  , className :: Maybe String
  , gridSize :: { r :: Number, c :: Number }
  , click :: Callback
  , squareClass :: Number -> Number -> Maybe String
  , squareFill :: Number -> Number -> Maybe String
  , enterSquare :: Number -> Number -> Callback
  , exitSquare :: Number -> Number -> Callback
  , clickSquare :: Number -> Number -> Callback
  , dblClickSquare :: Number -> Number -> Callback
  }

_GridViewSpec = lens (\(GridViewSpec a) -> a) (\_ n -> GridViewSpec n)
className = lens (\o -> o.className) (\o x -> o{className = x})
gridSize = lens (\o -> o.gridSize) (\o x -> o{gridSize = x})
click = lens (\o -> o.click) (\o x -> o{click = x})
squareClass = lens (\o -> o.squareClass) (\o x -> o{squareClass = x})
squareFill = lens (\o -> o.squareFill) (\o x -> o{squareFill = x})
enterSquare = lens (\o -> o.enterSquare) (\o x -> o{enterSquare = x})
exitSquare = lens (\o -> o.exitSquare) (\o x -> o{exitSquare = x})
clickSquare = lens (\o -> o.clickSquare) (\o x -> o{clickSquare = x})
dblClickSquare = lens (\o -> o.dlbClickSquare) (\o x -> o{dblClickSquare = x})


svgn = "http://www.w3.org/2000/svg"

svgGrid :: Number -> Number -> (Number -> Number -> Number -> Maybe VTree) -> VTree
svgGrid nr nc cellFun = 
  vnode "svg" { namespace: svgn
              , attributes: { width: nc * s
                            , height:nr * s 
                            }
              } $ A.catMaybes <<< A.concat $ flip A.map (0 A... nr-1) mkRow
  where
    s = 20
    mkRow rNum = flip A.map (0 A... nc-1) $ cellFun s rNum

gridView :: GridViewSpec -> VTree
gridView (GridViewSpec spec) = vnode "div" 
  { id:spec.id
  , attributes:{"class": maybeToUndef $ spec.className}
  , onclick: spec.click
  } 
  [svgGrid spec.gridSize.r spec.gridSize.c boardCell]
  where
    square s r c clss = vnode "rect"
      { attributes:{ x:c*s
                   , y:r*s
                   , width:s
                   , height:s
                   , fill: maybeToUndef $ spec.squareFill r c
                   , "class": clss
                   }
      , namespace: svgn
      , onmouseenter: spec.enterSquare r c
      , onmouseleave: spec.exitSquare r c
      , onclick: spec.clickSquare r c
      , ondblclick: spec.dblClickSquare r c
      } []
    boardCell s r c = square s r c <$> spec.squareClass r c 

instance gridDisplay :: Display GridViewSpec where
  display = gridView

newtype ComponentsContainerViewSpec a = ComponentsContainerViewSpec
  { id :: String
  , title :: Maybe String
  , components :: [a]
  }

_ComponentsContainerViewSpec = lens (\(ComponentsContainerViewSpec a) -> a) (\_ n -> ComponentsContainerViewSpec n)
components = lens (\o -> o.components) (\o x -> o{components = x})

componentsContainerView :: forall a. (Display a) => ComponentsContainerViewSpec a -> VTree
componentsContainerView (ComponentsContainerViewSpec spec) = 
  let comps =  [vnode "div" {id:spec.id ++ "-components"} (A.map display spec.components)]
      children = maybe 
        comps 
        (\t -> (vnode "div" {attributes:{"class":"header"}} [vtext t]):comps)
        spec.title
  in vnode "div" {id:spec.id} children

foreign import data Undefined :: *
foreign import undef """
  var undef = undefined;
  """ :: Undefined

foreign import define """
  var define = function(a) {
      return a;
  }""" :: forall a. a -> Undefined


instance displayContainer :: (Display a) 
          => Display (ComponentsContainerViewSpec a) where
  display = componentsContainerView

maybeToUndef Nothing = undef
maybeToUndef (Just a) = define a

