module View 
 ( puzzlerView
 , Callback()
 , callback
 , PuzzlerViewSpec()
 , GridViewSpec()
 , ComponentsContainerViewSpec()
 , viewRender
 , windowOnLoad
 , puzzlerInit
 , gridView
 )
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

type PuzzlerViewSpec = 
  { id :: String
  , title :: String
  , board :: GridViewSpec
  , pieces :: ComponentsContainerViewSpec
  --, instructions :: ComponentsContainerViewSpec
  --, buttons :: ComponentsContainerViewSpec
  }

foreign import data Callback :: *

foreign import callback """
  var callback = function(cfn) {
    return function(a) {
      cfn(a)();
    };
  };""" :: forall a e. (a -> Eff (|e) Unit) -> Callback


type GridViewSpec = 
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

type ComponentsContainerViewSpec = 
  { id :: String
  , title :: Maybe String
  , components :: [VTree]
  }


puzzlerView :: PuzzlerViewSpec -> VTree
puzzlerView spec =
  vnode "div" {id:spec.id} 
    [ vnode "div" {attributes:{"class":"header"}, id:spec.id ++ "-title"} 
      [ vtext spec.title ]
    , gridView spec.board
    , componentsContainerView spec.pieces
    --, componentsContainerView spec.instructions
    --, componentsContainerView spec.buttons
    ]

puzzlerInit :: VTree
puzzlerInit = vtext "Loading..."

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
gridView spec = vnode "div" 
  {id:spec.id, attributes:{"class": maybeToUndef $ spec.className}} 
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
      , onenter: spec.enterSquare
      , onexit: spec.exitSquare
      , onclick: spec.clickSquare 
      , ondblclick: spec.dblClickSquare 
      } []
    boardCell s r c = square s r c <$> spec.squareClass r c 

componentsContainerView :: ComponentsContainerViewSpec -> VTree
componentsContainerView spec = 
  let comps =  [vnode "div" {id:spec.id ++ "-components"} spec.components]
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

maybeToUndef Nothing = undef
maybeToUndef (Just a) = define a

