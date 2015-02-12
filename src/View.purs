module View 
 ( puzzlerView
 , viewRender
 , windowOnLoad
 , ViewEvent(..)
 , puzzlerInit
 )
where

import VirtualDOM.VTree
import VirtualDOM
import qualified Data.Array as A
import Model
import Model.Grid
import Data.Maybe.Unsafe
import Control.Monad.Eff
import DOM
import Data.Function
import qualified Signal as S
import qualified Signal.Channel as S
import qualified Signal.Time as S
import Data.Maybe
import Debug.Trace
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

data ViewEvent 
  = Init
  | PieceClicked Piece
  | SquareEntered Number Number
  | SquareExited Number Number
  | SquareClicked Number Number
  | SquareDblClicked Number Number Number
  | HintClicked
  | GiveUpClicked

type PuzzlerViewSpec = 
  { id :: String
  , title :: String
  , board :: GridViewSpec
  , pieces :: ComponentsContainerViewSpec
  , instructions :: ComponentsContainerViewSpec
  , buttons :: ComponentsContainerViewSpec
  }

foreign import data Event :: * 
foreign import data Callback :: *

foreign import callback """
  var callback = function(cfn) {
    return function(a) {
      cfn(a)();
    };
  };""" :: forall a e. (a -> Eff e Unit) -> Callback


type GridViewSpec = 
  { id :: Maybe String
  , click :: forall e. Event -> Eff e Unit
  , squareClass :: Number -> Number -> Maybe String
  , enterSquare :: forall e. Number -> Number -> Event -> Eff e Unit
  , exitSquare :: forall e. Number -> Number -> Event -> Eff e Unit 
  , clickSquare :: forall e. Number -> Number -> Event -> Eff e Unit 
  , dblClickSquare :: forall e. Number -> Number -> Event -> Eff e Unit
  }

type ComponentsContainerViewSpec = 
  { id :: Maybe String
  , title :: Maybe String
  , components :: [VTree]
  }


puzzlerView :: S.Channel ViewEvent -> GameState -> VTree
puzzlerView chan gs =
  vnode "div" {id:"view"} 
    [ vnode "div" {attributes:{"class":"header"}, id:"title"} 
      [ case gs.victory of
          Nothing -> vtext "Purescript Puzzler!"
          Just true -> vtext "You win!!!!!!"
          Just false -> vtext "You looooose.... :'("
      ]
    , viewBoard chan gs.dropTarget gs.board
    , viewPieces chan gs.selectedPiece gs.piecesLeft
    , viewInstructions
    , viewButtons chan gs.selectedPiece
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

viewBoard :: S.Channel ViewEvent -> DropCandidate -> Board -> VTree
viewBoard chan (DropCandidate target valid) b = vnode "div" {id:"board"} $ [svgGrid (rows b) (cols b) boardCell]
  where
    getCell r c = status r c b # fromJust
    inTarget r c = any (\{r:r',c:c'} -> r==r' && c==c') target
    mkAttr s r c clss = { x:c*s
                        , y:r*s
                        , width:s
                        , height:s
                        , "class":if inTarget r c then clss ++ if valid then " valid" else " invalid"
                                  else clss
                        }
    enterHook r c = callback $ const $ S.send chan $ SquareEntered r c
    exitHook r c = callback $ const $ S.send chan $ SquareExited r c
    clickHook r c = callback $ const $ S.send chan $ SquareClicked r c
    boardCell s r c = case getCell r c of
      Empty -> Just $ vnode "rect" { attributes: mkAttr s r c "empty"
                                   , namespace: svgn
                                   , onenter: enterHook r c
                                   , onexit: exitHook r c
                                   , onclick: clickHook r c
                                   } []
      Obstacle -> Just $ vnode "rect" { attributes: mkAttr s r c "obstacle"
                                      , namespace: svgn
                                      , onenter: enterHook r c
                                      , onexit: exitHook r c
                                      , onclick: clickHook r c
                                      } []
      (P id) -> Just $ vnode "rect" { attributes: { x:c*s
                                                  , y:r*s
                                                  , width:s
                                                  , height:s
                                                  , fill: colorMap id
                                                  , "class":"psquare"
                                                  }
                                    , onenter: enterHook r c
                                    , onexit: exitHook r c
                                    , onclick: clickHook r c
                                    , namespace: svgn
                                    , ondblclick: callback $ const $ S.send chan $ SquareDblClicked r c id
                                    } []

colorMap n = 
  let colors = ["red", "blue", "green", "orange", "yellow", "magenta", "cyan", "gray"]
  in colors A.!! (n % A.length colors) # fromJust

viewPieces :: S.Channel ViewEvent -> Maybe Piece -> [Piece] -> VTree
viewPieces chan mSel ps = 
  vnode "div" {id:"pieces-area"} 
    [ vnode "div" {attributes:{"class":"header"}} [vtext $ "Pieces (" ++ show (A.length ps) ++ ")"]
    , vnode "div" {id:"pieces"} pieces
    ]
  where
    getCell r c p = status r c p # fromJust
    pieceCell p s r c = case getCell r c p of
      Empty -> Nothing
      Obstacle -> Nothing
      (P id) -> Just $ vnode "rect" { attributes: { x:c*s
                                                  , y:r*s
                                                  , width:s
                                                  , height:s
                                                  , fill: colorMap id
                                                  , "class":"psquare"
                                                  }
                                    , namespace: svgn
                                    } []
    pieces = A.map (\p -> 
      vnode "div" 
        { attributes:{"class": if (Just p == mSel) then "piece selected" else "piece"}
        , onclick: callback $ const $ S.send chan $ PieceClicked p
        } [svgGrid (rows p) (cols p) $ pieceCell p] ) ps


viewInstructions = vnode "div" {id:"instructions"}
  [ vnode "p" {} [vtext "Place the pieces on the board to solve the puzzle!"]
  , vnode "p" {} [vtext "Click: Select/place piece."]  
  , vnode "p" {} [vtext "Double click: Remove piece from board."]
  , vnode "p" {} [vtext "Hint: Place the selected block in correct location if available."]
  , vnode "p" {} [vtext "Give up: Show a solution."]
  ]

viewButtons chan mSel = vnode "div" {id:"buttons"}
  [ vnode "button" { attributes: {disabled:if isNothing mSel then def else undef}
                   , onclick: callback $ const $ S.send chan HintClicked } [vtext "Hint"]
  , vnode "button" { onclick: callback $ const $ S.send chan GiveUpClicked } [vtext "Give up"]
  ]

foreign import data Undefined :: *
foreign import undef """
    var undef = undefined;
   """ :: Undefined
foreign import def """
    var def = "";
  """ :: Undefined



