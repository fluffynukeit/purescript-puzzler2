module Controller where

import Model
import Model.Grid
import View
import Signal.Channel
import qualified Data.Array as A
import Data.Maybe
import Data.Maybe.Unsafe

import Optic.Core


controller :: forall a. Channel (PuzzlerViewSpec -> PuzzlerViewSpec) 
           -> GameState 
           -> PuzzlerViewSpec
controller chan gs = PuzzlerViewSpec
  { id: "view"
  , title: case gs.victory of
      Nothing -> "Purescript puzzler!"
      Just true -> "You win!!!!!!"
      Just false -> "You looooose.... :'("
  , board: boardSpec gs.board
  , pieces: piecesAreaSpec gs.pieces
  --, instructions: instructionsSpec
  --, buttons: buttonsSpec
  }
  where
    boardSpec board = GridViewSpec
      { id: "board"
      , className: Nothing
      , gridSize: { r: rows board, c: cols board }
      , click: callback $ const $ return unit
      , squareClass: forSquare board boardSquareClass
      , squareFill: forSquare board squareFillP
      , enterSquare: \_ _ -> callback $ const $ send chan id
      , exitSquare: \_ _ -> callback $ const $ send chan id
      , clickSquare: \_ _ -> callback $ const $ send chan id
      , dblClickSquare: \_ _ -> callback $ const $ send chan id
      }
    piecesAreaSpec ps = 
      let pieceSpec mSel p = GridViewSpec
            { id: ""
            , className: if (mSel == Just p) then Just "piece selected" else Just "piece"
            , gridSize: { r: rows p, c:cols p }

            , click: callback $ const $ send chan $ 
                _PuzzlerViewSpec..pieces.._ComponentsContainerViewSpec..components .~
                  A.map (pieceSpec (if isNothing mSel then Just p else Nothing)) ps

            , squareClass: forSquare p pieceSquareClass 
            , squareFill: forSquare p squareFillP
            , enterSquare: \_ _ -> callback $ const $ send chan id
            , exitSquare: \_ _ -> callback $ const $ send chan id
            , clickSquare: \_ _ -> callback $ const $ send chan id
            , dblClickSquare: \_ _ -> callback $ const $ send chan id
            }
      in ComponentsContainerViewSpec
          { id: "pieces-area"
          , title: Just $ "Pieces (" ++ show (A.length ps) ++ ")"
          , components: A.map (pieceSpec Nothing) ps
          }        

forSquare :: forall a. Grid Square -> (Square -> a) -> Number -> Number -> a
forSquare grid fn r c = status r c grid # fromJust # fn

boardSquareClass Empty = Just "empty"
boardSquareClass Obstacle = Just "obstacle"
boardSquareClass (P _) = Just "psquare"

    
squareFillP Empty = Nothing
squareFillP Obstacle = Nothing
squareFillP (P id) = Just $ colorMap id

colorMap n =
  let colors = ["red", "blue", "green", "orange", "yellow", "magenta", "cyan", "gray"]
  in colors A.!! (n % A.length colors) # fromJust


pieceSquareClass Empty = Nothing
pieceSquareClass Obstacle = Nothing
pieceSquareClass p@(P _) = Just "psquare"
