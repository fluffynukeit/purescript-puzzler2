module Controller where

import Model
import Model.Grid
import View
import Signal.Channel
import qualified Data.Array as A
import Data.Maybe
import Data.Maybe.Unsafe


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
      , squareFill: forSquare board squareFill
      , enterSquare: \_ _ -> callback $ const $ send chan id
      , exitSquare: \_ _ -> callback $ const $ send chan id
      , clickSquare: \_ _ -> callback $ const $ send chan id
      , dblClickSquare: \_ _ -> callback $ const $ send chan id
      }
    piecesAreaSpec pieces = 
      let pieceSpec piece = GridViewSpec
            { id: ""
            , className: Just "piece"
            , gridSize: { r: rows piece, c:cols piece }
            , click: callback $ const $ return unit
            , squareClass: forSquare piece (pieceSquareClass Nothing)
            , squareFill: forSquare piece squareFill
            , enterSquare: \_ _ -> callback $ const $ send chan id
            , exitSquare: \_ _ -> callback $ const $ send chan id
            , clickSquare: \_ _ -> callback $ const $ send chan id
            , dblClickSquare: \_ _ -> callback $ const $ send chan id
            }
      in ComponentsContainerViewSpec
          { id: "pieces-area"
          , title: Just $ "Pieces (" ++ show (A.length pieces) ++ ")"
          , components: A.map pieceSpec pieces
          }        

forSquare grid fn r c = status r c grid # fromJust # fn

boardSquareClass Empty = Just "empty"
boardSquareClass Obstacle = Just "obstacle"
boardSquareClass (P _) = Just "psquare"

    
squareFill Empty = Nothing
squareFill Obstacle = Nothing
squareFill (P id) = Just $ colorMap id

colorMap n =
  let colors = ["red", "blue", "green", "orange", "yellow", "magenta", "cyan", "gray"]
  in colors A.!! (n % A.length colors) # fromJust


pieceSquareClass mSel Empty = Nothing
pieceSquareClass mSel Obstacle = Nothing
pieceSquareClass mSel p@(P _) = 
  if mSel == Just p 
     then Just "psquare selected" 
     else Just "psquare"
