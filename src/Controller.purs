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
controller chan gs =
  { id: "view"
  , title: case gs.victory of
      Nothing -> "Purescript puzzler!"
      Just true -> "You win!!!!!!"
      Just false -> "You looooose.... :'("
  , board: boardSpec gs.board Nothing
  --, pieces: piecesSpec
  --, instructions: instructionsSpec
  --, buttons: buttonsSpec
  }
  where
    boardSpec board mPiece = 
      { id: "board"
      , gridSize: { r: rows board, c: cols board }
      , click: callback $ const $ return unit
      , squareClass: boardSquare board (\_ _ _ -> "")
      , squareFill: boardFill board 
      , enterSquare: \_ _ -> callback $ const $ send chan id
      , exitSquare: \_ _ -> callback $ const $ send chan id
      , clickSquare: \_ _ -> callback $ const $ send chan id
      , dblClickSquare: \_ _ -> callback $ const $ send chan id
      }

boardSquare board modifier r c = 
  let square = status r c board # fromJust
      mod = modifier square r c
  in case square of
    Empty -> Just $ "empty" ++ mod
    Obstacle -> Just $ "obstacle" ++ mod
    (P id) -> Just $ "psquare" ++ mod

boardFill board r c = 
  case status r c board # fromJust of
    Empty -> Nothing
    Obstacle -> Nothing
    (P id) -> Just $ colorMap id

colorMap n =
  let colors = ["red", "blue", "green", "orange", "yellow", "magenta", "cyan", "gray"]
  in colors A.!! (n % A.length colors) # fromJust
