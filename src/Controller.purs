module Controller where

import Model
import Model.Grid
import View
import Signal.Channel
import qualified Data.Array as A
import Data.Maybe
import Data.Maybe.Unsafe

import Optic.Core
import Data.Lazy


controller :: forall a. Channel (Lazy (PuzzlerViewSpec -> PuzzlerViewSpec))
           -> GameState 
           -> PuzzlerViewSpec
controller chan gs = PuzzlerViewSpec
  { id: "view"
  , title: case gs.victory of
      Nothing -> "Purescript puzzler!"
      Just true -> "You win!!!!!!"
      Just false -> "You looooose.... :'("
  , board: boardSpec gs.board
  , pieces: piecesAreaSpec gs.pieces gs
  , instructions: instructionsSpec
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
      , enterSquare: \_ _ -> callback $ const $ send chan $ defer \_ -> id
      , exitSquare: \_ _ -> callback $ const $ send chan $ defer \_ -> id
      , clickSquare: \_ _ -> callback $ const $ send chan $ defer \_ -> id
      , dblClickSquare: \_ _ -> callback $ const $ send chan $ defer \_ -> id
          -- How to remove pieces here?  Old state might already be bound to 
          -- closures, so no way to update game state in the middle of an action,
      }
    piecesAreaSpec ps gs = 
      let b = gs.board
          pieceSpec mSel p = GridViewSpec
            { id: ""
            , className: if (mSel == Just p) then Just "piece selected" else Just "piece"
            , gridSize: { r: rows p, c:cols p }

            , click: callback $ const $ send chan $ defer \_ -> 
                let newSelection = case mSel of
                      Just sel | sel == p -> Nothing -- unselecting currently selected piece
                      _ -> Just p -- new selection

                in  if isNothing newSelection
                    -- if no piece is selected, return to base spec
                    then \_ -> controller chan gs
                    -- else, modify the behavior of the pieces area and board to
                    -- highlight the selected piece and enable drop preview
                    else
                    -- first change how the pieces are drawn so border is shown
                    (_PuzzlerViewSpec..pieces.._ComponentsContainerViewSpec..components .~
                      A.map (pieceSpec newSelection) ps
                    )
                    .. -- Now change hover behavior for board
                    (_PuzzlerViewSpec..board.._GridViewSpec..enterSquare .~ \r c -> callback $ const $ send chan $ defer \_ ->
                      let validityMod = maybe "invalid " (const "valid ") (place r c p b)
                      in _PuzzlerViewSpec..board.._GridViewSpec..squareClass .~ \r' c' -> 
                          if filled (r'-r) (c'-c) p
                            then (++) validityMod <$> forSquare b boardSquareClass r' c' -- valid/invalid coloring
                            else forSquare b boardSquareClass r' c' -- standard coloring
                    )      
                    .. -- Now change click behavior of board
                    (_PuzzlerViewSpec..board.._GridViewSpec..clickSquare .~ \r c -> callback $ const $ send chan $ defer \_ ->
                      \oldSpec -> case placeAt r c p gs of
                          Nothing -> oldSpec -- no behavior change
                          Just gs' -> controller chan gs' -- build a "new" game with one less piece
                    )
                      
            , squareClass: forSquare p pieceSquareClass 
            , squareFill: forSquare p squareFillP
            , enterSquare: \_ _ -> callback $ const $ send chan $ defer \_ -> id
            , exitSquare: \_ _ -> callback $ const $ send chan $ defer \_ -> id
            , clickSquare: \_ _ -> callback $ const $ send chan $ defer \_ -> id
            , dblClickSquare: \_ _ -> callback $ const $ send chan $ defer \_ -> id
            }
      in ComponentsContainerViewSpec
          { id: "pieces-area"
          , title: Just $ "Pieces (" ++ show (A.length ps) ++ ")"
          , components: A.map (pieceSpec Nothing) ps
          }        
    instructionsSpec = ComponentsContainerViewSpec
      { id: "instructions"
      , title: Nothing
      , components: [ p "Place the pieces on the board to solve the puzzle!"
                    , p "Click: Select/place piece."
                    , p "Double click: Remove piece from board."
                    , p "Hint: Place the selected block in correct location if available."
                    , p "Give up: Show a solution."
                    ]
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
