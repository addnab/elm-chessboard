module Chess.Moves exposing (getNextMovePositions)

import Chess.Square exposing (Square, MoveState(..))
import Chess.Position exposing (Position)
import Chess.Players exposing (Player(..))
import Chess.Pieces exposing (Piece(..), PlayerPiece)

getPawnNextMovePositions : Player -> Position -> List Position
getPawnNextMovePositions player position =
  let
    (secondRank, adder) =
      case player of
        White -> (2, 1)
        Black -> (7, -1)

    nextMoves = [ { position | rank = position.rank + adder } ]
  in
    if position.rank == secondRank then
      { position | rank = position.rank + adder*2 } :: nextMoves
    else
      nextMoves

getNextMovePositions : Player -> Square -> List Position
getNextMovePositions player square =
  case square.piece of
    Just playerPiece ->
      if playerPiece.player == player then
        case playerPiece.piece of
          K ->
            []
          Q ->
            []
          R ->
            []
          N ->
            []
          B ->
            []
          P ->
            getPawnNextMovePositions playerPiece.player square.position
      else []
    Nothing ->
      []
