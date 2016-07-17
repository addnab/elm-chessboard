module Chess.Board exposing (Board, Rank, getBoardViewForNextMoves, movePiece)

import Dict as Dict
import Maybe exposing (andThen, map)
import Chess.Square exposing (Square, MoveState(..))
import Chess.Position exposing (Position)
import Chess.Players exposing (Player(..))
import Chess.Pieces exposing (Piece(..), PlayerPiece)
import Chess.Moves exposing (getNextMovePositions)

import Debug

type alias Board = Dict.Dict Int Rank

type alias Rank = Dict.Dict Int Square

updateSquare : (Square -> Square) -> Position -> Board -> Board
updateSquare squareModifier position =
  Dict.update position.rank
    <| Maybe.map
      <| Dict.update position.file
        <| Maybe.map
          <| squareModifier

highlightSquare : Player -> Square -> Square
highlightSquare player square =
  let
    moveState =
      case square.piece of
        Nothing ->
          Movable
        Just playerPiece ->
          if playerPiece.player == player then
            None
          else
            Capturable
  in
    { square | moveState = moveState }

getBoardViewForNextMoves : Player -> Square -> Board -> Board
getBoardViewForNextMoves player square board =
  let
    nextMoves = getNextMovePositions player square
  in
    nextMoves
      |> List.foldr (updateSquare (highlightSquare player)) board

getPiece : Position -> Board -> Maybe PlayerPiece
getPiece position board =
  (Dict.get position.rank board)
    `andThen` (Dict.get position.file)
      `andThen` .piece

setPiece : Position -> (Board, Maybe PlayerPiece) -> Board
setPiece position (board, piece) =
  updateSquare
    (\square -> { square | piece = piece })
    position
    board

removePiece : Position -> Board -> (Board, Maybe PlayerPiece)
removePiece position board =
  let
    piece = getPiece position board
    newBoard = setPiece position (board, Nothing)
  in
    (newBoard, piece)

movePiece : Square -> Position -> Board -> { board: Board, capturedPiece: Maybe PlayerPiece }
movePiece fromSquare toPosition board =
  { board =
      board
        |> (removePiece fromSquare.position) >> (setPiece toPosition)
  , capturedPiece =
      getPiece toPosition board
  }
