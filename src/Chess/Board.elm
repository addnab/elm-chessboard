module Chess.Board exposing (Board, Rank, movePiece, getPiece, updateSquare)

import Dict as Dict
import Chess.Square exposing (Square, Highlight(..))
import Chess.Position exposing (Position)
import Chess.Players exposing (Player(..))
import Chess.Pieces exposing (Piece(..), PlayerPiece)

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

getPiece : Position -> Board -> Maybe PlayerPiece
getPiece position board =
  (Dict.get position.rank board)
    `Maybe.andThen` (Dict.get position.file)
      `Maybe.andThen` .piece

setPiece : Position -> (Board, Maybe PlayerPiece) -> Board
setPiece position (board, piece) =
  let
    movedPiece =
      Maybe.map
        (\playerPiece -> { playerPiece | moved = True })
        piece
  in
    updateSquare
      (\square -> { square | piece = movedPiece })
      position
      board

removePiece : Position -> Board -> (Board, Maybe PlayerPiece)
removePiece position board =
  let
    piece = getPiece position board
    newBoard = setPiece position (board, Nothing)
  in
    (newBoard, piece)

movePiece : Position -> Position -> Board -> { board: Board, capturedPiece: Maybe PlayerPiece }
movePiece fromPosition toPosition board =
  { board =
      board
        |> (removePiece fromPosition) >> (setPiece toPosition)
  , capturedPiece =
      getPiece toPosition board
  }
