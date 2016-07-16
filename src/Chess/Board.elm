module Chess.Board exposing (Board, Rank, getBoardForNextMoves)

import Dict as Dict
import Chess.Square exposing (Square)
import Chess.Position exposing (Position)
import Chess.Players exposing (Player(..))
import Chess.Pieces exposing (Piece(..), PlayerPiece)

type alias Board = Dict.Dict Int Rank

type alias Rank = Dict.Dict Int Square

getPawnNextMoves : Player -> Position -> List Position
getPawnNextMoves player position =
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


getNextMoves : Square -> List Position
getNextMoves square =
  case square.piece of
    Just playerPiece ->
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
          getPawnNextMoves playerPiece.player square.position
    Nothing ->
      []

highlightSquare : Player -> Maybe Square -> Maybe Square
highlightSquare player oldSquare =
  case oldSquare of
    Just square ->
      let
        highlightColor =
          case square.piece of
            Nothing ->
              Just "grey"
            Just playerPiece ->
              if playerPiece.player == player then
                Nothing
              else
                Just "red"
      in
        Just { square | highlightColor = highlightColor }
    Nothing ->
      Nothing

highlightRank : Player -> Int -> Maybe Rank -> Maybe Rank
highlightRank player rankNumber oldRank =
  case oldRank of
    Just rank ->
      rank
        |> Dict.update rankNumber (highlightSquare player)
        |> Just
    Nothing ->
      Nothing

highlightBoardView : Player -> Position -> Board -> Board
highlightBoardView player position board =
  let
    {file, rank} = position
  in
    board
      |> Dict.update rank (highlightRank player file)

getBoardViewForNextMoves : Player -> Square -> Board -> Board
getBoardViewForNextMoves player square board =
  let
    nextMoves = getNextMoves square
  in
    nextMoves
      |> List.foldr (highlightBoardView player) board
