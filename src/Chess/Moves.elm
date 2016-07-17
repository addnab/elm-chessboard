module Chess.Moves exposing (getBoardViewForNextMoves)

import Dict
import Chess.Square exposing (Square, highlightSquare)
import Chess.Position exposing (Position)
import Chess.Players exposing (Player(..), opponent)
import Chess.Pieces exposing (Piece(..), PlayerPiece)
import Chess.Board exposing (Board, getPiece, updateSquare)

type alias Direction =
  { x : Int
  , y : Int
  }

type Move = Capture Position | Goto Position

n  = Direction  0  1
s  = Direction  0 -1
w  = Direction  1  0
e  = Direction -1  0
nw = Direction  1  1
ne = Direction -1  1
sw = Direction  1 -1
se = Direction -1 -1

movesByDirection : PlayerPiece -> Board -> Position -> Maybe Int -> Direction -> List Move
movesByDirection playerPiece board position maxHopCount direction =
  let
    playerDirectionMultiplier =
      case playerPiece.player of
        White ->  1
        Black -> -1
    newPosition =
      { position
      | file = position.file + direction.x * playerDirectionMultiplier
      , rank = position.rank + direction.y * playerDirectionMultiplier
      }
    nextPlayerPieceMaybe = getPiece newPosition board
    validPosition =
      newPosition.file >= 1 && newPosition.file <= 8
        && newPosition.rank >= 1 && newPosition.rank <= 8
    hopCount = Maybe.withDefault 8 maxHopCount
  in
    if hopCount > 0 && validPosition then
      case nextPlayerPieceMaybe of
        Just nextPlayerPiece ->
          if nextPlayerPiece.player == playerPiece.player then
            []
          else
            [ Capture newPosition ]
        Nothing ->
          (Goto newPosition)
            :: movesByDirection playerPiece board newPosition (Just (hopCount - 1)) direction
    else
      []

pieceMoves : PlayerPiece -> Position -> Board -> Maybe Int -> List Direction -> List Move
pieceMoves playerPiece position board hopCountMaybe directions =
  List.foldr
    ( \direction positions ->
        (movesByDirection playerPiece board position hopCountMaybe direction) ++ positions
    )
  [] directions

rookMoves : PlayerPiece -> Position -> Board -> List Move
rookMoves playerPiece position board =
  let
    directions = [ n, s, e, w ]
  in
    pieceMoves playerPiece position board Nothing directions

knightMoves : PlayerPiece -> Position -> Board -> List Move
knightMoves playerPiece position board =
  let
    directions =
      [ Direction  1  2, Direction  2  1
      , Direction  1 -2, Direction  2 -1
      , Direction -1  2, Direction -2  1
      , Direction -1 -2, Direction -2 -1
      ]
  in
    pieceMoves playerPiece position board (Just 1) directions

bishopMoves : PlayerPiece -> Position -> Board -> List Move
bishopMoves playerPiece position board =
  let
    directions = [ ne, nw, se, sw ]
  in
    pieceMoves playerPiece position board Nothing directions

queenMoves : PlayerPiece -> Position -> Board -> List Move
queenMoves playerPiece position board =
  bishopMoves playerPiece position board
    ++ rookMoves playerPiece position board

kingMoves : PlayerPiece -> Position -> Board -> List Move
kingMoves playerPiece position board =
  let
    directions = [ n, s, e, w, ne, nw, se, sw ]
  in
    pieceMoves playerPiece position board (Just 1) directions

pawnMoves : PlayerPiece -> Position -> Board -> List Move
pawnMoves playerPiece position board =
  let
    forwardMoves =
      if not playerPiece.moved then
        pieceMoves playerPiece position board (Just 2) [ n ]
      else
        pieceMoves playerPiece position board (Just 1) [ n ]
    isCapture move =
      case move of
        Capture _
          -> True
        Goto _
          -> False
    captureMoves =
      pieceMoves playerPiece position board (Just 1) [ ne, nw ]
        |> List.filter isCapture
    normalMoves =
      forwardMoves
        |> List.filter (isCapture >> not)
  in
    normalMoves ++ captureMoves

getNextMoves : Player -> Square -> Board -> List Move
getNextMoves player square board =
  case square.piece of
    Just playerPiece ->
      if playerPiece.player == player then
        let
          getPieceMoves =
            case playerPiece.piece of
              K -> kingMoves
              Q -> queenMoves
              R -> rookMoves
              N -> knightMoves
              B -> bishopMoves
              P -> pawnMoves
        in
          getPieceMoves playerPiece square.position board
      else []
    Nothing ->
      []

toPosition : Move -> Position
toPosition move =
  case move of
    Capture pos ->
      pos
    Goto pos ->
      pos

getBoardViewForNextMoves : Player -> Square -> Board -> Board
getBoardViewForNextMoves player square board =
  let
    nextMoves =
      getNextMoves player square board
        |> List.map toPosition
  in
    nextMoves
      |> List.foldr (updateSquare (highlightSquare player)) board
