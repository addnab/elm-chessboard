module Chess.Moves exposing (getBoardViewForNextMoves, isKingSafe, applyMove)

import Dict
import Chess.Types exposing (PlayerInfo, Move(..), Piece(..), PlayerPiece, Position, Square, Board, Player(..))
import Chess.Players exposing (opponent)
import Chess.Pieces exposing (toPlayerPiece)
import Chess.Board exposing (getPiece, updateSquare, movePiece)

import Debug

type alias Direction =
  { x : Int
  , y : Int
  }

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

isCapture : Move -> Bool
isCapture move =
  case move of
    Capture _ ->
      True
    Goto _ ->
      False
    CastleKingSide _ ->
      False
    CastleQueenSide _ ->
      False

pawnMoves : PlayerPiece -> Position -> Board -> List Move
pawnMoves playerPiece position board =
  let
    forwardMoves =
      if not playerPiece.moved then
        pieceMoves playerPiece position board (Just 2) [ n ]
      else
        pieceMoves playerPiece position board (Just 1) [ n ]
    captureMoves =
      pieceMoves playerPiece position board (Just 1) [ ne, nw ]
        |> List.filter isCapture
    normalMoves =
      forwardMoves
        |> List.filter (isCapture >> not)
  in
    normalMoves ++ captureMoves

getPieceMoves : Position -> Board -> PlayerPiece -> List Move
getPieceMoves position board playerPiece =
  let
    getMovesForPiece =
      case playerPiece.piece of
        K -> kingMoves
        Q -> queenMoves
        R -> rookMoves
        N -> knightMoves
        B -> bishopMoves
        P -> pawnMoves
  in
    getMovesForPiece playerPiece position board

getNextMoves : Player -> Square -> Position -> Board -> List Move
getNextMoves player square kingPosition board =
  case square.piece of
    Just playerPiece ->
      if playerPiece.player == player then
        getPieceMoves square.position board playerPiece
          |> List.filter (isKingSafe player playerPiece.piece square.position kingPosition board)
      else []
    Nothing ->
      []

movePosition : Move -> Position
movePosition move =
  case move of
    Capture pos ->
      pos
    Goto pos ->
      pos
    CastleKingSide pos ->
      pos
    CastleQueenSide pos ->
      pos

isKingInAttack : Position -> Board -> PlayerPiece -> Bool
isKingInAttack kingPosition board playerPiece =
  getPieceMoves kingPosition board playerPiece
    |> List.filter isCapture
    |> List.map movePosition
    |> List.map (\pos -> getPiece pos board)
    |> List.map (Maybe.map .piece)
    |> List.map (Maybe.map ((==) playerPiece.piece))
    |> List.any (Maybe.withDefault False)

isKingSafe : Player -> Piece -> Position -> Position -> Board -> Move -> Bool
isKingSafe player piece piecePosition currentKingPosition board nextMove =
  let
    kingPosition =
      case piece of
        K -> movePosition nextMove
        _ -> currentKingPosition
    board = movePiece piecePosition (movePosition nextMove) board
  in
    [ K, Q, R, N, B, P ]
      |> List.map (toPlayerPiece player)
      |> List.any (isKingInAttack kingPosition board)
      |> not

getBoardViewForNextMoves : Player -> PlayerInfo -> Square -> Board -> Board
getBoardViewForNextMoves player playerInfo square board =
  let
    nextMoves =
      getNextMoves player square playerInfo.kingPosition board
  in
    List.foldr
      ( \move newBoard ->
          updateSquare
            (\newSquare -> { newSquare | moveToPlay = Just move })
            (movePosition move)
            newBoard
      )
      board
      nextMoves

applyMove : Position -> Move -> PlayerInfo -> Board ->  { board: Board, capturedPiece: Maybe PlayerPiece, playerInfo: PlayerInfo }
applyMove fromPosition move playerInfo board =
  let
    (newBoard, capturedPiece) =
      case move of
        Goto toPosition ->
          ( movePiece fromPosition toPosition board
          , Nothing
          )
        Capture toPosition ->
          ( movePiece fromPosition toPosition board
          , getPiece toPosition board
          )
        CastleKingSide toPosition ->
          ( movePiece fromPosition toPosition board
          , Nothing
          )
        CastleQueenSide toPosition ->
          ( movePiece fromPosition toPosition board
          , Nothing
          )
    toPosition = movePosition move
    playerPieceMaybe = getPiece toPosition newBoard
    kingPosition =
      case playerPieceMaybe of
        Just playerPiece ->
          case playerPiece.piece of
            K -> toPosition
            _ -> playerInfo.kingPosition
        Nothing ->
          playerInfo.kingPosition
  in
    { board = newBoard
    , capturedPiece = capturedPiece
    , playerInfo =
        { playerInfo
        | kingPosition = kingPosition
        }
    }
