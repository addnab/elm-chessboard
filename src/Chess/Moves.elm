module Chess.Moves exposing (getBoardViewForNextMoves, isKingSafe, applyMove)

import Dict
import Task
import Chess.Types exposing (PlayerInfo, Move, MoveType(..), Piece(..), PlayerPiece, Position, Square, Board, Player(..))
import Chess.Players exposing (opponent)
import Chess.Pieces exposing (toPlayerPiece)
import Chess.Board exposing (getPiece, updateSquare, movePiece, setPiece, removePiece)
import Actions exposing (Action(..))

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
            [ Move Capture newPosition ]
        Nothing ->
          (Move Goto newPosition)
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

castleConditionEmptySquares : Bool -> Int -> Board -> Bool
castleConditionEmptySquares isKingSide rank board =
  let
    squaresInBetween =
      if isKingSide then
        [ Position 7 rank, Position 6 rank ]
      else
        [ Position 2 rank, Position 3 rank, Position 4 rank ]
    isPositionEmpty pos =
      case (getPiece pos board) of
        Just _
          -> False
        Nothing
          -> True
  in
    squaresInBetween
      |> List.map isPositionEmpty
      |> List.all identity

castleConditionRookNotMoved : Bool -> Int -> Board -> Bool
castleConditionRookNotMoved isKingSide rank board =
  let
    rookPosition =
      if isKingSide then
        Position 8 rank
      else
        Position 1 rank
    playerPieceMaybe = getPiece rookPosition board
  in
    case playerPieceMaybe of
      Just playerPiece ->
        case playerPiece.piece of
          R -> not playerPiece.moved
          _ -> False
      Nothing ->
        False

castleConditionKingCheck : Bool -> PlayerPiece -> Position -> Int -> Board -> Bool
castleConditionKingCheck isKingSide kingPiece kingPosition rank board =
  let
    kingPassThroughPositions =
      if isKingSide then
        [ Position 7 rank, Position 6 rank, Position 5 rank ]
      else
        [ Position 3 rank, Position 4 rank, Position 5 rank ]
  in
    kingPassThroughPositions
      |> List.map (\pos -> isKingSafe True kingPiece kingPosition kingPosition board (Move Goto pos))
      |> List.all identity

checkCastleCondition : Bool -> PlayerPiece -> Position -> Int -> Board -> Bool
checkCastleCondition isKingSide kingPiece kingPosition rank board =
  List.all identity
    [ castleConditionRookNotMoved isKingSide rank board
    , castleConditionEmptySquares isKingSide rank board
    , castleConditionKingCheck isKingSide kingPiece kingPosition rank board
    ]

getCastleMoves : PlayerPiece -> Position -> Board -> List Move
getCastleMoves kingPiece kingPosition board =
  if not kingPiece.moved then
    let
      rank =
        case kingPiece.player of
          White -> 1
          Black -> 8
      kingSideCastle =
        if (checkCastleCondition True kingPiece kingPosition rank board) then
          [ Move CastleKingSide (Position 7 rank) ]
        else []
      queenSideCastle =
        if (checkCastleCondition False kingPiece kingPosition rank board) then
          [ Move CastleQueenSide (Position 3 rank) ]
        else []
    in
      kingSideCastle ++ queenSideCastle
  else []

kingMoves : Bool -> PlayerPiece -> Position -> Board -> List Move
kingMoves isCastling playerPiece position board =
  let
    directions = [ n, s, e, w, ne, nw, se, sw ]
    normalMoves = pieceMoves playerPiece position board (Just 1) directions
    castleMoves =
      if isCastling then
        []
      else
        getCastleMoves playerPiece position board
  in
    normalMoves ++ castleMoves

isCapture : Move -> Bool
isCapture move =
  case .moveType move of
    Capture ->
      True
    _ ->
      False

pawnMoves : Maybe Position -> PlayerPiece -> Position -> Board -> List Move
pawnMoves enPassant playerPiece position board =
  let
    lastRank =
      case playerPiece.player of
        White -> 8
        Black -> 1
    toPawnJump move =
      let
        newMoveType =
          case .moveType move of
            Goto -> PawnJump
            m    -> m
      in
        Move newMoveType (.position move)
    captureMoves =
      pieceMoves playerPiece position board (Just 1) [ ne, nw ]
        |> List.filter isCapture
        |> List.map promoteAtLastRank
    forwardMove =
      pieceMoves playerPiece position board (Just 1) [ n ]
        |> List.filter (isCapture >> not)
        |> List.map promoteAtLastRank
    promoteAtLastRank move =
      let
        toPosition = .position move
      in
        if .rank toPosition == lastRank then
          Move Promotion toPosition
        else
          move
    pawnJump =
      if not playerPiece.moved then
        forwardMove
          |> List.map .position
          |> List.concatMap (\pos -> pieceMoves playerPiece pos board (Just 1) [ n ])
          |> List.filter (isCapture >> not)
          |> List.map toPawnJump
        else
          []
    enpassantMove =
      case enPassant of
        Just toPosition ->
          [ Move Enpassant toPosition ]
        Nothing ->
          []
  in
    forwardMove ++ pawnJump ++ captureMoves ++ enpassantMove

getPieceMoves : Bool -> Position -> Board -> PlayerPiece -> List Move
getPieceMoves isCastling position board playerPiece =
  let
    getMovesForPiece =
      case playerPiece.piece of
        K ->
          kingMoves isCastling
        Q ->
          queenMoves
        R ->
          rookMoves
        N ->
          knightMoves
        B ->
          bishopMoves
        P { enPassant } ->
          pawnMoves enPassant
  in
    getMovesForPiece playerPiece position board

getNextMoves : Player -> Square -> Position -> Board -> List Move
getNextMoves player square kingPosition board =
  case square.piece of
    Just playerPiece ->
      if playerPiece.player == player then
        getPieceMoves False square.position board playerPiece
          |> List.filter (isKingSafe False playerPiece square.position kingPosition board)
      else []
    Nothing ->
      []

isKingInAttack : Bool -> Position -> Board -> PlayerPiece -> Bool
isKingInAttack isCastling kingPosition board playerPiece =
  getPieceMoves isCastling kingPosition board playerPiece
    |> List.filter isCapture
    |> List.map .position
    |> List.map (\pos -> getPiece pos board)
    |> List.map (Maybe.map .piece)
    |> List.map (Maybe.map ((==) playerPiece.piece))
    |> List.any (Maybe.withDefault False)

isKingSafe : Bool -> PlayerPiece -> Position -> Position -> Board -> Move -> Bool
isKingSafe isCastling playerPiece piecePosition currentKingPosition board nextMove =
  let
    kingPosition =
      case playerPiece.piece of
        K -> .position nextMove
        _ -> currentKingPosition
    newBoard = movePiece piecePosition (.position nextMove) board
  in
    [ K, Q, R, N, B, P { enPassant = Nothing } ]
      |> List.map (toPlayerPiece playerPiece.player)
      |> List.any (isKingInAttack isCastling kingPosition newBoard)
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
            (.position move)
            newBoard
      )
      board
      nextMoves

enableEnPassantPawn : Player -> Position -> (Position, Maybe PlayerPiece) -> (Position, Maybe PlayerPiece)
enableEnPassantPawn player capturePosition (atPosition, playerPieceMaybe) =
  let
    rankAdder =
      case player of
        White -> -1
        Black ->  1
    toPosition =
      Position
        (.file capturePosition)
        ((.rank capturePosition) + rankAdder)
  in
    case playerPieceMaybe of
      Just playerPiece ->
        if playerPiece.player == opponent player then
          case playerPiece.piece of
            P _ ->
              ( atPosition
              , Just
                  { playerPiece
                  | piece = P { enPassant = Just toPosition }
                  }
              )
            _ -> (atPosition, playerPieceMaybe)
        else
          (atPosition, playerPieceMaybe)
      Nothing ->
        (atPosition, Nothing)

enableEnPassantPawns : Player -> Position -> Board -> Board
enableEnPassantPawns player position board =
  [ (.file position) + 1, (.file position) - 1 ]
    |> List.filter ( \file -> 1 <= file && file <= 8 )
    |> List.map ( \file -> Position file (.rank position) )
    |> List.map ( \pos -> (pos, getPiece pos board) )
    |> List.map ( enableEnPassantPawn player position )
    |> List.foldr
        ( \(pos, playerPieceMaybe) board ->
            setPiece pos (board, playerPieceMaybe)
        )
        board

disableEnPassantPawn : Player -> (Position, Maybe PlayerPiece) -> Board -> Board
disableEnPassantPawn player (atPosition, playerPieceMaybe) board =
  case playerPieceMaybe of
    Just playerPiece ->
      if playerPiece.player == player then
        case playerPiece.piece of
          P _ ->
            setPiece
              atPosition
              ( board
              , Just
                  { playerPiece
                  | piece = P { enPassant = Nothing }
                  }
              )
          _ ->
            board
      else
        board
    Nothing ->
      board

disableEnPassantPawns : Player -> Board -> Board
disableEnPassantPawns player board =
  let
    ranks =
      case player of
        White -> [ 5, 6 ]
        Black -> [ 3, 4 ]
  in
    [1..8]
      |> List.map Position
      |> List.concatMap (\pos -> List.map pos ranks)
      |> List.map (\pos -> ( pos, getPiece pos board) )
      |> List.foldr (disableEnPassantPawn player) board

applyMove : Player -> Position -> Move -> PlayerInfo -> Board -> ({ board: Board, capturedPiece: Maybe PlayerPiece, playerInfo: PlayerInfo }, Cmd Action)
applyMove player fromPosition move playerInfo board =
  let
    rank =
      case player of
        White -> 1
        Black -> 8
    toPosition = .position move
    ((newBoard, capturedPiece), command) =
      case .moveType move of
        Goto ->
          ( ( movePiece fromPosition toPosition board
            , Nothing
            )
          , Cmd.none
          )
        Capture ->
          ( ( movePiece fromPosition toPosition board
            , getPiece toPosition board
            )
          , Cmd.none
          )
        PawnJump ->
          ( ( board
                |> movePiece fromPosition toPosition
                |> enableEnPassantPawns player toPosition
            , Nothing
            )
          , Cmd.none
          )
        CastleKingSide ->
          ( ( board
                |> movePiece fromPosition toPosition
                |> movePiece (Position 8 rank) (Position 6 rank)
            , Nothing
            )
          , Cmd.none
          )
        CastleQueenSide ->
          ( ( board
                |> movePiece fromPosition toPosition
                |> movePiece (Position 1 rank) (Position 4 rank)
            , Nothing
            )
          , Cmd.none
          )
        Enpassant ->
          let
            capturePosition = Position (.file toPosition) (.rank fromPosition)
          in
            ( board
                |> movePiece fromPosition toPosition
                |> removePiece capturePosition
            , Cmd.none
            )
        Promotion ->
          ( ( movePiece fromPosition toPosition board
            , getPiece toPosition board
            )
          , Task.perform
              (\_ -> Debug.crash "This failure cannot happen.")
              identity
              (Task.succeed (PickPromotionPiece toPosition))
          )
    updatedBoard = disableEnPassantPawns player newBoard
    playerPieceMaybe = getPiece toPosition updatedBoard
    kingPosition =
      case playerPieceMaybe of
        Just playerPiece ->
          case playerPiece.piece of
            K -> toPosition
            _ -> playerInfo.kingPosition
        Nothing ->
          playerInfo.kingPosition
  in
    ( { board = updatedBoard
      , capturedPiece = capturedPiece
      , playerInfo =
          { playerInfo
          | kingPosition = kingPosition
          }
      }
    , command
    )
