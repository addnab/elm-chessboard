module Chess.Board exposing (getPiece, movePiece, removePiece, setPiece, updateSquare)

import Chess.Types exposing (Board, Piece(..), Player(..), PlayerPiece, Position, Rank, Square)
import Debug
import Dict as Dict


updateSquare : (Square -> Square) -> Position -> Board -> Board
updateSquare squareModifier position =
    Dict.update position.rank <|
        Maybe.map <|
            Dict.update position.file <|
                Maybe.map <|
                    squareModifier


getPiece : Position -> Board -> Maybe PlayerPiece
getPiece position board =
    Dict.get position.rank board
        |> Maybe.andThen (Dict.get position.file)
        |> Maybe.andThen .piece


setPiece : Position -> ( Board, Maybe PlayerPiece ) -> Board
setPiece position ( board, piece ) =
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


removePiece : Position -> Board -> ( Board, Maybe PlayerPiece )
removePiece position board =
    let
        piece =
            getPiece position board

        newBoard =
            setPiece position ( board, Nothing )
    in
    ( newBoard, piece )


movePiece : Position -> Position -> Board -> Board
movePiece fromPosition toPosition board =
    board
        |> removePiece fromPosition
        >> setPiece toPosition
