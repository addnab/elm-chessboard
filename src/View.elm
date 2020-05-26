module View exposing (view)

import Actions exposing (Action(..))
import Chess.Pieces exposing (getPieceDisplayInfo, toPlayerPiece)
import Chess.Types exposing (Board, Move, MoveType(..), Piece(..), Player, PlayerPiece, Position, Rank, Square)
import Dict
import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (height, src, style, width)
import Html.Events exposing (onClick)
import Model exposing (Model)


disablePiece : Player -> Maybe PlayerPiece -> Bool
disablePiece playerInTurn playerPieceMaybe =
    playerPieceMaybe
        |> Maybe.map (\playerPiece -> playerPiece.player /= playerInTurn)
        |> Maybe.withDefault False


renderPiece : Int -> Player -> Maybe PlayerPiece -> Html Action
renderPiece sideLength playerInTurn piece =
    let
        pieceDisplayInfo =
            getPieceDisplayInfo piece

        pieceOpacity =
            if disablePiece playerInTurn piece then
                style "opacity" "0.6"

            else
                style "opacity" "1"

        pieceStyle =
            pieceOpacity
                :: [ style "z-index" "1"
                   , style "position" "absolute"
                   , style "width" (String.fromInt sideLength ++ "rem")
                   , style "height" (String.fromInt sideLength ++ "rem")
                   ]
    in
    case pieceDisplayInfo of
        Just imageName ->
            img (pieceStyle ++ [ src ("assets/" ++ imageName) ])
                []

        Nothing ->
            div [] []


squareStyle sideLength color =
    [ style "backgroundColor" color
    , style "height" (String.fromInt sideLength ++ "rem")
    , style "width" (String.fromInt sideLength ++ "rem")
    ]


highlightStyle sideLength hightlightColor =
    [ style "backgroundColor" hightlightColor
    , style "opacity" "0.9"
    , style "position" "absolute"
    , style "width" (String.fromInt sideLength ++ "rem")
    , style "height" (String.fromInt sideLength ++ "rem")
    ]


squareSelectAction : Maybe Square -> Square -> Action
squareSelectAction fromSquareMaybe toSquare =
    case fromSquareMaybe of
        Nothing ->
            Select toSquare

        Just fromSquare ->
            case toSquare.moveToPlay of
                Just move ->
                    MovePiece fromSquare.position move

                Nothing ->
                    Select toSquare


renderSquare : Int -> Maybe Square -> Player -> Square -> Html Action
renderSquare sideLength selectedSquare playerInTurn square =
    let
        { file, rank } =
            square.position

        isSelected =
            case selectedSquare of
                Just selected ->
                    selected == square

                Nothing ->
                    False

        highlightColor =
            case square.moveToPlay of
                Just move ->
                    case .moveType move of
                        Goto ->
                            "grey"

                        PawnJump ->
                            "grey"

                        Capture ->
                            "red"

                        Enpassant ->
                            "red"

                        Promotion ->
                            "orange"

                        _ ->
                            "blue"

                Nothing ->
                    if isSelected then
                        "yellow"

                    else
                        ""

        baseColor =
            if modBy 2 (file + rank) /= 0 then
                "white"

            else
                "green"
    in
    div (squareStyle sideLength baseColor ++ [ onClick (squareSelectAction selectedSquare square) ])
        [ div (highlightStyle sideLength highlightColor) []
        , renderPiece sideLength playerInTurn square.piece
        ]


rankStyle sideLength =
    [ style "height" (String.fromInt sideLength ++ "rem")
    , style "display" "flex"
    ]


renderRank : Int -> Maybe Square -> Player -> Rank -> Html Action
renderRank sideLength selectedSquare playerInTurn rank =
    div (rankStyle sideLength) <|
        List.map (renderSquare sideLength selectedSquare playerInTurn) <|
            Dict.values rank


boardStyle sideLength =
    [ style "box-shadow" "0 0 2rem -0.2rem black"
    , style "width" (String.fromInt (sideLength * 8) ++ "rem")
    , style "height" (String.fromInt (sideLength * 8) ++ "rem")
    , style "position" "absolute"
    ]


renderBoard : Int -> Maybe Square -> Player -> Board -> Html Action
renderBoard sideLength selectedSquare playerInTurn board =
    div (boardStyle sideLength) <|
        List.map (renderRank sideLength selectedSquare playerInTurn) <|
            List.reverse <|
                Dict.values board


promotePiecePickerStyle sideLength =
    [ style "position" "absolute"
    , style "width" (String.fromInt (sideLength * 4) ++ "rem")
    , style "height" (String.fromInt sideLength ++ "rem")
    , style "backgroundColor" "white"
    , style "z-index" "2"
    , style "display" "flex"
    , style "border" "2px solid orange"
    ]


promotePieceStyle sideLength =
    [ style "width" (String.fromInt sideLength ++ "rem")
    , style "height" (String.fromInt sideLength ++ "rem")
    ]


renderPromotePiecePicker : Int -> Player -> Maybe Position -> Html Action
renderPromotePiecePicker sideLength playerInTurn promote =
    let
        renderPromotePiece playerPiece position =
            div (promotePieceStyle sideLength ++ [ onClick (PromotePawn playerPiece position) ])
                [ renderPiece sideLength playerInTurn (Just playerPiece)
                ]
    in
    case promote of
        Just position ->
            div (promotePiecePickerStyle sideLength)
                (List.map
                    (\piece -> renderPromotePiece (toPlayerPiece playerInTurn piece) position)
                    [ R, N, B, Q ]
                )

        Nothing ->
            div [] []


viewStyle sideLength =
    [ style "height" (String.fromInt (sideLength * 8) ++ "rem")
    , style "width" (String.fromInt (sideLength * 8) ++ "rem")
    , style "display" "flex"
    , style "align-items" "center"
    , style "justify-content" "center"
    ]


view : Model -> Html Action
view model =
    div (viewStyle 5)
        [ renderBoard 5 model.selectedSquare model.playerInTurn model.boardView
        , renderPromotePiecePicker 7 model.playerInTurn model.promote
        ]
