module View exposing (view)

import Html exposing (Html, div, span, text)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Actions exposing (Action(..))
import Dict
import Color exposing (Color)
import Chess.Square exposing (Square, MoveState(..))
import Chess.Board exposing (Board, Rank)
import Chess.Pieces exposing (PlayerPiece, getPieceDisplayInfo)

import Debug

pieceStyle color =
  let
    textShadow =
      if color == "white" then
        "#000 0px 0px 3px"
      else
        "none"
  in
    style
      [ ("font-size", "30px")
      , ("color", color)
      , ("text-shadow", textShadow)
      ]

renderPiece : Maybe PlayerPiece -> Html Action
renderPiece piece =
  let
    pieceDisplayInfo =
      getPieceDisplayInfo piece
  in
    div [ pieceStyle pieceDisplayInfo.color ]
      [ text pieceDisplayInfo.text ]

squareStyle sideLength color =
  style
    [ ("backgroundColor", color)
    , ("height", toString sideLength ++ "px")
    , ("width", toString sideLength ++ "px")
    , ("display", "flex")
    , ("align-items", "center")
    , ("justify-content", "center")
    ]

squareSelectAction : Maybe Square -> Square -> Action
squareSelectAction fromSquareMaybe toSquare =
  case fromSquareMaybe of
    Nothing ->
      Select toSquare
    Just fromSquare ->
      case toSquare.moveState of
        Movable ->
          Move fromSquare toSquare.position
        Capturable ->
          Deselect
        None ->
          Select toSquare

renderSquare : Int -> Maybe Square -> Square -> Html Action
renderSquare sideLength selectedSquare square =
  let
    { file, rank } = square.position
    isSelected =
      case selectedSquare of
        Just selected ->
          selected == square
        Nothing ->
          False
    color =
      case square.moveState of
        Movable ->
          "grey"
        Capturable ->
          "red"
        None ->
          if isSelected then
            "yellow"
          else if (file + rank) % 2 == 0 then
            "white"
          else
            "green"
  in
    div [ squareStyle sideLength color, onClick (squareSelectAction selectedSquare square) ]
      [ renderPiece square.piece ]

rankStyle sideLength =
  style
    [ ("height", toString sideLength ++ "px")
    , ("display", "flex")
    ]

renderRank : Int -> Maybe Square -> Rank -> Html Action
renderRank sideLength selectedSquare rank =
  div [ rankStyle sideLength ]
    <| List.map (renderSquare sideLength selectedSquare)
      <| Dict.values rank

renderBoard : Int -> Maybe Square -> Board -> Html Action
renderBoard sideLength selectedSquare board =
  div []
    <| List.map (renderRank sideLength selectedSquare)
      <| Dict.values board

view : Model -> Html Action
view model =
  renderBoard 50 model.selectedSquare model.boardView
