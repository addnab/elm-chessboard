module View exposing (view)

import Html exposing (Html, div, span, text)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Actions exposing (Action(..))
import Array as Array
import Color exposing (Color)
import Chess.Square exposing (Square)
import Chess.Board exposing (Board)
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
      if isSelected then
        "yellow"
      else if (file + rank) % 2 == 0 then
        "white"
      else
        "green"
  in
    div [ squareStyle sideLength color, onClick (Select square) ]
      [ renderPiece square.piece ]

rankStyle sideLength =
  style
    [ ("height", toString sideLength ++ "px")
    , ("display", "flex")
    ]

renderRank : Int -> Maybe Square -> Array.Array Square -> Html Action
renderRank sideLength selectedSquare rank =
  rank
    |> Array.map (renderSquare sideLength selectedSquare)
    |> Array.toList
    |> div [ rankStyle sideLength ]

renderBoard : Int -> Maybe Square -> Board -> Html Action
renderBoard sideLength selectedSquare board =
  board
    |> Array.map (renderRank sideLength selectedSquare)
    |> Array.toList
    |> div []

view : Model -> Html Action
view model =
  renderBoard 50 model.selectedSquare model.board
