module View exposing (view)

import Html exposing (Html, div, span, text, img)
import Html.App as App
import Html.Attributes exposing (style, src, width, height)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Actions exposing (Action(..))
import Dict
import Color exposing (Color)
import Chess.Square exposing (Square, Highlight(..))
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

renderPiece : Int -> Maybe PlayerPiece -> Html Action
renderPiece sideLength piece =
  let
    pieceDisplayInfo =
      getPieceDisplayInfo piece
    pieceStyle =
      [ ("z-index", "1")
      , ("position", "absolute")
      ]
  in
    case pieceDisplayInfo of
      Just imageName ->
        img [ style pieceStyle, src ("../assets/" ++ imageName), width sideLength, height sideLength]
            []
      Nothing ->
        div [] []

squareStyle sideLength color =
  style
    [ ("backgroundColor", color)
    , ("height", toString sideLength ++ "px")
    , ("width", toString sideLength ++ "px")
    ]

hightlightStyle sideLength hightlightColor =
  style
    [ ("backgroundColor", (Debug.log "color" hightlightColor))
    , ("opacity", "0.9")
    , ("position", "absolute")
    , ("width", toString sideLength ++ "px")
    , ("height", toString sideLength ++ "px")
    ]

squareSelectAction : Maybe Square -> Square -> Action
squareSelectAction fromSquareMaybe toSquare =
  case fromSquareMaybe of
    Nothing ->
      Select toSquare
    Just fromSquare ->
      case toSquare.highlight of
        Movable ->
          Move fromSquare toSquare.position
        Capturable ->
          Move fromSquare toSquare.position
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
    highlightColor =
      case square.highlight of
        Movable ->
          "grey"
        Capturable ->
          "red"
        None ->
          if isSelected then
            "yellow"
          else
            ""

    baseColor =
      if (file + rank) % 2 == 0 then
        "white"
      else
        "green"
  in
    div [ squareStyle sideLength baseColor, onClick (squareSelectAction selectedSquare square) ]
      [ div [ hightlightStyle sideLength highlightColor ] []
      , renderPiece sideLength square.piece
      ]

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
