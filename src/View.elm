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

renderPiece : Int -> Maybe PlayerPiece -> Html Action
renderPiece sideLength piece =
  let
    pieceDisplayInfo =
      getPieceDisplayInfo piece
    pieceStyle =
      [ ("z-index", "1")
      , ("position", "absolute")
      , ("width", toString sideLength ++ "rem")
      , ("height", toString sideLength ++ "rem")
      ]
  in
    case pieceDisplayInfo of
      Just imageName ->
        img [ style pieceStyle, src ("assets/" ++ imageName) ]
            []
      Nothing ->
        div [] []

squareStyle sideLength color =
  style
    [ ("backgroundColor", color)
    , ("height", toString sideLength ++ "rem")
    , ("width", toString sideLength ++ "rem")
    ]

hightlightStyle sideLength hightlightColor =
  style
    [ ("backgroundColor", hightlightColor)
    , ("opacity", "0.9")
    , ("position", "absolute")
    , ("width", toString sideLength ++ "rem")
    , ("height", toString sideLength ++ "rem")
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
    [ ("height", toString sideLength ++ "rem")
    , ("display", "flex")
    ]

renderRank : Int -> Maybe Square -> Rank -> Html Action
renderRank sideLength selectedSquare rank =
  div [ rankStyle sideLength ]
    <| List.map (renderSquare sideLength selectedSquare)
      <| Dict.values rank

boardStyle sideLength =
  style
    [ ("box-shadow", "0 0 2rem -0.2rem black")
    , ("width", toString (sideLength * 8) ++ "rem")
    , ("height", toString (sideLength * 8) ++ "rem")
    ]

renderBoard : Int -> Maybe Square -> Board -> Html Action
renderBoard sideLength selectedSquare board =
  div [ boardStyle sideLength ]
    <| List.map (renderRank sideLength selectedSquare)
      <| Dict.values board

view : Model -> Html Action
view model =
  renderBoard 5 model.selectedSquare model.boardView
