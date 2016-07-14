module View exposing (view)

import Html exposing (Html, div, span, text)
import Html.App as App
import Model exposing (Model)
import Array as Array
import Collage
import Text
import Color exposing (Color)
import Chess.Square exposing (Square)
import Chess.Board exposing (Board)
import Chess.Pieces exposing (getPieceDisplayInfo)
import Element

main = view Model.initial

renderSquare sideLength square =
  let
    { file, rank } = square.position
    pieceDisplayInfo =
      getPieceDisplayInfo square.piece
    color =
      if (file + rank) % 2 == 0 then
        Color.white
      else
        Color.black
    xOrg = -sideLength/2 + sideLength/16
    yOrg = sideLength/2 - sideLength/16
    xOff = (toFloat (file - 1) * sideLength/8)
    yOff = (toFloat (rank - 1) * sideLength/8)
    squareForm =
      Collage.rect (sideLength/8) (sideLength/8)
        |> Collage.filled color
    pieceForm =
      Text.fromString pieceDisplayInfo.text
        |> Text.style
            { typeface = [ "Times New Roman", "serif" ]
            , height   = Just 40
            , color    = pieceDisplayInfo.color
            , bold     = False
            , italic   = False
            , line     = Nothing
            }
        |> Collage.text
  in
    [ squareForm, pieceForm ]
      |> Collage.group
      |> Collage.move (xOrg + xOff, yOrg - yOff)

renderRank : Float -> Array.Array Square -> Collage.Form
renderRank sideLength rank =
  rank
    |> Array.map (renderSquare sideLength)
    |> Array.toList
    |> Collage.group

renderBoard sideLength board =
  board
    |> Array.map (renderRank (toFloat sideLength))
    |> Array.toList
    |> Collage.collage sideLength sideLength
    |> Element.toHtml

view model =
  renderBoard 800 model.board
