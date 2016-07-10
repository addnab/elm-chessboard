module View exposing (view)

import Html exposing (Html, div, span, text)
import Html.App as App
import Model exposing (Model)
import Array as Array
import Collage
import Color exposing (Color)
import Chess.Square exposing (Square)
import Chess.Board exposing (Board)
import Element

main = view Model.initial

renderSquare sideLength square =
  let
    { file, rank } = square.position
    color =
      if (file + rank) % 2 == 0 then
        Color.white
      else
        Color.black
    xOrg = -sideLength/2 + sideLength/16
    yOrg = sideLength/2 - sideLength/16
    xOff = (toFloat (file - 1) * sideLength/8)
    yOff = (toFloat (rank - 1) * sideLength/8)
  in
    Collage.rect (sideLength/8) (sideLength/8)
      |> Collage.filled color
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
