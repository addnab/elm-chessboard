module Chess.Square exposing (Square, Highlight(..), highlightSquare)

import Chess.Position exposing (Position)
import Chess.Pieces exposing (PlayerPiece)
import Chess.Players exposing (Player)

type Highlight = Movable | Capturable | None

type alias Square =
  { piece : Maybe PlayerPiece
  , position : Position
  , highlight: Highlight
  }

highlightSquare : Player -> Square -> Square
highlightSquare player square =
  let
    highlight =
      case square.piece of
        Nothing ->
          Movable
        Just playerPiece ->
          if playerPiece.player == player then
            None
          else
            Capturable
  in
    { square | highlight = highlight }
