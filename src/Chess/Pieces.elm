module Chess.Pieces exposing (PlayerPiece, Piece(..))

import Chess.Players exposing (Player)

type Piece = K | Q | R | N | B | P

type alias PlayerPiece =
  { player : Player
  , piece : Piece
  }
