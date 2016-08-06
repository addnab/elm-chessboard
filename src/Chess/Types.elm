module Chess.Types exposing (..)

import Dict

type Player = White | Black

type Piece = K | Q | R | N | B | P

type alias PlayerPiece =
  { player : Player
  , moved : Bool
  , piece : Piece
  }

type alias PlayerInfo =
  { canCastle : Bool
  , inCheck : Bool
  , kingPosition : Position
  }

type alias Position =
  { file : Int
  , rank : Int
  }

type Move
  = Capture Position
  | Goto Position
  | CastleKingSide Position
  | CastleQueenSide Position

type alias Square =
  { piece : Maybe PlayerPiece
  , position : Position
  , moveToPlay : Maybe Move
  }

type alias Rank = Dict.Dict Int Square

type alias Board = Dict.Dict Int Rank
