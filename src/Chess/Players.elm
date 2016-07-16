module Chess.Players exposing (Player(..), opponent)

type Player = White | Black

opponent : Player -> Player
opponent player =
  case player of
    White -> Black
    Black -> White
