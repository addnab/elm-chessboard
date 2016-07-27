module Chess.Players exposing (Player(..), PlayerInfo, opponent, playerInit, getPlayerInfo)

import Chess.Position exposing (Position)

type Player = White | Black

type alias PlayerInfo =
  { canCastle : Bool
  , inCheck : Bool
  , kingPosition : Position
  }

getPlayerInfo : Player -> { white : PlayerInfo, black : PlayerInfo } -> PlayerInfo
getPlayerInfo player players =
  case player of
    White -> players.white
    Black -> players.black

playerInit =
  PlayerInfo False False

opponent : Player -> Player
opponent player =
  case player of
    White -> Black
    Black -> White
