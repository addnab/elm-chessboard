module Chess.Position exposing
  ( Position
  , File, Rank
  )

type alias File = Int

type alias Rank = Int

type alias Position =
  { file : File
  , rank : Rank
  }
