module Chess.Position exposing
  ( Position
  , File, Rank
  )

type File = Int

type Rank = Int

type alias Position =
  { file : File
  , rank : Rank
  }
