module Define where

import Data.Text (Text)
import Linear.V2 (V2(..))

data Game = Game {_cnt :: Int
                 ,_chrs :: [Chara]} deriving stock (Eq, Show)

data GEvent = GTick | GOk | GSub | GLeft | GUp | GDown | GRight
                                                  deriving stock (Eq, Show)

data Dir =  South | North | East | West | NoDir deriving stock (Eq, Show, Enum)

type Pos = V2 Int

type ChNum = Int
type ChName = Text
type ChMov = Bool
type ChDir = Dir
type ChPos = Pos

data Chara = Ch ChNum ChName ChMov ChDir ChPos deriving stock (Eq, Show)

