module Generator where
import Squares
import Game
import Moves

data Dir = NN | NE | EE | SE | SS | SW | WW | NW
  deriving (Show, Eq, Ord)

dir_deltas = [(NN,10), (NE,9), (EE,1),(SE,-11),(SS,-10)
  , (SW,-9), (WW,-1), (NW,11)]

squareAttack :: Square -> Game -> Bool
squareAttack _ _ = False

moveGenerator :: Game -> [Move]
moveGenerator _ = undefined

