-- | This is the documentation for the module Chess.
module Chess where

import Test.HUnit
-- import Test.QuickCheck

import Data.Char
import Control.Error.Util (note)
import Data.Maybe (fromMaybe)
import Data.List (find)
import Control.Arrow ((&&&))
import Control.Applicative

type Board = [[Square]]

initialBoardStr ::  String
initialBoardStr = unlines ["rnbqkbnr"
                          ,"pppppppp"
                          ,"........"
                          ,"........"
                          ,"........"
                          ,"........"
                          ,"PPPPPPPP"
                          ,"RNBQKBNR"
                          ]

readBoard ::  String -> Either String Board
readBoard = (mapM . mapM) readSquare . lines

showBoard :: Board -> String
showBoard = unlines . (map . map) showSquare

type Square = Maybe Piece

-- | Show a square using FEN notation or ' ' for an empty square.
showSquare :: Square -> Char
showSquare = maybe '.' showPiece

-- | Read a square using FEN notation or ' ' for an empty square.
readSquare :: Char -> Either String Square
readSquare '.' = return Nothing
readSquare  c  = note errormsg $ fmap return (readPiece c)
  where errormsg = "Error reading square: " ++ show c ++ " is not a valid square"

data Piece = Piece PColor PType deriving (Show)
data PColor = White | Black deriving (Show)
data PType = Pawn | Knight | Bishop | Rook | Queen | King
           deriving (Show, Eq, Enum)

showPiece :: Piece -> Char
showPiece (Piece color ptype) = colorChar color . typeToChar $ ptype

colorChar :: PColor -> Char -> Char
colorChar White = toUpper
colorChar Black = toLower

charColor :: Char -> PColor
charColor c | isUpper c = White
            | otherwise = Black

typeToChar :: PType -> Char
typeToChar Pawn   = 'p'
typeToChar Knight = 'k'
typeToChar Bishop = 'b'
typeToChar Rook   = 'r'
typeToChar Queen  = 'q'
typeToChar King   = 'k'

charToType :: Char -> Maybe PType
charToType c = lookup (toLower c) typeList

typeList :: [(Char, PType)]
typeList = map (typeToChar &&& id) (enumFrom Pawn)

-- | Reads a piece using FEN notation.
--
-- * White pieces are "PNBRQG"
-- * Black pieces are "pnbrqg"
readPiece :: Char -> Maybe Piece
readPiece c = Piece <$> Just (charColor c) <*> charToType c

-- Tests

tests ::  Test
tests = TestList $ map TestCase
  [assertEqual "add tests here"  1 (1 :: Int)
  ]

prop_empty ::  Int -> Bool
prop_empty c1 = (c1::Int) == c1

runTests ::  IO ()
runTests =
  -- runTestTT tests
  -- quickCheck prop_empty
  return ()

-- | For now, main will run our tests.
main :: IO ()
main = runTests
