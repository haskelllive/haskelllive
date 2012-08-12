-- | This is the documentation for the module Chess.
module Chess where

import Test.HUnit
-- import Test.QuickCheck

import Data.Char
import Control.Error.Util (note)
import Data.Tuple (swap)

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

-- | Show a square using FEN notation or '.' for an empty square.
showSquare :: Square -> Char
showSquare = maybe '.' showPiece

-- | Read a square using FEN notation or '.' for an empty square.
readSquare :: Char -> Either String Square
readSquare '.' = return Nothing
readSquare  c  = note errormsg $ fmap return (readPiece c)
  where errormsg = "Error reading square: " ++ show c ++ " is not a valid square"

data Piece = Piece PColor PType deriving (Show)
data PColor = White | Black deriving (Show, Eq)
data PType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

typeList :: [(Char, PType)]
typeList =  [('p', Pawn)
            ,('n', Knight)
            ,('b', Bishop)
            ,('r', Rook)
            ,('q', Queen)
            ,('k', King)
            ]

-- | Shows a piece using FEN notation.
--
-- * White pieces are "PNBRQG"
-- * Black pieces are "pnbrqg"
showPiece :: Piece -> Maybe Char
showPiece (Piece color ptype) = parseColor lookupChar
  where lookupChar          = lookup ptype (map swap typeList)
        parseColor Nothing  = '.'
        parseColor (Just c) = if color == White then toUpper c else c

-- | Reads a piece using FEN notation.
--
-- * White pieces are "PNBRQG"
-- * Black pieces are "pnbrqg"
readPiece :: Char -> Maybe Piece
readPiece c = fmap makePiece lookupType
  where color      = if isUpper c then White else Black
        lookupType = lookup (toLower c) typeList
        makePiece  = Piece color

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
