import Data.Char (isDigit)
import Data.List

data Waveform = Up Int | Down Int | Zero deriving (Show, Eq)

parseWaveform :: String -> Waveform
parseWaveform "0" = Zero
parseWaveform x
  | last x == 'd' = Up (read (takeWhile isDigit x) :: Int)
  | last x == 'u' = Down (read (takeWhile isDigit x) :: Int)

waveforms :: [[Waveform]]
waveforms = map (map parseWaveform) points

reduceWaveform :: [Waveform] -> [Waveform]
reduceWaveform [] = []
reduceWaveform (Zero : xs) = reduceWaveform xs
reduceWaveform (Up n : xs) | Down n `elem` xs = reduceWaveform (delete (Down n) xs)
reduceWaveform (Down n : xs) | Up n `elem` xs = reduceWaveform (delete (Up n) xs)
reduceWaveform (x : xs) = x : reduceWaveform xs

printWaveform :: Waveform -> String
printWaveform Zero = "0"
printWaveform (Up n) = show n ++ "u" ++ show n ++ "d"
printWaveform (Down n) = show n ++ "d" ++ show n ++ "u"

col =
  map
    parseWaveform
    [ "3u3d",
      "4u4d",
      "6d6u",
      "5u5d",
      "7d7u",
      "3d3u",
      "6u6d",
      "1u1d",
      "6d6u",
      "6u6d",
      "7u7d",
      "7d7u",
      "8d8u",
      "3u3d",
      "1d1u",
      "6d6u",
      "6u6d",
      "1d1u",
      "7u7d",
      "8u8d",
      "6u6d",
      "0",
      "5d5u",
      "1u1d",
      "3d3u",
      "6d6u"
    ]

points =
  transpose
    [ ["5u5d", "3u3d", "6u6d", "8u8d", "5u5d", "3u3d", "3u3d", "8u8d"],
      ["1u1d", "4u4d", "3u3d", "4u4d", "3u3d", "7u7d", "6u6d", "3u3d"],
      ["3d3u", "6d6u", "4d4u", "2d2u", "0", "2d2u", "3d3u", "3d3u"],
      ["0", "5u5d", "2u2d", "6u6d", "3u3d", "8u8d", "4u4d", "6u6d"],
      ["8d8u", "7d7u", "6d6u", "0", "2d2u", "5d5u", "6d6u", "8d8u"],
      ["1d1u", "3d3u", "2d2u", "4d4u", "8d8u", "3d3u", "4d4u", "0"],
      ["7u7d", "6u6d", "5u5d", "6u6d", "0", "1u1d", "3u3d", "7u7d"],
      ["4u4d", "1u1d", "7u7d", "8u8d", "7u7d", "4u4d", "6u6d", "4u4d"],
      ["7d7u", "6d6u", "3d3u", "8d8u", "3d3u", "0", "8d8u", "7d7u"],
      ["1u1d", "6u6d", "5u5d", "4u4d", "8u8d", "5u5d", "1u1d", "4u4d"],
      ["2u2d", "7u7d", "1u1d", "2u2d", "4u4d", "8u8d", "5u5d", "0"],
      ["4d4u", "7d7u", "0", "8d8u", "5d5u", "1d1u", "4d4u", "7d7u"],
      ["7d7u", "8d8u", "5d5u", "2d2u", "6d6u", "3d3u", "5d5u", "8d8u"],
      ["8u8d", "3u3d", "2u2d", "3u3d", "3u3d", "2u2d", "4u4d", "7u7d"],
      ["4d4u", "1d1u", "1d1u", "4d4u", "5d5u", "8d8u", "0", "6d6u"],
      ["0", "6d6u", "2d2u", "0", "4d4u", "0", "3d3u", "4d4u"],
      ["3u3d", "3u3d", "4u4d", "5u5d", "2u2d", "5u5d", "8u8d", "8u8d"],
      ["1d1u", "1d1u", "1d1u", "6u6d", "3d3u", "4d4u", "1d1u", "5d5u"],
      ["3u3d", "7u7d", "8u8d", "8u8d", "1u1d", "2u2d", "1u1d", "5d5u"],
      ["7u7d", "8u8d", "1u1d", "1u1d", "8u8d", "6u6d", "3u3d", "6u6d"],
      ["2u2d", "6u6d", "3u3d", "0", "5u5d", "3u3d", "2u2d", "7u7d"],
      ["2d2u", "0", "5d5u", "5d5u", "3d3u", "5d5u", "6d6u", "5u5d"],
      ["2d2u", "5d5u", "8d8u", "8d8u", "4d4u", "8d8u", "4d4u", "4d4u"],
      ["4u4d", "1u1d", "4u4d", "2u2d", "3u3d", "0", "4u4d", "7d7u"],
      ["3d3u", "3d3u", "3d3u", "1d1u", "7d7u", "7d7u", "3d3u", "6d6u"],
      ["0", "6d6u", "4d4u", "6d6u", "0", "2d2u", "1d1u", "5d5u"]
    ]
