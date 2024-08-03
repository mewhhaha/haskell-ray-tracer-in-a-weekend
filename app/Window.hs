module Window (Size (..), Window (..), mkWindow) where

data Size = Size
  { width :: Int,
    ratio :: (Int, Int)
  }

data Window = Window
  { ratio :: Double,
    height :: Double,
    width :: Double
  }

mkWindow :: Size -> Window
mkWindow Size {width, ratio = (rw, rh)} = Window ratio (fromIntegral height) (fromIntegral width)
  where
    ratio = fromIntegral rw / fromIntegral rh
    height = Prelude.max (1 :: Int) $ floor (fromIntegral width / ratio)
