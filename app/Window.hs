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
mkWindow Size {width, ratio = (rw, rh)} = do
  let ratio = fromIntegral rw / fromIntegral rh

  let height :: Int = Prelude.max 1 $ floor (fromIntegral width / ratio)

  Window ratio (fromIntegral height) (fromIntegral width)
