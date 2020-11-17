-- Add gaps between windows.
-- To use, copy this file to .xmonad/lib, then add
--      import Gaps
-- to your xmonad.hs. Then you can modify any layout with, for example,
--      gaps True 2 6 30
-- to add gaps of size 6 between windows.
-- Add keybindings for sendMessage Inc and sendMessage Dec to
-- increase or decrease the size of the gaps.


{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Gaps where

import XMonad
import XMonad.Core
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W

data Gaps a = Gaps { smartGaps    :: Bool
                   , gapSizeDelta :: Dimension
                   , gapSize      :: Dimension
                   , maxGapSize   :: Dimension
                   }
              deriving (Show, Read, Eq)

instance LayoutModifier Gaps a where
    modifyLayout (Gaps b δ s m) w r = do (x,y) <- runLayout w (f r)
                                         if b && length x == 1 then runLayout w r
                                                               else return $ ((\(p,q) -> (p, f q)) <$> x,y)
          where f (Rectangle x y w h) = Rectangle (x+fromIntegral s) (y+fromIntegral s)
                                                  (fromInteger . max 0 $ fromIntegral w-2*fromIntegral s)
                                                  (fromInteger . max 0 $ fromIntegral h-2*fromIntegral s)
    pureMess (Gaps b δ s m) i = runID <$> fromMessage i where runID Inc = Gaps b δ (min m $ s+δ) m
                                                              runID Dec = Gaps b δ (fromInteger . max 0 $ fromIntegral s-fromIntegral δ) m

data IncDec = Inc | Dec deriving (Show, Read, Eq, Typeable)
instance Message IncDec

gaps :: Bool -> Dimension -> Dimension -> Dimension -> l a -> ModifiedLayout Gaps l a
gaps b δ s m = ModifiedLayout (Gaps b δ s m)
