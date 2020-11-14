-- Add gaps between windows.
-- To use, copy this file to .xmonad/lib, then add
--      import Gaps
-- to your xmonad.hs. Then you can modify any layout with, for example,
--      gaps 2 6
-- to add gaps of size 6 between windows.
-- Add keybindings for sendmessage Inc and sendMessage Dec to
-- increase or decrease the size of the gaps.


{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Gaps where

import XMonad
import XMonad.Core
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W

data Gaps a = Gaps Dimension Dimension deriving (Show, Read, Eq)

instance LayoutModifier Gaps a where
    modifyLayout (Gaps δ s) w r = do (x,y) <- runLayout w (f r)
                                     return ((\(p,q) -> (p, f q)) <$> x,y)
          where f (Rectangle x y w h) = Rectangle (x+fromIntegral s) (y+fromIntegral s)
                                                  (fromInteger . max 0 $ fromIntegral w-2*fromIntegral s)
                                                  (fromInteger . max 0 $ fromIntegral h-2*fromIntegral s)
    pureMess (Gaps δ s) m = runID <$> fromMessage m where runID Inc = Gaps δ $ s+δ
                                                          runID Dec = Gaps δ . fromInteger . max 0 $ fromIntegral s-fromIntegral δ

data IncDec = Inc | Dec deriving (Show, Read, Eq, Typeable)
instance Message IncDec

gaps :: Dimension -> Dimension -> l a -> ModifiedLayout Gaps l a
gaps δ s = ModifiedLayout (Gaps δ s)
