-- Binary tree layout for xmonad.
-- To use, copy this file to .xmonad/lib. Then add something like:
--      import Binary
--      myLayout = Binary (3/100) (1/2) ||| ...
-- to your xmonad.hs.
-- You can resize the master window with mod+h and mod+l, just like in Tall.

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Binary where

import XMonad
import XMonad.Core
import qualified XMonad.StackSet as W

data Binary a = Binary Rational Rational deriving (Show, Read, Eq)

instance LayoutClass Binary a where
    pureLayout (Binary f δ) r s = zip ws . bin f r $ length ws 
        where ws = (reverse $ W.up s) ++ W.focus s : W.down s
              bin f r 1 = [r]
              bin f r 2 = [Rectangle x y nw h, Rectangle nx y (w-nw) h]
                            where x  = rect_x r
                                  y  = rect_y r
                                  w  = rect_width r
                                  h  = rect_height r
                                  nx = x + fromIntegral nw
                                  nw = floor $ fromIntegral w * f
              bin f r l = Rectangle x y nw h : (bin (f/2+1/4) ur ul) ++ (bin (f/2+1/4) dr dl)
                            where x  = rect_x r
                                  y  = rect_y r
                                  w  = rect_width r
                                  h  = rect_height r
                                  nx = x + fromIntegral nw
                                  ny = y + fromIntegral nh
                                  nw = floor $ fromIntegral w*f
                                  nh = h`div`2
                                  ul = dl + (l-1)`mod`2
                                  dl = (l-1)`div`2
                                  ur = Rectangle nx y (w-nw) nh
                                  dr = Rectangle nx ny (w-nw) nh
    pureMessage (Binary f δ) m = resize <$> fromMessage m
                            where resize Shrink = Binary (max 0 $ f-δ) δ
                                  resize Expand = Binary (min 1 $ f+δ) δ
    description _ = "Binary"
