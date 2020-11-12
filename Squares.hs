-- Squares layout for XMonad.
-- All windows are squares.
-- To use, copy to your .xmonad/libs, then add
--      import Squares
-- to your xmonad.hs. Then you can use Squares as a layout.

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Squares where

import XMonad
import XMonad.Core
import qualified XMonad.StackSet as W

data Squares a = Squares deriving (Show, Read, Eq)

instance LayoutClass Squares a where
    pureLayout _ r s = zip ws $ rs g  where
        ws = (reverse $ W.up s) ++ W.focus s : W.down s
        rs r = Rectangle x y s s :
                (rs $ Rectangle nx ny nw nh)
                    where x  = rect_x r
                          y  = rect_y r
                          w  = rect_width r
                          h  = rect_height r
                          s  = min w h
                          nx = if s<w then x + fromIntegral s else x
                          ny = if s<h then y + fromIntegral s else y 
                          nw = if s<w then w-s else w
                          nh = if s<h then h-s else h
        g  = Rectangle mx my mw mh where
                          x  = rect_x r
                          y  = rect_y r
                          w  = rect_width r
                          h  = rect_height r
                          s  = min w h
                          mx = x + (fromIntegral $ (w-mw)`div`2)
                          my = y + (fromIntegral $ (h-mh)`div`2)
                          t  = floor $ fromIntegral s * τ
                          mw = if s == w then if t <= h then w else floor $ fromIntegral t / τ else t
                          mh = if s == h then if t <= w then h else floor $ fromIntegral t / τ else t
                          τ  = sqrt 3
