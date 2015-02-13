module Main where

import Model
import View
import Controller

import Control.Monad.Eff
import Signal.Channel
import Signal
import VirtualDOM.VTree
import Signal.Time

main = do
  b <- mkBoard 10 10 4
  c <- channel id
  let initSpec = controller c $ gameInit b
      specSig  = foldp ($) (controller c $ gameInit b) $ subscribe c
  windowOnLoad $ viewRender puzzlerInit (puzzlerView <~ specSig)


