module Game (start) where

import Graphics.Gloss (Display(InWindow), play)
import qualified Graphics.Gloss.Data.Color as Color

import Game.Screen (dimensions, screenOffset)
import qualified Game.State as State

start :: IO ()
start = play
  (InWindow "ASTEROIDS" dimensions screenOffset)
  (Color.greyN 0.9)
  60
  State.initial
  State.render
  (\_ state -> state)
  State.tick