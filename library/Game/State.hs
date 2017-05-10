{-# LANGUAGE NamedFieldPuns #-}

module Game.State 
  ( State
  , initial
  , render
  , tick
  ) where

import Game.Asteroid (Asteroid)
import qualified Game.Asteroid as Asteroid
import Game.Ship (Ship)
import qualified Game.Ship as Ship
import Graphics.Gloss.Data.Picture (Picture)
import qualified Graphics.Gloss.Data.Picture as Picture

data State = State
  { player :: Ship
  , asteroids :: [Asteroid]
  }

initial :: State
initial = State
  { player = Ship.new (250, 250) 0
  , asteroids = 
      [ Asteroid.new (250, 100) 20
      , Asteroid.new (100, 400) 60
      , Asteroid.new (400, 400) 270
      ]
  }

render :: State -> Picture
render State{player, asteroids} =
  Picture.pictures $
    Ship.render player : fmap Asteroid.render asteroids
    

tick :: Float -> State -> State
tick _ State{player, asteroids} =
  let
    newPlayer =
      Ship.tick player
    
    newAsteroids = do
      a <- asteroids
      spawned <- Asteroid.tick a
      return spawned
  in
    State
      { player = newPlayer
      , asteroids = newAsteroids
      }