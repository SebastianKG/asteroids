{-# LANGUAGE NamedFieldPuns #-}

module Game.State
  ( State
  , handleInput
  , initial
  , render
  , tick
  ) where

import           Graphics.Gloss.Data.Picture      (Picture)
import qualified Graphics.Gloss.Data.Picture      as Picture
import           Graphics.Gloss.Interface.IO.Game (Event, Key, KeyState)
import qualified Graphics.Gloss.Interface.IO.Game as Event

import           Game.Asteroid                    (Asteroid)
import qualified Game.Asteroid                    as Asteroid
import           Game.Projectile                  (Projectile)
import qualified Game.Projectile                  as Projectile
import           Game.Ship                        (Ship)
import qualified Game.Ship                        as Ship

data State = State
  { player      :: Ship
  , projectiles :: [Projectile]
  , asteroids   :: [Asteroid]
  }

handleInput :: Event -> State -> State
handleInput event state =
  case event of
    Event.EventKey key keyState _ _ ->
      handleKey key keyState state

    _ ->
      state

handleKey :: Key -> KeyState -> State -> State
handleKey (Event.Char 'w') Event.Down state@State{player} = state
  { player = Ship.move player }
handleKey (Event.Char 'w') Event.Up state@State{player} = state
  { player = Ship.stop player }
handleKey (Event.Char 'a') keyState state@State{player} =
  case keyState of
    Event.Down -> state
      { player = Ship.goLeft player }
    Event.Up ->
      if Ship.goingLeft player then
        state { player = Ship.resetRotation player }
      else
        state
handleKey (Event.Char 'd') keyState state@State{player} =
  case keyState of
    Event.Down -> state
      { player = Ship.goRight player }
    Event.Up ->
      if Ship.goingRight player then
        state { player = Ship.resetRotation player }
      else
        state
handleKey (Event.Char 'j') keyState state@State{player} =
  case keyState of
    Event.Down -> state
      { player = Ship.shoot player }
    Event.Up -> state
      { player = Ship.ceaseFire player }
handleKey _ _ state =
  state

initial :: State
initial = State
  { player = Ship.new (250, 250) 0
  , projectiles = []
  , asteroids =
      [ Asteroid.new (250, 100) 20
      , Asteroid.new (100, 400) 60
      , Asteroid.new (400, 400) 270
      ]
  }

render :: State -> Picture
render State{player, asteroids, projectiles} =
  Picture.pictures $ concat
    [ Projectile.render <$> projectiles
    , if Ship.dead player then
        []
      else
        [ Ship.render player ]
    , Asteroid.render <$> asteroids
    ]

tick :: Float -> State -> State
tick _ State{player, projectiles, asteroids} =
  let
    (newPlayer, justGeneratedProjectiles) =
        Ship.tick player asteroids

    newProjectiles =
      filter (not . Projectile.dead) $
        Projectile.tick <$> projectiles

    newAsteroids = do
      a <- asteroids
      spawned <- Asteroid.tick a projectiles
      return spawned
  in
    if Ship.dead player || null asteroids then
      initial
    else
      State
        { player = newPlayer
        , projectiles = newProjectiles ++ justGeneratedProjectiles
        , asteroids = newAsteroids
        }
