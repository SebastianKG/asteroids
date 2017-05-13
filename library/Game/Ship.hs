{-# LANGUAGE NamedFieldPuns #-}

module Game.Ship 
  ( Ship
  , ceaseFire
  , dead
  , goingLeft
  , goingRight
  , goLeft
  , goRight
  , move
  , new
  , render
  , resetRotation
  , shoot
  , stop
  , tick
  ) where

import Data.Fixed (mod')
import Data.Function ((&))
import qualified Graphics.Gloss.Data.Color as Color
import Graphics.Gloss.Data.Picture (Picture)
import qualified Graphics.Gloss.Data.Picture as Picture
import Graphics.Gloss.Geometry.Angle (degToRad)

import Game.Asteroid (Asteroid)
import qualified Game.Asteroid as Asteroid
import Game.Projectile (Projectile)
import qualified Game.Projectile as Projectile
import qualified Game.Screen as Screen
import qualified Game.Vector as Vector

shipCollisionRadius :: Float
shipCollisionRadius = 16

shipSpeed :: Float
shipSpeed = 5

rotationPerTick :: Float
rotationPerTick = 6 -- degrees

maxCooldown :: Int
maxCooldown = 30

data RotationDirection 
  = RotateLeft 
  | RotateRight
  | Neutral

data Ship = Ship
  { pos :: (Float, Float)
  , speed :: Float
  , angle :: Float
  , collisionRadius :: Float
  , cooldown :: Int
  , shouldShoot :: Bool
  , rotationDirection :: RotationDirection
  , moving :: Bool
  , dead :: Bool
  }

new :: (Float, Float) -> Float -> Ship
new pos speed' =
  Ship
    { pos = pos
    , speed = speed'
    , angle = 270
    , collisionRadius = shipCollisionRadius
    , rotationDirection = Neutral
    , shouldShoot = False
    , cooldown = 0
    , moving = False
    , dead = False
    }

render :: Ship -> Picture
render Ship{angle, collisionRadius, pos} =
  Picture.polygon [(-1,-1), (0,1), (1, -1)]
    & Picture.scale collisionRadius collisionRadius
    & Picture.rotate (angle + 90)
    & uncurry Picture.translate (Screen.fromTopLeft pos)
    & Picture.color Color.azure 

tick :: Ship -> [Asteroid] -> (Ship, [Projectile])
tick ship@Ship{pos = (x, y), angle, cooldown, moving, rotationDirection, shouldShoot, speed} asteroids =
  let
    newAngle =
      case rotationDirection of
        RotateLeft -> angle - rotationPerTick
        RotateRight -> angle + rotationPerTick
        Neutral -> angle
    
    shooting = shouldShoot && cooldown == 0

    collided =
      any (collides ship) asteroids
  in 
    if collided then
      ( ship { dead = True }, [] )
    else
      ( ship
          { pos =
              ( speed * cos (degToRad angle) + x
                  `mod'` fromIntegral (fst Screen.dimensions)
              , speed * sin (degToRad angle) + y
                  `mod'` fromIntegral (snd Screen.dimensions)
              )
          , speed = if moving then shipSpeed else speed / 1.1
          , angle = newAngle
          , cooldown = if shooting then maxCooldown else max 0 (cooldown - 1)
          }
      , if shooting then 
          [ Projectile.new (x, y) angle ] 
        else
          []
      )

move :: Ship -> Ship
move ship = ship
  { moving = True
  }

stop :: Ship -> Ship
stop ship = ship
  { moving = False
  }

goLeft :: Ship -> Ship
goLeft ship = ship
  { rotationDirection = RotateLeft
  }

goRight :: Ship -> Ship
goRight ship = ship
  { rotationDirection = RotateRight
  }

resetRotation :: Ship -> Ship
resetRotation ship = ship
  { rotationDirection = Neutral
  }

shoot :: Ship -> Ship
shoot ship = ship
  { shouldShoot = True
  }

ceaseFire :: Ship -> Ship
ceaseFire ship = ship
  { shouldShoot = False
  }

goingRight :: Ship -> Bool
goingRight Ship{rotationDirection = RotateRight} = True
goingRight _ = False

goingLeft :: Ship -> Bool
goingLeft Ship{rotationDirection = RotateLeft} = True
goingLeft _ = False

collides :: Ship -> Asteroid -> Bool
collides ship asteroid =
  let
    sPos = pos ship
    sRadius = collisionRadius ship

    aPos = Asteroid.position asteroid
    aRadius = Asteroid.collisionRadius asteroid
  in
    Vector.distance sPos aPos <= (sRadius + aRadius)