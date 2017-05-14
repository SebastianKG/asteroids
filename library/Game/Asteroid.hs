{-# LANGUAGE NamedFieldPuns #-}

module Game.Asteroid 
  ( Asteroid
  , collisionRadius
  , new
  , position
  , render
  , tick
  ) where

import Data.Fixed (mod')
import Data.Function ((&))

import Graphics.Gloss.Data.Picture (Picture)
import qualified Graphics.Gloss.Data.Picture as Picture
import qualified Graphics.Gloss.Data.Color as Color
import Graphics.Gloss.Geometry.Angle (degToRad)

import Game.Projectile (Projectile)
import qualified Game.Projectile as Projectile
import qualified Game.Screen as Screen
import qualified Game.Vector as Vector

bigCollisionRadius :: Float
bigCollisionRadius = 50

bigSpeed :: Float
bigSpeed = 1

littleCollisionRadius :: Float
littleCollisionRadius = 25

littleSpeed :: Float
littleSpeed = 1.5

data Asteroid = Big
  { pos :: (Float, Float)
  , speed :: Float
  , angle :: Float
  , collisionRadius :: Float
  } | Little
  { pos :: (Float, Float)
  , speed :: Float
  , angle :: Float
  , collisionRadius :: Float
  }

new :: (Float, Float) -> Float -> Asteroid
new pos' facingAngle =
  Big
    { pos = pos'
    , speed = bigSpeed
    , angle = facingAngle
    , collisionRadius = bigCollisionRadius
    }

render :: Asteroid -> Picture
render asteroid =
  Picture.circleSolid (collisionRadius asteroid)
    & Picture.rotate (angle asteroid)
    & uncurry Picture.translate (Screen.fromTopLeft (pos asteroid)) 
    & Picture.color (Color.greyN 0.7)

tick :: Asteroid -> [Projectile] -> [Asteroid]
tick asteroid projectiles =
  let
    speed' = speed asteroid
    angle' = degToRad $ angle asteroid
    (x, y) = pos asteroid

    newAsteroid = asteroid
      { pos =
          ( speed' * cos angle' + x
              `mod'` fromIntegral (fst Screen.dimensions)
          , speed' * sin angle' + y
              `mod'` fromIntegral (snd Screen.dimensions)
          )
      }
    
    collided =
      any (collides asteroid) projectiles
  in
    if not collided then
      [newAsteroid]
    else
      debris asteroid

debris :: Asteroid -> [Asteroid]
debris Little{} = []
debris Big{pos, angle} =
  let
    particle angle' = Little
      { pos = pos
      , speed = littleSpeed
      , angle = angle'
      , collisionRadius = littleCollisionRadius
      }
  in 
    [ particle (angle - 90)
    , particle (angle + 90)
    ]

collides :: Asteroid -> Projectile -> Bool
collides asteroid projectile =
  let
    aPos = pos asteroid
    aRadius = collisionRadius asteroid

    pPos = Projectile.position projectile
  in
    Vector.distance aPos pPos <= aRadius

position :: Asteroid -> (Float, Float)
position = pos