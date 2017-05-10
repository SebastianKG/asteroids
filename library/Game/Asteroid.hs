module Game.Asteroid (Asteroid, new, render, tick) where

import Data.Function ((&))

import Graphics.Gloss.Data.Picture (Picture)
import qualified Graphics.Gloss.Data.Picture as Picture
import qualified Graphics.Gloss.Data.Color as Color

import qualified Game.Screen as Screen

bigCollisionRadius :: Float
bigCollisionRadius = 50

bigSpeed :: Float
bigSpeed = 1

-- littleCollisionRadius :: Float
-- littleCollisionRadius = 50

-- littleSpeed :: Float
-- littleSpeed = 2

data Asteroid = Big
  { pos :: (Float, Float)
  , speed :: Float
  , angle :: Float
  , collisionRadius :: Float
  }
  -- | Little
  -- { pos :: (Float, Float)
  -- , speed :: Float
  -- , angle :: Float
  -- , collisionRadius :: Float
  -- }

new :: (Float, Float) -> Float -> Asteroid
new position facingAngle =
  Big
    { pos = position
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

tick :: Asteroid -> [Asteroid]
tick asteroid =
  [asteroid]
