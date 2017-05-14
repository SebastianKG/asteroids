{-# LANGUAGE NamedFieldPuns #-}

module Game.Projectile
  ( Projectile
  , dead
  , new
  , position
  , render
  , tick
  ) where

import Data.Fixed (mod')
import Data.Function ((&))
import qualified Graphics.Gloss.Data.Color as Color
import Graphics.Gloss.Data.Picture (Picture)
import qualified Graphics.Gloss.Data.Picture as Picture
import Graphics.Gloss.Geometry.Angle (degToRad)

import qualified Game.Screen as Screen

projectileRadius :: Float
projectileRadius = 3

projectileSpeed :: Float
projectileSpeed = 8

projectileTtl :: Int
projectileTtl = 35

data Projectile = Projectile
  { pos :: (Float, Float)
  , speed :: Float
  , angle :: Float
  , ttl :: Int
  }

new :: (Float, Float) -> Float -> Projectile
new pos' angle = Projectile
  { pos = pos'
  , speed = projectileSpeed
  , angle = angle
  , ttl = projectileTtl
  }

render :: Projectile -> Picture
render projectile =
  Picture.circleSolid projectileRadius
    & Picture.rotate (angle projectile)
    & uncurry Picture.translate (Screen.fromTopLeft (pos projectile)) 
    & Picture.color (Color.dark Color.azure)

tick :: Projectile -> Projectile
tick projectile@Projectile{pos = (x,y), angle, speed, ttl} = projectile
  { pos =
      ( speed * cos (degToRad angle) + x
          `mod'` fromIntegral (fst Screen.dimensions)
      , speed * sin (degToRad angle) + y
          `mod'` fromIntegral (snd Screen.dimensions)
      )
  , ttl = ttl - 1
  }

dead :: Projectile -> Bool
dead Projectile{ttl} =
  ttl <= 0

position :: Projectile -> (Float, Float)
position = pos