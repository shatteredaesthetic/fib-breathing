module Main where

import Prelude
import Control.Monad.RWS (RWS, evalRWS)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Control.Monad.Writer (tell)
import Data.Array (range, uncons)
import Data.Foldable (foldl)
import Data.Int (toStringAs, hexadecimal)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Effect.Timer (clearInterval, setInterval, setTimeout)
import Graphics.Canvas (Context2D, fillRect, getCanvasElementById,
                        getCanvasWidth, getCanvasHeight, getContext2D,
                        setFillStyle, setStrokeStyle, strokeText)
import Partial.Unsafe (unsafePartial)

data Inhale = Inhale Int Int
data Exhale = Exhale Int Int

data Breath = In Inhale | Ex Exhale

type Dim = { w :: Number, h :: Number }
newtype BreathEnv = Env { in :: Inhale, out :: Exhale, dim :: Dim }
mkDim :: Number -> Number -> Dim
mkDim w h = { w, h }

data BreathState = State Breath

type Log = Array (Tuple Int String)

type Breathing = RWS BreathEnv Log BreathState

inLbl :: Int -> Int -> Tuple Int String
inLbl x i =
  let
    g = if i == 1
      then "ff"
      else toStringAs hexadecimal $ (255 / x) * (x - (i - 1))
    b = if i == 1
      then "00"
      else toStringAs hexadecimal $ (255 / x) * (i - 1)
  in
    Tuple i $ "#00" <> g <> b

exLbl :: Int -> Int -> Tuple Int String
exLbl y i =
  let
    r = if i == 1
      then "00"
      else toStringAs hexadecimal $ (255 / y) * (i - 1)
    g = if i == 1
      then "ff"
      else toStringAs hexadecimal $ (255 / y) * (y - (i - 1))
  in
    Tuple i $ "#" <> r <> g <> "00"

breathe :: Breathing Unit
breathe = do
  Env env <- ask
  State curr <- get
  let (Inhale x _) = env.in
  let (Exhale y _) = env.out
  case curr of
    In (Inhale x' hld) -> do
      let ex = (inLbl x) <$> range 1 x'
      let hold = (inLbl hld) <$> range hld 1
      tell $ ex <> hold
      put $ State $ Ex env.out
      breathe
    Ex (Exhale x' hld) -> do
      let ex = (exLbl y) <$> range 1 x'
      let hold = (exLbl hld) <$> range hld 1
      tell $ ex <> hold

fillScreen :: Context2D -> Tuple Int String -> Dim -> Effect Unit
fillScreen ctx (Tuple i str) { w, h } = do
  _ <- setFillStyle ctx str
  _ <- fillRect ctx { x: 0.0, y: 0.0, width: w, height: h }
  _ <- setStrokeStyle ctx "#ffffff"
  strokeText ctx (show i) (w / 2.0) (h / 2.0)

print :: Context2D -> Ref Log -> Dim -> Effect Unit
print ctx ref dim = void do
  log <- read ref
  case uncons log of
    Nothing -> pure unit
    Just { head, tail } -> do
      _ <- fillScreen ctx head dim
      write tail ref

cycle :: Context2D -> BreathEnv -> Effect Unit
cycle ctx env@(Env e) = do
  let state = State $ In e.in
  let { in: Inhale a b, out: Exhale c d } = e
  let Tuple _ log = evalRWS breathe env state
  logRef <- new log
  iId <- setInterval 1000 $ print ctx logRef e.dim
  _ <- setTimeout ((*) 1000 $ foldl (+) 1 [a, b, c, d]) $ clearInterval iId
  pure unit

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  let env@(Env e) = Env { in: Inhale 8 5, out: Exhale 13 3, dim: mkDim w h }
  ctx <- getContext2D canvas
  let { in: Inhale a b, out: Exhale c d } = e
  let time = (*) 1000 $ foldl (+) 1 [a, b, c, d]
  _ <- cycle ctx env
  _ <- setTimeout time main
  pure unit
