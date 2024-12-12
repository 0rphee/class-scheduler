module Validation {-(collectValidationResults, runProgLogic)-} where

-- import CmdLineOpts
-- import Control.Monad (when)
-- import Data.Bifunctor (Bifunctor (first))
-- import Data.List (foldl', tails)

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List as L
import Data.Map as M
import Data.Maybe as May
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\), (/\))
import Model (Clase, Materia, VSesion, validISesion)
import Unsafe.Coerce (unsafeCoerce)

intervalsOverlap :: VSesion -> VSesion -> Boolean
intervalsOverlap { dia: d1, sesionHorario: { inicio: _ /\ a, final: _ /\ b } } { dia: d2, sesionHorario: { inicio: _ /\ x, final: _ /\ y } }
  | d1 /= d2 = false
  | (a == x) && (b == y) = true
  | (a < x) && (x < b) = true
  | (a < y) && (y < b) = true
  | (x < a) && (a < y) = true
  | (x < b) && (b < y) = true
  | otherwise = false

genPossibleClassCombinations
  :: M.Map
       String -- Materia: "Calculus"
       (List String) -- Clases: "[Calculus w/Roberts (id: 1234), Calculus w/Nicholson (id: 4321)]"
  -> List (List String) -- outputs the list of lists of ids as Text: [["1234", "9532"], ["4321", "9532"]]
genPossibleClassCombinations = sequence <<< M.values

-- ex = M.fromFoldable $ [ "Calc" /\ L.fromFoldable [ "123", "4321" ], "Quim" /\ L.fromFoldable [ "5421", "5945" ] ]
-- res = genPossibleClassCombinations ex

validate
  :: M.Map
       String -- Materia: "Calculus"
       Materia
  -- (List String) -- Clases: "[Calculus w/Roberts (id: 1234), Calculus w/Nicholson (id: 4321)]"
  -> M.Map
       String -- Clase id: "1234"
       Clase -- Array (Sesion: dia, hora inicia, hora final )
  -> Either String (List (List String))
validate subjMap classMap =
  if L.null validClassCombinations then Left "err"
  else Right validClassCombinations
  where
  subjMap2 = map (\v -> L.fromFoldable v.materiaClases) subjMap

  classCombinations = genPossibleClassCombinations subjMap2
  validClassCombinations = L.filter classCombIsValid classCombinations

  classCombIsValid :: List String -> Boolean
  classCombIsValid classComb = not $ L.any (\(a /\ b) -> intervalsOverlap a b) pairs
    where
    asClase :: List Clase
    asClase = L.mapMaybe (\id -> M.lookup id classMap) classComb

    allSesiones :: List VSesion
    allSesiones = L.concatMap (L.mapMaybe validISesion <<< L.fromFoldable <<< _.claseSesiones) asClase

    pairs :: List (VSesion /\ VSesion)
    pairs = tuples allSesiones

-- | Generate all choices of n elements out of the list x respecting the order in x and without repetitions.
tuples :: forall a. List a -> List (a /\ a)
tuples =
  let
    go r =
      case compare r 0 of
        LT -> const Nil
        EQ -> const (L.singleton Nil)
        GT ->
          L.concatMap
            ( \a -> case a of
                (y : ys) -> map (y : _) (go (r - 1) ys)
                _ -> unsafeCoerce unit
            )
            <<< May.fromMaybe (unsafeCoerce unit)
            <<< L.init
            <<< tails

    tails Nil = L.singleton Nil
    tails as'@(Cons _ as) = as' : tails as
  in
    map
      ( \a -> case a of
          (x : y : Nil) -> x /\ y
          _ -> unsafeCoerce unit
      )
      <<< go 2
