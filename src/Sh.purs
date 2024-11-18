module Sh where

import Prelude

import Data.Array as Array
import Data.Bounded as Bounded
import Data.DateTime as DT
import Data.Enum as Enum
import Data.Formatter.DateTime as DFT
import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Number as Number
import Data.Time as T
import Data.Time.Duration as TDU
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Elmish (ReactElement)
import Elmish.HTML as H
import Model (HorarioClase)

hours :: Number -> T.Time
hours n = T.Time hour minu bottom bottom
  where
  helper :: forall v. Bounded v => Number -> v
  helper v
    | v > 23.0 = Bounded.top
    | otherwise = Bounded.bottom
  hour = Maybe.fromMaybe (helper n) $ Enum.toEnum (Int.trunc $ Number.abs n) :: T.Hour
  minu = Maybe.fromMaybe (helper minNum) $ Enum.toEnum (Int.floor minNum) :: T.Minute
    where
    minNum = (n - (Number.trunc n)) * 60.0

formatTime :: T.Time -> String
formatTime t = DFT.format (List.fromFoldable [ DFT.Hours24, DFT.Placeholder ":", DFT.MinutesTwoDigits ]) (DT.DateTime bottom t)

ratio :: T.Time -> T.Time -> Number
ratio denom num =
  de / num
  where
  (TDU.Minutes de) = T.diff bottom denom
  (TDU.Minutes num) = T.diff bottom num

sub :: T.Time -> T.Time -> T.Time
sub x y = hours numRes
  where
  (TDU.Hours xx) = T.diff bottom x
  (TDU.Hours yy) = T.diff bottom y
  numRes = xx - yy

genHorarioClase :: Number -> Number -> HorarioClase T.Time
genHorarioClase ini fin =
  let
    toTup float =
      let
        ttime = hours float
      in
        (formatTime ttime /\ ttime)
  in
    { inicio: toTup ini
    , final: toTup fin
    }

emptyHorario :: HorarioClase (Maybe T.Time)
emptyHorario =
  { inicio: ("" /\ Nothing)
  , final: ("" /\ Nothing)
  }

-- mockData :: Model.Materia
-- mockData =
--   { materiaId: "1234"
--   , materiaProf: "Bernabe"
--   , materiaLunes: [ genHorarioClase 8.5 10, genHorarioClase 10 12, genHorarioClase 17.5 20, genHorarioClase 7 8 ]
--   , materiaMartes: []
--   , materiaMiercoles: [ genHorarioClase 8.5 10 ]
--   , materiaJueves: []
--   , materiaViernes: [ genHorarioClase 8.5 10 ]
--   , materiaSabado: []
--   , materiaDomingo: []
--   }
--

mockData = { materiaLunes: [ genHorarioClase 8.5 10.0, genHorarioClase 10.0 12.0, genHorarioClase 17.5 20.0, genHorarioClase 7.0 8.0 ] }

generateHorizontalRows :: Number -> Number -> Array (ReactElement)
generateHorizontalRows startF endF =
  let
    helper :: Array (ReactElement) -> Number -> Array (ReactElement)
    helper carry next =
      if next >= startF then
        helper
          ( ( H.div
                { style: H.css { "position": "relative", "height": "1.5rem", "top": "-0.5rem" } }
                (H.text (formatTime $ hours next))
            ) `Array.cons` carry
          )
          (next - 0.5)
      else
        carry
  in
    helper
      [ H.div
          { style: H.css { "position": "relative", "top": "calc(100% - 0.5rem)", "width": "100%" } }
          (H.text (formatTime $ hours endF))
      ]
      (endF - 0.5)

renderDay :: Array (String /\ HorarioClase T.Time) -> Array (ReactElement)
renderDay xs =
  let
    renderClass :: (String /\ HorarioClase T.Time) -> ReactElement
    renderClass (nameStr /\ { inicio, final }) =
      let
        start = (\x -> (*) 100.0 $ ratio (hours 24.0) x) (Tuple.snd inicio)
        dif = (\x y -> (*) 100.0 $ ratio (hours 24.0) (sub x y)) (Tuple.snd final) (Tuple.snd inicio)
      in
        H.div
          { style: H.css
              { "position": "relative"
              , "height": (show dif <> "%")
              , "width": "100%"
              , "top": (show start <> "%")
              }
          }
          [ H.div
              { style: H.css
                  { "border-radius": "0 15px 15px 0"
                  , "background-color": "rgb(222, 244, 230)"
                  , "color": "rgb(91, 172, 116)"
                  , "display": "flex"
                  , "height": "98%"
                  , "width": "95%"
                  , "font-size": "0.8rem"
                  }
              }
              [ H.div
                  { style: H.css
                      { "padding": "8%"
                      , "max-width": "80%"
                      , "border-left": "0.2rem solid rgb(91, 172, 116)"
                      }
                  }
                  [ H.text nameStr
                  , H.div
                      { style: H.css
                          { "overflow": "hidden"
                          , "text-overflow": "ellipsis"
                          , "white-space": "nowrap"
                          }
                      }
                      [ H.text (Tuple.fst inicio <> " - " <> Tuple.fst final) ]
                  ]
              ]
          ]
  in
    map renderClass xs

main :: ReactElement
main =
  H.div
    { style: H.css
        { "margin": "2%"
        , "border-radius": "2%"
        -- , "border": "solid"
        , "padding": "1%"
        , "background-color": "rgb(248, 249, 255)"
        }
    }
    [ H.div
        -- HEADER
        { style: H.css { "display": "flex" } }
        [ H.div { style: H.css { "min-width": "5%" } } (H.text "") -- buffer
        , H.div
            { style: H.css
                { "display": "flex"
                , "justify-content": "space-around"
                , "flex-grow": "1"
                }
            }
            [ H.div {} [ H.text "LUN" ]
            , H.div {} [ H.text "MAR" ]
            , H.div {} [ H.text "MIÉ" ]
            , H.div {} [ H.text "JUE" ]
            , H.div {} [ H.text "VIE" ]
            , H.div {} [ H.text "SÁB" ]
            , H.div {} [ H.text "DOM" ]
            ]
        ]
    , H.div
        -- WEEKLY VIEW
        { style: H.css
            { "display": "flex"
            , "flex-direction": "row"
            , "height": "100%"
            , "padding": "6px 0 0 0"
            }
        }
        [ H.div
            -- DATESTAMPS
            { style: H.css
                { "display": "flex"
                , "flex-direction": "column"
                , "min-width": "5%"
                , "text-align": "center"
                , "margin": "0 0.5rem 0 0 "
                , "position": "relative"
                }
            }
            (generateHorizontalRows 0.0 24.0)
        , H.div
            -- DAY COLUMNS
            { style: H.css
                { "display": "flex"
                , "justify-content": "space-evenly"
                , "flex-grow": "1"
                , "background-color": "white"
                }
            }
            ( Array.replicate 7
                ( H.div
                    { style: H.css
                        { "flex-grow": "1"
                        , "border-left": "1px solid black"
                        , "position": "relative"
                        }
                    }
                    (renderDay (map (\x -> ("Cálculo" /\ x)) mockData.materiaLunes))
                )
            )
        ]
    ]
