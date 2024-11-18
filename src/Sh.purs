module Sh where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Time as T
import Data.Time.Duration as TDU
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Elmish (ReactElement)
import Elmish.HTML as H
import Elmish.HTML.Styled as HS
import Model (HorarioClase)

durHours :: Number -> TDU.Hours
durHours = TDU.Hours

durRatio :: TDU.Hours -> TDU.Hours -> Number
durRatio (TDU.Hours denum) (TDU.Hours num) = num / denum

durSub :: TDU.Hours -> TDU.Hours -> TDU.Hours
durSub (TDU.Hours x) (TDU.Hours y) = TDU.Hours $ x - y

formatDur :: TDU.Hours -> String
formatDur (TDU.Hours h) = hs <> ":" <> ms
  where
  hs = x <> Int.toStringAs Int.decimal hourNum
    where
    x = if hourNum < 10 then "0" else ""
  ms = x <> Int.toStringAs Int.decimal minNum
    where
    x = if minNum < 10 then "0" else ""
  hourNum = Int.floor h
  minNum = Int.floor $ (h - (Number.trunc h)) * 60.0

-- hours :: Number -> T.Time
-- hours n = T.Time hour minu bottom bottom
--   where
--   helper :: forall v. Bounded v => Number -> v
--   helper v
--     | v > 23.0 = Bounded.top
--     | otherwise = Bounded.bottom
--   hour = Maybe.fromMaybe (helper n) $ Enum.toEnum (Int.trunc $ Number.abs n) :: T.Hour
--   minu = Maybe.fromMaybe (helper minNum) $ Enum.toEnum (Int.floor minNum) :: T.Minute
--     where
--     minNum = (n - (Number.trunc n)) * 60.0

-- formatTime :: T.Time -> String
-- formatTime t = DFT.format (List.fromFoldable [ DFT.Hours24, DFT.Placeholder ":", DFT.MinutesTwoDigits ]) (DT.DateTime bottom t)

-- ratio :: T.Time -> T.Time -> Number
-- ratio denom num =
--   num / de
--   where
--   (TDU.Minutes de) = T.diff bottom denom
--   (TDU.Minutes num) = T.diff bottom num

-- sub :: T.Time -> T.Time -> T.Time
-- sub x y = hours numRes
--   where
--   (TDU.Hours xx) = T.diff bottom x
--   (TDU.Hours yy) = T.diff bottom y
--   numRes = xx - yy

genHorarioClase :: Number -> Number -> HorarioClase TDU.Hours
genHorarioClase ini fin = { inicio: toTup ini, final: toTup fin }
  where
  toTup float = (formatDur ttime /\ ttime)
    where
    ttime = durHours float

emptyHorario :: HorarioClase (Maybe T.Time)
emptyHorario = { inicio: ("" /\ Nothing), final: ("" /\ Nothing) }

mockData :: { materiaLunes :: Array { final :: String /\ TDU.Hours, inicio :: String /\ TDU.Hours } }
mockData = { materiaLunes: [ genHorarioClase 8.5 10.0, genHorarioClase 10.0 12.0, genHorarioClase 17.5 20.0, genHorarioClase 7.0 8.0 ] }

generateHorizontalRows :: Number -> Number -> Array (ReactElement)
generateHorizontalRows startF endF =
  let
    helper :: Array (ReactElement) -> Number -> Array (ReactElement)
    helper carry next =
      if next >= startF then
        helper
          ( (H.div {} (H.text (formatDur $ durHours next)))
              `Array.cons`
                carry
          )
          (next - 0.5)
      else
        carry
  in
    helper [] endF

renderDay :: Array (String /\ HorarioClase TDU.Hours) -> Array (ReactElement)
renderDay xs = map renderClass xs
  where
  renderClass :: (String /\ HorarioClase TDU.Hours) -> ReactElement
  renderClass (nameStr /\ { inicio, final }) =
    HS.div_ "class-sesion"
      { style: H.css { "height": (show dif <> "%"), "top": (show start <> "%") }
      }
      ( H.div {}
          [ H.h3 {} nameStr
          , H.div {} (H.text (Tuple.fst inicio <> " - " <> Tuple.fst final))
          , H.div {} (H.text "Profesor")
          , H.div {} (H.text "Clase ID")
          ]
      )
    where
    start = (\x -> (*) 100.0 $ durRatio (durHours 24.0) x) (Tuple.snd inicio)
    dif = (\x y -> (*) 100.0 $ durRatio (durHours 24.0) (durSub x y)) (Tuple.snd final) (Tuple.snd inicio)

main :: ReactElement
main =
  HS.div "main-box-shadow calendar-container"
    [ HS.div "calendar-headers"
        -- HEADER
        [ H.div {} (H.text "") -- buffer
        , H.div {}
            (map (\s -> H.div {} $ H.text s) [ "LUN", "MAR", "MIÉ", "JUE", "VIE", "SÁB", "DOM" ])
        ]
    , HS.div "calendar-content"
        -- CALENDAR CONTENT
        [ HS.div "time-labels-container"
            -- TIME LABELS
            (generateHorizontalRows 0.0 24.0)
        , HS.div "week-column-container"
            -- DAY COLUMNS
            ( Array.replicate 7
                ( HS.div "week-column"
                    (renderDay (map (\x -> ("Cálculo" /\ x)) mockData.materiaLunes))
                )
            )
        ]
    ]
