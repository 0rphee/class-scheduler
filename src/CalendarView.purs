module CalendarView where

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
import Model (CalendarModel, HorarioClase, CalendarSesionInfo, timeToHours)
import Web.HTML.HTMLMediaElement (duration)

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
  -- TODO: check if there are better ways to deal with floats
  hourNum = Int.floor h
  minNum = Int.round $ (h - (Number.trunc h)) * 60.0

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

genHorarioClase :: Number -> Number -> { inicio :: TDU.Hours, final :: TDU.Hours }
genHorarioClase inicio final = { inicio: durHours inicio, final: durHours final }

mockData :: CalendarModel
mockData =
  { lun:
      helper <$>
        [ genHorarioClase 8.5 10.0
        , genHorarioClase 10.0 12.0
        , genHorarioClase 17.5 20.0
        , genHorarioClase 7.0 8.0
        ]
  , mar: helper <$> [ genHorarioClase 10.0 15.0 ]
  , mie: helper <$> [ genHorarioClase 15.0 18.0 ]
  , jue: helper <$> [ genHorarioClase 22.0 23.5 ]
  , vie: helper <$> [ genHorarioClase 8.0 13.5 ]
  , sab: helper <$> [ genHorarioClase 6.0 8.0 ]
  , dom: helper <$> [ genHorarioClase 12.0 13.0 ]
  }
  where
  helper = (\{ inicio, final } -> { matNombre: "Cálculo", prof: "Bernabé", id: "12", inicio: inicio, final: final })

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

renderDay :: Array CalendarSesionInfo -> Array (ReactElement)
renderDay xs = map renderClass xs
  where
  renderClass :: CalendarSesionInfo -> ReactElement
  renderClass c =
    HS.div_ "class-sesion"
      { style: H.css { "height": (show dif <> "%"), "top": (show start <> "%") }
      }
      ( H.div {}
          [ H.h3 {} c.matNombre
          , H.div {} (H.text (formatDur c.inicio <> " - " <> formatDur c.final))
          , H.div {} (H.text c.prof)
          , H.div {} (H.text $ "ID: " <> c.id)
          ]
      )
    where
    start = (\x -> (*) 100.0 $ durRatio (durHours 24.0) x) (c.inicio)
    dif = (\x y -> (*) 100.0 $ durRatio (durHours 24.0) (durSub x y)) (c.final) (c.inicio)

main :: CalendarModel -> ReactElement
main calData =
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
            ( map
                (\day -> HS.div "week-column" (renderDay day))
                calDataArr
            )
        ]
    ]
  where
  calDataArr =
    [ calData.lun, calData.mar, calData.mie, calData.jue, calData.vie, calData.sab, calData.dom ]
