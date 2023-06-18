module ScheduleView exposing (weekView)

import Element exposing (Attribute, Element, alignTop, column, el, fill, height, none, padding, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HAttributes
import Model exposing (..)
import Msg exposing (..)
import String exposing (String(..))
import Svg
import Svg.Attributes as SvgAttr
import TypedTime as T exposing (TypedTime)


zStack : List (Attribute msg) -> List (Element msg) -> Element msg
zStack attrs children =
    case children of
        [] ->
            Element.none

        bottomMost :: otherChildren ->
            el
                (Element.inFront (zStack attrs otherChildren) :: attrs)
                bottomMost


genHorarioClase : Float -> Float -> HorarioClase
genHorarioClase ini fin =
    let
        toTup float =
            let
                ttime =
                    T.hours float
            in
            ( T.toString T.Hours ttime, Just ttime )
    in
    { inicio = toTup ini
    , final = toTup fin
    }


genTT : Float -> Float -> List (Element Msg)
genTT startF endF =
    let
        helper : List (Element Msg) -> Float -> List (Element Msg)
        helper carry next =
            if next > startF then
                helper
                    (row [] [ text (T.toString T.Hours <| T.hours next), svgHorizontalLine ] :: carry)
                    (next - 0.5)

            else
                carry
    in
    helper [] endF


svgHorizontalLine : Element msg
svgHorizontalLine =
    el [] <|
        Element.html <|
            Svg.svg
                [ SvgAttr.width "2000", SvgAttr.height "2" ]
                [ Svg.line [ SvgAttr.x1 "0", SvgAttr.y1 "0", SvgAttr.x2 "2000", SvgAttr.y2 "0", SvgAttr.stroke "rgb(231 233 242)", SvgAttr.strokeWidth "2" ] [] ]


svgVerticalLine : Element msg
svgVerticalLine =
    el [] <|
        Element.html <|
            Svg.svg
                [ SvgAttr.width "2", SvgAttr.height "300" ]
                [ Svg.line [ SvgAttr.x1 "0", SvgAttr.y1 "0", SvgAttr.x2 "0", SvgAttr.y2 "300", SvgAttr.stroke "rgb(231 233 242)", SvgAttr.strokeWidth "2" ] [] ]


mockData : Materia
mockData =
    { materiaId = "1234"
    , materiaProf = "Bernabe"
    , materiaLunes = genHorarioClase 8.5 10
    , materiaMartes = emptyHorario
    , materiaMiercoles = genHorarioClase 8.5 10
    , materiaJueves = emptyHorario
    , materiaViernes = genHorarioClase 8.5 10
    , materiaSabado = emptyHorario
    , materiaDomingo = emptyHorario
    }


main =
    Element.layout
        [ Font.family
            [ Font.typeface "Arial Rounded MT Bold"
            , Font.sansSerif
            ]
        , Font.size 19
        , Background.color <| Element.rgb255 245 246 250
        , Element.padding 15
        ]
        (el
            [ Element.width Element.fill

            {- Background.color <| Element.rgba255 0 0 0 255 -}
            ]
            -- (Element.html svg)
            (weekView mockData)
        )


emptyRow : Element msg
emptyRow =
    row [ Element.spacing 10 ]
        (List.repeat 8 <| text "empty")


weekView : Materia -> Element Msg
weekView m =
    let
        materiasStrs : List String
        materiasStrs =
            [ "LUNES", "MARTES", "MIERCOLES", "JUEVES", "VIERNES", "SABADO", "DOMINGO" ]

        topBarItems : List (Element msg)
        topBarItems =
            List.map (\x -> el [ alignTop, Font.center, width <| Element.fillPortion (String.length x) ] (text x)) materiasStrs
    in
    el
        [ width fill
        , height fill
        , Border.rounded 15
        , Background.color <| Element.rgb255 255 255 255
        ]
        (column
            [ width fill
            , height fill
            , Element.clip

            -- vertical lines on top
            , Element.inFront <|
                row
                    [ Element.spaceEvenly
                    , width fill
                    , Element.paddingXY 50 0
                    , Border.rounded 15
                    , Font.color (Element.rgba 1 0 0 0)
                    , alignTop
                    ]
                    (List.map (\x -> el [ width <| Element.fillPortion (String.length x) ] svgVerticalLine) materiasStrs)

            --List.repeat 6 svgVerticalLine)
            -- (List.intersperse svgVerticalLine topBarItems)
            ]
            -- topBar
            [ row
                [ Element.spaceEvenly
                , width fill
                , Element.paddingXY 50 35
                , Background.color <| Element.rgb255 248 249 255
                , Border.rounded 15
                , Font.color (Element.rgb255 143 164 190)
                ]
                topBarItems
            , column
                [ Element.spaceEvenly
                , height fill
                , spacing 100
                , Element.paddingXY 0 30
                ]
                (List.repeat 7 svgHorizontalLine)
            ]
        )
