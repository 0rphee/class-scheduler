module Sh exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (HorarioClase)
import Msg exposing (Msg)
import Tuple exposing (first, second)
import TypedTime as T


genHorarioClase : Float -> Float -> HorarioClase T.TypedTime
genHorarioClase ini fin =
    let
        toTup float =
            let
                ttime =
                    T.hours float
            in
            ( T.toString T.Hours ttime, ttime )
    in
    { inicio = toTup ini
    , final = toTup fin
    }


emptyHorario : HorarioClase (Maybe T.TypedTime)
emptyHorario =
    { inicio = ( "", Nothing )
    , final = ( "", Nothing )
    }


mockData : Model.Materia T.TypedTime
mockData =
    { materiaId = "1234"
    , materiaProf = "Bernabe"
    , materiaLunes = [ genHorarioClase 8.5 10, genHorarioClase 10 12, genHorarioClase 17.5 20, genHorarioClase 7 8 ]
    , materiaMartes = []
    , materiaMiercoles = [ genHorarioClase 8.5 10 ]
    , materiaJueves = []
    , materiaViernes = [ genHorarioClase 8.5 10 ]
    , materiaSabado = []
    , materiaDomingo = []
    }


generateHorizontalRows : Float -> Float -> List (Html Msg)
generateHorizontalRows startF endF =
    let
        helper : List (Html Msg) -> Float -> List (Html Msg)
        helper carry next =
            if next >= startF then
                helper
                    (div
                        [ style "position" "relative"
                        , style "height" "1.5rem"
                        , style "top" "-0.5rem"
                        ]
                        [ text (T.toString T.Hours <| T.hours next) ]
                        :: carry
                    )
                    (next - 0.5)

            else
                carry
    in
    helper
        [ div
            [ style "position" "absolute"
            , style "top" "calc(100% - 0.5rem)"
            , style "width" "100%"
            ]
            [ text (T.toString T.Hours <| T.hours endF) ]
        ]
        (endF - 0.5)


renderDay : List ( String, HorarioClase T.TypedTime ) -> List (Html msg)
renderDay xs =
    let
        renderClass : ( String, HorarioClase T.TypedTime ) -> Html msg
        renderClass ( nameStr, { inicio, final } ) =
            let
                start =
                    (\x -> (*) 100 <| T.ratio (T.hours 24) x) (second inicio)

                dif =
                    (\x y -> (*) 100 <| T.ratio (T.hours 24) (T.sub x y)) (second final) (second inicio)
            in
            div
                [ style "position" "absolute"
                , style "height" (String.fromFloat dif ++ "%")
                , style "width" "100%"
                , style "top" (String.fromFloat start ++ "%")
                ]
                [ div
                    [ style "border-radius" "0 15px 15px 0"
                    , style "background-color" "rgb(222, 244, 230)"
                    , style "color" "rgb(91, 172, 116)"
                    , style "display" "flex"
                    , style "height" "98%"
                    , style "width" "95%"
                    , style "font-size" "0.8rem"
                    ]
                    [ div
                        [ style "padding" "8%"
                        , style "max-width" "80%"
                        , style "border-left" "0.2rem solid rgb(91, 172, 116)"
                        ]
                        [ text nameStr
                        , div
                            [ style "overflow" "hidden"
                            , style "text-overflow" "ellipsis"
                            , style "white-space" "nowrap"
                            ]
                            [ text (first inicio ++ " - " ++ first final) ]
                        ]
                    ]
                ]
    in
    List.map renderClass xs


main : Html Msg
main =
    div
        [ style "margin" "2%"
        , style "border-radius" "2%"

        -- , style "border" "solid"
        , style "padding" "1%"
        , style "background-color" "rgb(248, 249, 255)"
        ]
        [ div
            -- HEADER
            [ style "display" "flex"
            ]
            [ div [ style "min-width" "5%" ] [] -- buffer
            , div
                [ style "display" "flex"
                , style "justify-content" "space-around"
                , style "flex-grow" "1"
                ]
                [ div [] [ text "LUN" ]
                , div [] [ text "MAR" ]
                , div [] [ text "MIÉ" ]
                , div [] [ text "JUE" ]
                , div [] [ text "VIE" ]
                , div [] [ text "SÁB" ]
                , div [] [ text "DOM" ]
                ]
            ]
        , div
            -- WEEKLY VIEW
            [ style "display" "flex"
            , style "flex-direction" "row"
            , style "height" "100%"
            , style "padding" "6px 0 0 0"
            ]
            [ div
                -- DATESTAMPS
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "min-width" "5%"
                , style "text-align" "center"
                , style "margin" "0 0.5rem 0 0 "
                , style "position" "relative"
                ]
                (generateHorizontalRows 0 24)
            , div
                -- DAY COLUMNS
                [ style "display" "flex"
                , style "justify-content" "space-evenly"
                , style "flex-grow" "1"
                , style "background-color" "white"
                ]
                (List.repeat 7
                    (div
                        [ style "flex-grow" "1"
                        , style "border-left" "1px solid black"
                        , style "position" "relative"
                        ]
                        (renderDay (List.map (\x -> ( "Cálculo", x )) mockData.materiaLunes))
                    )
                )
            ]
        ]



-- type alias Week =
--     { lu : List (HorarioClase T.TypedTime)
--     , ma : List (HorarioClase T.TypedTime)
--     , mi : List (HorarioClase T.TypedTime)
--     , ju : List (HorarioClase T.TypedTime)
--     , vi : List (HorarioClase T.TypedTime)
--     , sa : List (HorarioClase T.TypedTime)
--     , do : List (HorarioClase T.TypedTime)
--     }
-- renderCalendar : List (Model.Materia T.TypedTime) -> Html Msg
-- renderCalendar m =
--     div
--         [ style "margin" "2%"
--         , style "border-radius" "2%"
--         -- , style "border" "solid"
--         , style "padding" "1%"
--         , style "background-color" "rgb(248, 249, 255)"
--         ]
--         [ div
--             -- HEADER
--             [ style "display" "flex"
--             ]
--             [ div [ style "min-width" "5%" ] [] -- buffer
--             , div
--                 [ style "display" "flex"
--                 , style "justify-content" "space-around"
--                 , style "flex-grow" "1"
--                 ]
--                 [ div [] [ text "LUN" ]
--                 , div [] [ text "MAR" ]
--                 , div [] [ text "MIÉ" ]
--                 , div [] [ text "JUE" ]
--                 , div [] [ text "VIE" ]
--                 , div [] [ text "SÁB" ]
--                 , div [] [ text "DOM" ]
--                 ]
--             ]
--         , div
--             -- WEEKLY VIEW
--             [ style "display" "flex"
--             , style "flex-direction" "row"
--             , style "height" "100%"
--             , style "padding" "6px 0 0 0"
--             ]
--             [ div
--                 -- DATESTAMPS
--                 [ style "display" "flex"
--                 , style "flex-direction" "column"
--                 , style "min-width" "5%"
--                 , style "text-align" "center"
--                 , style "margin" "0 0.5rem 0 0 "
--                 , style "position" "relative"
--                 ]
--                 (generateHorizontalRows 0 24)
--             , div
--                 -- DAY COLUMNS
--                 [ style "display" "flex"
--                 , style "justify-content" "space-evenly"
--                 , style "flex-grow" "1"
--                 , style "background-color" "white"
--                 ]
--                 (List.repeat 7
--                     (div
--                         [ style "flex-grow" "1"
--                         , style "border-left" "1px solid black"
--                         , style "position" "relative"
--                         ]
--                         (List.repeat
--                             1
--                             (renderDay "Cálculo" mockData.materiaLunes)
--                         )
--                     )
--                 )
--             ]
--         ]
