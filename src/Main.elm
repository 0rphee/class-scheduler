module Main exposing (main)

import Browser
import Debug
import Dict exposing (Dict)
import Element exposing (Element, column, el, fill, px, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Tuple exposing (..)
import TypedTime as T exposing (TypedTime)


maybeBind : Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind m f =
    Maybe.andThen f m



{-

   Materia (calculo)):
       - n clases
            - id de clase?
            - profesor
            - horario lunes
            - horario martes
            - horario miercoles
            - horario jueves
            - horario viernes
            - horario sabado
            - horario domingo
-}
-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Warnings =
    { emptyNameWhenClickingNewMateria : Bool
    , alreadyExistingMateriaRename : Maybe String
    }


emptyWarnings : Warnings
emptyWarnings =
    { emptyNameWhenClickingNewMateria = False
    , alreadyExistingMateriaRename = Nothing
    }


setEmptyNameWhenClickingNewMateria : Bool -> Warnings -> Warnings
setEmptyNameWhenClickingNewMateria b warning =
    { warning | emptyNameWhenClickingNewMateria = b }


setAlreadyExistingMateriaRename : Maybe String -> Warnings -> Warnings
setAlreadyExistingMateriaRename b warning =
    { warning | alreadyExistingMateriaRename = b }


type alias Model =
    { focusedMateriaName : String
    , focusedMateria : InstanciaMateria
    , lMaterias : Dict String InstanciaMateria
    , warnings : Warnings
    }


setModelMaterias : InstanciaMateria -> Model -> Model
setModelMaterias mat model =
    let
        newModel =
            { model | focusedMateria = mat }
    in
    { newModel | lMaterias = Dict.insert newModel.focusedMateriaName newModel.focusedMateria newModel.lMaterias }



-- Instancia Materia "POO con iván", "POO con roberto", "PA con sanchez"


type alias HorarioClase =
    { inicio : ( String, Maybe TypedTime )
    , final : ( String, Maybe TypedTime )
    }


emptyHorario : HorarioClase
emptyHorario =
    { inicio = ( "", Nothing )
    , final = ( "", Nothing )
    }


type alias InstanciaMateria =
    { materiaId : String
    , materiaProf : String
    , materiaLunes : HorarioClase
    , materiaMartes : HorarioClase
    , materiaMiercoles : HorarioClase
    , materiaJueves : HorarioClase
    , materiaViernes : HorarioClase
    , materiaSabado : HorarioClase
    , materiaDomingo : HorarioClase
    }


setMateriaId : String -> InstanciaMateria -> InstanciaMateria
setMateriaId idMat materia =
    { materia | materiaId = idMat }


setMateriaProf : String -> InstanciaMateria -> InstanciaMateria
setMateriaProf prof materia =
    { materia | materiaProf = prof }


setMateriaLunes : HorarioClase -> InstanciaMateria -> InstanciaMateria
setMateriaLunes lunes materia =
    { materia | materiaLunes = lunes }


setMateriaMartes : HorarioClase -> InstanciaMateria -> InstanciaMateria
setMateriaMartes martes materia =
    { materia | materiaMartes = martes }


setMateriaMiercoles : HorarioClase -> InstanciaMateria -> InstanciaMateria
setMateriaMiercoles miercoles materia =
    { materia | materiaMiercoles = miercoles }


setMateriaJueves : HorarioClase -> InstanciaMateria -> InstanciaMateria
setMateriaJueves jueves materia =
    { materia | materiaJueves = jueves }


setMateriaViernes : HorarioClase -> InstanciaMateria -> InstanciaMateria
setMateriaViernes viernes materia =
    { materia | materiaViernes = viernes }


setMateriaSabado : HorarioClase -> InstanciaMateria -> InstanciaMateria
setMateriaSabado sabado materia =
    { materia | materiaSabado = sabado }


setMateriaDomingo : HorarioClase -> InstanciaMateria -> InstanciaMateria
setMateriaDomingo domingo materia =
    { materia | materiaDomingo = domingo }


setModelWarnings : Warnings -> Model -> Model
setModelWarnings warnings model =
    { model | warnings = warnings }



-- MSG


type TimeRangeChangeType
    = StartTimeUpdate
    | EndTimeUpdate


type Msg
    = FocusedMateriaNameUpdate String
    | FocusedMateriaIdUpdate String
    | FocusedMateriaProfUpdate String
    | FocusedMateriaLunesUpdate { changeType : TimeRangeChangeType, newStr : String }
    | FocusedMateriaMartesUpdate { changeType : TimeRangeChangeType, newStr : String }
    | FocusedMateriaMiercolesUpdate { changeType : TimeRangeChangeType, newStr : String }
    | FocusedMateriaJuevesUpdate { changeType : TimeRangeChangeType, newStr : String }
    | FocusedMateriaViernesUpdate { changeType : TimeRangeChangeType, newStr : String }
    | FocusedMateriaSabadoUpdate { changeType : TimeRangeChangeType, newStr : String }
    | FocusedMateriaDomingoUpdate { changeType : TimeRangeChangeType, newStr : String }
    | ListaMateriasNewMateria
    | ListaMateriasSelectMateria String



-- INIT


emptyMateria : InstanciaMateria
emptyMateria =
    { materiaId = ""
    , materiaProf = ""
    , materiaLunes = emptyHorario
    , materiaMartes = emptyHorario
    , materiaMiercoles = emptyHorario
    , materiaJueves = emptyHorario
    , materiaViernes = emptyHorario
    , materiaSabado = emptyHorario
    , materiaDomingo = emptyHorario
    }


init : Model
init =
    Model "" emptyMateria (Dict.singleton "" emptyMateria) emptyWarnings



-- UPDATE


strToTypedTime : String -> Maybe TypedTime
strToTypedTime originalHourString =
    maybeBind
        (T.fromString T.Minutes originalHourString)
        (\validTypedTime ->
            if T.gte T.zero validTypedTime && T.lt validTypedTime (T.hours 24) then
                Just validTypedTime

            else
                Nothing
        )


modifyDate : { changeType : TimeRangeChangeType, newStr : String } -> HorarioClase -> HorarioClase
modifyDate change previoHorarioClase =
    let
        strIsValid =
            let
                semicolonNum =
                    List.length <| String.indexes ":" change.newStr
            in
            String.all Char.isDigit (String.replace ":" "" change.newStr) && (semicolonNum <= 1)
    in
    if not strIsValid then
        -- if the input is incorrect, then it reverts back to the previous state
        previoHorarioClase

    else
        case change.changeType of
            StartTimeUpdate ->
                case strToTypedTime change.newStr of
                    Nothing ->
                        { previoHorarioClase | inicio = mapFirst (always change.newStr) previoHorarioClase.inicio }

                    Just typedTime ->
                        { previoHorarioClase | inicio = ( change.newStr, Just typedTime ) }

            EndTimeUpdate ->
                case strToTypedTime change.newStr of
                    Nothing ->
                        { previoHorarioClase | final = mapFirst (always change.newStr) previoHorarioClase.final }

                    Just typedTime ->
                        { previoHorarioClase | final = ( change.newStr, Just typedTime ) }


update : Msg -> Model -> Model
update msg model =
    let
        dayUpdater : { changeType : TimeRangeChangeType, newStr : String } -> (InstanciaMateria -> HorarioClase) -> (HorarioClase -> InstanciaMateria -> InstanciaMateria) -> Model
        dayUpdater newFields getter setter =
            model |> setModelMaterias (model.focusedMateria |> setter (modifyDate newFields (getter model.focusedMateria)))

        textUpdater : (String -> InstanciaMateria -> InstanciaMateria) -> String -> Model
        textUpdater setter newStr =
            model |> setModelMaterias (model.focusedMateria |> setter newStr)

        tryToAddNewMateria : Model -> Model
        tryToAddNewMateria m =
            if not <| Dict.member "" m.lMaterias then
                { focusedMateriaName = ""
                , focusedMateria = emptyMateria
                , lMaterias = Dict.insert "" emptyMateria m.lMaterias
                , warnings = m.warnings |> setEmptyNameWhenClickingNewMateria False
                }

            else
                { m | warnings = m.warnings |> setEmptyNameWhenClickingNewMateria True }
    in
    Debug.log "update" <|
        case msg of
            FocusedMateriaNameUpdate newName ->
                if Dict.member newName model.lMaterias then
                    model
                        |> setModelWarnings
                            (model.warnings
                                |> setAlreadyExistingMateriaRename (Just newName)
                            )

                else
                    -- remove old name of the currently focusedMateria to add it later with setMaterias
                    { model | lMaterias = Dict.remove model.focusedMateriaName model.lMaterias, focusedMateriaName = newName }
                        |> setModelMaterias model.focusedMateria
                        |> setModelWarnings
                            (model.warnings
                                |> setAlreadyExistingMateriaRename Nothing
                            )

            FocusedMateriaIdUpdate newId ->
                textUpdater setMateriaId newId

            FocusedMateriaProfUpdate newProf ->
                textUpdater setMateriaProf newProf

            FocusedMateriaLunesUpdate fieldUpdate ->
                dayUpdater fieldUpdate .materiaLunes setMateriaLunes

            FocusedMateriaMartesUpdate fieldUpdate ->
                dayUpdater fieldUpdate .materiaMartes setMateriaMartes

            FocusedMateriaMiercolesUpdate fieldUpdate ->
                dayUpdater fieldUpdate .materiaMiercoles setMateriaMiercoles

            FocusedMateriaJuevesUpdate fieldUpdate ->
                dayUpdater fieldUpdate .materiaJueves setMateriaJueves

            FocusedMateriaViernesUpdate fieldUpdate ->
                dayUpdater fieldUpdate .materiaViernes setMateriaViernes

            FocusedMateriaSabadoUpdate fieldUpdate ->
                dayUpdater fieldUpdate .materiaSabado setMateriaSabado

            FocusedMateriaDomingoUpdate fieldUpdate ->
                dayUpdater fieldUpdate .materiaDomingo setMateriaDomingo

            ListaMateriasNewMateria ->
                tryToAddNewMateria model

            ListaMateriasSelectMateria _ ->
                Debug.todo "branch 'ListaMateriasSelectMateria _' not implemented"



-- VIEW


view : Model -> Html Msg
view m =
    Element.layout
        [ Font.family
            [ Font.typeface "Arial Rounded MT Bold"
            , Font.sansSerif
            ]
        , Font.size 19
        , Background.color <| Element.rgb255 245 246 250
        ]
        (appUI m)


appUI : Model -> Element Msg
appUI m =
    Element.column
        [ Element.padding 30
        , Element.spacing 10
        , Element.width fill
        , Element.height fill
        ]
        [ title
        , el [] Element.none
        , Element.row [ Element.width Element.fill ]
            [ vistaDeMateria m
            , el [ width <| px 40 ] Element.none
            , listaDeMaterias m
            ]
        ]


title : Element Msg
title =
    Element.paragraph []
        [ el [ Font.bold, Font.size 45 ] <| text "Class-Scheduler"
        , el [ Font.extraLight ] <| text " obtén el horario perfecto para tu próximo semestre"
        ]


materiaInfoStyle : List (Element.Attribute msg)
materiaInfoStyle =
    [ Border.rounded 15
    , Border.width 0
    , Background.color <| Element.rgb255 248 249 255
    , Element.width <| Element.fill
    ]


materiaTextInfoInput : { onChange : String -> msg, text : String, placeholder : Maybe (Input.Placeholder msg), label : Input.Label msg } -> Element msg
materiaTextInfoInput =
    Input.text ((Element.htmlAttribute <| Html.Attributes.maxlength 60) :: materiaInfoStyle)


dayInputEl : ({ changeType : TimeRangeChangeType, newStr : String } -> Msg) -> HorarioClase -> String -> Element Msg
dayInputEl msgConstructor horario dayStr =
    let
        constructorHelper constructor changeT nStr =
            constructor
                { changeType = changeT, newStr = nStr }
    in
    column [ Element.spacing 20 ]
        [ el [ Element.alignLeft, Font.size 22 ] (text dayStr)
        , row
            [ Border.rounded 15
            , Border.width 0
            , Element.width <| Element.minimum 20 Element.fill
            , Element.spacing 40
            , Font.size 17
            ]
            [ materiaDayInput
                { onChange = constructorHelper msgConstructor <| StartTimeUpdate
                , text = first horario.inicio
                , placeholder = Just (Input.placeholder [] <| text "10:00")
                , label = Input.labelAbove [] (text <| "Hora de inicio")
                }
            , materiaDayInput
                { onChange = constructorHelper msgConstructor <| EndTimeUpdate
                , text = first horario.final
                , placeholder = Just (Input.placeholder [] <| text "11:30")
                , label = Input.labelAbove [] (text <| "Hora de término")
                }
            ]
        ]


materiaDayInput : { onChange : String -> msg, text : String, placeholder : Maybe (Input.Placeholder msg), label : Input.Label msg } -> Element msg
materiaDayInput =
    Input.text ((Element.htmlAttribute <| Html.Attributes.maxlength 5) :: materiaInfoStyle)


vistaDeMateria : Model -> Element Msg
vistaDeMateria m =
    el
        [ Element.width (Element.fillPortion 3)
        , Border.rounded 15
        , Element.padding 15
        , Background.color <| Element.rgb255 255 255 255
        ]
        (column [ Element.spacing 10, Element.width <| Element.fill ]
            [ materiaTextInfoInput
                { onChange = FocusedMateriaNameUpdate -- : String -> Msg
                , text = m.focusedMateriaName -- : String
                , placeholder = Just (Input.placeholder [] <| text "nombre de materia")
                , label = Input.labelLeft [ Font.bold, Font.size 30 ] (text "Materia") -- : Label Msg
                }
            , materiaTextInfoInput
                { onChange = FocusedMateriaIdUpdate
                , text = m.focusedMateria.materiaId
                , placeholder = Just (Input.placeholder [] <| text "12309")
                , label = Input.labelAbove [ Font.extraLight, Font.size 22 ] (text "ID de la clase")
                }
            , materiaTextInfoInput
                { onChange = FocusedMateriaProfUpdate
                , text = m.focusedMateria.materiaProf
                , placeholder = Just (Input.placeholder [] <| text "nombre del profesor")
                , label = Input.labelAbove [ Font.extraLight, Font.size 22 ] (text "Profesor")
                }
            , dayInputEl FocusedMateriaLunesUpdate m.focusedMateria.materiaLunes "Lunes"
            , dayInputEl FocusedMateriaMartesUpdate m.focusedMateria.materiaMartes "Martes"
            , dayInputEl FocusedMateriaMiercolesUpdate m.focusedMateria.materiaMiercoles "Miércoles"
            , dayInputEl FocusedMateriaJuevesUpdate m.focusedMateria.materiaJueves "Jueves"
            , dayInputEl FocusedMateriaViernesUpdate m.focusedMateria.materiaViernes "Viernes"
            , dayInputEl FocusedMateriaSabadoUpdate m.focusedMateria.materiaSabado "Sábado"
            , dayInputEl FocusedMateriaDomingoUpdate m.focusedMateria.materiaDomingo "Domingo"
            ]
        )


botonNuevaMateria : Element Msg
botonNuevaMateria =
    Input.button
        [ Element.width Element.fill
        , Border.rounded 15
        , Element.padding 10
        , Background.color <| Element.rgb255 248 249 255
        , Element.spacing 15
        , Element.mouseOver [ Background.color <| Element.rgb255 214 217 222 ]
        ]
        { onPress = Just ListaMateriasNewMateria
        , label = Element.row [] [ el [ Font.bold, Font.size 35 ] (text "+"), el [ Element.centerY ] (text " Nueva Materia") ]
        }


botonMateria : String -> Element Msg
botonMateria materiaNameStr =
    Input.button
        [ Element.width Element.fill
        , Border.rounded 15
        , Element.padding 10
        , Background.color <| Element.rgb255 248 249 255
        , Element.spacing 15
        , Element.mouseOver [ Background.color <| Element.rgb255 214 217 222 ]
        ]
        { onPress = Just <| ListaMateriasSelectMateria materiaNameStr
        , label = el [] <| text materiaNameStr
        }


listaDeMaterias : Model -> Element Msg
listaDeMaterias model =
    let
        botonesMaterias : List (Element Msg)
        botonesMaterias =
            List.map
                (\matName ->
                    if matName /= "" then
                        botonMateria matName

                    else
                        Element.none
                )
                (Dict.keys model.lMaterias)

        newMateriaWarning : Element Msg
        newMateriaWarning =
            if model.warnings.emptyNameWhenClickingNewMateria then
                el [ Font.size 18, Font.color <| Element.rgb255 219 51 53 ] (text "\nCambia el nombre de la materia\nactual antes de crear una nueva")

            else
                Element.none

        renameToExistingMateriaNameWarning : Element Msg
        renameToExistingMateriaNameWarning =
            case model.warnings.alreadyExistingMateriaRename of
                Just alreadyExistingName ->
                    el [ Font.size 18, Font.color <| Element.rgb255 219 51 53 ]
                        (text <| "\nYa existe una materia llamada\n" ++ alreadyExistingName)

                Nothing ->
                    Element.none
    in
    Element.column
        [ Element.width (Element.fillPortion 1 |> Element.maximum 550) -- TODO BETTER MAXIMUM
        , Element.spacing 15
        , Element.alignTop
        ]
        [ Element.textColumn [ Element.width Element.shrink ]
            [ el [ Font.bold, Font.size 22 ] (text "Materias")
            , newMateriaWarning
            , renameToExistingMateriaNameWarning
            ]
        , Element.column
            [ Element.width <| Element.fill
            , Border.rounded 15
            , Element.padding 15
            , Background.color <| Element.rgb255 255 255 255
            , Element.spacing 15
            ]
            (botonNuevaMateria
                :: botonesMaterias
            )
        ]
