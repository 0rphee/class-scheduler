module Main2 exposing (main)

import Array
import Browser
import Debug exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Msg exposing (Msg(..), TimeRangeChangeType(..))
import Set exposing (Set)
import Tuple exposing (mapFirst)
import TypedTime as T exposing (TypedTime)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    log "init"
        { focusedMateriaName = ""
        , dictMaterias = Dict.singleton "" emptyMateria
        , dictClases = Dict.empty
        , warnings = emptyWarnings
        }


view : Model -> Html Msg
view m =
    div
        [ style "font-family" "sans-serif"
        ]
        [ node "style" [ type_ "text/css" ] [ text "@import url(/styles/style.css)" ]
        , div []
            [ div
                [ class "header" ]
                [ h1
                    []
                    [ text "Class scheduler", span [] [ text "Crea tus horarios" ] ]
                ]
            , div
                [ style "border-radius" "2rem"

                -- , style "background-color" "grey"
                , style "margin" "2em 6%"
                , style "height" "100%"
                ]
                [ div
                    [ style "display" "flex"
                    , style "flex-direction" "row"
                    , style "gap" "4%"
                    ]
                    [ vistaDeMateria m
                    , listaDeMaterias m
                    ]
                ]
            ]
        ]


listaDeMaterias : Model -> Html Msg
listaDeMaterias model =
    let
        botonesMaterias : List (Html Msg)
        botonesMaterias =
            List.map
                (\matName -> botonSelectMateria matName)
                (Dict.keys model.dictMaterias)

        warningDiv : String -> Html Msg
        warningDiv s =
            div
                [ class "warning-div"
                ]
                [ text s ]

        newMateriaWarning : Html Msg
        newMateriaWarning =
            if model.warnings.emptyNameWhenClickingNewMateria then
                warningDiv "Cambia el nombre de la materia actual antes de crear una nueva"

            else
                text ""

        renameToExistingMateriaNameWarning : Html Msg
        renameToExistingMateriaNameWarning =
            case model.warnings.alreadyExistingMateriaRename of
                Just alreadyExistingName ->
                    warningDiv <| "\nYa existe una materia llamada\n'" ++ alreadyExistingName ++ "'"

                Nothing ->
                    text ""

        alreadyExistingClaseNameWarning : Html Msg
        alreadyExistingClaseNameWarning =
            case model.warnings.alreadyExistingClaseName of
                Just alreadyExistingClaseId ->
                    warningDiv <| "\nYa existe una clase con el ID\n'" ++ alreadyExistingClaseId ++ "'"

                Nothing ->
                    text ""

        generalErrorWarning : Html Msg
        generalErrorWarning =
            case model.warnings.generalError of
                Just errorMsg ->
                    warningDiv errorMsg

                Nothing ->
                    text ""
    in
    div
        [ class "lista-materias" ]
        [ div
            []
            [ span [ style "font-weight" "bold" ] [ text "Materias" ]
            , newMateriaWarning
            , renameToExistingMateriaNameWarning
            , alreadyExistingClaseNameWarning
            , generalErrorWarning
            ]
        , div
            [ style "border-radius" "15px"
            , style "gap" "15px"
            , style "display" "flex"
            , style "flex-direction" "column"
            ]
            (botonNuevaMateria :: botonesMaterias)
        ]


botonNuevaMateria : Html Msg
botonNuevaMateria =
    button
        [ onClick NewMateria
        ]
        [ span [ style "font-weight" "bold", style "font-size" "1.5rem" ] [ text "+" ]
        , span [ style "text-align" "center" ] [ text " Nueva Materia" ]
        ]


botonSelectMateria : String -> Html Msg
botonSelectMateria materiaNameStr =
    let
        buttonContent =
            if String.isEmpty materiaNameStr then
                em [] [ text "Materia sin nombre" ]

            else
                text materiaNameStr
    in
    button
        [ onClick <| ListaMateriasSelectMateria materiaNameStr
        ]
        [ buttonContent ]


vistaDeMateria : Model -> Html Msg
vistaDeMateria m =
    let
        focusedMateria =
            Maybe.withDefault emptyMateria <|
                Dict.get m.focusedMateriaName m.dictMaterias
    in
    div
        [ class "vista-materia"
        ]
        [ textInput
            { onChange = FocusedMateriaNameUpdate -- : String -> Msg
            , text = m.focusedMateriaName
            , placeholder = "nombre de materia"
            , label = span [ style "font-weight" "bold", style "font-size" "30" ] [ text "Materia" ] -- : Label Msg
            }
        , button
            [ onClick NewClase
            ]
            [ span [ style "font-weight" "bold" ] [ text "+" ]
            , text " Nueva opción de clase para esta materia"
            ]
        , div []
            (focusedMateria.materiaClases
                |> Set.toList
                |> List.map (\x -> vistaDeClase x m.dictClases)
            )
        ]


vistaDeClase : String -> Dict String Clase -> Html Msg
vistaDeClase idClase dictClases =
    let
        clase =
            Maybe.withDefault
                emptyClase
                (Dict.get idClase dictClases)
    in
    div []
        [ div
            [ class "vista-clase"
            ]
            [ textInput
                { onChange = \newClaseId -> FocusedMateriaClaseIdUpdate { oldClaseId = idClase, newClaseId = newClaseId }
                , text = clase.claseId
                , placeholder = "1234"
                , label = span [ class "label-light-span" ] [ text "ID de la clase" ]
                }
            , textInput
                { onChange = \newProfName -> FocusedMateriaClaseProfUpdate { claseId = idClase, newProfName = newProfName }
                , text = clase.claseProf
                , placeholder = "nombre del profesor"
                , label = span [ class "label-light-span" ] [ text "Profesor" ]
                }
            , button
                [ onClick <| NewSesion { idClase = idClase }
                ]
                [ span [ style "font-weight" "bold" ] [ text "+" ]
                , text " Nueva sesión"
                ]
            ]
        , div [] <| Array.toList <| Array.indexedMap (\i sesion -> vistaSesion idClase i sesion) clase.claseSesiones
        ]


vistaSesion : String -> Int -> Sesion -> Html Msg
vistaSesion idClase indexSesion sesion =
    let
        constrMsg ty newStr =
            UpdateSesionTime { idClase = idClase, indexSesion = indexSesion, changeType = ty, newStr = newStr }

        materiaDayInput : { onChange : String -> msg, text : String, placeholder : String, label : Html msg } -> Html msg
        materiaDayInput x =
            Html.label []
                [ x.label
                , Html.input [ placeholder x.placeholder, value x.text, onInput x.onChange, maxlength 5 ] []
                ]

        inputHelper : String -> Msg
        inputHelper dia =
            let
                val =
                    Maybe.withDefault Lunes (stringToDay dia)
            in
            UpdateSesionDay { idClase = idClase, indexSesion = indexSesion, newDay = val }
    in
    div
        -- column
        [ class "vista-sesion"
        ]
        [ select
            [ value <| dayToString sesion.dia
            , onInput <| inputHelper
            ]
          <|
            List.map (\day -> option [] [ text <| dayToString day ])
                [ Lunes, Martes, Miercoles, Jueves, Viernes, Sabado, Domingo ]
        , div
            -- row
            []
            [ materiaDayInput
                { onChange = constrMsg StartTimeUpdate
                , text = Tuple.first sesion.sesionHorario.inicio
                , placeholder = "10:00"
                , label = span [ class "label-light-span" ] [ text "Hora de inicio" ]
                }
            , materiaDayInput
                { onChange = constrMsg EndTimeUpdate
                , text = Tuple.first sesion.sesionHorario.final
                , placeholder = "11:30"
                , label = span [ class "label-light-span" ] [ text "Hora de término" ]
                }
            ]
        ]


textInput : { onChange : String -> msg, text : String, placeholder : String, label : Html msg } -> Html msg
textInput x =
    Html.label
        []
        [ x.label
        , Html.input [ placeholder x.placeholder, value x.text, onInput x.onChange, maxlength 60 ] []
        ]


strToTypedTime : String -> Maybe TypedTime
strToTypedTime originalHourString =
    Maybe.andThen
        (\validTypedTime ->
            if T.lte T.zero validTypedTime && T.lt validTypedTime (T.hours 24) then
                Just validTypedTime

            else
                Nothing
        )
        (T.fromString T.Minutes originalHourString)


modifyDate : { changeType : TimeRangeChangeType, newStr : String } -> HorarioClase (Maybe TypedTime) -> HorarioClase (Maybe TypedTime)
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


changeMateriaClaseIds : String -> String -> Materia -> Dict String Clase -> Maybe ( Materia, Dict String Clase )
changeMateriaClaseIds oldId newId mat oldDictClases =
    if Dict.member newId oldDictClases then
        Nothing

    else
        let
            newClase : Clase
            newClase =
                (case Dict.get oldId oldDictClases of
                    Nothing ->
                        todo "oldClase"

                    Just x ->
                        x
                )
                    |> (\x -> { x | claseId = newId })

            newSet : Set String
            newSet =
                mat.materiaClases
                    |> Set.remove oldId
                    |> Set.insert newId

            newDict : Dict String Clase
            newDict =
                oldDictClases
                    |> Dict.remove oldId
                    |> Dict.insert newId newClase

            newMateria : Materia
            newMateria =
                { mat | materiaClases = newSet }
        in
        Just <| ( newMateria, newDict )


update : Msg -> Model -> Model
update msg model =
    let
        mym =
            log "model" model

        getClase : String -> Model -> Clase
        getClase idClase mm =
            case Dict.get idClase mm.dictClases of
                Nothing ->
                    Debug.todo "error!"

                Just y ->
                    y

        getSesion : Int -> Clase -> Sesion
        getSesion arrIndex c =
            case Array.get arrIndex c.claseSesiones of
                Nothing ->
                    Debug.todo "error!"

                Just y ->
                    y

        focusedMateria =
            Maybe.withDefault
                --(todo "didnt find focusedmateria")
                emptyMateria
                (Dict.get model.focusedMateriaName model.dictMaterias)

        dayUpdater : { changeType : TimeRangeChangeType, newStr : String } -> (Materia -> HorarioClase (Maybe TypedTime)) -> (HorarioClase (Maybe TypedTime) -> Materia -> Materia) -> Model
        dayUpdater newFields getter setter =
            model |> setModelMateria (focusedMateria |> setter (modifyDate newFields (getter focusedMateria)))

        materiaStringUpdater : (String -> Materia -> Materia) -> String -> Model
        materiaStringUpdater setter newStr =
            model |> setModelMateria (focusedMateria |> setter newStr)

        tryToAddNewMateria : Model -> Model
        tryToAddNewMateria m =
            if not <| Dict.member "" m.dictMaterias then
                { focusedMateriaName = ""
                , dictMaterias = Dict.insert "" emptyMateria m.dictMaterias
                , dictClases = m.dictClases
                , warnings = m.warnings |> setEmptyNameWhenClickingNewMateria False
                }

            else
                { m | warnings = m.warnings |> setEmptyNameWhenClickingNewMateria True }

        tryToAddNewClase : Model -> Model
        tryToAddNewClase m =
            if not <| Dict.member "" m.dictClases then
                let
                    newFocusedMateria =
                        { focusedMateria | materiaClases = Set.insert "" focusedMateria.materiaClases }
                in
                { m
                    | dictClases = Dict.insert "" emptyClase model.dictClases
                    , dictMaterias = Dict.insert newFocusedMateria.materiaNombre newFocusedMateria model.dictMaterias
                    , warnings = m.warnings |> setAlreadyExistingClaseName Nothing
                }

            else
                { m | warnings = m.warnings |> setAlreadyExistingClaseName (Just "") }
    in
    case msg of
        FocusedMateriaNameUpdate newName ->
            if Dict.member newName model.dictMaterias then
                model
                    |> setModelWarnings
                        (model.warnings
                            |> setAlreadyExistingMateriaRename (Just newName)
                        )

            else
                let
                    focusedMateriaWithUpdatedName =
                        { focusedMateria | materiaNombre = newName }
                in
                -- remove old name of the currently focusedMateria to add it later with setMaterias
                { model | dictMaterias = Dict.remove model.focusedMateriaName model.dictMaterias, focusedMateriaName = focusedMateriaWithUpdatedName.materiaNombre }
                    |> setModelMateria focusedMateriaWithUpdatedName
                    |> setModelWarnings
                        (model.warnings
                            |> setAlreadyExistingMateriaRename Nothing
                        )

        FocusedMateriaClaseProfUpdate { claseId, newProfName } ->
            let
                newDictClases =
                    model.dictClases
                        |> Dict.update claseId
                            (Maybe.map (\clase -> { clase | claseProf = newProfName }))
            in
            { model | dictClases = newDictClases }

        FocusedMateriaClaseIdUpdate { oldClaseId, newClaseId } ->
            let
                ( newFocusedMateria, newDictClases ) =
                    case changeMateriaClaseIds oldClaseId newClaseId focusedMateria model.dictClases of
                        Nothing ->
                            todo "claseid repetido"

                        Just y ->
                            y

                newDictMaterias =
                    Dict.insert newFocusedMateria.materiaNombre newFocusedMateria model.dictMaterias
            in
            { model | dictMaterias = newDictMaterias, dictClases = newDictClases }

        NewMateria ->
            tryToAddNewMateria model

        ListaMateriasSelectMateria newFocusedMateriaName ->
            model
                |> setFocusedMateriaName newFocusedMateriaName
                |> setModelWarnings
                    (model.warnings
                        |> setGeneralError
                            Nothing
                    )

        NewClase ->
            tryToAddNewClase model

        NewSesion { idClase } ->
            let
                clase =
                    getClase idClase model

                newClase =
                    clase
                        |> setClaseSesiones
                            (Array.push defaultSesion clase.claseSesiones)
            in
            model
                |> setModelDictClaseInd newClase

        UpdateSesionDay { idClase, indexSesion, newDay } ->
            let
                clase =
                    getClase idClase model

                newSesion =
                    getSesion indexSesion clase
                        |> setSesionDia newDay

                newClase =
                    clase
                        |> setClaseSesionInd indexSesion newSesion
            in
            model
                |> setModelDictClaseInd newClase

        UpdateSesionTime { idClase, indexSesion, changeType, newStr } ->
            let
                clase =
                    getClase idClase model

                sesion =
                    getSesion indexSesion clase

                newTime =
                    sesion.sesionHorario
                        |> modifyDate { changeType = changeType, newStr = newStr }

                newSesion =
                    sesion
                        |> setSesionHorario newTime

                newClase =
                    clase
                        |> setClaseSesionInd indexSesion newSesion
            in
            model
                |> setModelDictClaseInd newClase
