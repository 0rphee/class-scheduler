module Model exposing (..)

import Array exposing (Array)
import Debug exposing (todo)
import Dict exposing (Dict)
import Html.Events exposing (targetValue)
import Set exposing (Set)
import TypedTime exposing (TypedTime)



-- MODEL


type alias Model =
    { focusedMateriaName : String
    , dictMaterias : Dict String Materia
    , dictClases : Dict String Clase
    , warnings : Warnings
    }


setFocusedMateriaName : String -> Model -> Model
setFocusedMateriaName newFocusedMateriaName model =
    { model | focusedMateriaName = newFocusedMateriaName }


setModelMateria : Materia -> Model -> Model
setModelMateria materia model =
    let
        newModel =
            setFocusedMateriaName materia.materiaNombre model
    in
    { newModel | dictMaterias = Dict.insert newModel.focusedMateriaName materia newModel.dictMaterias }



-- WARNING FLAGS
-- used to activate user warnings in the view


type alias Warnings =
    { emptyNameWhenClickingNewMateria : Bool
    , alreadyExistingMateriaRename : Maybe String
    , alreadyExistingClaseName : Maybe String
    , generalError : Maybe String
    }


emptyWarnings : Warnings
emptyWarnings =
    { emptyNameWhenClickingNewMateria = False
    , alreadyExistingMateriaRename = Nothing
    , alreadyExistingClaseName = Nothing
    , generalError = Nothing
    }


setEmptyNameWhenClickingNewMateria : Bool -> Warnings -> Warnings
setEmptyNameWhenClickingNewMateria b warnings =
    { warnings | emptyNameWhenClickingNewMateria = b }


setAlreadyExistingMateriaRename : Maybe String -> Warnings -> Warnings
setAlreadyExistingMateriaRename maybeString warnings =
    { warnings | alreadyExistingMateriaRename = maybeString }


setAlreadyExistingClaseName : Maybe String -> Warnings -> Warnings
setAlreadyExistingClaseName maybeString warnings =
    { warnings | alreadyExistingClaseName = maybeString }


setGeneralError : Maybe String -> Warnings -> Warnings
setGeneralError maybeString warning =
    { warning | generalError = maybeString }



-- HORARIO
-- horas de inicio y final de clase en determinado Día


type alias HorarioClase maybetime =
    { inicio : ( String, maybetime )
    , final : ( String, maybetime )
    }


emptyHorario : HorarioClase (Maybe TypedTime)
emptyHorario =
    { inicio = ( "", Nothing )
    , final = ( "", Nothing )
    }



-- MATERIA
-- la info de cada materia
-- a futuro esto se modificará, para que cada materia tenga
-- diferentes clases (clase1 con prof1, clase2 con prof2, etc)


type alias Materia =
    { materiaNombre : String
    , materiaClases : Set String -- String: Clase.claseId
    }


type alias Clase =
    { claseId : String
    , claseProf : String
    , claseSesiones : Array Sesion
    }


setClaseSesiones : Array Sesion -> Clase -> Clase
setClaseSesiones arr c =
    { c | claseSesiones = arr }


setClaseSesionInd : Int -> Sesion -> Clase -> Clase
setClaseSesionInd i s c =
    { c | claseSesiones = Array.set i s c.claseSesiones }


setMateriaNombre : String -> Materia -> Materia
setMateriaNombre materiaNombre materia =
    { materia | materiaNombre = materiaNombre }


type alias Sesion =
    { dia : Day, sesionHorario : HorarioClase (Maybe TypedTime) }


setSesionDia : Day -> Sesion -> Sesion
setSesionDia d s =
    { s | dia = d }


setSesionHorario : HorarioClase (Maybe TypedTime) -> Sesion -> Sesion
setSesionHorario h s =
    { s | sesionHorario = h }


defaultSesion : Sesion
defaultSesion =
    { dia = Lunes, sesionHorario = { inicio = ( "", Nothing ), final = ( "", Nothing ) } }


dayToString : Day -> String
dayToString x =
    case x of
        Lunes ->
            "Lunes"

        Martes ->
            "Martes"

        Miercoles ->
            "Miercoles"

        Jueves ->
            "Jueves"

        Viernes ->
            "Viernes"

        Sabado ->
            "Sabado"

        Domingo ->
            "Domingo"


setModelDictClaseInd : Clase -> Model -> Model
setModelDictClaseInd clase model =
    { model | dictClases = Dict.update clase.claseId (\_ -> Just clase) model.dictClases }


stringToDay : String -> Maybe Day
stringToDay x =
    case x of
        "Lunes" ->
            Just Lunes

        "Martes" ->
            Just
                Martes

        "Miercoles" ->
            Just
                Miercoles

        "Jueves" ->
            Just
                Jueves

        "Viernes" ->
            Just
                Viernes

        "Sabado" ->
            Just
                Sabado

        "Domingo" ->
            Just
                Domingo

        _ ->
            Nothing


type Day
    = Lunes
    | Martes
    | Miercoles
    | Jueves
    | Viernes
    | Sabado
    | Domingo


emptyMateria : Materia
emptyMateria =
    { materiaNombre = "", materiaClases = Set.empty }


emptyClase : Clase
emptyClase =
    { claseId = "", claseProf = "", claseSesiones = Array.empty }


setModelWarnings : Warnings -> Model -> Model
setModelWarnings warnings model =
    { model | warnings = warnings }


setModelDictClases : Dict String Clase -> Model -> Model
setModelDictClases d m =
    { m | dictClases = d }
