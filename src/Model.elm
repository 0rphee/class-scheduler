module Model exposing (..)

import Dict exposing (Dict)
import TypedTime exposing (TypedTime)



-- MODEL


type alias Model =
    { focusedMateriaName : String
    , focusedMateria : Materia
    , lMaterias : Dict String Materia
    , warnings : Warnings
    }


setFocusedMateriaName : String -> Model -> Model
setFocusedMateriaName newFocusedMateriaName model =
    { model | focusedMateriaName = newFocusedMateriaName }


setModelFocusedMateria : Materia -> Model -> Model
setModelFocusedMateria newFocusedMateria model =
    { model | focusedMateria = newFocusedMateria }


setModelMaterias : Materia -> Model -> Model
setModelMaterias mat model =
    let
        newModel =
            setModelFocusedMateria mat model
    in
    { newModel | lMaterias = Dict.insert newModel.focusedMateriaName newModel.focusedMateria newModel.lMaterias }



-- WARNING FLAGS
-- used to activate user warnings in the view


type alias Warnings =
    { emptyNameWhenClickingNewMateria : Bool
    , alreadyExistingMateriaRename : Maybe String
    , generalError : Maybe String
    }


emptyWarnings : Warnings
emptyWarnings =
    { emptyNameWhenClickingNewMateria = False
    , alreadyExistingMateriaRename = Nothing
    , generalError = Nothing
    }


setEmptyNameWhenClickingNewMateria : Bool -> Warnings -> Warnings
setEmptyNameWhenClickingNewMateria b warning =
    { warning | emptyNameWhenClickingNewMateria = b }


setAlreadyExistingMateriaRename : Maybe String -> Warnings -> Warnings
setAlreadyExistingMateriaRename maybeString warning =
    { warning | alreadyExistingMateriaRename = maybeString }


setGeneralError : Maybe String -> Warnings -> Warnings
setGeneralError maybeString warning =
    { warning | generalError = maybeString }



-- HORARIO
-- horas de inicio y final de clase en determinado Día


type alias HorarioClase =
    { inicio : ( String, Maybe TypedTime )
    , final : ( String, Maybe TypedTime )
    }


emptyHorario : HorarioClase
emptyHorario =
    { inicio = ( "", Nothing )
    , final = ( "", Nothing )
    }



-- MATERIA
-- la info de cada materia
-- a futuro esto se modificará, para que cada materia tenga
-- diferentes clases (clase1 con prof1, clase2 con prof2, etc)


type alias Materia =
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


emptyMateria : Materia
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


setMateriaId : String -> Materia -> Materia
setMateriaId idMat materia =
    { materia | materiaId = idMat }


setMateriaProf : String -> Materia -> Materia
setMateriaProf prof materia =
    { materia | materiaProf = prof }


setMateriaLunes : HorarioClase -> Materia -> Materia
setMateriaLunes lunes materia =
    { materia | materiaLunes = lunes }


setMateriaMartes : HorarioClase -> Materia -> Materia
setMateriaMartes martes materia =
    { materia | materiaMartes = martes }


setMateriaMiercoles : HorarioClase -> Materia -> Materia
setMateriaMiercoles miercoles materia =
    { materia | materiaMiercoles = miercoles }


setMateriaJueves : HorarioClase -> Materia -> Materia
setMateriaJueves jueves materia =
    { materia | materiaJueves = jueves }


setMateriaViernes : HorarioClase -> Materia -> Materia
setMateriaViernes viernes materia =
    { materia | materiaViernes = viernes }


setMateriaSabado : HorarioClase -> Materia -> Materia
setMateriaSabado sabado materia =
    { materia | materiaSabado = sabado }


setMateriaDomingo : HorarioClase -> Materia -> Materia
setMateriaDomingo domingo materia =
    { materia | materiaDomingo = domingo }


setModelWarnings : Warnings -> Model -> Model
setModelWarnings warnings model =
    { model | warnings = warnings }
