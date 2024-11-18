module Model where

-- import Data.Array
-- import Debug (todo)
-- import Html.Events (targetValue)

import Data.Time

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))

-- MODEL

type Model =
  { focusedMateriaName :: String
  , dictMaterias :: Map String Materia
  , dictClases :: Map String Clase
  , warnings :: Warnings
  }

setFocusedMateriaName :: String -> Model -> Model
setFocusedMateriaName newFocusedMateriaName model =
  model { focusedMateriaName = newFocusedMateriaName }

setModelMateria :: Materia -> Model -> Model
setModelMateria materia model =
  let
    newModel =
      setFocusedMateriaName materia.materiaNombre model
  in
    newModel { dictMaterias = Map.insert newModel.focusedMateriaName materia newModel.dictMaterias }

-- WARNING FLAGS
-- used to activate user warnings in the view

type Warnings =
  { emptyNameWhenClickingNewMateria :: Boolean
  , alreadyExistingMateriaRename :: Maybe String
  , alreadyExistingClaseName :: Maybe String
  , generalError :: Maybe String
  }

emptyWarnings :: Warnings
emptyWarnings =
  { emptyNameWhenClickingNewMateria: false
  , alreadyExistingMateriaRename: Nothing
  , alreadyExistingClaseName: Nothing
  , generalError: Nothing
  }

setEmptyNameWhenClickingNewMateria :: Boolean -> Warnings -> Warnings
setEmptyNameWhenClickingNewMateria b warnings =
  warnings { emptyNameWhenClickingNewMateria = b }

setAlreadyExistingMateriaRename :: Maybe String -> Warnings -> Warnings
setAlreadyExistingMateriaRename maybeString warnings =
  warnings { alreadyExistingMateriaRename = maybeString }

setAlreadyExistingClaseName :: Maybe String -> Warnings -> Warnings
setAlreadyExistingClaseName maybeString warnings =
  warnings { alreadyExistingClaseName = maybeString }

setGeneralError :: Maybe String -> Warnings -> Warnings
setGeneralError maybeString warnings =
  warnings { generalError = maybeString }

-- HORARIO
-- horas de inicio y final de clase en determinado Día

type HorarioClase (maybetime :: Type) =
  { inicio :: (String /\ maybetime)
  , final :: (String /\ maybetime)
  }

emptyHorario :: HorarioClase (Maybe Time)
emptyHorario =
  { inicio: ("" /\ Nothing)
  , final: ("" /\ Nothing)
  }

-- MATERIA
-- la info de cada materia
-- a futuro esto se modificará, para que cada materia tenga
-- diferentes clases (clase1 con prof1, clase2 con prof2, etc)

type Materia =
  { materiaNombre :: String
  , materiaClases :: Set String -- String:: Clase.claseId
  }

type Clase =
  { claseId :: String
  , claseProf :: String
  , claseSesiones :: Array Sesion
  }

setClaseSesiones :: Array Sesion -> Clase -> Clase
setClaseSesiones arr c =
  c { claseSesiones = arr }

setClaseSesionInd :: Int -> Sesion -> Clase -> Clase
setClaseSesionInd i s c =
  c { claseSesiones = Array.updateAtIndices [ i /\ s ] c.claseSesiones }

setMateriaNombre :: String -> Materia -> Materia
setMateriaNombre materiaNombre materia =
  materia { materiaNombre = materiaNombre }

type Sesion =
  { dia :: Day, sesionHorario :: HorarioClase (Maybe Time) }

setSesionDia :: Day -> Sesion -> Sesion
setSesionDia d s =
  s { dia = d }

setSesionHorario :: HorarioClase (Maybe Time) -> Sesion -> Sesion
setSesionHorario h s =
  s { sesionHorario = h }

defaultSesion :: Sesion
defaultSesion =
  { dia: Lunes, sesionHorario: { inicio: ("" /\ Nothing), final: ("" /\ Nothing) } }

dayToString :: Day -> String
dayToString x =
  case x of
    Lunes -> "Lunes"
    Martes -> "Martes"
    Miercoles -> "Miercoles"
    Jueves -> "Jueves"
    Viernes -> "Viernes"
    Sabado -> "Sabado"
    Domingo -> "Domingo"

setModelMapClaseInd :: Clase -> Model -> Model
setModelMapClaseInd clase model = model { dictClases = n }
  where
  n = Map.update (\_ -> Just clase) clase.claseId model.dictClases

stringToDay :: String -> Maybe Day
stringToDay x =
  case x of
    "Lunes" -> Just Lunes
    "Martes" -> Just Martes
    "Miercoles" -> Just Miercoles
    "Jueves" -> Just Jueves
    "Viernes" -> Just Viernes
    "Sabado" -> Just Sabado
    "Domingo" -> Just Domingo
    _ -> Nothing

data Day
  = Lunes
  | Martes
  | Miercoles
  | Jueves
  | Viernes
  | Sabado
  | Domingo

emptyMateria :: Materia
emptyMateria =
  { materiaNombre: "", materiaClases: Set.empty }

emptyClase :: Clase
emptyClase =
  { claseId: "", claseProf: "", claseSesiones: [] }

setModelWarnings :: Warnings -> Model -> Model
setModelWarnings warnings model =
  model { warnings = warnings }

setModelMapClases :: Map String Clase -> Model -> Model
setModelMapClases d m =
  m { dictClases = d }
