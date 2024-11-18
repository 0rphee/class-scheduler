module Model where

import Prelude

import Control.Monad.RWS (modify)
import Control.Monad.State as State
import Data.Array as Array
import Data.Foldable as Foldable
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.Time (Time)
import Data.Time as T
import Data.Time.Duration as TDU
import Data.Traversable (for_)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Debug (spy)
import Unsafe.Coerce (unsafeCoerce)

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

isAnyWarningActive :: Warnings -> Boolean
isAnyWarningActive w = w.emptyNameWhenClickingNewMateria || Foldable.any (Maybe.isJust)
  [ w.alreadyExistingMateriaRename
  , w.alreadyExistingClaseName
  , w.generalError
  ]

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

type CalendarSesionInfo =
  { matNombre :: String
  , prof :: String
  , id :: String
  , inicio :: TDU.Hours -- T.Time
  , final :: TDU.Hours -- T.Time
  }

type CalendarModel =
  { lun :: Array CalendarSesionInfo
  , mar :: Array CalendarSesionInfo
  , mie :: Array CalendarSesionInfo
  , jue :: Array CalendarSesionInfo
  , vie :: Array CalendarSesionInfo
  , sab :: Array CalendarSesionInfo
  , dom :: Array CalendarSesionInfo
  }

emptyCalendarModel :: CalendarModel
emptyCalendarModel =
  { lun: [], mar: [], mie: [], jue: [], vie: [], sab: [], dom: [] }

timeToHours :: T.Time -> TDU.Hours
timeToHours t = T.diff t bottom

toCalendar :: Model -> CalendarModel
toCalendar m = Tuple.snd $ State.runState helper emptyCalendarModel
  where
  helper :: State.State CalendarModel Unit
  helper =
    for_ m.dictMaterias $ \el -> do
      let matName = spy "cal- matnom" el.materiaNombre
      let matClasesIdArr = spy "cal- matclasesidarr" $ Array.fromFoldable el.materiaClases
      for_ matClasesIdArr $ \claseId -> do
        case Map.lookup claseId m.dictClases of
          Nothing -> pure unit
          Just clase -> do
            let claseProf = spy "cal- claseprof" $ clase.claseProf
            for_ (spy "cal-sesiones" clase.claseSesiones) $ \sesion -> do
              let (getter /\ setter) = funDia sesion.dia
              let { inicio: (_ /\ itime), final: (_ /\ ftime) } = spy "cal sesionhorario" $ sesion.sesionHorario
              case spy "cal itime" itime /\ ftime of
                (Just i /\ Just f) -> do
                  let
                    newSesionInfItem =
                      { matNombre: matName
                      , prof: claseProf
                      , id: claseId
                      , inicio: timeToHours i
                      , final: timeToHours f
                      }
                  prevSt <- State.get
                  let oldDia = getter prevSt
                  let newDia = Array.snoc oldDia newSesionInfItem
                  let newSt = setter newDia prevSt
                  State.put $ newSt
                _ -> pure unit
  funDia d = case d of
    Lunes -> _.lun /\ (\v -> _ { lun = v })
    Martes -> _.mar /\ (\v -> _ { mar = v })
    Miercoles -> _.mie /\ (\v -> _ { mie = v })
    Jueves -> _.jue /\ (\v -> _ { jue = v })
    Viernes -> _.vie /\ (\v -> _ { vie = v })
    Sabado -> _.sab /\ (\v -> _ { sab = v })
    Domingo -> _.dom /\ (\v -> _ { dom = v })
