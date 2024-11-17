module Main (main) where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Bounded as T
import Data.CodePoint.Unicode as U
import Data.Enum (toEnum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.CodePoints as CodePoints
import Data.String.CodeUnits as CodeUnits
import Data.Time (Time(..)) as Time
import Data.Time (Time)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Debug (spy)
import Effect (Effect)
import Elmish (ReactElement, Transition, Dispatch, (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML as H
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as HS
import Model (Clase, Day(..), HorarioClase, Materia, Model, Sesion, dayToString, defaultSesion, emptyClase, emptyMateria, emptyWarnings, setAlreadyExistingClaseName, setAlreadyExistingMateriaRename, setClaseSesionInd, setClaseSesiones, setEmptyNameWhenClickingNewMateria, setFocusedMateriaName, setGeneralError, setModelMapClaseInd, setModelMateria, setModelWarnings, setSesionDia, setSesionHorario, stringToDay)
import Msg (Msg(..), TimeRangeChangeType(..))
import Unsafe.Coerce (unsafeCoerce)

log :: forall a. String -> a -> a
log = spy

main :: Effect Unit
main = defaultMain { def: { init, view, update }, elementId: "app" }

init :: Transition Msg Model
init = pure
  -- log "init"
  { focusedMateriaName: ""
  , dictMaterias: Map.singleton "" emptyMateria
  , dictClases: Map.empty
  , warnings: emptyWarnings
  }

view :: Model -> Dispatch Msg -> ReactElement
view m dispatch =
  H.div {}
    ( H.div {}
        [ HS.div
            "header"
            [ H.h1 {}
                [ HS.span "h1-name" (HS.text "Class scheduler"), HS.span "h1-extra" (HS.text "Crea tus horarios") ]
            ]
        , H.div
            { style: HS.css
                { "borderRadius": "2rem"
                -- , "backgroundColor": "grey"
                , "margin": "2em 6%"
                , "height": "100%"
                }
            }
            ( H.div
                { style: H.css
                    { "display": "flex"
                    , "flexDirection": "row"
                    , "gap": "4%"
                    }
                }
                [ vistaDeMateria dispatch m
                , listaDeMaterias dispatch m
                ]
            )
        ]
    )

listaDeMaterias :: Dispatch Msg -> Model -> ReactElement
listaDeMaterias dispatch model =
  let
    botonesMaterias :: Array (ReactElement)
    botonesMaterias =
      (\matName -> botonSelectMateria dispatch matName) <$>
        (Array.fromFoldable $ Map.keys model.dictMaterias)

    warningDiv :: String -> ReactElement
    warningDiv s = HS.div "warning-div" (HS.text s)

    newMateriaWarning :: ReactElement
    newMateriaWarning =
      if model.warnings.emptyNameWhenClickingNewMateria then
        warningDiv "Cambia el nombre de la materia actual antes de crear una nueva"
      else
        HS.text ""

    renameToExistingMateriaNameWarning :: ReactElement
    renameToExistingMateriaNameWarning =
      case model.warnings.alreadyExistingMateriaRename of
        Just alreadyExistingName ->
          warningDiv $ "\nYa existe una materia llamada\n'" <> alreadyExistingName <> "'"
        Nothing -> HS.text ""

    alreadyExistingClaseNameWarning :: ReactElement
    alreadyExistingClaseNameWarning =
      case model.warnings.alreadyExistingClaseName of
        Just alreadyExistingClaseId ->
          warningDiv $ "\nYa existe una clase con el ID\n'" <> id <> "'"
          where
          id =
            if String.null alreadyExistingClaseId then "<vacío>"
            else alreadyExistingClaseId
        Nothing -> HS.text ""

    generalErrorWarning :: ReactElement
    generalErrorWarning =
      case model.warnings.generalError of
        Just errorMsg -> warningDiv errorMsg
        Nothing -> HS.text ""
  in
    HS.div
      "lista-materias"
      [ H.div {}
          [ H.span { style: H.css { "fontWeight": "bold" } } (HS.text "Materias")
          , newMateriaWarning
          , renameToExistingMateriaNameWarning
          , alreadyExistingClaseNameWarning
          , generalErrorWarning
          ]
      , H.div
          { style: H.css
              { "borderRadius": "15px"
              , "gap": "15px"
              , "display": "flex"
              , "flexDirection": "column"
              }
          }
          (botonNuevaMateria dispatch `Array.cons` (Array.fromFoldable botonesMaterias))
      ]

botonNuevaMateria :: Dispatch Msg -> ReactElement
botonNuevaMateria dispatch =
  HS.button_ "btn-important"
    { onClick: dispatch <| NewMateria }
    (HS.text "+ Nueva Materia")

botonSelectMateria :: Dispatch Msg -> String -> ReactElement
botonSelectMateria dispatch materiaNameStr =
  H.button
    { onClick: dispatch <| ListaMateriasSelectMateria materiaNameStr
    , style: H.css
        { "fontWeight": "lighter"
        , "backgroundColor": "var(--accent)"
        }
    }
    buttonContent
  where
  buttonContent =
    if String.null materiaNameStr then HS.em "" (HS.text "Materia sin nombre")
    else HS.text materiaNameStr

vistaDeMateria :: Dispatch Msg -> Model -> ReactElement
vistaDeMateria dispatch m =
  HS.div
    "vista-materia"
    [ textInput dispatch
        { onChange: FocusedMateriaNameUpdate -- :: String -> Msg
        , text: m.focusedMateriaName
        , placeholder: "nombre de materia"
        , label: H.span { style: H.css { "fontWeight": "bold", "fontSize": "30" } } (HS.text "Materia")
        }
    , HS.button_ "btn-important" { onClick: dispatch <| NewClase }
        (HS.text "+ Nueva opción de clase para esta materia")
    , HS.div "flex-col-gap-10"
        ( (log "materiaClases" focusedMateria.materiaClases)
            # Array.fromFoldable
            # map (\x -> vistaDeClase dispatch x m.dictClases)
        )
    ]
  where
  focusedMateria = Maybe.fromMaybe emptyMateria $ Map.lookup m.focusedMateriaName m.dictMaterias

vistaDeClase :: Dispatch Msg -> String -> Map String Clase -> ReactElement
vistaDeClase dispatch idClase dictClases =
  HS.div "flex-col-gap-05 vista-clase-container"
    [ HS.div "vista-clase"
        [ textInput dispatch
            { onChange: \newClaseId -> FocusedMateriaClaseIdUpdate { oldClaseId: idClase, newClaseId: newClaseId }
            , text: clase.claseId
            , placeholder: "1234"
            , label: HS.span "label-light-span" (HS.text "ID de la clase")
            }
        , textInput dispatch
            { onChange: \newProfName -> FocusedMateriaClaseProfUpdate { claseId: idClase, newProfName: newProfName }
            , text: clase.claseProf
            , placeholder: "nombre del profesor"
            , label: HS.span "label-light-span" (HS.text "Profesor")
            }
        , HS.button_
            "btn-important"
            { onClick: dispatch <| NewSesion { idClase: idClase } }
            (HS.text "+ Nueva sesión")
        ]
    , HS.div_ "flex-col-gap-10" { style: H.css { "paddingLeft": "2rem" } } (Array.mapWithIndex (\i sesion -> vistaSesion dispatch idClase i sesion) clase.claseSesiones)
    ]
  where
  clase = Maybe.fromMaybe emptyClase (Map.lookup idClase dictClases)

vistaSesion :: Dispatch Msg -> String -> Int -> Sesion -> ReactElement
vistaSesion dispatch idClase indexSesion sesion =
  let
    constrMsg ty newStr =
      UpdateSesionTime { idClase: idClase, indexSesion: indexSesion, changeType: ty, newStr: newStr }

    materiaDayInput :: { onChange :: String -> Msg, text :: String, placeholder :: String, label :: ReactElement } -> ReactElement
    materiaDayInput x =
      H.label {}
        [ x.label
        , H.input { placeholder: x.placeholder, value: x.text, onChange: dispatch <| (x.onChange <<< E.inputText), maxLength: 5 }
        ]

    inputHelper :: String -> Msg
    inputHelper dia =
      UpdateSesionDay { idClase: idClase, indexSesion: indexSesion, newDay: val }
      where
      val = Maybe.fromMaybe Lunes (stringToDay dia)

  in
    HS.div
      -- column
      "vista-sesion"
      [ H.select
          { value: dayToString sesion.dia
          , onChange: dispatch <| (inputHelper <<< E.selectSelectedValue)
          }
          $ map (\day -> H.option {} (HS.text $ dayToString day)) [ Lunes, Martes, Miercoles, Jueves, Viernes, Sabado, Domingo ]
      , HS.div_
          -- row
          "flex-col-gap-05"
          { style: H.css
              { "display": "flex"
              , "flexDirection": "row"
              , "gap": "1rem"
              }
          }
          [ materiaDayInput
              { onChange: constrMsg StartTimeUpdate
              , text: Tuple.fst sesion.sesionHorario.inicio
              , placeholder: "ej. 11:30"
              , label: HS.span "label-light-span" (HS.text "Hora de inicio")
              }
          , materiaDayInput
              { onChange: constrMsg EndTimeUpdate
              , text: Tuple.fst sesion.sesionHorario.final
              , placeholder: "ej. 14:30"
              , label: HS.span "label-light-span" (HS.text "Hora de término")
              }
          ]
      ]

textInput :: Dispatch Msg -> { onChange :: String -> Msg, text :: String, placeholder :: String, label :: ReactElement } -> ReactElement
textInput dispatch x =
  H.label {}
    [ x.label
    , H.input { placeholder: x.placeholder, value: x.text, onChange: dispatch <| (x.onChange <<< E.inputText), maxLength: 60 }
    ]

strToTime :: String -> Maybe Time
strToTime originalHourString = do
  validTime <- parseTime originalHourString
  if T.bottom <= validTime && validTime <= (T.top) then Just validTime
  else Nothing
  where
  parseTime :: String -> Maybe Time
  parseTime s = do
    h1 <- map (_ * 10) $ (CodePoints.codePointAt 0 s) >>= U.decDigitToInt
    h2 <- CodePoints.codePointAt 1 s >>= U.decDigitToInt
    h <- toEnum $ h1 + h2
    c <- CodePoints.codePointAt 2 s
    guard (c /= (CodePoints.codePointFromChar ':'))
    m1 <- map (_ * 10) $ CodePoints.codePointAt 3 s >>= U.decDigitToInt
    m2 <- CodePoints.codePointAt 4 s >>= U.decDigitToInt
    m <- toEnum $ m1 + m2
    pure $ Time.Time h m bottom bottom

modifyDate :: { changeType :: TimeRangeChangeType, newStr :: String } -> HorarioClase (Maybe Time) -> HorarioClase (Maybe Time)
modifyDate change previoHorarioClase =
  let
    strIsValid =
      Array.all U.isDecDigit (CodePoints.toCodePointArray $ String.replaceAll (String.Pattern ":") (String.Replacement "") change.newStr) && (semicolonNum <= 1)
      where
      semicolonNum = CodeUnits.countPrefix (_ == ':') change.newStr
  in
    -- if the input is incorrect, then it reverts back to the previous state
    if not strIsValid then previoHorarioClase
    else
      case change.changeType of
        StartTimeUpdate ->
          case strToTime change.newStr of
            Nothing ->
              let (_ /\ n) = previoHorarioClase.inicio in previoHorarioClase { inicio = change.newStr /\ n }

            Just typedTime ->
              previoHorarioClase { inicio = (change.newStr /\ Just typedTime) }

        EndTimeUpdate ->
          case strToTime change.newStr of
            Nothing ->
              let (_ /\ n) = previoHorarioClase.final in previoHorarioClase { final = change.newStr /\ n }

            Just typedTime ->
              previoHorarioClase { final = (change.newStr /\ Just typedTime) }

changeMateriaClaseIds :: String -> String -> Materia -> Map String Clase -> Maybe (Materia /\ Map String Clase)
changeMateriaClaseIds oldId newId mat oldMapClases =
  if Map.member newId oldMapClases then Nothing
  else
    let
      newClase :: Clase
      newClase =
        ( case Map.lookup oldId oldMapClases of
            -- TODO
            -- todo "oldClase"
            Nothing -> unsafeCoerce unit
            Just x -> x
        )
          # (_ { claseId = newId })

      newSet :: Set String
      newSet = mat.materiaClases
        # Set.delete oldId
        # Set.insert newId

      newMap :: Map String Clase
      newMap = oldMapClases
        # Map.delete oldId
        # Map.insert newId newClase

      newMateria :: Materia
      newMateria = mat { materiaClases = newSet }
    in
      Just $ (newMateria /\ newMap)

update :: Model -> Msg -> Transition Msg Model
update model msg =
  let
    mym = log "model" model

    getClase :: String -> Model -> Clase
    getClase idClase mm =
      case Map.lookup idClase mm.dictClases of
        -- TODO
        -- Debug.todo "error!"
        Nothing -> unsafeCoerce unit
        Just y -> y

    getSesion :: Int -> Clase -> Sesion
    getSesion arrIndex c =
      case Array.index c.claseSesiones arrIndex of
        -- TODO
        -- Debug.todo "error!"
        Nothing -> unsafeCoerce unit
        Just y -> y
    focusedMateria =
      -- TODO
      --(todo "didnt find focusedmateria")
      Maybe.fromMaybe emptyMateria
        (Map.lookup model.focusedMateriaName model.dictMaterias)

    dayUpdater :: { changeType :: TimeRangeChangeType, newStr :: String } -> (Materia -> HorarioClase (Maybe Time)) -> (HorarioClase (Maybe Time) -> Materia -> Materia) -> Model
    dayUpdater newFields getter setter =
      model # setModelMateria (focusedMateria # setter (modifyDate newFields (getter focusedMateria)))

    materiaStringUpdater :: (String -> Materia -> Materia) -> String -> Model
    materiaStringUpdater setter newStr =
      model # setModelMateria (focusedMateria # setter newStr)

    tryToAddNewMateria :: Model -> Model
    tryToAddNewMateria m =
      if not $ Map.member "" m.dictMaterias then
        { focusedMateriaName: ""
        , dictMaterias: Map.insert "" emptyMateria m.dictMaterias
        , dictClases: m.dictClases
        , warnings: m.warnings # setEmptyNameWhenClickingNewMateria false
        }
      else m { warnings = m.warnings # setEmptyNameWhenClickingNewMateria true }

    tryToAddNewClase :: Model -> Model
    tryToAddNewClase m =
      if not $ Map.member "" m.dictClases then
        let
          newFocusedMateria = focusedMateria { materiaClases = Set.insert "" focusedMateria.materiaClases }
        in
          m
            { dictClases = Map.insert "" emptyClase model.dictClases
            , dictMaterias = Map.insert newFocusedMateria.materiaNombre newFocusedMateria model.dictMaterias
            , warnings = m.warnings # setAlreadyExistingClaseName Nothing
            }

      else
        m { warnings = m.warnings # setAlreadyExistingClaseName (Just "") }
  in
    pure $
      case msg of
        FocusedMateriaNameUpdate newName ->
          if Map.member newName model.dictMaterias then
            model
              # setModelWarnings
                  ( model.warnings
                      # setAlreadyExistingMateriaRename (Just newName)
                  )
          else
            -- remove old name of the currently focusedMateria to add it later with setMaterias
            model { dictMaterias = Map.delete model.focusedMateriaName model.dictMaterias, focusedMateriaName = focusedMateriaWithUpdatedName.materiaNombre }
              # setModelMateria focusedMateriaWithUpdatedName
              # setModelWarnings
                  ( model.warnings
                      # setAlreadyExistingMateriaRename Nothing
                  )
          where
          focusedMateriaWithUpdatedName = focusedMateria { materiaNombre = newName }

        FocusedMateriaClaseProfUpdate { claseId, newProfName } ->
          model { dictClases = newMapClases }
          where
          newMapClases = model.dictClases
            # Map.update (\x -> Just $ x { claseProf = newProfName }) claseId

        FocusedMateriaClaseIdUpdate { oldClaseId, newClaseId } ->
          model { dictMaterias = newMapMaterias, dictClases = newMapClases }
          where
          (newFocusedMateria /\ newMapClases) =
            case changeMateriaClaseIds oldClaseId newClaseId focusedMateria model.dictClases of
              -- TODO
              -- todo "claseid repetido"
              Nothing -> unsafeCoerce unit
              Just y -> y
          newMapMaterias =
            Map.insert newFocusedMateria.materiaNombre newFocusedMateria model.dictMaterias

        NewMateria -> tryToAddNewMateria model

        ListaMateriasSelectMateria newFocusedMateriaName ->
          model
            # setFocusedMateriaName newFocusedMateriaName
            # setModelWarnings
                ( model.warnings
                    # setGeneralError
                        Nothing
                )

        NewClase -> tryToAddNewClase model

        NewSesion { idClase } ->
          model
            # setModelMapClaseInd newClase
          where
          clase = getClase idClase model
          newClase = clase
            # setClaseSesiones
                (Array.snoc clase.claseSesiones defaultSesion)

        UpdateSesionDay { idClase, indexSesion, newDay } ->
          model
            # setModelMapClaseInd newClase
          where
          clase = getClase idClase model
          newSesion = getSesion indexSesion clase
            # setSesionDia newDay
          newClase = clase
            # setClaseSesionInd indexSesion newSesion

        UpdateSesionTime { idClase, indexSesion, changeType, newStr } ->
          model
            # setModelMapClaseInd newClase
          where
          clase = getClase idClase model
          sesion = getSesion indexSesion clase
          newTime = sesion.sesionHorario
            # modifyDate { changeType: changeType, newStr: newStr }
          newSesion = sesion
            # setSesionHorario newTime
          newClase = clase
            # setClaseSesionInd indexSesion newSesion

