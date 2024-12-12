module Msg where

import Model (Day)

-- MSG

data TimeRangeChangeType
  = StartTimeUpdate
  | EndTimeUpdate

data Msg
  = FocusedMateriaNameUpdate String -- done
  | FocusedMateriaClaseIdUpdate { oldClaseId :: String, newClaseId :: String }
  | FocusedMateriaClaseProfUpdate { claseId :: String, newProfName :: String }
  | NewMateria -- done
  | ListaMateriasSelectMateria String -- done
  | NewClase -- done
  | NewSesion { idClase :: String }
  | UpdateSesionTime { idClase :: String, indexSesion :: Int, changeType :: TimeRangeChangeType, newStr :: String }
  | UpdateSesionDay { idClase :: String, indexSesion :: Int, newDay :: Day }
  | ValidateHorarios -- TODO
  | SelectHorario Int
