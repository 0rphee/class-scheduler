module Msg exposing (..)

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
