StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "Ninety four"
getStringOrInt True = 94


valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString False y = trim y
valToString True y = cast y
