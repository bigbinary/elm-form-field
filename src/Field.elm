module Field exposing (..)

{-| A Field is a simple data type that helps capture and validate form data better.
The left side of a field represents a function that takes in a value of arbitrary type, 
validates it and returns a tuple consisting of a Maybe String represting an error message if the validation fails
and the actual value of the field

# Definition

@docs Field


# Helpers

@docs value, validate


# Mapping

@docs map

-}


{-| A simple data type that helps capture and validate form data better
-}
type Field a
    = Field (a -> (Maybe String, a)) a


{-| Extract the value out of a Field
-}
value : Field a -> a
value (Field err value) =
    value


{-| Validate a Field and get a Maybe String with a error message
-}
validate : Field a -> Maybe String
validate (Field fn value) =
    Tuple.first <| fn value


{-| Map over a Field
-}
map : (a -> a) -> Field a -> Field a
map mapFn (Field fn value) =
    Field fn (mapFn value)
