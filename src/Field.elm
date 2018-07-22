module Field exposing (Field(..), value, update, validate, validateAll, isValid, isAllValid, map, andThen)

{-| A Field is a simple data type that helps capture and validate form data better.
The left side of a field represents a function that takes in a value of arbitrary type,
validates it and returns a tuple consisting of a Maybe String and the actual value of the field.
The Maybe String can represting an fnor message if the validation fails


# Definition

@docs Field


# Helpers

@docs value, update, validate, validateAll, isValid, isAllValid


# Mapping

@docs map


# Chaining validations

@docs andThen

-}


{-| A simple data type that helps capture and validate form data better
-}
type Field a
    = Field (a -> ( Maybe String, a )) a


{-| Extract the value out of a Field
-}
value : Field a -> a
value (Field fn value) =
    value


{-| Update the value of a Field
-}
update : Field a -> a -> Field a
update (Field fn value) v =
    Field fn v


{-| Validate a Field and get a Maybe String which may contain an error message
-}
validate : Field a -> Maybe String
validate (Field fn value) =
    Tuple.first <| fn value


{-| Validate a List of Fields and get a List of Maybe String which may contain error messages for all fields
-}
validateAll : List (Field a) -> List (Maybe String)
validateAll =
    List.map validate


{-| Check if a Field is valid
-}
isValid : Field a -> Bool
isValid f =
    case validate f of
        Just _ ->
            False

        Nothing ->
            True


{-| Check if all Fields in the list are valid
-}
isAllValid : List (Field a) -> Bool
isAllValid f =
    List.all ((==) Nothing) <| validateAll f


{-| Map over a Field
-}
map : (a -> a) -> Field a -> Field a
map mapFn (Field fn value) =
    Field fn (mapFn value)


{-| Chain multiple validation functions
-}
andThen : (a -> ( Maybe String, a )) -> ( Maybe String, a ) -> ( Maybe String, a )
andThen fn t =
    case t of
        ( Just err, v ) ->
            ( Just err, v )

        ( Nothing, v ) ->
            fn v
