module Field exposing (Field(..), value, update, validate, validateAll, isValid, isAllValid, map)

{-| A `Field` is a simple data type that helps capture and validate form data better.
The left side of a field represents a function that takes in a value of arbitrary type,
validates it and returns a `ValidationResult`.
The `ValidationResult` can contain the value of the field if the validation `Passed`, or
an error if the validation `Failed`


# Definition

@docs Field


# Helpers

@docs value, update, validate, validateAll, isValid, isAllValid


# Mapping

@docs map

-}

import Field.ValidationResult exposing (..)


{-| Type that helps capture and validate form data better
-}
type Field e v
    = Field (v -> ValidationResult e v) v


{-| Extract the value out of a Field
-}
value : Field e v -> v
value (Field fn value) =
    value


{-| Update the value of a Field
-}
update : Field e v -> v -> Field e v
update (Field fn value) v =
    Field fn v


{-| Validate a Field and get a `ValidationResult`
-}
validate : Field e v -> ValidationResult e v
validate (Field fn value) =
    fn value


{-| Validate a List of Fields and get a List of `ValidationResult`
-}
validateAll : List (Field e v) -> List (ValidationResult e v)
validateAll =
    List.map validate


{-| Check if a Field is valid
-}
isValid : Field e v -> Bool
isValid =
    isValid_ << validate


isValid_ : ValidationResult e v -> Bool
isValid_ f =
    case f of
        Failed _ ->
            False

        Passed _ ->
            True


{-| Check if all Fields in the list are valid
-}
isAllValid : List (Field e v) -> Bool
isAllValid f =
    List.all (isValid_) <| validateAll f


{-| Map over a Field
-}
map : (v -> v) -> Field e v -> Field e v
map mapFn (Field fn value) =
    Field fn (mapFn value)
