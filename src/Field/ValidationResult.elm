module Field.ValidationResult exposing (ValidationResult(..), map, contraMap, andThen)

{-|


# Definition

@docs ValidationResult


# Mapping

@docs map, contraMap


# Chaining

@docs andThen

-}


{-| A `ValidationResult` describes the state of a `Field` once it has been validated.
It can contain the value of the field if the validation `Passed` or
an error if the validation `Failed`
-}
type ValidationResult e v
    = Passed v
    | Failed e


{-| Apply a function to a `ValidationResult`. If the `ValidationResult` is `Passed`, it will be converted.
if the `ValidationResult` has `Failed` then the failure will propogate
-}
map : (a -> b) -> ValidationResult x a -> ValidationResult x b
map fn v =
    case v of
        Failed err ->
            Failed err

        Passed val ->
            Passed <| fn val


{-| Apply a function to a `ValidationResult`. If the `ValidationResult` is `Failed`, it will be converted.
if the `ValidationResult` has `Passed` then the passed state will propogate
-}
contraMap : (a -> b) -> ValidationResult a x -> ValidationResult b x
contraMap fn e =
    case e of
        Failed err ->
            Failed <| fn err

        Passed val ->
            Passed val


{-| Chain multiple validation functions
-}
andThen : (v -> ValidationResult e v) -> ValidationResult e v -> ValidationResult e v
andThen fn t =
    case t of
        Failed err ->
            Failed err

        Passed v ->
            fn v
