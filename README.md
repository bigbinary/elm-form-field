# Field

A Field is a simple data type that helps capture and validate form data better.
The left side of a field represents a function that takes in a value of arbitrary type, 
validates it and returns a tuple consisting of a Maybe String and the actual value of the field.
The Maybe String can represting an error message if the validation fails

