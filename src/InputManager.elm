module InputManager exposing (..)
import Keyboard exposing (..)

type alias Model =
    { left : Bool
    , right : Bool
    }

model : Model
model =
    { left = False
    , right = False
    }

type Msg
    = KeyDown KeyCode
    | KeyUp KeyCode

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        KeyDown keyCode ->
            setKey keyCode model ! []
        KeyUp keyCode ->
            unSetKey keyCode model ! []

setKey : KeyCode -> Model -> Model
setKey = assignKey True

unSetKey : KeyCode -> Model -> Model
unSetKey = assignKey False

assignKey : Bool -> KeyCode -> Model -> Model
assignKey newValue keyCode model =
    case getFieldSetterFromKeyCode keyCode of
        Just fieldSetter ->
            fieldSetter newValue model
        Nothing ->
            model

getFieldSetterFromKeyCode : KeyCode -> Maybe (Bool -> Model -> Model)
getFieldSetterFromKeyCode keyCode =
    case keyCode of
        37 -> Just (\newValue model -> { model | left = newValue })
        39 -> Just (\newValue model -> { model | right = newValue })
        otherwise ->
            Nothing

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [downs KeyDown, ups KeyUp]
