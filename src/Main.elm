module Main exposing (..)

import Html.App exposing (program)
import Html
import Collage exposing (collage)
import Element exposing (toHtml)
import Character exposing (..)
import InputManager
import AnimationFrame
import Time exposing (Time)


type alias Model =
    { worldSize :
        { x : Int
        , y : Int
        }
    , character : Character.Model
    , inputManager : InputManager.Model
    }


model : Model
model =
    { worldSize =
        { x = 800
        , y = 600
        }
    , character = Character.model
    , inputManager = InputManager.model
    }


init =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map InputManagerMsg (InputManager.subscriptions model.inputManager)
        , AnimationFrame.diffs Tick
        ]


type Msg
    = InputManagerMsg InputManager.Msg
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputManagerMsg subMsg ->
            let
                ( updatedInputManagerModel, inputManagerCmd ) =
                    InputManager.update subMsg model.inputManager
            in
                ( { model | inputManager = updatedInputManagerModel }
                , Cmd.map InputManagerMsg inputManagerCmd
                )

        Tick deltaTime ->
            tickCharacter deltaTime model


tickCharacter : Time -> Model -> ( Model, Cmd Msg )
tickCharacter deltaTime model =
    let
        ( updatedCharacterModel, characterCmd ) =
            Character.update (Character.Tick deltaTime) model.character model.inputManager
    in
        { model | character = updatedCharacterModel } ! []


view : Model -> Html.Html Msg
view model =
    toHtml (collage model.worldSize.x model.worldSize.y [ character model.character ])


main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
