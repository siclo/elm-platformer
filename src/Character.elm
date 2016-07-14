module Character exposing (..)

import Collage exposing (..)
import Color exposing (..)
import Time exposing (Time, inSeconds)
import InputManager


gravity : Float
gravity =
    400


type alias Model =
    { posX : Float
    , posY : Float
    , speedX : Float
    , topSpeedX : Float
    , accelerationX : Float
    , stoppingDecelerationX : Float
    , speedY : Float
    , jumpSpeed : Float
    }


model : Model
model =
    { posX = 0
    , posY = 0
    , speedX =
        0
        -- Pixel per second
    , topSpeedX = 150
    , accelerationX = 300
    , stoppingDecelerationX = 400
    , speedY = 0
    , jumpSpeed = 300
    }


characterModel : Form
characterModel =
    rect 10 30 |> filled orange


character : Model -> Form
character model =
    characterModel
        |> move ( model.posX, model.posY )


type Msg
    = Tick Time


update : Msg -> Model -> InputManager.Model -> ( Model, Cmd Msg )
update msg model inputManager =
    case msg of
        Tick deltaTime ->
            updateModel deltaTime inputManager model ! []


updateModel : Time -> InputManager.Model -> Model -> Model
updateModel deltaTime inputManager model =
    updateSpeedX deltaTime inputManager model
        |> updatePosX deltaTime
        |> jumpIfValid inputManager
        |> applyGravity deltaTime
        |> updatePosY deltaTime
        |> clampOnFloor


updateSpeedX : Time -> InputManager.Model -> Model -> Model
updateSpeedX deltaTime inputManager model =
    let
        newSpeed =
            if inputManager.left then
                max -model.topSpeedX (model.speedX - model.accelerationX * inSeconds deltaTime)
            else if inputManager.right then
                min model.topSpeedX (model.speedX + model.accelerationX * inSeconds deltaTime)
            else if model.speedX < 0 then
                min 0 (model.speedX + model.stoppingDecelerationX * inSeconds deltaTime)
            else
                max 0 (model.speedX - model.stoppingDecelerationX * inSeconds deltaTime)
    in
        { model | speedX = newSpeed }


updatePosX : Time -> Model -> Model
updatePosX deltaTime model =
    { model | posX = model.posX + model.speedX * (inSeconds deltaTime) }


jumpIfValid : InputManager.Model -> Model -> Model
jumpIfValid inputManager model =
    if canJump inputManager model then
        { model | speedY = model.jumpSpeed }
    else
        model


canJump : InputManager.Model -> Model -> Bool
canJump inputManager model =
    inputManager.up && isOnFloor model


isOnFloor : Model -> Bool
isOnFloor model =
    model.posY == 0


applyGravity : Time -> Model -> Model
applyGravity deltaTime model =
    { model | speedY = model.speedY - gravity * inSeconds deltaTime }


updatePosY : Time -> Model -> Model
updatePosY deltaTime model =
    { model | posY = model.posY + model.speedY * inSeconds deltaTime }


clampOnFloor : Model -> Model
clampOnFloor model =
    { model | posY = max model.posY 0 }
