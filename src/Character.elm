module Character exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Time exposing (Time, inSeconds)
import InputManager

type alias Model =
    { posX : Float
    , posY : Float
    , speedX : Float
    , topSpeedX : Float
    , accelerationX : Float
    , stoppingDecelerationX : Float
    }

model : Model
model =
    { posX = 0
    , posY = 0
    , speedX = 0 -- Pixel per second
    , topSpeedX = 150
    , accelerationX = 300
    , stoppingDecelerationX = 400
    }

characterModel = rect 10 30 |> filled orange

character : Model -> Form
character model =
    characterModel |>
        move (model.posX, model.posY)

type Msg =
    Tick Time

update : Msg -> Model -> InputManager.Model -> (Model, Cmd Msg)
update msg model inputManager =
    case msg of
        Tick deltaTime ->
            updateModel deltaTime inputManager model ! []
            
updateModel : Time -> InputManager.Model -> Model -> Model
updateModel deltaTime inputManager model =
    updateSpeedX deltaTime inputManager model |>
    updatePosX deltaTime

updateSpeedX : Time -> InputManager.Model -> Model -> Model
updateSpeedX deltaTime inputManager model =
    let newSpeed =
        if inputManager.left then
            max -model.topSpeedX (model.speedX - model.accelerationX * inSeconds deltaTime)
        else if inputManager.right then
            min model.topSpeedX (model.speedX + model.accelerationX * inSeconds deltaTime)
        else
            if model.speedX < 0 then
                min 0 (model.speedX + model.stoppingDecelerationX * inSeconds deltaTime)
            else
                max 0 (model.speedX - model.stoppingDecelerationX * inSeconds deltaTime)
    in
       { model | speedX = newSpeed }

updatePosX : Time -> Model -> Model
updatePosX deltaTime model =
    { model | posX = model.posX + model.speedX * (inSeconds deltaTime) }

