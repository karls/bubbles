module Main where

import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg exposing (Svg)
import Svg.Attributes exposing (cx, cy, r, fill, opacity)
import Signal exposing (Signal, Address, Mailbox)
import Effects exposing (Effects, Never)
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Json.Encode exposing (int, object, encode)
import Json.Decode exposing (tuple3, (:=), decodeString)
import Time exposing (Time)
import Debug exposing (log)
import Task exposing (Task)
import Random
import Mouse
import SocketIO as SIO
import StartApp


mailbox : Mailbox (Int, Int, Int)
mailbox = Signal.mailbox (0, 0, 0)

type alias BubblePos
  = { t : Int
    , x : Int
    , y : Int
    }

mkBubblePos : (Int, Int, Int) -> BubblePos
mkBubblePos (t, x, y) = { t = t, x = x, y = y}

port newBubble : Signal BubblePos
port newBubble =
  Signal.map mkBubblePos mailbox.signal


port newIncomingBubble : Signal BubblePos

incomingBubbles : Signal Action
incomingBubbles = Signal.map Incoming newIncomingBubble


type alias Bubble
  = { x : Int
    , y : Int
    , r : Int
    , o : Float
    , c : Color
    }


initBubble : Int -> Int -> Int -> Bubble
initBubble t x y
  = { x = x
    , y = y
    , r = 0
    , o = 1.0
    , c = generateColor t
    }


generateColor : Int -> Color
generateColor t =
  let intGenerator = Random.int 0 255
      seed = Random.initialSeed t
      (r, s) = Random.generate intGenerator seed
      (g, s') = Random.generate intGenerator s
      (b, _) = Random.generate intGenerator s'
  in Color.rgb r b g


parseBubblePosition : String -> Result String (Int, Int, Int)
parseBubblePosition s =
  let decoder = tuple3 (,,) Json.Decode.int Json.Decode.int Json.Decode.int
  in decodeString decoder s


type alias Model = List Bubble


type Action
  = Click (Time, (Int, Int))
  | Tick Time
  | MsgEmitted ()
  | Incoming BubblePos


init : (Model, Effects a)
init = ([], Effects.none)


targetRadius : Int
targetRadius =
  250


expandBubble : Bubble -> Bubble
expandBubble b =
  { b | r = b.r + 1, o = b.o - (1.0 / (toFloat (targetRadius))) }


extant : Bubble -> Bool
extant bubble =
  bubble.r <= targetRadius


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Click (t, (x, y)) ->
      let msg = Signal.send mailbox.address (truncate t, x, y)
      in (model, msg |> Task.map MsgEmitted |> Effects.task)

    Tick _ ->
      let newModel = List.filter extant model
      in (List.map expandBubble newModel, Effects.none)

    MsgEmitted _ ->
      (model, Effects.none)

    Incoming {t,x,y} -> ((initBubble t x y) :: model, Effects.none)


-- drawBubble : Bubble -> Svg
drawBubble bubble =
  Svg.circle
    [ cx (toString bubble.x)
    , cy (toString bubble.y)
    , r  (toString bubble.r)
    , fill (colorToHex bubble.c)
    , opacity (toString bubble.o)
    ]
    []


view : Address Action -> Model -> Html
view address model =
  Svg.svg
    [ style
        [ ("width", "100%")
        , ("height", "100vh")
        ]
    ]
    (List.map drawBubble model)


mouseClicks : Signal (Time, (Int, Int))
mouseClicks =
  Time.timestamp (Signal.sampleOn Mouse.clicks Mouse.position)


ticking : Signal Time
ticking =
  Time.every (Time.inMilliseconds 50)


driver : Signal Action
driver =
  Signal.merge
          (Signal.map Click mouseClicks)
          (Signal.map Tick ticking)


app =
  StartApp.start { init = init
                 , update = update
                 , view = view
                 , inputs = [driver, incomingBubbles] }


main =
  app.html


port tasks : Signal (Task Never ())
port tasks =
  app.tasks
