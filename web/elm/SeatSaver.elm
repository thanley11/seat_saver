module SeatSaver where

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import StartApp
import Effects exposing (Effects, Never)
import Task exposing (Task)

--main : Html
--main =
--   view init

-- After adding elm package install evancz/start-app -y
app = 
    StartApp.start
    { init = init,
      update = update,
      view = view,
      inputs = []
    }

main : Signal Html
main =
    app.html

port tasks : Signal (Task Never ())
port tasks =
    app.tasks

-- MODEL
type alias Seat = {seatNo : Int, occupied: Bool}

type alias Model = 
    List Seat

init : (Model, Effects Action)
init =
    let
        seats =
        [ { seatNo = 1, occupied = False },
        { seatNo = 2, occupied = False },
        { seatNo = 3, occupied = False },
        { seatNo = 4, occupied = False },
        { seatNo = 5, occupied = False },
        { seatNo = 6, occupied = False },
        { seatNo = 7, occupied = False },
        { seatNo = 8, occupied = False },
        { seatNo = 9, occupied = False },
        { seatNo = 10, occupied = False },
        { seatNo = 11, occupied = False },
        { seatNo = 12, occupied = False }
        ]
    in
       (seats, Effects.none)
-- UPDATE

-- Toggle action takes arg of Seat, two actions would have a | pipe
-- Single possible value, so single case statement below
type Action = Toggle Seat

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Toggle seatToToggle ->
            let
                updateSeat seatFromModel =
                    -- switch boolean to opposite of seat passed in, if seat# =
                    if seatFromModel.seatNo == seatToToggle.seatNo then
                       { seatFromModel | occupied <- not seatFromModel.occupied }
                    else seatFromModel
            in
               (List.map updateSeat model, Effects.none)

-- VIEW
-- the View func taks a Model as arg and return Html
-- view : Model -> Html

-- After adding elm package install evancz/start-app -y
view : Signal.Address Action -> Model -> Html

-- run List.map seatItem model first and then pass the 
-- result of that as an argument to the ul function. Seeing as List.map returns a List, 
-- we don’t have to wrap that result in []

--view model =
--    ul [ class "seats" ] (List.map seatItem model)


-- After adding elm package install evancz/start-app -y
view address  model =
    ul [ class "seats" ] (List.map (seatItem address)  model)


-- After adding elm package install evancz/start-app -y
seatItem : Signal.Address Action -> Seat -> Html
-- The seatItem function returns an HTML list item,
-- which shows the seat’s seatNo as HTML text 
-- (after parsing into a string).

--seatItem seat =
--    li [ class "seat available" ] [ text (toString seat.seatNo) ] 

-- After adding elm package install evancz/start-app -y
seatItem address seat =
    let
      occupiedClass =
        if seat.occupied then "occupied" else "available"
    in
      li 
        [ class ("seat " ++ occupiedClass)
        , onClick address (Toggle seat)
        ] 
        [ text (toString seat.seatNo) ] 


