import Browser
import Html exposing (..)
import Html.Attributes exposing (..)

-- MAIN

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type TaskStatus = NotStarted | InProgress | Done
type alias Task =
    { id : Int
    , status : TaskStatus
    , text : String
    }

type alias Model = List Task


init : () -> ( Model, Cmd Msg )
init _ =
  ( [], Cmd.none )


-- UPDATE

type Msg
    = Add Task
    | Remove Int
    | ShiftStatus Int
    | Getjson

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Add task ->
        ( model, Cmd.none )
    Remove id ->
        ( model, Cmd.none )
    ShiftStatus id ->
        ( model, Cmd.none )
    Getjson ->
        ( model, Cmd.none )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "Goal Tracker"
  , body =
      [ text "I am here"]
  }
