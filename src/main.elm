import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

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
init _ = ([{ id = 1
  , status = NotStarted
  , text = "do the thing"
  },{ id = 2
  , status = InProgress
  , text = "finish work"
  }], Cmd.none )


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
      ( List.map (modifyById shiftStatus id) model, Cmd.none )
    Getjson ->
      ( model, Cmd.none )

modifyById : (Task -> Task) -> Int -> Task -> Task
modifyById func id task =
  if task.id == id
  then func task
  else task

shiftStatus : Task -> Task
shiftStatus task =
  case task.status of
    NotStarted -> { task | status = InProgress }
    InProgress -> { task | status = Done }
    Done -> { task | status = NotStarted }

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "Goal Tracker"
  , body =
    [ h1 [] [text "Your Goals"]
    , p [] [text ("Percent complete: " ++ (percentFromFloat (ratioComplete model)))]
    , listTasks model ]
  }

listTasks : List Task -> Html Msg
listTasks list =
  ul [] (List.map taskListItem list)

taskListItem : Task -> Html Msg
taskListItem task =
  li []
  [ button [onClick (ShiftStatus task.id)] [ text "shift status" ]
  , text (task.text ++ " - " ++ taskStatusString task.status)
  ]

taskStatusString : TaskStatus -> String
taskStatusString status =
  case status of
    NotStarted -> "Not Started"
    InProgress -> "In Progress"
    Done -> "Done!"

percentFromFloat: Float -> String
percentFromFloat x = (String.fromFloat (x * 100)) ++ "%"

ratioComplete : List Task -> Float
ratioComplete list = (toFloat (List.length (List.filter isComplete list))) / (toFloat (List.length list))

isComplete : Task -> Bool
isComplete task = if task.status == Done then True else False
