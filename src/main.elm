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

type alias Model =
  { list : List Task
  , newTask : String
  , message : String
  }


init : () -> ( Model, Cmd Msg )
init _ =
  (
    { list = [{ id = 1, status = NotStarted, text = "do the thing"}
             ,{ id = 2, status = InProgress, text = "finish work"}
             ]
    , newTask = ""
    , message = ""
    }
  , Cmd.none )


-- UPDATE

type Msg
  = Add
  | TextChange String
  | Remove Int
  | Shift Int
  | Getjson

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Add ->
      if model.newTask == "" then
        ( { model | message = "Task not created, missing description" } , Cmd.none )
      else
        let newId = nextId model.list
        in
        ( { model
          | list = model.list ++ [{ id = newId
                                  , text = model.newTask
                                  , status = NotStarted }]
          , newTask = ""
          , message = "Task " ++ String.fromInt newId ++ " created."
          }
        , Cmd.none
        )
    TextChange description ->
      ( { model | newTask = description }, Cmd.none )
    Remove id ->
      ( model, Cmd.none )
    Shift id ->
      ( {model
        | list = (List.map (modifyById shiftStatus id) model.list)
        }
      , Cmd.none
      )
    Getjson ->
      ( model, Cmd.none )

nextId : List Task -> Int
nextId list =
  case List.maximum (List.map .id list) of
    Just val -> val + 1
    Nothing -> 1000

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
    , p [] [text ("Percent complete: " ++ (percentFromFloat (ratioComplete model.list)))]
    , listTasks model.list
    , input [value model.newTask, onInput TextChange] [], label [] [text "task description"], button [onClick Add] [text "Add"]
    , p [] [text model.message]
    ]
  }

listTasks : List Task -> Html Msg
listTasks list =
  ul [] (List.map taskListItem list)

taskListItem : Task -> Html Msg
taskListItem task =
  li []
  [ button [onClick (Shift task.id)] [ text "shift status" ]
  , text (task.text ++ " - " ++ taskStatusString task.status)
  ]

taskStatusString : TaskStatus -> String
taskStatusString status =
  case status of
    NotStarted -> "Not Started"
    InProgress -> "In Progress"
    Done -> "Done!"

percentFromFloat: Float -> String
percentFromFloat x = (String.fromInt (round (x * 100))) ++ "%"

ratioComplete : List Task -> Float
ratioComplete list = (toFloat (List.length (List.filter isComplete list))) / (toFloat (List.length list))

isComplete : Task -> Bool
isComplete task = if task.status == Done then True else False
