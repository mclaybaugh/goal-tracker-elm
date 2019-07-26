import Browser
{-
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
-}
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Css exposing (..)

-- MAIN

main =
  Browser.element
  { init = init
  , view = view >> toUnstyled
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
  | SetStatus Int TaskStatus
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
      ( { model | list = removeTask id model.list }, Cmd.none )
    SetStatus id status ->
      ( { model | list = List.map (modifyById (setStatus status) id) model.list }
      , Cmd.none
      )
    Getjson ->
      ( model, Cmd.none )

removeTask : Int -> List Task -> List Task
removeTask id list = List.filter (\t -> id /= t.id) list

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

setStatus : TaskStatus -> Task -> Task
setStatus status task =
  { task | status = status }

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [text "Goals"]
    , p [] [text <| "Percent complete: " ++ (percentFromFloat <| ratioComplete model.list)]
    , ol [] (List.map taskListItem model.list)
    , input [value model.newTask, onInput TextChange] []
    , label [] [text "task description"]
    , button [onClick Add] [text "Add"]
    , p [] [text model.message]
    ]

taskListItem : Task -> Html Msg
taskListItem task =
  li []
    [ fieldset [css
        [border zero]
      ] <| List.map (statusRadio task.status task.id) [NotStarted, InProgress, Done]
    , text task.text
    , button [onClick <| Remove task.id ] [text "Delete"]
    ]

statusRadio : TaskStatus -> Int -> TaskStatus -> Html Msg
statusRadio taskStatus id status =
  label []
    [ input
      [ type_ "radio"
      , name <| "status" ++ (String.fromInt id)
      , Html.Styled.Attributes.checked (taskStatus == status)
      , onInput (\n -> SetStatus id status)
      ] []
    , text <| taskStatusString status
    ]

taskStatusString : TaskStatus -> String
taskStatusString status =
  case status of
    NotStarted -> "Not Started"
    InProgress -> "In Progress"
    Done -> "Done"

percentFromFloat: Float -> String
percentFromFloat x =
  (x * 100 |> Basics.round |> String.fromInt) ++ "%"

ratioComplete : List Task -> Float
ratioComplete list =
  case list of
    [] -> 1
    _ ->
      (List.filter (\n -> n.status == Done) list
        |> List.length |> toFloat) / (List.length list |> toFloat)
