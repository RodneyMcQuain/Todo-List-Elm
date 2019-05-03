module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, h1, span, button, div, input, ul, li, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

-- MAIN
main =
    Browser.sandbox 
    { 
        init = init
        , update = update
        , view = view
    }


-- MODEL
type alias Model = 
    { 
        id: Int
        , currentItem: String
        , todos: List Item
    }

init : Model
init =
    {
        id = 0
        , currentItem = ""
        , todos = []
    }

type alias Item =
    {
        id: Int
        , description: String
        , isComplete: Bool
    }

createItem : Int -> String -> Item
createItem newId newDesc =
    {
        id = newId
        , description = newDesc
        , isComplete = False
    }


-- UPDATE
type Msg = ChangeAddField String 
    | AddItem String 
    | DeleteItem Int
    | SetComplete Int

update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeAddField newTodoItem -> 
            { model | currentItem = newTodoItem }

        AddItem newTodoAdd ->
            { model 
                | id = model.id + 1
                , currentItem = ""
                , todos = (createItem model.id newTodoAdd) :: model.todos 
            }
        
        DeleteItem deleteId ->
            { model
                | todos = List.filter(\item -> item.id /= deleteId) model.todos
            }

        SetComplete completeId ->
            { model 
                | todos = List.map(\item ->
                    if item.id == completeId 
                    then { item | isComplete = not item.isComplete } 
                    else item) model.todos 
            }


-- VIEW
view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Todo List" ]
            , input [ placeholder "Item to Add", value model.currentItem, onInput ChangeAddField ] [] 
            , button [ onClick (AddItem model.currentItem) ] [ text "+" ]
            , div [] [ renderTodos model.todos ]
        ]

renderTodos : List Item -> Html Msg
renderTodos list = 
    div []
        (List.map (\item -> (
            div [] 
                [ input [ type_ "checkbox"
                    , checked item.isComplete 
                    , onClick (SetComplete item.id)
                ] []
                    , span [ classList [ ("completed", item.isComplete) ] ] [ text item.description ] 
                    , button [ onClick (DeleteItem item.id) ] [ text "✗" ]
                ]
        )) list)