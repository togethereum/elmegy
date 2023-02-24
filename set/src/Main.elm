module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text, li, ul, h1)
import Html.Events exposing (onClick)
import Html.Attributes exposing (shape)



-- MAIN


main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type Shape = Circle | Square | Triangle
type Number = One | Two | Three
type Color = Red | Blue | Green

type alias Card = 
  { shape : Shape
  , number : Number
  , color : Color
  }

type alias SelectableCard =
  { card : Card
  , selected : Bool
  }
type alias Model =
  { found : List Card
  , table : List Card
  , selection : List Card
  , deck : List Card
  }


init : Model
init =
  { found = []
  , table = [
    { number = One
    , color = Green
    , shape = Triangle}
    ,
    { number = Three
    , color = Red
    , shape = Circle}
  ]
  , selection = []
  , deck = []
  }



-- UPDATE


type Msg
  = Toggle Card


update : Msg -> Model -> Model
update msg model =
  case msg of
    Toggle _ ->
      model
      {- 
      if List.member card model.selected then
        List.filter 
      else
        model.selection ++ [card]
        -}



-- VIEW
shapeToString : Shape -> String
shapeToString shape =
  case shape of
     Triangle -> "triangle" 
     Square -> "square"
     Circle -> "circle"

numberToString : Number -> String
numberToString number =
  case number of
    One -> "1"
    Two -> "2"
    Three -> "3"

viewCard : Card -> Html Msg
viewCard card =
 let t = numberToString card.number ++ " " ++ shapeToString card.shape in
   li [] [text t]
      

viewTable : List Card -> Html Msg
viewTable cards =
  ul []
    (List.map viewCard cards)

view : Model -> Html Msg
view model =
  div []
    [ 
      h1 [] [text "hei"],
      viewTable model.table
    ]