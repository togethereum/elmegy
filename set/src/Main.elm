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
import Html.Attributes exposing (selected)



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
  , selected : Bool
  }

type alias Model =
  { found : List Card
  , table : List Card
  , deck : List Card
  }


init : Model
init =
  { found = []
  , table = [
    { number = One , color = Green , shape = Triangle , selected = False }
    ,
    { number = Three , color = Red , shape = Circle , selected = False }
  ]
  , deck = []
  }


-- UPDATE


type Msg
  = Toggle Card

toggleIfSame: Card -> Card -> Card
toggleIfSame reference toToggle =
  if { reference | selected = True } == { toToggle | selected = True } then
    { toToggle | selected = not toToggle.selected }
  else
    toToggle

update : Msg -> Model -> Model
update msg model =
  case msg of
    Toggle card ->
      { model | table = List.map (toggleIfSame card) model.table }



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
 let t = numberToString card.number ++ " " ++ shapeToString card.shape 
     tt = if card.selected then "[" ++ t ++ "]" else t
  in
   li [] 
      [ button 
          [onClick (Toggle card)]  
          [text tt] ]
      

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