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
  }

type alias Model =
  { found : List Card
  , table : List Card
  , deck : List Card
  , selection : List Card
  }


init : Model
init =
  { found = []
  , table = [
    { number = One , color = Green , shape = Triangle }
    , { number = Three , color = Red , shape = Circle }
    , { number = Two , color = Blue , shape = Square }
    , { number = One , color = Red , shape = Triangle }
  ]
  , deck = []
  , selection = []
  }


-- UPDATE


type Msg
  = Toggle Card

toggleMember: a -> List a -> List a
toggleMember m xs =
  if List.member m xs then
    List.filter (\x -> x /= m) xs
  else
    xs ++ [m]

maxSelectableCards : number
maxSelectableCards = 3

update : Msg -> Model -> Model
update msg model =
  case msg of
    Toggle card ->
      if List.length model.selection == maxSelectableCards && not (List.member card model.selection) then
         model
      else
        { model | selection = toggleMember card model.selection }



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

viewCard : List Card -> Card -> Html Msg
viewCard selectedCards card =
 let t = numberToString card.number ++ " " ++ shapeToString card.shape 
     tt = if List.member card selectedCards then "[" ++ t ++ "]" else t
  in
   li [] 
      [ button 
          [onClick (Toggle card)]  
          [text tt] ]
      

viewTable : List Card -> List Card -> Html Msg
viewTable cards selectedCards =
  ul []
    (List.map (viewCard selectedCards) cards)

view : Model -> Html Msg
view model =
  div []
    [ 
      h1 [] [text "hei"],
      viewTable model.table model.selection
    ]