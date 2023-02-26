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
import Array



-- MAIN


main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

maxSelectableCards : number
maxSelectableCards = 3

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

removeFromList : List a -> List a -> List a
removeFromList toRemove list =
  List.filter (\x -> not (List.member x toRemove))list

allTheSame : (a, a, a) -> Bool
allTheSame (x, y, z) =
  x == y && y == z

allDifferent : (a, a, a) -> Bool
allDifferent (x, y, z) =
  x /= y && y /= z && x /= y

listToTuple3 : List a -> Maybe (a, a, a)
listToTuple3 xs =
    let arr = Array.fromList xs in
    case Array.get 0 arr of
      Nothing -> Nothing
      Just x ->
        case Array.get 1 arr of
            Nothing -> Nothing
            Just y ->
              case Array.get 2 arr of
                  Nothing -> Nothing
                  Just z -> Just (x, y, z)

isSet : List a -> Bool
isSet xs =
  case listToTuple3 xs of
      Nothing -> False
      Just xyz -> allTheSame xyz || allDifferent xyz

hasSet : List Card -> Bool
hasSet cards =
  isSet (List.map .shape cards)
  && isSet (List.map .number cards)
  && isSet (List.map .color cards)

-- UPDATE


type Msg
  = Toggle Card

toggleMember: a -> List a -> List a
toggleMember m xs =
  if List.member m xs then
    List.filter (\x -> x /= m) xs
  else
    xs ++ [m]

update : Msg -> Model -> Model
update msg model =
  case msg of
    Toggle card ->
      if List.length model.selection == maxSelectableCards && not (List.member card model.selection) then
         model
      else
        let newSelection = toggleMember card model.selection in
          if hasSet newSelection then
            { model |
              selection = [],
              found = model.found ++ model.selection,
              table = removeFromList model.selection model.table }
          else
            { model | selection = newSelection }

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
      viewTable model.table model.selection,
      text ("found: " ++ (model.found |> List.length |> String.fromInt))

    ]