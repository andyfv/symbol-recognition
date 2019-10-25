module Main exposing (main)

import Html exposing (Html, text, div)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onMouseDown, onMouseUp, custom)
import String exposing (fromInt)
import Svg exposing (mpath, svg )
import Svg.Attributes exposing (..)
import Browser
import Json.Decode as Json exposing (..)

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }

cWidth = 500
cHeight = 500

-- MODEL


type alias Model =
    { path : List Point
    , currentlyDrawing : Bool
    , pointerPosition : Position
    }


initialModel : () -> (Model, Cmd Msg)
initialModel _ =
    ( { path = []
      , currentlyDrawing = False
      , pointerPosition = Position 0 0
      }
    , Cmd.none
    )

type alias Point =
    ( Float
    , Float
    )

type alias Position =
    { x : Int
    , y : Int
    }


modelToSvg : List Point -> Svg.Svg msg
modelToSvg points =
    let
        x =
            List.map pointToString points
            |> String.join " "

        polyColor =
            "red"
    in
        Svg.polyline
            [ Svg.Attributes.points x
            , fill "none"
            , stroke polyColor
            , strokeWidth "2"
            ]
            []


pointToString : (Float, Float) -> String
pointToString point =
    let
        x =
            String.fromFloat <| Tuple.first point

        y =
            String.fromFloat <| Tuple.second point

    in
    x ++ "," ++ y

-- UPDATE

type MouseState
    = Up
    | Down


type Msg
    = DrawStart
    | DrawEnd
    | UpdatePointerPosition Position


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdatePointerPosition pos ->
            case model.currentlyDrawing of
                True ->
                    let pathSoFar = model.path
                        xPos = toFloat pos.x
                        yPos = toFloat pos.y
                        pathNew = (xPos, yPos) :: pathSoFar
                    in
                    ({ model | pointerPosition = pos
                    , path = pathNew
                    }, Cmd.none)

                False ->
                    ({model | pointerPosition = pos }, Cmd.none)


        DrawStart ->
            ({ model | currentlyDrawing = True }, Cmd.none)

        DrawEnd ->
            ({ model | currentlyDrawing = False }, Cmd.none)



-- VIEW

view : Model -> Html Msg
view model =
    let
       x =
           toFloat <| model.pointerPosition.x - round (cWidth / 2)

       y =
           toFloat <| -model.pointerPosition.y + round (cHeight / 2)

       mouseX =
           String.fromInt model.pointerPosition.x

       mouseY =
           String.fromInt model.pointerPosition.y

       vBox =
           viewBox <| "0 0 " ++ String.fromFloat cWidth ++ " " ++ String.fromFloat cHeight

       linesToDraw =
           modelToSvg model.path
   in
        div
        [ Html.Attributes.style "background-color" "rgb(230,230,230)"]
        [text <|
            "Number of Points : "
            ++ (String.fromInt <| List.length model.path )
        , text <|
            "Current Coordinates: "
            ++ "x: "
            ++ (String.fromInt model.pointerPosition.x)
            ++ "y: "
            ++ (String.fromInt model.pointerPosition.y)
        , drawingBox vBox mouseX mouseY linesToDraw
        ]



drawingBox : Svg.Attribute Msg -> String -> String -> Svg.Svg Msg -> Html Msg
drawingBox vBox mouseX mouseY linesToDraw =
    svg
        [ vBox
        , Svg.Attributes.width <| String.fromInt cWidth ++ "px"
        , Svg.Attributes.width <| String.fromInt cHeight ++ "px"
        , Html.Attributes.style "border" "1px solid black"
        , Html.Attributes.style "border-radius" "5px"
        , Html.Events.on "mousemove" (Json.map UpdatePointerPosition offsetPosition)
        , Html.Events.onMouseDown DrawStart
        , Html.Events.onMouseUp DrawEnd
        ]
        [linesToDraw]



offsetPosition : Json.Decoder Position
offsetPosition =
    map2 Position
        (field "offsetX" int)
        (field "offsetY" int)