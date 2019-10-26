module Main exposing (main)

import Html exposing (Html, text, div)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onMouseDown, onMouseUp, custom)
import String exposing (fromInt)
import Svg exposing (mpath, svg )
import Svg.Attributes exposing (..)
import Browser
import Json.Decode as Json exposing (..)
import Element as Element exposing (rgb, fill)
import Element.Background as Background
import Element.Border as Border


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
            , Svg.Attributes.fill "none"
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

       mouseX =
           String.fromInt model.pointerPosition.x

       mouseY =
           String.fromInt model.pointerPosition.y

       vBox =
           viewBox <| "0 0 " ++ String.fromFloat cWidth ++ " " ++ String.fromFloat cHeight

       linesToDraw =
           modelToSvg model.path
   in
   Element.layout [] <|
     Element.column
        [ Background.color (rgb 230 230 230)
        , Element.width fill
        , Element.height fill
        , Element.spacing 0
        ]
        [ Element.text <|
            "Number of Points : "
            ++ (String.fromInt <| List.length model.path )
        , Element.text <|
            "Current Coordinates: "
            ++ "x: "
            ++ (String.fromInt model.pointerPosition.x)
            ++ "y: "
            ++ (String.fromInt model.pointerPosition.y)
        , drawingBox vBox mouseX mouseY linesToDraw
        ]



drawingBox : Svg.Attribute Msg -> String -> String -> Svg.Svg Msg -> Element.Element Msg
drawingBox vBox mouseX mouseY linesToDraw =
    Element.html <| Svg.svg
        [ vBox
        , Svg.Attributes.width <| String.fromInt cWidth ++ "px"
        , Svg.Attributes.width <| String.fromInt cHeight ++ "px"
        , Html.Attributes.style "border" "1px solid black"
        , Html.Attributes.style "border-radius" "5px"
--        , Html.Events.on "mousemove" (Json.map UpdatePointerPosition offsetPosition)
        , Html.Events.stopPropagationOn "mousemove" offsetPosition
        , Html.Events.onMouseDown DrawStart
        , Html.Events.onMouseUp DrawEnd
        ]
        [linesToDraw]



offsetPosition : Json.Decoder (Msg, Bool)
offsetPosition =
    map2 Position
        (field "offsetX" int)
        (field "offsetY" int)
    |> Json.map UpdatePointerPosition
    |> Json.map (\msg -> (msg, True))


--eventConfig : Bool -> Bool -> msg -> {message: Decoder Msg , stopPropagation : Bool, preventDefault : Bool}
--eventConfig stopPropagation preventDefault msg =
--    { message = options
--    , stopPropagation = stopPropagation
--    , preventDefault = preventDefault
--    }


--options = Json.map UpdatePointerPosition offsetPosition

--decoder = Json.map UpdatePointerPosition offsetPosition

--mouseMoveDecoder : Position -> Json.Decoder (Msg, Bool)
--mouseMoveDecoder pos =
--     let
--         options message =
--             { message = message
--             , stopPropagation = True
--             , preventDefault = False
--             }
--
--         decoder =
--             offsetPosition
--     in
--     Html.Events.custom "mousemove" decoder
--
--
