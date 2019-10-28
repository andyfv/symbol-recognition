module Main exposing (main, Point)

import Array exposing (fromList)
import Browser
import Element as Element exposing (fill, rgb)
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (custom, on, onMouseDown, onMouseUp)
import Json.Decode as Json exposing (..)
import String exposing (fromInt)
import Svg exposing (mpath, svg)
import Svg.Attributes exposing (..)




-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }


cWidth =
    500


cHeight =
    500



-- MODEL


type alias Model =
    { path : List Point
    , smoothedPath : List Point
    , thinnedPath : List Point
    , curvedPath : List Point
    , currentlyDrawing : Bool
    , pointerPosition : Position
    , smoothingFactor : Float
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { path = []
      , smoothedPath = []
      , thinnedPath = []
      , curvedPath = []
      , currentlyDrawing = False
      , pointerPosition = Position 0 0
      , smoothingFactor = 0.75
      }
    , Cmd.none
    )


type alias Point =
    ( Int, Int )


type alias Position =
    { x : Int
    , y : Int
    }


pointToString : ( Int, Int ) -> String
pointToString point =
    let
        x =
            String.fromInt <| Tuple.first point

        y =
            String.fromInt <| Tuple.second point
    in
    x ++ "," ++ y



-- UPDATE


type Msg
    = DrawStart
    | DrawEnd
    | UpdatePointerPosition Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePointerPosition pos ->
            case model.currentlyDrawing of
                True ->
                    let
                        pathSoFar =
                            model.path

                        newPoint = (pos.x , pos.y)

                        pathNew =
                            newPoint :: pathSoFar

                        smoothPath = smoothing
                                        pathNew
                                        model.smoothedPath
                                        newPoint
                                        model.smoothingFactor

                    in
                    ( { model
                        | pointerPosition = pos
                        , path = pathNew
                        , smoothedPath = smoothPath
                      }
                    , Cmd.none
                    )

                False ->
                    ( { model | pointerPosition = pos }, Cmd.none )

        DrawStart ->
            ( { model
                | currentlyDrawing = True
                , path = []
                , smoothedPath = []
              }
            , Cmd.none
            )

        DrawEnd ->
            ( { model | currentlyDrawing = False }, Cmd.none )


smoothing : List Point -> List Point -> Point -> Float -> List Point
smoothing rawList smoothedList newPoint sf =
    if (List.length smoothedList > 1 ) then
        let
            -- 1. Take the last raw point
            -- 2. Take the last smoothed point
            lastRawPoint = fromMaybe <| List.head rawList
            previousSmoothedPoint = fromMaybe <| List.head smoothedList

            -- 3. Decode the lastRawPoint Point tuple to:
            --     xRi, yRi - coordinates of the i-th raw point
            (xRi, yRi) = Tuple.mapBoth toFloat toFloat lastRawPoint

            -- 4. Destruct the previousSmoothedPoint tuple to:
            --     ySi, ySi - coordinates of the (i-1) smoothed point
            (xSi_sub_1, ySi_sub_1 ) = Tuple.mapBoth toFloat toFloat previousSmoothedPoint

            -- 5. Calculate next smoothed point
            xSi = (sf * xSi_sub_1) + (( 1 - sf ) * xRi )
            ySi = (sf * ySi_sub_1) + (( 1 - sf ) * yRi )
        in
            -- 6. Add the last smoothed point to the smoothedList
            (Tuple.mapBoth round round (xSi, ySi)) :: smoothedList

    else
        -- 1. If  smoothedList is empty
        newPoint :: smoothedList



fromMaybe : Maybe Point -> Point
fromMaybe x =
    case x of
        Just y -> y
        Nothing -> (0, 0)


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
            pathToSvg model.path

        smoothedLines = pathToSvg model.smoothedPath
    in
    Element.layout [] <|
        Element.column
            [ Background.color (rgb 253 246 227)
            , Element.width Element.fill
            , Element.height Element.fill
            , Element.spacing 0
            ]
            [ Element.text <|
                "Number of Points : "
                    ++ (String.fromInt <| List.length model.path)
            , Element.text <|
                "Current Coordinates: "
                    ++ "x: "
                    ++ mouseX
                    ++ "y: "
                    ++ mouseY
            , drawingBox vBox mouseX mouseY linesToDraw
            , drawingBox vBox mouseX mouseY smoothedLines
            --        , mouseCircle mouseX mouseY
            ]


pathToSvg : List Point -> Svg.Svg msg
pathToSvg points =
    let
        x =
            List.map pointToString points
                |> String.join " "

        polyColor =
            "grey"
    in
    Svg.polyline
        [ Svg.Attributes.points x
        , Svg.Attributes.fill "none"
        , stroke polyColor
        , strokeWidth "2"
        ]
        []


drawingBox : Svg.Attribute Msg -> String -> String -> Svg.Svg Msg -> Element.Element Msg
drawingBox vBox mouseX mouseY linesToDraw =
    Element.html <|
        Svg.svg
            [ vBox
            , Svg.Attributes.width <| String.fromInt cWidth ++ "px"
            , Svg.Attributes.width <| String.fromInt cHeight ++ "px"
            , Html.Attributes.style "border" "5px solid grey"
            , Html.Attributes.style "border-radius" "5px"
            --        , Html.Events.on "mousemove" (Json.map UpdatePointerPosition offsetPosition)
            , Html.Events.stopPropagationOn "mousemove" offsetPosition
            , Html.Events.onMouseDown DrawStart
            , Html.Events.onMouseUp DrawEnd
            ]
            [ Svg.rect
                [ Svg.Attributes.width <| String.fromInt cWidth
                , Svg.Attributes.height <| String.fromInt cHeight
                , Svg.Attributes.fill "#fdf6e3"
                , Svg.Attributes.x "0"
                , Svg.Attributes.y "0"
                ]
                []
            , Svg.circle
                [ cx mouseX
                , cy mouseY
                , r "10"
                , Svg.Attributes.fill "#fd635e"
                , Svg.Attributes.opacity "0.7"
                ]
                []
            , linesToDraw
            ]


offsetPosition : Json.Decoder ( Msg, Bool )
offsetPosition =
    map2 Position
        (field "offsetX" int)
        (field "offsetY" int)
        |> Json.map UpdatePointerPosition
        |> Json.map (\msg -> ( msg, True ))



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
