module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as HtmlAttributes
import Html.Events as HtmlEvents
import Array exposing (Array)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Json.Decode as Json exposing (..)
import Svg as Svg
import Svg.Attributes as SvgAttributes
import Types exposing (..)
import Symbols exposing (recognizeSymbol)
import DataManipulation exposing (..)


-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }


canvasSize =
    { width = 250
    , height = 300
    }



-- MODEL


type alias Model =
    { path : Array Point
    , smoothedPath : Array Point
    , thinnedPath : Array Point
    , directionsPath : Array Direction
    , corners : Array Point
    , currentlyDrawing : Bool
    , pointerPosition : Position
    , smoothingFactor : Float
    , thinningFactor : Int
    , cornerThreshold : Float
    , startingCoordinates : Point
    , endingCoordinates : Point
    , recognizedSymbol : String
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { path = Array.empty
      , smoothedPath = Array.empty
      , thinnedPath = Array.empty
      , directionsPath = Array.empty
      , corners = Array.empty
      , currentlyDrawing = False
      , pointerPosition = Position 0 0
      , smoothingFactor = 0.75
      , thinningFactor = 2
      , cornerThreshold = 110
      , startingCoordinates = ( 0, 0 )
      , endingCoordinates = ( 0, 0 )
      , recognizedSymbol = ""
      }
    , Cmd.none
    )


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
                        rawPointsSoFar =
                            model.path

                        newPoint =
                            ( pos.x, pos.y )

                        rawPointsNew =
                            Array.push newPoint rawPointsSoFar

                        smoothedPath =
                            smoothing
                                rawPointsNew
                                model.smoothedPath
                                model.smoothingFactor
                                newPoint

                        thinnedPath =
                            thinning
                                smoothedPath
                                model.thinnedPath
                                model.thinningFactor

                        directionsPath =
                            getDirections thinnedPath model.directionsPath


                        corners =
                            detectCorners
                                thinnedPath
                                model.corners
                                model.cornerThreshold
                    in
                    ( { model
                        | pointerPosition = pos
                        , path = rawPointsNew
                        , smoothedPath = smoothedPath
                        , thinnedPath = thinnedPath
                        , directionsPath = directionsPath
                        , corners = corners
                      }
                    , Cmd.none
                    )

                False ->
                    ( { model | pointerPosition = pos }, Cmd.none )

        DrawStart ->
            ( { model
                | currentlyDrawing = True
                , path = Array.empty
                , smoothedPath = Array.empty
                , thinnedPath = Array.empty
                , directionsPath = Array.empty
                , corners = Array.empty
              }
            , Cmd.none
            )

        DrawEnd ->
            let
                ( startQuadrant, endQuadrant ) =
                    getStartAndEndPosition model.thinnedPath

                conditionedDirections =
                    conditioningDirections model.directionsPath

                mainDirections = Array.toList <| Array.slice 0 4 conditionedDirections
                secondaryDirection  = Array.toList <| Array.slice 4 6 conditionedDirections

                symbol : Symbol
                symbol =
                    { mainDirections = mainDirections
                    , secondaryDirections = secondaryDirection
                    , corners = model.corners
                    , startQuadrant = startQuadrant
                    , endQuadrant = endQuadrant
                    }

                recognizedSymbol =
                    recognizeSymbol symbol
            in
            ( { model
                | currentlyDrawing = False
                , recognizedSymbol = recognizedSymbol
                , directionsPath = conditionedDirections
              }
            , Cmd.none
            )


-- VIEW


view : Model -> Html Msg
view model =
    let
        mouseX =
            String.fromInt model.pointerPosition.x

        mouseY =
            String.fromInt model.pointerPosition.y

        vBox =
            SvgAttributes.viewBox <|
                "0 0 "
                    ++ String.fromFloat canvasSize.width
                    ++ " "
                    ++ String.fromFloat canvasSize.height

        linesToDraw =
            pathToSvg model.path

        smoothedLines =
            pathToSvg model.smoothedPath

        thinnedLines =
            pathToSvg model.thinnedPath
    in
--    Debug.log (Debug.toString model.directionsPath)
--    Debug.log (Debug.toString model.corners)
--    Debug.log (Debug.toString model.directionsPath)
        --    <|
        layout
        []
    <|
        column
            [ Background.color (rgb 253 246 227)
            , width fill
            , height fill
            , spacing 10
            , centerX
            ]
            [ column [centerX, width (px 300), padding 10]
                [ row [ Font.bold ] [text <| "Number of Points: " ++ (String.fromInt <| Array.length model.path)]
                , row [ Font.bold, height (px 50)]
                    [ text "Current Coordinates: "
                    , column []
                        [ text <| "x: " ++ mouseX
                        , text <| "y: " ++ mouseY
                        ]
                    ]
                , row [centerX, width fill, Font.bold]
                                [text <| "Symbol: " ++ model.recognizedSymbol]
                ]
            , row
                [ padding 20
                , centerX
                , spacing 10
                ]
                [ column []
                    [ el [centerX] (text "Raw Input")
                    , drawingBox vBox mouseX mouseY linesToDraw]
                , column []
                    [ el [centerX] (text "Smoothed Input")
                    , drawingBox vBox mouseX mouseY smoothedLines]
                , column []
                    [ el [centerX] (text "Thinned Input")
                    , drawingBox vBox mouseX mouseY thinnedLines]
                ]
            --        , mouseCircle mouseX mouseY
            ]


pathToSvg : Array Point -> Svg.Svg msg
pathToSvg points =
    let
        x =
            List.map pointToString (Array.toList points)
                |> String.join " "

        polyColor =
            "grey"
    in
    Svg.polyline
        [ SvgAttributes.points x
        , SvgAttributes.fill "none"
        , SvgAttributes.stroke polyColor
        , SvgAttributes.strokeWidth "3"
        ]
        []


drawingBox : Svg.Attribute Msg -> String -> String -> Svg.Svg Msg -> Element.Element Msg
drawingBox vBox mouseX mouseY linesToDraw =
    Element.html <|
        Svg.svg
            [ vBox
            , SvgAttributes.width <| String.fromInt canvasSize.width ++ "px"
            , SvgAttributes.height <| String.fromInt canvasSize.height ++ "px"
            , HtmlAttributes.style "border" "5px solid grey"
            , HtmlAttributes.style "border-radius" "5px"

            -- , Html.Events.on "mousemove" (Json.map UpdatePointerPosition offsetPosition)
            -- , Html.Events.on "mousedown" (Json.map DrawStart mouseCoord)
            -- , Html.Events.on "mouseup" (Json.map DrawEnd mouseCoord)
            , HtmlEvents.onMouseDown DrawStart
            , HtmlEvents.onMouseUp DrawEnd
            , HtmlEvents.stopPropagationOn "mousemove" offsetPosition
            ]
            [ Svg.rect
                [ SvgAttributes.width <| String.fromInt canvasSize.width
                , SvgAttributes.height <| String.fromInt canvasSize.height
                , SvgAttributes.fill "#fdf6e3"
                , SvgAttributes.x "0"
                , SvgAttributes.y "0"
                ]
                []
            , Svg.circle
                [ SvgAttributes.cx mouseX
                , SvgAttributes.cy mouseY
                , SvgAttributes.r "10"
                , SvgAttributes.fill "#fd635e"
                , SvgAttributes.opacity "0.7"
                ]
                []
            , linesToDraw
            ]


offsetPosition : Json.Decoder ( Msg, Bool )
offsetPosition =
    mouseCoord
        |> Json.map UpdatePointerPosition
        |> Json.map (\msg -> ( msg, True ))


mouseCoord : Decoder Position
mouseCoord =
    map2 Position
        (field "offsetX" int)
        (field "offsetY" int)



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
