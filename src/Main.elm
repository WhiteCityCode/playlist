module Main exposing (..)

import Array as Ar exposing (Array, fromList, toList)
import Browser
import Bulma.CDN exposing (..)
import Bulma.Columns as Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography exposing (textCentered)
import Debug
import Embed.Youtube as Y
import Embed.Youtube.Attributes as Yatt
import Embed.Youtube.Thumbnail as Yth
import Html exposing (Attribute, Html, a, img, main_, p, span, strong, text)
import Html.Attributes as Hatt exposing (attribute, class, href, src, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D exposing (Decoder, array, field, map2, string)
import Url


type alias Model =
    { videos : Array Video, current : Int, loading : Bool, error : Maybe String }


type Msg
    = PlayNew Int
    | GotVideos (Result Http.Error (Array Video))


type alias VideoId =
    String


type alias VideoTitle =
    String


type alias Video =
    { id : VideoId, title : VideoTitle }


videosDecoder : Decoder (Array Video)
videosDecoder =
    array <| map2 Video (D.field "id" string) (D.field "title" string)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init flags =
    let
        emptyModel : Model
        emptyModel =
            { videos = Ar.empty, current = 0, loading = True, error = Nothing }
    in
    ( emptyModel
    , Http.get
        { url = "https://gist.githubusercontent.com/rredpoppy/dc37d37587fb37458cfa7c2a8beb44eb/raw/b4e98798e9e7df3e47dbe75b9ad91921ae4d3a14/playlist.json"
        , expect = Http.expectJson GotVideos videosDecoder
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayNew id ->
            ( { model | current = id }, Cmd.none )

        GotVideos (Ok vids) ->
            ( { model | loading = False, videos = vids }, Cmd.none )

        GotVideos (Err e) ->
            let
                err =
                    case e of
                        Http.BadUrl _ ->
                            "Incorrect URL"

                        Http.Timeout ->
                            "Destination has timed out"

                        Http.NetworkError ->
                            "Could not connect to data source"

                        Http.BadStatus cd ->
                            "Unexpected response status"

                        Http.BadBody _ ->
                            "Could not parse response"
            in
            ( { model | error = Just err }, Cmd.none )


view : Model -> Html Msg
view model =
    main_ []
        [ stylesheet
        , videoNavbar
        , case model.error of
            Nothing ->
                if model.loading then
                    progress
                        { size = Large
                        , color = Info
                        }
                        [ Hatt.max "100"
                        , style "margin" "20px 100px"
                        ]
                        []

                else
                    playlist model.videos model.current

            Just e ->
                notification
                    Danger
                    [ style "margin" "20px 100px" ]
                    [ text e ]
        , videoFooter
        ]


videoNavbar : Html Msg
videoNavbar =
    navbar { color = Dark, transparent = False }
        []
        [ navbarBrand []
            (navbarBurger False
                []
                [ span [] []
                , span [] []
                , span [] []
                ]
            )
            [ navbarItem False
                []
                [ img [ src "https://whitecitycode.com/images/logo/whitecity-logo.png" ] [] ]
            , navbarItem False
                []
                [ title H3 [ style "color" "white" ] [ text "Christmas Tunes" ] ]
            ]
        , navbarMenu False [] []
        , navbarEnd [] []
        ]


video : Int -> Bool -> Video -> Html Msg
video idx ac v =
    panelBlock ac
        [ onClick <| PlayNew idx ]
        [ media []
            [ mediaLeft []
                [ easyImage FourByThree [ style "width" "120px" ] <|
                    Url.toString <|
                        Yth.toUrl Yth.First <|
                            Y.fromString v.id
                ]
            , mediaContent []
                [ title H5
                    [ style "margin-top" "25px"
                    , style "color" <|
                        if ac then
                            "#333"

                        else
                            "#777"
                    , style "cursor" "pointer"
                    ]
                    [ text v.title ]
                ]
            , mediaRight [] []
            ]
        ]


playlist : Array Video -> Int -> Html Msg
playlist lst c =
    columns columnsModifiers
        [ style "margin-top" "20px" ]
        [ column columnModifiers [] <|
            Ar.toList <|
                Ar.indexedMap
                    (\i v -> video i (i == c) v)
                    lst
        , column columnModifiers
            []
            [ case Ar.get c lst of
                Just v ->
                    player v

                Nothing ->
                    text "No such video"
            ]
        ]


player : Video -> Html Msg
player v =
    Y.fromString v.id
        |> Y.attributes
            [ Yatt.width 640
            , Yatt.height 400
            , Yatt.autoplay
            , Yatt.modestBranding
            , Yatt.language "ro"
            ]
        |> Y.toHtml


videoFooter : Html Msg
videoFooter =
    footer []
        [ container []
            [ content Standard
                [ textCentered ]
                [ p []
                    [ strong [] [ text "Christmas Tunes" ]
                    , text " by "
                    , a [ href "https://whitecitycode.com" ] [ text "Adrian Rosian" ]
                    , text ". The source code is licensed "
                    , a [ href "http://opensource.org/licenses/mit-license.php" ] [ text "MIT" ]
                    ]
                ]
            ]
        ]
