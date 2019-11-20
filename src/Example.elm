module Example exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Masonry exposing (Masonry)
import Random


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { masonry : Masonry ( Int, Int )
    , masonryWithImages : Masonry String
    , randomHeights : List Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { masonry = Masonry.empty (Just "example-id")
      , masonryWithImages = Masonry.empty (Just "example-id-with-images")
      , randomHeights = []
      }
    , getRandomVales
    )


getRandomVales : Cmd Msg
getRandomVales =
    Cmd.batch
        [ Random.generate GotRandom <|
            Random.list 16 (Random.weighted ( 1, 24 ) [ ( 0.5, 120 ) ])
        , Random.generate GotRandomPath <|
            Random.list 16 (Random.weighted ( 1, "img2.png" ) [ ( 0.5, "img1.png" ) ])
        ]


type Msg
    = GotRandom (List Int)
    | GotRandomPath (List String)
    | MasonryMsg Masonry.Msg
    | MasonryWithImagesMsg Masonry.Msg
    | AddItems
    | ImageLoaded Masonry.Id


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRandom heights ->
            let
                currentLength =
                    List.length model.randomHeights

                addIndex i x =
                    ( currentLength + i, x )

                ( masonry, cmds ) =
                    Masonry.append
                        (List.indexedMap addIndex heights)
                        model.masonry
            in
            ( { model
                | masonry = masonry
                , randomHeights = model.randomHeights ++ heights
              }
            , Cmd.map MasonryMsg cmds
            )

        GotRandomPath paths ->
            let
                ( masonry, cmds ) =
                    Masonry.append paths model.masonryWithImages
            in
            ( { model | masonryWithImages = masonry }
            , Cmd.map MasonryMsg cmds
            )

        MasonryMsg masonryMsg ->
            ( { model | masonry = Masonry.update masonryMsg model.masonry }
            , Cmd.none
            )

        MasonryWithImagesMsg masonryMsg ->
            ( { model
                | masonryWithImages =
                    Masonry.update masonryMsg model.masonryWithImages
              }
            , Cmd.none
            )

        AddItems ->
            ( model, getRandomVales )

        ImageLoaded id ->
            ( model
            , Cmd.map MasonryWithImagesMsg (Masonry.getHeight id)
            )


view : Model -> Browser.Document Msg
view model =
    let
        config =
            { toView = viewItem
            , columns = 4
            }

        configWithImages =
            { toView = viewImage
            , columns = 3
            }
    in
    { title = "elm-masonry"
    , body =
        [ main_ []
            [ div [ style "display" "flex" ]
                [ div [ style "margin-right" "1rem" ]
                    [ h2 [] [ text "Balanced" ]
                    , Masonry.view config model.masonry
                    ]
                , div [ style "margin-right" "1rem" ]
                    [ h2 [] [ text "Unbalanced" ]
                    , Masonry.viewList config <|
                        List.indexedMap Tuple.pair model.randomHeights
                    ]
                , div []
                    [ h2 [] [ text "With images" ]
                    , Masonry.view configWithImages model.masonryWithImages
                    ]
                ]
            , button [ onClick AddItems ] [ text "add" ]
            ]
        ]
    }


viewItem : Masonry.Id -> ( Int, Int ) -> Html msg
viewItem _ ( index, height ) =
    div
        [ style "background" "orange"
        , style "margin" "1px"
        , style "width" "2rem"
        , style "height" (String.fromInt height ++ "px")
        ]
        [ text (String.fromInt index) ]


viewImage : Masonry.Id -> String -> Html Msg
viewImage id path =
    img
        [ src path
        , style "width" "5rem"
        , style "margin" "1px"
        , on "load" (Decode.succeed (ImageLoaded id))
        ]
        []
