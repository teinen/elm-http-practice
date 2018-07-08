module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Task exposing (Task)



-- MODEL
type Tag =
    Tag String


type alias Article =
    { title : String
    , content : String
    }


type alias Model =
    { tags : List Tag
    , articles : List Article
    }


init : (Model, Cmd Msg)
init =
    ( Model [] []
    , Cmd.none
    )



-- MESSAGES
type Msg =
    GetTags
    | GetArticles
    | GetTagsAndArticles
    | NewTags (Result Http.Error (List Tag))
    | NewArticles (Result Http.Error (List Article))
    | NewTagsAndArticles (Result Http.Error Model)



-- VIEW
view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "ボタン一覧" ]
        , button [ onClick GetTags ] [ text "Get Tags" ]
        , button [ onClick GetArticles ] [ text "Get Articles" ]
        , button [ onClick GetTagsAndArticles ] [ text "Get Tags and Articles" ]
        , h2 [] [ text "タグ一覧" ]
        , ul [] (List.map viewTags model.tags)
        , h2 [] [ text "記事一覧" ]
        , ul [] (List.map viewArticles  model.articles)
        ]

viewTags : Tag -> Html Msg
viewTags (Tag t) =
    li [] [ text t ]

viewArticles : Article -> Html Msg
viewArticles a =
    li []
        [ h3 [] [ text a.title ]
        , p [] [ text a.content ]
        ]



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTags ->
            ( model, getTags )

        GetArticles ->
            ( model, getArticles )

        GetTagsAndArticles ->
            ( model, getTagsAndArticles )

        NewTags (Ok newtags) ->
            ( Model newtags [], Cmd.none )

        NewTags (Err _) ->
            ( model, Cmd.none )

        NewArticles (Ok newarticles) ->
            ( Model [] newarticles , Cmd.none )

        NewArticles (Err _) ->
            ( model, Cmd.none )

        NewTagsAndArticles (Ok newmodel) ->
            ( newmodel, Cmd.none )

        NewTagsAndArticles (Err _) ->
            ( model, Cmd.none )



-- HTTPS
url_tags : String
url_tags =
    "http://localhost:3000/path1"

url_articles : String
url_articles =
    "http://localhost:3000/path2"


requestTags : Http.Request (List Tag)
requestTags =
    Http.get url_tags ( Decode.field "tags" tagCollectionDecoder )

tagCollectionDecoder : Decode.Decoder (List Tag)
tagCollectionDecoder =
    Decode.list tagDecoder

tagDecoder : Decode.Decoder Tag
tagDecoder =
    Decode.map Tag
        Decode.string


requestArticles : Http.Request (List Article)
requestArticles =
    Http.get url_articles ( Decode.field "articles" articleCollectionDecoder )

articleCollectionDecoder : Decode.Decoder (List Article)
articleCollectionDecoder =
    Decode.list articleDecoder

articleDecoder : Decode.Decoder Article
articleDecoder =
    Decode.map2 toArticle
        (Decode.field "title" Decode.string)
        (Decode.field "content" Decode.string)

toArticle : String -> String -> Article
toArticle t c =
    { title = t, content = c }


getTags : Cmd Msg
getTags =
    Http.send NewTags requestTags

getArticles : Cmd Msg
getArticles =
    Http.send NewArticles requestArticles

getTagsAndArticles : Cmd Msg
getTagsAndArticles =
    Task.attempt NewTagsAndArticles ( Task.map2 toModel ( Http.toTask (requestTags) )  ( Http.toTask (requestArticles) ) )

toModel : List Tag -> List Article -> Model
toModel t a =
    { tags = t, articles = a }



-- MAIN
main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }