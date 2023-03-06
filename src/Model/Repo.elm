module Model.Repo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Json.Decode as De
import Json.Decode exposing (Decoder, map5, field, int, string, maybe)


type alias Repo =
    { name : String
    , description : Maybe String
    , url : String 
    , pushedAt : String
    , stars : Int
    }

fromJustString : Maybe String -> String
fromJustString x = case x of
    Just y -> y
    Nothing -> ""

view : Repo -> Html msg
view repo =
     
    div [] [
     p[][]
    ,div[class "repo"] [] 
    ,div[class "repo-name"] [text ("repo name: " ++ repo.name)] 
    ,div[class "repo-description"] [text ("repo description: " ++ (fromJustString repo.description))]      
    ,div[class "repo-url"]
      [ a
             [ href repo.url]
             [ text repo.url]
      ]
    ,div[class "repo-stars"] [text ("repo stars: " ++ (String.fromInt(repo.stars)))]
     ]
    --Debug.todo "Implement Model.Repo.view"


sortByStars : List Repo -> List Repo
sortByStars = \repos -> sortByWith .stars ascending repos
    --Debug.todo "Implement Model.Repo.sortByStars"

sortByWith : (a -> comparable) -> (comparable -> comparable -> Order) -> List a -> List a
sortByWith accessor sortFunc list =
    List.sortWith (orderBy accessor sortFunc) list

orderBy : (a -> comparable) -> (comparable -> comparable -> Order) -> a -> a -> Order
orderBy accessor orderFunc a b =
        orderFunc (accessor a) (accessor b)

ascending : comparable -> comparable -> Order
ascending a b =
    case compare a b of
        LT -> LT
        EQ -> EQ
        GT -> GT

{-| Deserializes a JSON object to a `Repo`.
Field mapping (JSON -> Elm):

  - name -> name
  - description -> description
  - html\_url -> url
  - pushed\_at -> pushedAt
  - stargazers\_count -> stars

-}
decodeRepo : De.Decoder Repo
decodeRepo =
     De.map5 Repo
    (De.field "name" De.string)
    (De.maybe(De.field "description" De.string))
    (De.field "html_url" De.string)
    (De.field "pushed_at" De.string)
    (De.field "stargazers_count" De.int)
    --Debug.todo "Implement Model.Repo.decodeRepo"

