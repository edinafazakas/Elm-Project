module Model.Event exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Model.Event.Category exposing (EventCategory(..))
import Model.Interval as Interval exposing (Interval)
import Compare exposing (Comparator)
import Html.Attributes exposing (class, href)

type alias Event =
    { title : String
    , interval : Interval
    , description : Html Never
    , category : EventCategory
    , url : Maybe String
    , tags : List String
    , important : Bool
    }


categoryView : EventCategory -> Html Never
categoryView category =
    case category of
        Academic ->
            text "Academic"

        Work ->
            text "Work"

        Project ->
            text "Project"

        Award ->
            text "Award"

categoryView1 : EventCategory -> String
categoryView1 category =
    case category of
        Academic -> "Academic"

        Work -> "Work"

        Project -> "Project"

        Award -> "Award"

stringFromBool : Bool -> String
stringFromBool  = \value -> if value then "True" else "False"

sortByWith : (a -> Interval) -> (Interval -> Interval -> Order) -> List a -> List a
sortByWith accessor sortFunc list =
    List.sortWith (orderBy accessor sortFunc) list

orderBy : (a -> Interval) -> (Interval -> Interval -> Order) -> a -> a -> Order
orderBy accessor orderFunc a b =
        orderFunc (accessor a) (accessor b)

sortByInterval : List Event -> List Event
sortByInterval events =
    sortByWith .interval Interval.compare events
    --Debug.todo "Implement Event.sortByInterval"

fromJustString : Maybe String -> String
fromJustString x = case x of
    Just y -> y
    Nothing -> ""

view : Event -> Html Never
view event = 
    div []  
    [
    p[][]
    ,if event.important == True then
    div[class "event event-important"] [text "The event is important"]
    else
    div[class "event"][]        
    ,div[class "event-title"] [text ("title: " ++ event.title)] 
    ,div[class "event-interval"] [ ul [] [ Interval.view event.interval]  ]
    ,div[class "event-description"] [ ul[] [ p[] [event.description] ]] 
    ,div[class "event-category"] [ ul [] [ p[][text ("category: " ++ categoryView1(event.category))]]]
    ,div[class "event-tags"] (List.map viewTags event.tags)  
    ,if event.url /= Nothing then
        div[class "event-url"] 
            [ a
                [ href (fromJustString(event.url))]
                [ text (fromJustString(event.url))]
            ]
        
    else div[][] 
    ]

viewTags tag =
       div[]
         [ul[]
           [       
            p [] [ text tag]
            ]
          ]
    --Debug.todo "Implement the Model.Event.view function"
