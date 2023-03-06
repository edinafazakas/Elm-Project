module Main exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (checked, placeholder, style, type_, value, disabled)
import Html.Events exposing (..)
import Http
import Json.Decode as De
import Model exposing (..)
import Model.Event as Event
import Model.Event.Category as EventCategory
import Model.PersonalDetails as PersonalDetails
import Model.Repo as Repo


type Msg
    = GetRepos
    | GotRepos (Result Http.Error (List Repo.Repo))
    | SelectEventCategory EventCategory.EventCategory
    | DeselectEventCategory EventCategory.EventCategory
    | ActivateCheckBox Bool

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetRepos ->
            ( model, getRepos)

        GotRepos res ->
            case res of
                Ok repo ->
                    ({model | repos = repo}, Cmd.none)
                Err err -> 
                    ({model | error = "err"}, Cmd.none)
            
            --(model, Cmd.none)
        SelectEventCategory category ->
            ({model | selectedEventCategories = (setselectedEventCategory category model)} , Cmd.none)

        DeselectEventCategory category ->
            ( {model | selectedEventCategories = (deSetselectedEventCategory category model)}, Cmd.none )
        
        ActivateCheckBox status ->
            ({model | activateCheckBox = status}, Cmd.none)


eventCategoryToMsg : ( EventCategory.EventCategory, Bool ) -> Msg
eventCategoryToMsg ( event, selected ) =
    if selected then
        SelectEventCategory event

    else
        DeselectEventCategory event

setselectedEventCategory category model= 
    if category == EventCategory.Academic then
        EventCategory.set category True model.selectedEventCategories
    else if category == EventCategory.Work then
        EventCategory.set category True model.selectedEventCategories
    else if category == EventCategory.Project then
        EventCategory.set category True model.selectedEventCategories
    else if category == EventCategory.Award then
        EventCategory.set category True model.selectedEventCategories
    else EventCategory.set category False model.selectedEventCategories

deSetselectedEventCategory category model= 
    if category == EventCategory.Academic then
        EventCategory.set category False model.selectedEventCategories
    else if category == EventCategory.Work then
        EventCategory.set category False model.selectedEventCategories
    else if category == EventCategory.Project then
        EventCategory.set category False model.selectedEventCategories
    else if category == EventCategory.Award then
        EventCategory.set category False model.selectedEventCategories
    else EventCategory.set category False model.selectedEventCategories


getRepos : Cmd Msg
getRepos = Http.get 
        { url = "https://api.github.com/users/edinafazakas/repos"
        , expect = Http.expectJson GotRepos (De.list Repo.decodeRepo) 
        }

view : Model -> Html Msg
view model =
    let
        eventCategoriesView =
            EventCategory.view model.selectedEventCategories |> Html.map eventCategoryToMsg

        eventsView =
            model.events
                |> List.filter (.category >> (\cat -> EventCategory.isEventCategorySelected cat model.selectedEventCategories))
                |> List.map Event.view
                |> div[]
                |> Html.map never

        reposView =
            model.repos
                |> Repo.sortByStars
                |> List.take 5
                |> List.map Repo.view
                |> div []
    in
    div []
        [ PersonalDetails.view model.personalDetails
        , h2 [] [ text "Experience" ]
        , eventCategoriesView
        , eventsView
        , h2 [] [ text "My top repos" ]
        , p[][]
        , div[] [button [ onClick GetRepos ] [ text "Get Repos" ]]
        , reposView
        ]
