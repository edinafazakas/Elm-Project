module Model.Event.Category exposing (EventCategory(..), SelectedEventCategories, allSelected, eventCategories, isEventCategorySelected, set, view, checkbox)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (checked, class, style, type_)
import Html.Events exposing (onCheck)


type EventCategory
    = Academic
    | Work
    | Project
    | Award


eventCategories =
    [ Academic, Work, Project, Award ]


{-| Type used to represent the state of the selected event categories
-}
type alias SelectedEventCategories = 
   {
        academic : Bool
       ,work : Bool
       ,project : Bool
       ,award : Bool
    }

{-| Returns an instance of `SelectedEventCategories` with all categories selected

    isEventCategorySelected Academic allSelected --> True

-}
allSelected : SelectedEventCategories
allSelected =
     {
      academic = True
    , work = True
    , project = True
    , award = True
    }
    --Debug.todo "Implement Model.Event.Category.allSelected"

{-| Returns an instance of `SelectedEventCategories` with no categories selected

-- isEventCategorySelected Academic noneSelected --> False

-}
noneSelected : SelectedEventCategories
noneSelected =
     {
      academic = False
    , work = False
    , project = False
    , award = False
    }
    --Debug.todo "Implement Model.Event.Category.noneSelected"

{-| Given a the current state and a `category` it returns whether the `category` is selected.

    isEventCategorySelected Academic allSelected --> True

-}
isEventCategorySelected : EventCategory -> SelectedEventCategories -> Bool
isEventCategorySelected category current =
     if current == allSelected then
            True
        else if current == noneSelected then
            False
        else if category == Academic then
            if current.academic == True then
                    True
                else False
        else if category == Work then
            if current.work == True then
                True
            else False
        else if category == Project then
            if current.project == True then
                True
            else False
        else if category == Award then
            if current.award == True then
                True
            else False
        else
         False

    --Debug.todo "Implement Model.Event.Category.isEventCategorySelected"


{-| Given an `category`, a boolean `value` and the current state, it sets the given `category` in `current` to `value`.

    allSelected |> set Academic False |> isEventCategorySelected Academic --> False

    allSelected |> set Academic False |> isEventCategorySelected Work --> True

-}
set : EventCategory -> Bool -> SelectedEventCategories -> SelectedEventCategories
set category value current =
     if category == Academic then 
        {academic = value
        ,work = current.work
        ,project = current.project
        ,award = current.award
        }
    else if category == Work then 
        {academic = current.academic
        ,work = value
        ,project = current.project
        ,award = current.award
        }
    else if category == Project then 
        {academic = current.academic
        ,work = current.work
        ,project = value
        ,award = current.award
        }
    else 
        {academic = current.academic
        ,work = current.work
        ,project = current.project
        ,award = value
        }
    --Debug.todo "Implement Model.Event.Category.set"


checkbox : String -> Bool -> EventCategory -> Html ( EventCategory, Bool )
checkbox name state category =
    div [ style "display" "inline", class "category-checkbox" ]
        [ input [ type_ "checkbox", onCheck (\c -> ( category, c )), checked state ] []
        , text name
        ]


view : SelectedEventCategories -> Html ( EventCategory, Bool )
view model =
     div [] [
         checkbox "Academic" model.academic Academic
        ,checkbox "Work" model.work Work
        ,checkbox "Project" model.project Project
        ,checkbox "Award" model.award Award
     ]
    --Debug.todo "Implement the Model.Event.Category.view function"
