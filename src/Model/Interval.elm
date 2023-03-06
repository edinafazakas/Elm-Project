module Model.Interval exposing (Interval, compare, full, length, oneYear, open, view, withDurationMonths, withDurationYears)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, style)
import Model.Date as Date exposing (Date, Month)
import Model.Util exposing (chainCompare)


type Interval
    = Interval { start : Date, end : Maybe Date }


{-| Create an `Interval` from 2 `Date`s. If the second date is before the first the date, the function will return
`Nothing`. When possible, use the `withDurationMonths` or `withDurationYears` functions.
-}
full : Date -> Date -> Maybe Interval
full start end =
    if Date.compare start end == GT then
        Nothing

    else
        Just <| Interval { start = start, end = Just end }


{-| Create an `Interval` from a start year, start month, and a duration in months.
The start year and month are explicitly required because the duration in months is only specified if the start date
also includes a month.
This function, (assuming positive inputs) by definition, can always return a valid `Interval`.
-}
withDurationMonths : Int -> Month -> Int -> Interval
withDurationMonths startYear startMonth duration =
    let
        start =
            Date.full startYear startMonth

        end =
            Date.offsetMonths duration start
    in
    Interval { start = start, end = Just end }


{-| Create an `Interval` from a start `Date`, and a duration in years. This function, (assuming positive inputs)
by definition, can always return a valid `Interval`.
-}
withDurationYears : Date -> Int -> Interval
withDurationYears start duration =
    let
        end =
            Date.offsetMonths (duration * 12) start
    in
    Interval { start = start, end = Just end }


{-| Create an open `Interval` from a start `Date`. Usually used for creating ongoing events.
-}
open : Date -> Interval
open start =
    Interval { start = start, end = Nothing }


{-| Convenience function to create an `Interval` that represents one year.
-}
oneYear : Int -> Interval
oneYear year =
    withDurationYears (Date.onlyYear year) 1


{-| The length of an Interval, in (years, months)
-}
length : Interval -> Maybe ( Int, Int )
length (Interval interval) =
    interval.end
        |> Maybe.andThen (Date.monthsBetween interval.start)
        |> Maybe.map (\totalMonths -> ( totalMonths // 12, modBy 12 totalMonths ))


{-| Compares two intervals.

Intervals are first compared compare by the `start` field.
If the `start` field is equal, the they are compare by the `end` fields:

  - If both are missing (`Nothing`), the intervals are considered equal
  - If both are present (`Just`), the longer interval is considered greater
  - If only one interval is open (its `end` field is `Nothing`) then it will be considered greater

```
    import Model.Date as Date

    Model.Interval.compare (oneYear 2019) (oneYear 2020) --> LT
    Model.Interval.compare (oneYear 2019) (withDurationYears (Date.onlyYear 2020) 2) --> LT
    Model.Interval.compare (withDurationMonths 2019 Date.Jan 2) (withDurationMonths 2019 Date.Jan 2) --> EQ
    Model.Interval.compare (withDurationMonths 2019 Date.Feb 2) (withDurationMonths 2019 Date.Jan 2) --> GT
    Model.Interval.compare (withDurationMonths 2019 Date.Jan 2) (open (Date.onlyYear 2019)) --> LT
```

-}
compare : Interval -> Interval -> Order
compare (Interval intA) (Interval intB) =
    if (Date.compare intA.start intB.start) == LT then
        LT
    else if  (Date.compare intA.start intB.start) == GT then
        GT
    else 
        if intA.end == Nothing && intB.end == Nothing then
            EQ
        else if intA.end == Nothing || intB.end == Nothing then
            if intA.end == Nothing then
                GT
            else LT
    else
        if (Date.fromJustInt <| (durationInterval (Interval intA))) == (Date.fromJustInt <| (durationInterval (Interval intB))) then 
           EQ
        else if (Date.fromJustInt <| (durationInterval (Interval intA))) < (Date.fromJustInt <| (durationInterval (Interval intB))) then
           LT
        else GT

    --Debug.todo "Implement Model.Interval.compare"


view : Interval -> Html msg
view (Interval interval) =
     div [] [
        div[class "interval"] [] 
       ,div[class "interval-start"]
        [
        text ("start of interval: " ++ (String.fromInt <| Date.year(interval.start)) ++ " " ++ (Date.monthToString <| Date.fromJustMonth <| Date.month(interval.start)))
        ] 
       ,div[class "interval-end"] 
       [text <|
                if interval.end == Nothing then
                    "Present"

                else
                    ("end of interval: " ++ (String.fromInt <| Date.year <| Date.fromJustDate(interval.end)) ++ " " ++  (Date.monthToString <| Date.fromJustMonth <| Date.month <| Date.fromJustDate(interval.end)))
       ] 
       ,div[class "interval-length"]
       [text <|
                if (length (Interval interval)) /= Nothing then
                    ("length of interval: " ++ (Date.from <| Date.fromJustIntt <| length (Interval interval)))

                else
                    "Length could not be calculated"
       ] 
     ]
    --Debug.todo "Implement Model.Interval.view"

durationInterval = \(Interval interval) -> Date.monthsBetween interval.start (Date.fromJustDate interval.end)


