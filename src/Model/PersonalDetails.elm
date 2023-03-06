module Model.PersonalDetails exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, id)
import Html.Attributes exposing (..)

type alias DetailWithName =
    { name : String
    , detail : String
    }


type alias PersonalDetails =
    { name : String
    , contacts : List DetailWithName
    , intro : String
    , socials : List DetailWithName
    }


view : PersonalDetails -> Html msg
view details =
    div []  
    [
     h1 [id "name"] [text details.name]
    ,em [id "intro"] [text ("Intro: " ++ details.intro)]
    ,div[class "contact-detail"] [text "contact-detail: "] 
    ,div[class "contact-detail"] (List.map viewContacts details.contacts)
    ,div[class "social-link"] (List.map viewSocials details.socials)
    ]
   --Debug.todo "Implement the Model.PersonalDetails.view function"
     
viewContacts contact =
   div[class "contact-detail"]
    [ul[]
    [       
     p [] [ text contact.name]
    ,p [] [ text contact.detail]
    ]
    ]
   
viewSocials social =
    div[class "social-link"]
    [ul[]
    [       
     p [] [ text social.name]
    ,div []
        [ a
             [ href social.detail]
             [ text social.detail]
         ]
    ]
    ]
  
   