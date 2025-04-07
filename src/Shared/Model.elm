module Shared.Model exposing
    ( Background
    , DateBirth
    , EventDate
    , FileUploadModel
    , GraveyardName
    , Letter
    , Model
    , Photo
    , User
    , YearBirth
    , YearDeath
    )

import File exposing (File)


type alias Model =
    { user : Maybe User }


type alias User =
    { token : String
    , id : String
    , name : String
    , image : String
    , email : String
    , role : String
    }


type alias Letter =
    { name : String
    , photo : Photo
    , yearBirth : YearBirth
    , yearDeath : YearDeath
    , date : EventDate
    , graveyardName : GraveyardName
    , background : Background
    }


type alias YearBirth =
    Int


type alias YearDeath =
    Int


type alias DateBirth =
    { year : Int
    , month : Int
    , day : Int
    }


type alias EventDate =
    { day : Int
    , month : Int
    , year : Int
    , time : String
    }


type alias GraveyardName =
    String


type alias Background =
    File


type alias Photo =
    File


type alias FileUploadModel =
    { hover : Bool
    , file : Maybe File
    }
