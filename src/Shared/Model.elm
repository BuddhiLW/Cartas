module Shared.Model exposing
    ( EventDate
    , FileUpload(..)
    , FileUploadModel
    , FileUploadType(..)
    , GraveyardName
    , Letter
    , Model
    , User
    , YearBirth
    , YearDeath
    )

import File exposing (File)


type alias Model =
    { baseUrl : String
    , user : Maybe User
    }


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
    , photo : Maybe File
    , yearBirth : YearBirth
    , yearDeath : YearDeath
    , date : EventDate
    , graveyardName : GraveyardName
    , background : Maybe File
    , hour : Int
    , minute : Int
    }


type alias YearBirth =
    Int


type alias YearDeath =
    Int


type alias EventDate =
    { day : Int
    , month : Int
    , year : Int
    }


type alias GraveyardName =
    String


type FileUploadType
    = Photo
    | Background


type FileUpload
    = FileUpload FileUploadType File


type alias FileUploadModel =
    { hover : Bool
    , file : Maybe FileUpload
    , fileType : FileUploadType
    }
