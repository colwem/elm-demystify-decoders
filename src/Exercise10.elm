module Exercise10 exposing (Person, PersonDetails, Role(..), decoder)

import Json.Decode as D exposing (Decoder, fail)



{- Let's try and do a complicated decoder, this time. No worries, nothing new
   here: applying the techniques you've used in the previous decoders should
   help you through this one.

   A couple of pointers:
    - try working "inside out". Write decoders for the details and role first
    - combine those decoders + the username and map them into the Person constructor
    - finally, wrap it all together to build it into a list of people


   Example input:

        [ { "username": "Phoebe"
          , "role": "regular"
          , "details":
            { "registered": "yesterday"
            , "aliases": [ "Phoebs" ]
            }
          }
        ]
-}


type alias Person =
    { username : String
    , role : Role
    , details : PersonDetails
    }


type alias PersonDetails =
    { registered : String
    , aliases : List String
    }


type Role
    = Newbie
    | Regular
    | OldFart


decoder : Decoder (List Person)
decoder =
   D.list <| D.map3 Person
      (D.field "username" D.string)
      (D.field "role" roleDecoder)
      (D.field "details" personDetailsDecoder)


personDetailsDecoder : Decoder PersonDetails
personDetailsDecoder =
   D.map2 PersonDetails
      (D.field "registered" D.string)
      (D.field "aliases" <| D.list D.string)

roleDecoder : Decoder Role
roleDecoder =
   D.string |> D.andThen (\s -> 
      case s of
         "regular" ->
            D.succeed Regular

         "newbie" ->
            D.succeed Newbie

         "oldfart" ->
            D.succeed OldFart

         _ ->
            D.fail "Unknown role"
      )

{- Once you think you're done, run the tests for this exercise from the root of
   the project:

   - If you have installed `elm-test` globally:
        `elm test tests/Exercise10`

   - If you have installed locally using `npm`:
        `npm run elm-test tests/Exercise10`

   - If you have installed locally using `yarn`:
        `yarn elm-test tests/Exercise10`
-}
