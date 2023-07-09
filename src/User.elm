module User exposing (Email, Password, ValidCredentials, andThenValidateConfirmPassword, credentialsEncoder, validateCredentials)

import Helpers exposing (validEmail)
import Json.Encode as Encode
import Regex


type Email
    = Email String


type Password
    = Password String


type alias ValidCredentials =
    { email : Email
    , password : Password
    }


credentialsEncoder : ValidCredentials -> Encode.Value
credentialsEncoder { email, password } =
    Encode.object
        [ ( "email", Encode.string <| fromEmailToString email )
        , ( "password", Encode.string <| fromPasswordToString password )
        ]


fromEmailToString : Email -> String
fromEmailToString (Email validEmail) =
    validEmail


fromPasswordToString : Password -> String
fromPasswordToString (Password validPassword) =
    validPassword


fromStringToValidEmail : String -> Result String Email
fromStringToValidEmail email =
    let
        trimmedEmail =
            String.trim email
    in
    if String.isEmpty trimmedEmail then
        Err "Email can't be empty"

    else if not <| Regex.contains validEmail trimmedEmail then
        Err "Email is invalid"

    else
        Ok (Email trimmedEmail)


fromStringToValidPassword : String -> Result String Password
fromStringToValidPassword password =
    let
        trimmedPassword =
            String.trim password
    in
    if String.isEmpty trimmedPassword then
        Err "Password can't be empty"

    else if String.length trimmedPassword < 10 then
        Err "Password can't be less then 10 characters"

    else
        Ok (Password trimmedPassword)


andThenValidateConfirmPassword : String -> Result String ValidCredentials -> Result String ValidCredentials
andThenValidateConfirmPassword confirmPassword resultCredential =
    resultCredential
        |> Result.andThen
            (\{ email, password } ->
                let
                    validPassword =
                        fromPasswordToString password

                    trimmedConfirmPassword =
                        String.trim confirmPassword
                in
                if String.length trimmedConfirmPassword < 10 then
                    Err "Confirm password can't be less then 10 characters"

                else if validPassword /= trimmedConfirmPassword then
                    Err "Passwords doesn't match"

                else if String.isEmpty trimmedConfirmPassword then
                    Err "Confirm password can't be empty"

                else
                    Ok { email = email, password = password }
            )


validateCredentials : { email : String, password : String } -> Result String ValidCredentials
validateCredentials { email, password } =
    Result.map2
        (\validEmail validPassword ->
            { email = validEmail, password = validPassword }
        )
        (fromStringToValidEmail email)
        (fromStringToValidPassword password)
