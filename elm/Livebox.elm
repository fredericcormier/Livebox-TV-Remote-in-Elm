port module Livebox exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Regex exposing (..)


type alias ServerResponse =
    { responseCode : String
    , message : String
    }


type Msg
    = MenuClicked
    | OnOffClicked
    | UpClicked
    | RightClicked
    | DownClicked
    | LeftClicked
    | OkClicked
    | VolumeUpClicked
    | VolumeDownClicked
    | ChannelUpClicked
    | ChannelDownClicked
    | MuteClicked
    | BackClicked
    | Pad1Clicked
    | Pad2Clicked
    | Pad3Clicked
    | Pad4Clicked
    | Pad5Clicked
    | Pad6Clicked
    | Pad7Clicked
    | Pad8Clicked
    | Pad9Clicked
    | Pad0Clicked
    | VodClicked
    | FBWDClicked
    | FFWDClicked
    | PlayPauseClicked
    | RecClicked
    | NothingClicked
    | IPAddressChanged String
    | SubmitIPAddress
    | RemoteRequest (Result Http.Error ServerResponse)
    | InvalidIPFormat


lbModePressOnce : Int
lbModePressOnce =
    0


lbModePressLong : Int
lbModePressLong =
    1


lbModePressRelease : Int
lbModePressRelease =
    2


lbCodeOnOff : Int
lbCodeOnOff =
    116


lbCodePad0 : Int
lbCodePad0 =
    512


lbCodePad1 : Int
lbCodePad1 =
    513


lbCodePad2 : Int
lbCodePad2 =
    514


lbCodePad3 : Int
lbCodePad3 =
    515


lbCodePad4 : Int
lbCodePad4 =
    516


lbCodePad5 : Int
lbCodePad5 =
    517


lbCodePad6 : Int
lbCodePad6 =
    518


lbCodePad7 : Int
lbCodePad7 =
    519


lbCodePad8 : Int
lbCodePad8 =
    520


lbCodePad9 : Int
lbCodePad9 =
    521


lbCodeChannelUp : Int
lbCodeChannelUp =
    402


lbCodeChannelDown : Int
lbCodeChannelDown =
    403


lbCodeVolumeUp : Int
lbCodeVolumeUp =
    115


lbCodeVolumeDown : Int
lbCodeVolumeDown =
    114


lbCodeMute : Int
lbCodeMute =
    113


lbCodeDirectionUp : Int
lbCodeDirectionUp =
    103


lbCodeDirectionDown : Int
lbCodeDirectionDown =
    108


lbCodeDirectionLeft : Int
lbCodeDirectionLeft =
    105


lbCodeDirectionRight : Int
lbCodeDirectionRight =
    106


lbCodeOK : Int
lbCodeOK =
    352


lbCodeBack : Int
lbCodeBack =
    158


lbCodeMenu : Int
lbCodeMenu =
    139


lbCodePlayPause : Int
lbCodePlayPause =
    164


lbCodeFBWD : Int
lbCodeFBWD =
    168


lbCodeFFWD : Int
lbCodeFFWD =
    159


lbCodeREC : Int
lbCodeREC =
    167


lbCodeVOD : Int
lbCodeVOD =
    393



-- MODEL


type alias ButtonMode =
    Int


type alias ButtonCode =
    Int



{- Format of json returned from livebox
   {
   "result":
      {
      "responseCode": "-11",
      "message": "bad/invalid request",
      "data":{}
      }
   }
-}


type alias Model =
    { lastMessage : String
    , ipAddress : String
    , port_ : String
    , messageReturned : String
    }



{-
   The decoder works but , because of the 'XXX is not allowed by Access-Control-Allow-Origin' thing,
   every remote request returns a network error even though the remote request succeded.
   This happens in all browser except in the Atom Browser-plus package -
   That's what used for debugging
-}


serverResponseDecoder : Decode.Decoder ServerResponse
serverResponseDecoder =
    Decode.map2
        ServerResponse
        (Decode.at [ "result", "responseCode" ] Decode.string)
        (Decode.at [ "result", "message" ] Decode.string)


main : Program (Maybe String) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : Maybe String -> ( Model, Cmd Msg )
init ip =
    ( Model
        (toString NothingClicked)
        (Maybe.withDefault "192.168.1.10" ip)
        "8080"
        ""
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


port saveIP : String -> Cmd msg


port getIP : (String -> msg) -> Sub msg


sendRemoteCommand : Model -> ButtonMode -> ButtonCode -> Cmd Msg
sendRemoteCommand model mode code =
    let
        url =
            "http://"
                ++ model.ipAddress
                ++ ":"
                ++ model.port_
                ++ "/remoteControl/cmd?operation=01&key="
                ++ (code |> toString)
                ++ "&mode="
                ++ (mode |> toString)

        request =
            Http.get url serverResponseDecoder
    in
        Http.send RemoteRequest request



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RemoteRequest (Ok answer) ->
            ( { model | messageReturned = "Response: " ++ answer.responseCode ++ " Message: " ++ answer.message }, Cmd.none )

        RemoteRequest (Err error) ->
            case error of
                Http.BadUrl s ->
                    ( { model | messageReturned = "Bad Url: " ++ s }, Cmd.none )

                Http.Timeout ->
                    ( { model | messageReturned = "Http Timeout" }, Cmd.none )

                Http.NetworkError ->
                    ( { model | messageReturned = "NetworkError" }, Cmd.none )

                Http.BadStatus response ->
                    ( { model | messageReturned = "Bad Http Status: " ++ toString response.status.code }, Cmd.none )

                Http.BadPayload message response ->
                    ( { model
                        | messageReturned =
                            "Bad Http Payload: "
                                ++ toString message
                                ++ " ("
                                ++ toString response.status.code
                                ++ ")"
                      }
                    , Cmd.none
                    )

        MenuClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodeMenu )

        OnOffClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodeOnOff )

        UpClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodeDirectionUp )

        RightClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodeDirectionRight )

        DownClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodeDirectionDown )

        LeftClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodeDirectionLeft )

        OkClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodeOK )

        VolumeUpClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodeVolumeUp )

        VolumeDownClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodeVolumeDown )

        ChannelUpClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodeChannelUp )

        ChannelDownClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodeChannelDown )

        MuteClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodeMute )

        BackClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodeBack )

        Pad1Clicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodePad1 )

        Pad2Clicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodePad2 )

        Pad3Clicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodePad3 )

        Pad4Clicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodePad4 )

        Pad5Clicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodePad5 )

        Pad6Clicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodePad6 )

        Pad7Clicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodePad7 )

        Pad8Clicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodePad8 )

        Pad9Clicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodePad9 )

        Pad0Clicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodePad0 )

        VodClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodeVOD )

        FBWDClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodeFBWD )

        FFWDClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodeFFWD )

        RecClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodeREC )

        PlayPauseClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, sendRemoteCommand model lbModePressOnce lbCodePlayPause )

        NothingClicked ->
            ( { model | lastMessage = (msg |> toString), messageReturned = "" }, Cmd.none )

        IPAddressChanged s ->
            ( { model | ipAddress = s, messageReturned = "" }, Cmd.none )

        SubmitIPAddress ->
            if validateIPAddress model.ipAddress then
                ( { model | lastMessage = (msg |> toString), messageReturned = "" }, saveIP model.ipAddress )
            else
                ( { model | ipAddress = "IPAddress Bad Format" }, Cmd.none )

        InvalidIPFormat ->
            ( model, Cmd.none )


validateIPAddress : String -> Bool
validateIPAddress ipAddress =
    let
        ipAddressRegex =
            "^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$"
    in
        if Regex.contains (Regex.regex ipAddressRegex) ipAddress then
            True
        else
            False


type alias CSSClass =
    String


imageButton : CSSClass -> Msg -> Bool -> String -> Html Msg
imageButton cssClass message clickable img =
    let
        altMessage message =
            case message of
                NothingClicked ->
                    ""

                _ ->
                    message |> toString
    in
        Html.button
            [ onClick message
            , classList
                [ ( cssClass, True )
                , ( "clickable", clickable )
                ]
            ]
            [ Html.span
                []
                [ Html.img [ src img, alt (altMessage message) ]
                    []
                ]
            ]


type alias Label =
    String



-- The fieldChangedMsg parameter is a Msg type with a payload of a string thus (String -> Msg)


submitIPAddressBox :
    CSSClass
    -> Msg
    -> (String -> Msg)
    -> Label
    -> String
    -> Html Msg
submitIPAddressBox submitClass submitMsg fieldChangedMsg label fieldContent =
    div [ class submitClass ]
        [ div [ class "form-label" ] [ text label ]
        , Html.input [ value fieldContent, onInput fieldChangedMsg ] []
        , Html.button [ onClick submitMsg ] [ text "Submit" ]
        ]


view : Model -> Html Msg
view model =
    let
        buttonClass =
            "lb-cell"

        submitBoxClass =
            "single-submit-box"

        submitBoxLabel =
            "TV Decoder IP Address"
    in
        div
            [ class "" ]
            [ Html.h1 [ id "title" ] [ text "Livebox TV Remote" ]
            , submitIPAddressBox
                submitBoxClass
                SubmitIPAddress
                IPAddressChanged
                submitBoxLabel
                model.ipAddress
            , div
                [ id "remote" ]
                [ Html.div
                    [ class "lb-row row-1" ]
                    [ imageButton buttonClass MenuClicked True "./img/lb-menu.png"
                    , imageButton buttonClass NothingClicked False ""
                    , imageButton buttonClass UpClicked True "./img/lb-up.png"
                    , imageButton buttonClass NothingClicked False ""
                    , imageButton buttonClass OnOffClicked True "./img/lb-onoff.png"
                    ]
                , Html.div
                    [ class "lb-row row-2" ]
                    [ imageButton buttonClass NothingClicked False ""
                    , imageButton buttonClass LeftClicked True "./img/lb-left.png"
                    , imageButton buttonClass OkClicked True "./img/lb-ok.png"
                    , imageButton buttonClass RightClicked True "./img/lb-right.png"
                    , imageButton buttonClass NothingClicked False ""
                    ]
                , Html.div
                    [ class "lb-row row-3" ]
                    [ imageButton buttonClass NothingClicked False ""
                    , imageButton buttonClass NothingClicked False ""
                    , imageButton buttonClass DownClicked True "./img/lb-down.png"
                    , imageButton buttonClass NothingClicked False ""
                    , imageButton buttonClass NothingClicked False ""
                    ]
                , Html.div
                    [ class "lb-row row-4" ]
                    [ imageButton buttonClass VolumeUpClicked True "./img/lb-volplus.png"
                    , imageButton buttonClass Pad1Clicked True "./img/lb-1.png"
                    , imageButton buttonClass Pad2Clicked True "./img/lb-2.png"
                    , imageButton buttonClass Pad3Clicked True "./img/lb-3.png"
                    , imageButton buttonClass ChannelUpClicked True "./img/lb-chanplus.png"
                    ]
                , Html.div
                    [ class "lb-row row-5" ]
                    [ imageButton buttonClass VolumeDownClicked True "./img/lb-volminus.png"
                    , imageButton buttonClass Pad4Clicked True "./img/lb-4.png"
                    , imageButton buttonClass Pad5Clicked True "./img/lb-5.png"
                    , imageButton buttonClass Pad6Clicked True "./img/lb-6.png"
                    , imageButton buttonClass ChannelDownClicked True "./img/lb-chanminus.png"
                    ]
                , Html.div
                    [ class "lb-row row-6" ]
                    [ imageButton buttonClass MuteClicked True "./img/lb-mute.png"
                    , imageButton buttonClass Pad7Clicked True "./img/lb-7.png"
                    , imageButton buttonClass Pad8Clicked True "./img/lb-8.png"
                    , imageButton buttonClass Pad9Clicked True "./img/lb-9.png"
                    , imageButton buttonClass BackClicked True "./img/lb-back.png"
                    ]
                , Html.div
                    [ class "lb-row row-7" ]
                    [ imageButton buttonClass NothingClicked False ""
                    , imageButton buttonClass NothingClicked False ""
                    , imageButton buttonClass Pad0Clicked True "./img/lb-0.png"
                    , imageButton buttonClass NothingClicked False ""
                    , imageButton buttonClass NothingClicked False ""
                    ]
                , Html.div
                    [ class "lb-row row-6" ]
                    [ imageButton buttonClass VodClicked True "./img/lb-vod.png"
                    , imageButton buttonClass FBWDClicked True "./img/lb-fbwd.png"
                    , imageButton buttonClass PlayPauseClicked True "./img/lb-playpause.png"
                    , imageButton buttonClass FFWDClicked True "./img/lb-ffwd.png"
                    , imageButton buttonClass RecClicked True "./img/lb-rec.png"
                    ]
                ]
            ]
