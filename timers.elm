import Html exposing (Html, div, h1, h3, input, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)
import Table
import Time.DateTime as DateTime exposing (zero)
import Time exposing (Time, second)


main =
  Html.program
    { init = init timers
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { timers : List Timer
  , tableState : Table.State
  , query : String
  , currentTime : Maybe Time
  , nextTimer : Maybe Timer
  }


init : List Timer -> ( Model, Cmd Msg )
init timers =
  let
    model =
      { timers = timers
      , tableState = Table.initialSort "Next"
      , query = ""
      , currentTime = Nothing
      , nextTimer = Nothing
      }
  in
    ( model, Cmd.none )



-- UPDATE


type Msg
  = SetQuery String
  | SetTableState Table.State
  | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SetQuery newQuery ->
      ( { model | query = newQuery }
      , Cmd.none
      )

    SetTableState newState ->
      ( { model | tableState = newState }
      , Cmd.none
      )

    Tick time ->
      ( { model | currentTime = Just time, nextTimer = List.head model.timers}
      , Cmd.none
      )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick


-- VIEW


view : Model -> Html Msg
view { timers, tableState, query, currentTime, nextTimer } =
  let
    lowerQuery =
      String.toLower query

    acceptableTimers =
      List.filter (String.contains lowerQuery << String.toLower << .name) timers
  in
    div []
    [
      div []
        [ h1 [] [ text "Timers" ]
        , input [ placeholder "Search by Name", onInput SetQuery ] []
        , Table.view config tableState acceptableTimers
        ]
      , div []
        [ h1 [] [ text "Next" ]
        , h3 [] [ text (showExpiration nextTimer currentTime)]
        ]
    ]

showExpiration : Maybe Timer -> Maybe Time -> String
showExpiration timer time =
  case timer of
    Just timer ->
      case time of
        Just t -> (DateTime.toTimestamp timer.next) - t |> DateTime.fromTimestamp |> DateTime.second |> toString
        Nothing -> ""
    Nothing -> ""

config : Table.Config Timer Msg
config =
  Table.config
    { toId = .name
    , toMsg = SetTableState
    , columns =
        [ Table.stringColumn "Name" .name
        , Table.stringColumn "Next" (\model -> model.next |> DateTime.toISO8601)
        ]
    }



-- TIMERS

type alias Timer =
  { name : String
  , next : DateTime.DateTime
  }

timers : List Timer
timers =
  [ Timer "Aaa" (DateTime.dateTime {zero | year = 2017, month = 4, day = 23, hour = 23, minute = 37, second = 0})
  , Timer "Bbb" (DateTime.dateTime {zero | year = 2018, month = 1, day = 1, hour = 17, minute = 59, second = 0})
  ]
