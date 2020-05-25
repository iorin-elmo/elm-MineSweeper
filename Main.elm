module Main exposing (..)

import Browser
import Array exposing (Array)
import Html exposing (Html, div, text, br, input, span, button)
import Html.Attributes as A exposing (id, class, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Random exposing (Generator)
import Task exposing (perform, succeed)

import Debug exposing (log)

type alias Array2 a = Array (Array a)

type alias Model =
  { field : Array2 Status
  , viewField : Array2 ViewStatus
  , numOfMines : Int
  , fieldSize : ( Int, Int ) -- (x,y)
  , gameStatus : GameStatus
  , clickMode : Mode
  , startPos : ( Int, Int )
  }

type Status
  = Mine
  | Space Int -- number of mines around (0~8)

type ViewStatus
  = Opened Int -- number of mines around (0~8)
  | Flagged
  | Hidden

type GameStatus
  = Start
  | FirstOne
  | Playing
  | GameOver
  | Clear

type Mode
  = Open
  | Flag

initialModel =
  { field = Array.empty
  , viewField = Array.empty
  , numOfMines = 3
  , fieldSize = ( 5, 5 )
  , gameStatus = Start
  , clickMode = Open
  , startPos = (0,0)
  }

type Msg
  = Click ( Int, Int )
  | Place Int ( Int, Int )
  | ToggleMode
  | MakeField ()
  | Button
  | TextBox Type String

type Type
  = Width
  | Height
  | NumOfMines

putMine : (Int,Int) -> Generator ( Int, Int )
putMine ( x, y ) =
  Random.pair
    (Random.int 0 (x - 1))
    (Random.int 0 (y - 1))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    noChange = ( model, Cmd.none )
    debug = model |> log "model"
  in
    case msg of
      Button ->
        case model.gameStatus of
          Start ->
            ( { model |
                 gameStatus = FirstOne
              ,  clickMode = Open
              , field =
                (\(x,y) ->
                  Array.repeat y (Array.repeat x (Space 0))
                )
                model.fieldSize
              , viewField =
                (\(x,y) ->
                  Array.repeat y (Array.repeat x (Hidden))
                )
                model.fieldSize
              }
            , Cmd.none
            )
          GameOver ->
            ( { model | gameStatus = Start }
            , Cmd.none
            )
          Clear ->
            ( { model | gameStatus = Start }
            , Cmd.none
            )
          _ -> noChange
      Click (x,y) ->
        case model.clickMode of
          Open ->
            if model.gameStatus == FirstOne
            then
              ( { model |
                  startPos = (x,y)
                , viewField =
                  array2Set x y
                    (Hidden)
                    model.viewField
                , gameStatus = Playing
                }
              , Random.generate
                (Place 1)
                (putMine model.fieldSize)
              )
            else
              case array2Get x y model.field of
                Just Mine ->
                  ( { model |
                      gameStatus = GameOver
                    , viewField =
                      array2Set x y (Opened -1) model.viewField
                    }
                  , Cmd.none
                  )
                _ ->
                  let
                    num =
                      case array2Get x y model.field of
                        Just (Space n) -> n
                        _ -> -1
                    setViewField =
                      array2Set x y (Opened num)
                        model.viewField
                    newViewField =
                      if num == 0
                      then
                        autoOpen (x,y)
                          model.field
                          setViewField
                      else
                        setViewField
                  in
                    ( { model |
                        viewField = newViewField
                      }
                      |> clearCheck
                    , Cmd.none )
          Flag ->
            case array2Get x y model.viewField of
              Just Flagged ->
                ( { model |
                    viewField =
                      array2Set x y Hidden model.viewField
                  }
                , Cmd.none
                )
              Just Hidden ->
                ( { model |
                    viewField =
                      array2Set x y Flagged model.viewField
                  }
                , Cmd.none
                )

              _ -> noChange
      Place n (x,y) ->
        if
          ( (||)
            (array2Get x y model.field == Just Mine)
            ((x,y)==model.startPos)
          )
        then
          ( model
          , Random.generate
            (Place n)
            (putMine model.fieldSize)
          )
        else
          let
            newModel =
              { model |
                field =
                  array2Set
                  x y
                  Mine
                  model.field
              }
          in
            if n == model.numOfMines
            then
              ( newModel, perform MakeField (Task.succeed ()))
            else
            ( newModel
            , Random.generate
              (Place (n + 1))
              (putMine model.fieldSize)
            )
      ToggleMode ->
        if model.gameStatus == Playing
        then
          ( { model |
              clickMode =
                if model.clickMode == Open
                then Flag
                else Open
            }
          , Cmd.none
          )
        else
          noChange
      MakeField _ ->
        let
          (x,y) = model.startPos
          setModel = makeField model
          startPosNum =
            case array2Get x y setModel.field of
              Just (Space n) -> n
              _ -> -1
          newModel =
            if startPosNum == 0
            then
              { setModel |
                viewField =
                  autoOpen model.startPos
                    setModel.field
                    setModel.viewField
              }
            else
              setModel
        in
          ( newModel, Cmd.none )
      TextBox t s ->
        let
          n = Maybe.withDefault 2 (String.toInt s)
        in
          case t of
            Width ->
              ( { model |
                  fieldSize =
                    ( (\(x,y) nx -> (nx,y) )
                      model.fieldSize n
                    )
                }
              , Cmd.none
              )
            Height ->
              ( { model |
                  fieldSize =
                    ( (\(x,y) ny -> (x,ny) )
                      model.fieldSize n
                    )
                }
              , Cmd.none
              )
            NumOfMines ->
              ( { model |
                  numOfMines = n
                }
              , Cmd.none
              )

clearCheck : Model -> Model
clearCheck model =
  if
    (==)
      ( array2Count
        (\s -> case s of
          Opened n -> False
          _ -> True
        )
        model.viewField
      )
      model.numOfMines
  then
    { model | gameStatus = Clear }
  else
    model


checkList =
  [(-1,-1),(0,-1),(1,-1)
  ,(-1, 0)       ,(1, 0)
  ,(-1, 1),(0, 1),(1, 1)
  ]

autoOpen : (Int,Int) -> Array2 Status -> Array2 ViewStatus -> Array2 ViewStatus
autoOpen (x,y) f vf =
  let
    check (x_,y_) f_ vf_ =
      case (array2Get x_ y_ f_, array2Get x_ y_ vf_ ) of
        (Just (Space 0), Just Hidden) -> True
        _ -> False
    autoOpenHelper (x_,y_) f_ vf_ =
      checkList
        |> List.foldl
          (\(ax,ay) nvf ->
            if check ( ax+x_, ay+y_) f_ nvf
            then
              autoOpenHelper ( ax+x_, ay+y_) f_
                ( array2Set (ax+x_) (ay+y_)
                  (Opened 0) nvf
                )
            else
              let
                getNum =
                  case array2Get (ax+x_) (ay+y_) f_ of
                    Just (Space n) -> n
                    _ -> -1
              in
                array2Set (ax+x_) (ay+y_)
                  (Opened getNum) nvf
          )
          vf_
  in
    autoOpenHelper (x,y) f vf



makeField : Model -> Model
makeField model =
  let
    debug = log "called" "makeField"
    newField =
      getCoodList model.fieldSize
        |> List.concat
        |> List.foldl
          (\(x,y) field ->
            array2Set x y
              ( case array2Get x y field of
                Just Mine -> Mine
                _ ->
                  ( checkList
                    |> List.foldl
                      (\(x_,y_) n ->
                        case array2Get (x+x_) (y+y_) field of
                          Just Mine -> n + 1
                          _ -> n
                      )
                      0
                  )
                    |> Space
              )
            field
          )
          model.field
    newViewField =
      let
        (x,y) = model.startPos
      in
        array2Set x y
          (case array2Get x y newField of
            Just (Space n) -> Opened n
            _ -> Hidden
          )
          model.viewField
  in
    { model |
      field = newField
    , viewField = newViewField
    }

array2Get : Int -> Int -> Array2 a -> Maybe a
array2Get x y arr =
  case Array.get y arr of
    Just a -> Array.get x a
    _ -> Nothing

array2Set : Int -> Int -> a -> Array2 a -> Array2 a
array2Set x y after arr =
  Array.set
    y
    ((Maybe.withDefault
        Array.empty
        (Array.get y arr)
      )
      |> Array.set x after
    )
    arr

array2Count : (a -> Bool) -> Array2 a -> Int
array2Count f arr =
  arr
    |> Array.toList
    |> List.concatMap Array.toList
    |> List.filter f
    |> List.length


getCoodList : (Int,Int) -> List (List (Int,Int) )
getCoodList (x,y) =
  List.range 0 (y - 1)
    |> List.map
      (\y_ ->
        List.range 0 (x - 1)
          |> List.map
            (\x_ -> ( x_, y_ ))
      )

view : Model -> Html Msg
view model =
  let
    ( width, height ) = model.fieldSize
    fieldCoodList = getCoodList model.fieldSize
    buttonSize = "30px"
    checkBombView x y =
      (&&)
        (array2Get x y model.field == Just Mine)
        (model.gameStatus == Clear || model.gameStatus == GameOver)
    field4View =
      getCoodList model.fieldSize
        |> List.map
          (\li ->
            li
            |> List.map
            (\(x,y) ->
              case array2Get x y model.viewField of
                Just (Opened n) ->
                  button
                    [ onClick <| Click (x,y)
                    , style "width" buttonSize
                    , style "height" buttonSize
                    ]
                    [ if checkBombView x y
                      then
                        text "💣"
                      else
                        text (" "++(String.fromInt n)++" ")
                    ]
                Just Flagged ->
                  button
                    [ onClick <| Click (x,y)
                    , style "width" buttonSize
                    , style "height" buttonSize
                    , style "background-color" "gray"
                    ]
                    [ if checkBombView x y then text "💣" else text "🚩" ]
                Just Hidden ->
                  button
                    [ onClick <| Click (x,y)
                    , style "width" buttonSize
                    , style "height" buttonSize
                    , style "background-color" "gray"
                    ]
                    [ if checkBombView x y then text "💣" else text "　" ]
                _ -> text ""
            )
          )
        |> List.intersperse [br[][]]
        |> List.concat

  in
    case model.gameStatus of
      Start ->
        div[]
          [ text "width : "
          , input
            [ type_ "number"
            , A.max "30"
            , A.min "2"
            , value <| String.fromInt <| Tuple.first model.fieldSize
            , onInput <| TextBox Width
            ][]
          , text " × height : "
          , input
            [ type_ "number"
            , A.max "30"
            , A.min "2"
            , value <| String.fromInt <| Tuple.second model.fieldSize
            , onInput <| TextBox Height
            ][]
          , br[][]
          , text "number of mines : "
          , input
            [ type_ "number"
            , A.max
              <| String.fromInt
                ((\(x,y)->x*y-2) model.fieldSize)
            , A.min "1"
            , value <| String.fromInt model.numOfMines
            , onInput <| TextBox NumOfMines
            ][]
          , br[][]
          , button[onClick Button][text "start"]
          ]
      GameOver ->
        div[]
          <| List.append
            field4View
            [ br[][]
            , text "Game Over"
            , br[][]
            , button[onClick Button][text "continue"]
            ]
      Clear ->
        div[]
          <| List.append
            field4View
            [ br[][]
            , text "Clear"
            , br[][]
            , button[onClick Button][text "next game"]
            ]
      _ ->
        div[]
          <| List.append
            field4View
            [ br[][]
            , button
              [ onClick ToggleMode ]
              [ text
                ( "ModeChange (Current Mode : "
                ++ (mode2Str model.clickMode)
                ++ " )"
                )
              ]
            ]

mode2Str : Mode -> String
mode2Str mode =
  case mode of
    Open -> "Open"
    Flag -> "Flag"

main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> ( initialModel, Cmd.none )
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
