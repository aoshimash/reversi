import Array2D exposing (Array2D, get, set)
import Browser
import Html exposing (Html, button, div, nav, ul, li, h1, h2, h3, text, table, th, td, tr)
import Html.Attributes exposing (style, property, attribute, class)
import Html.Events exposing (onClick)
import Svg exposing (svg, circle, rect)
import Svg.Attributes exposing (viewBox, width, cx, cy, r, fill, fillOpacity, stroke, strokeWidth, strokeDashoffset, strokeDasharray, height, x, y, rx, ry, color, points, strokeWidth)
import String exposing (fromInt, fromFloat)
import List exposing (map4, repeat, range)


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Disk
  = Dark | Light


flipDisk : Disk -> Disk
flipDisk disk =
  if disk == Dark then Light else Dark


type Cell
  = Captured Disk
  | Empty

type alias Board = Array2D Cell


type alias Model = 
  { turn : Disk
  , board : Board
  , result : String
  }



-- INIT


init : Model
init =
  { turn = Dark
  , board = initialBoard
  , result = ""
  }




-- UPDATE



type Msg
  = Initialize
  | ClickCell Int Int
  | Pass
  | Counting




update : Msg -> Model -> Model
update msg model =
  case msg of
    
    -- 初期化: board, msg, turnを初期化
    Initialize ->
      { model | board = initialBoard
              , turn = Dark
              , result = ""
      }

    -- クリック
    ClickCell y x ->
      case place y x model.turn model.board of
        
        Just newboard ->
          { model | board = newboard
                  , turn = flipDisk model.turn
                  , result = ""
          }
        Nothing ->
          model
            

    -- パス
    Pass ->
      case model.turn of
        Light ->
          { model | turn = Dark }
        
        Dark ->
          { model | turn = Light }


    -- 集計
    Counting ->
      { model | result = "黒：" ++ fromInt (countDarkDisks model.board)
                         ++ "個, 白: " ++ fromInt (countLightDisks model.board) ++ "個"
      }





-- Initialization


{-
  初期状態のボードを作成する関数
-}
initialBoard : Board
initialBoard =
  Array2D.fromList [ [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
                   , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
                   , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
                   , [ Empty, Empty, Empty, Captured Light, Captured Dark, Empty, Empty, Empty ]
                   , [ Empty, Empty, Empty, Captured Dark, Captured Light, Empty, Empty, Empty ]
                   , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
                   , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
                   , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ] 
                   ]
  


{-
  石を設置する
-}
verifyPlacing : Int -> Int -> Disk -> Board -> Bool
verifyPlacing y x disk board =
  case Array2D.get y x board of
    
    Just cell ->
      (cell == Empty) && (countCapturableDisks y x disk board > 0)
    
    Nothing ->
      False



place : Int -> Int -> Disk -> Board -> Maybe Board
place y x disk board =
  if verifyPlacing y x disk board then
    Just (captureDisks y x disk (set y x (Captured disk) board))
  else
    Nothing


countCells : Int -> Int -> Cell -> Board -> Int -> Int
countCells y x cell board sum =
  if y > 7 then
    sum
  else if x > 7 then
    countCells (y + 1) 0 cell board sum
  else
    case get y x board of
      
      Just cellinfo ->
        if cellinfo == cell then
          countCells y (x + 1) cell board (sum + 1)
        else 
          countCells y (x + 1) cell board sum
      
      Nothing ->
        0


countDarkDisks : Board -> Int
countDarkDisks board =
  countCells 0 0 (Captured Dark) board 0


countLightDisks : Board -> Int
countLightDisks board =
  countCells 0 0 (Captured Light) board 0


countEmptyCells : Board -> Int
countEmptyCells board =
  countCells 0 0 (Empty) board 0





-- Getting info




{-
  反転可能な石の数を数える関数
-}
countCapturableDisks : Int -> Int -> Disk -> Board -> Int
countCapturableDisks y x disk board =
  countCapturableDisks4OneDirection y x (\ny -> ny - 1) (\nx -> nx - 1) disk board 0    -- Upper left
  + countCapturableDisks4OneDirection y x (\ny -> ny - 1) (\nx -> nx) disk board 0      -- Upper
  + countCapturableDisks4OneDirection y x (\ny -> ny - 1) (\nx -> nx + 1) disk board 0  -- Upper right
  + countCapturableDisks4OneDirection y x (\ny -> ny) (\nx -> nx - 1) disk board 0      -- Left
  + countCapturableDisks4OneDirection y x (\ny -> ny) (\nx -> nx + 1) disk board 0      -- Right
  + countCapturableDisks4OneDirection y x (\ny -> ny + 1) (\nx -> nx - 1) disk board 0  -- LowerLeft
  + countCapturableDisks4OneDirection y x (\ny -> ny + 1) (\nx -> nx) disk board 0      -- Lower
  + countCapturableDisks4OneDirection y x (\ny -> ny + 1) (\nx -> nx + 1) disk board 0  -- LowerRight





{-
  石を反転させる関数
-}
captureDisks : Int -> Int -> Disk -> Board -> Board
captureDisks y x disk board =
  captureDisk4OneDirection y x (\ny -> ny - 1) (\nx -> nx - 1) disk
  ( captureDisk4OneDirection y x (\ny -> ny - 1) (\nx -> nx) disk
    ( captureDisk4OneDirection y x (\ny -> ny - 1) (\nx -> nx + 1) disk
      ( captureDisk4OneDirection y x (\ny -> ny) (\nx -> nx - 1) disk
        ( captureDisk4OneDirection y x (\ny -> ny) (\nx -> nx + 1) disk
          ( captureDisk4OneDirection y x (\ny -> ny + 1) (\nx -> nx - 1) disk
            ( captureDisk4OneDirection y x (\ny -> ny + 1) (\nx -> nx) disk
              ( captureDisk4OneDirection y x (\ny -> ny + 1) (\nx -> nx + 1) disk
                board
                (countCapturableDisks4OneDirection y x (\ny -> ny + 1) (\nx -> nx + 1) disk board 0) 0
              )
              (countCapturableDisks4OneDirection y x (\ny -> ny + 1) (\nx -> nx) disk board 0) 0
            )
            (countCapturableDisks4OneDirection y x (\ny -> ny + 1) (\nx -> nx - 1) disk board 0) 0
          )
          (countCapturableDisks4OneDirection y x (\ny -> ny) (\nx -> nx + 1) disk board 0) 0
        )
        (countCapturableDisks4OneDirection y x (\ny -> ny) (\nx -> nx - 1) disk board 0) 0
      )
      (countCapturableDisks4OneDirection y x (\ny -> ny - 1) (\nx -> nx + 1) disk board 0) 0
    )
    (countCapturableDisks4OneDirection y x (\ny -> ny - 1) (\nx -> nx) disk board 0) 0
  )
  (countCapturableDisks4OneDirection y x (\ny -> ny - 1) (\nx -> nx - 1) disk board 0) 0




countCapturableDisks4OneDirection : Int -> Int -> (Int -> Int) -> (Int -> Int) -> Disk -> Board -> Int -> Int
countCapturableDisks4OneDirection y x howtomove_y howtomove_x origin board sum =
  case (Array2D.get (howtomove_y y) (howtomove_x x) board) of
    
    Just nextcell ->
      case nextcell of
        
        Empty ->
          0

        Captured nextdisk ->
          if nextdisk == origin then
            sum
          else
            countCapturableDisks4OneDirection
              (howtomove_y y)
              (howtomove_x x)
              howtomove_y
              howtomove_x
              origin
              board
              (sum + 1)
    
    Nothing ->
      0



captureDisk4OneDirection : Int -> Int -> (Int -> Int) -> (Int -> Int) -> Disk -> Board -> Int -> Int -> Board
captureDisk4OneDirection y x howtomove_y howtomove_x disk board num counter =
  if num == counter then
    board
  else
    captureDisk4OneDirection      
      (howtomove_y y)
      (howtomove_x x)
      howtomove_y
      howtomove_x
      disk
      (Array2D.set (howtomove_y y) (howtomove_x x) (Captured disk) board)
      num
      (counter + 1)
      
      




-- VIEW


view : Model -> Html Msg
view model =
  div []
      [ nav [ class "navbar navbanr-expand-lg navbar-dark bg-dark text-light" ] [ h3 [] [text "おせろ"] ]
      , div [ class "container pt-5" ]
            [ div [ class "row" ]
                  [ div [ class "col"]
                        [ ]
                  , div [ class "col-6"]
                        [ viewBoard 500 model.board
                        , button [ onClick Pass ] [ text "パス" ]
                        , button [ onClick Initialize ] [ text "最初から" ]
                        ]
                  , div [ class "col"]
                        [ h2 [] [ text ("手番: " ++ if model.turn == Dark then "黒" else "白")]
                        , button [ onClick Counting ] [ text "集計" ]
                        , h3 [] [ text model.result ]
                        ]
                  ]

            ]
      ]




viewBoard : Float -> Board -> Html Msg
viewBoard d board =
  table [ attribute "border" "1"
        , style "border-colapse" "colapse"
        , style "border" "solid 3px"
        ]
        [ tr [] (map4 viewCell (repeat 8 0) (range 0 7) (repeat 8 (d / 8)) (repeat 8 board))
        , tr [] (map4 viewCell (repeat 8 1) (range 0 7) (repeat 8 (d / 8)) (repeat 8 board))
        , tr [] (map4 viewCell (repeat 8 2) (range 0 7) (repeat 8 (d / 8)) (repeat 8 board))
        , tr [] (map4 viewCell (repeat 8 3) (range 0 7) (repeat 8 (d / 8)) (repeat 8 board))
        , tr [] (map4 viewCell (repeat 8 4) (range 0 7) (repeat 8 (d / 8)) (repeat 8 board))
        , tr [] (map4 viewCell (repeat 8 5) (range 0 7) (repeat 8 (d / 8)) (repeat 8 board))
        , tr [] (map4 viewCell (repeat 8 6) (range 0 7) (repeat 8 (d / 8)) (repeat 8 board))
        , tr [] (map4 viewCell (repeat 8 7) (range 0 7) (repeat 8 (d / 8)) (repeat 8 board))
        ]
        



viewCell : Int -> Int -> Float -> Board -> Html Msg
viewCell y x d board =
  case get y x board of
    
    Just cell ->
      td [ style "border" "solid 3px"
         , onClick (ClickCell y x)
         ]
         [ case cell of
             Captured Dark ->
               viewDarkCellSvg d
        
             Captured Light ->
               viewLightCellSvg d
        
             Empty ->
               viewEmptyCellSvg d
         ]
    
    Nothing ->
      td [ style "border" "solid 3px" ]
         [ text "Err"]





viewLightCellSvg : Float -> Html Msg
viewLightCellSvg d = 
  svg
    [ width (fromFloat d)
    , height (fromFloat d)
    ]
    [ circle [ cx (fromFloat (d / 2))
             , cy (fromFloat (d / 2))
             , r (fromFloat (0.9 * d / 2))
             , stroke "Black"
             , strokeWidth "3"
             , fill "none"
             ]
             []
    ]



viewDarkCellSvg : Float -> Html Msg
viewDarkCellSvg d = 
  svg
    [ width (fromFloat d)
    , height (fromFloat d)
    ]
    [ circle [ cx (fromFloat (d / 2))
             , cy (fromFloat (d / 2))
             , r (fromFloat (0.9 * d / 2))
             , stroke "Black"
             , strokeWidth "3"
             , fill "Black"
             ]
             []
    ]


viewEmptyCellSvg : Float -> Html Msg
viewEmptyCellSvg d =
  svg
    [ width (fromFloat d)
    , height (fromFloat d)
    ]
    []