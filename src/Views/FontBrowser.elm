module Views.FontBrowser exposing (State, init, view)

{-| Dialog listing all available fonts.
-}

import Css
import Dict exposing (Dict)
import Document exposing (..)
import Fonts
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
--import Html.Entity as Entity
import Html.Events as E
--import Html.Keyed as Keyed
import Icons
--import Palette
import Style.Font as Font exposing (FontFamily)
import Svg.Attributes exposing (fontSize)
--import Views.Common exposing (fieldId, none)

type alias State msg =
    { sampleText : String
    , fontSize : Int
    , families : Dict String FontFamily
    , addFontMsg: FontFamily -> msg
    , removeFontMsg: FontFamily -> msg
    }


init addFontMsg removeFontMsg families =
    { sampleText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
    , fontSize = 18
    , families = families
    , addFontMsg = addFontMsg
    , removeFontMsg = removeFontMsg
    }

-- update : Msg -> Model -> Model
-- update msg model =
--     case msg of
--         Increment ->
--             { model | count = model.count + 1 }

view : State msg -> Html msg
view state =
    H.div [ A.class "modal fade show", A.style "display" "block", A.tabindex -1 ]
        [ H.div [ A.class "modal-dialog  modal-dialog-centered modal-dialog-scrollable modal-xl" ]
            [ H.div [ A.class "modal-content" ]
                [ H.div [ A.class "modal-header" ]
                    [ H.h2 [ A.class "modal-title" ] [ H.text "Browse Google Fonts" ]
                    ]
                , H.div [ A.class "modal-body d-flex" ]
                    [ familyListView state
                    , familySpecimen state Fonts.defaultFamily 18 "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
                    ]
                , H.div [ A.class "modal-footer" ]
                    [ H.button [ A.type_ "button", A.class "btn btn-primary" ] [ H.text "Done" ]
                    ]
                ]
            ]
        ]


familySpecimen : State msg -> FontFamily -> Int -> String -> Html msg
familySpecimen state family size sample =
    let
        fontSize =
            A.style "font-size" (Css.px size)
    in
    H.div [ A.class "bp-2 flex-grow-1" ]
        (H.h2 []
            [ H.text family.name

            --, H.a [ A.href "https://fonts.google.com/specimen/", A.target "_blank" ] [ H.text "View on Google Fonts"]
            ]
            :: List.map
                (\weight ->
                    let
                        fontStyle =
                            A.style "font-style"
                                (if Font.isItalic weight then
                                    "italic"

                                 else
                                    "normal"
                                )

                        fontWeight =
                            -- FIXME: Passed CSS weight is not 100% accurate (101, 201, etc) but seems to work
                            A.style "font-weight" (String.fromInt (Font.weightNumber weight))
                    in
                    H.div [ A.class "border-bottom mt-3" ]
                        [ H.div [ A.class "small text-muted text-uppercase" ] [ H.text (Font.weightName weight) ]
                        , H.p [ fontSize, fontWeight, fontStyle ]
                            [ H.text sample
                            ]
                        ]
                )
                family.weights
        )


familyListView : State msg  -> Html msg
familyListView state =
    H.div []
        [ H.ul [ A.class "list-unstyled bg-light" ]
            (Dict.toList state.families
                |> List.map
                    (\( _, family ) ->
                        H.li [ A.class "border-bottom" ]
                            [ H.button [ E.onClick (state.addFontMsg family), A.class "btn btn-link text-body" ]
                                [ --Icons.checkSquare
                                  Icons.square
                                , H.text (" " ++ family.name)
                                ]
                            ]
                    )
            )
        ]
