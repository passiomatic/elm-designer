module Views.FontBrowser exposing (State, init, update, view)

{-| Dialog listing all available fonts.
-}

--import Element.Font exposing (family)

import Css
import Dict exposing (Dict)
import Document exposing (..)
import Fonts
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as E
import Icons
import Set
import Style.Font as Font exposing (FontCategory(..), FontFamily)


type Msg
    = SampleTextChanged String


type alias State msg =
    { sampleText : String
    , fontSize : Int
    , families : Dict String FontFamily
    , currentFamily : Maybe FontFamily
    , addFontMsg : FontFamily -> msg
    , removeFontMsg : FontFamily -> msg
    }


init addFontMsg removeFontMsg families =
    { sampleText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
    , fontSize = 18
    , families = families
    , currentFamily = Nothing
    , addFontMsg = addFontMsg
    , removeFontMsg = removeFontMsg
    }


update : Msg -> State msg -> State msg
update msg state =
    case msg of
        SampleTextChanged value ->
            { state | sampleText = value }


view : State msg -> Html msg
view state =
    let
        currentFamily =
            case state.currentFamily of
                Just family ->
                    family

                Nothing ->
                    Fonts.defaultFamily
    in
    H.div [ A.class "modal fade show", A.style "display" "block", A.tabindex -1 ]
        [ H.div [ A.class "modal-dialog  modal-dialog-centered modal-dialog-scrollable modal-xl" ]
            [ H.div [ A.class "modal-content" ]
                [ H.div [ A.class "modal-header" ]
                    [ H.h2 [ A.class "modal-title" ] [ H.text "Browse Google Fonts" ]
                    ]
                , H.div [ A.class "modal-body" ]
                    [ filtersView state
                    , H.div [ A.class "d-flex" ]
                        [ familyListView state
                        , familySpecimen state currentFamily
                        ]
                    ]
                , H.div [ A.class "modal-footer" ]
                    [ H.button [ A.type_ "button", A.class "btn btn-primary" ] [ H.text "Done" ]
                    ]
                ]
            ]
        ]


familySpecimen : State msg -> FontFamily -> Html msg
familySpecimen state family =
    let
        fontSize =
            A.style "font-size" (Css.px state.fontSize)
    in
    H.div [ A.class "bp-3 flex-grow-1" ]
        (H.div [ A.class "d-flex justify-content-between align-items-center mb-4" ]
            [ H.h2 []
                [ H.text family.name
                ]

            -- , H.a [ A.class "btn btn-light btn-sm", A.href ("https://fonts.google.com/specimen/" ++ family.name), A.target "_blank" ]
            --     [ H.text "View on Google Fonts"
            --     , Icons.eternalLink
            --     ]
            , H.button [ A.class "btn btn-primary" ] [ H.text "Use Font" ]
            ]
            :: settingsView state
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
                            [ H.text state.sampleText
                            ]
                        ]
                )
                family.weights
        )



--settingsView : State msg -> State msg


settingsView : State msg -> Html msg
settingsView state =
    H.div [ A.class "row" ]
        [ H.div
            [ A.class "col-md-7"
            ]
            [ H.label
                [ A.class "form-label" ]
                [ H.text "Sample text" ]
            , H.input
                [ A.type_ "text"
                , A.class "form-control"
                , A.value state.sampleText

                --, E.onInput SampleTextChanged
                ]
                []
            ]
        , H.div
            [ A.class "offset-md-1 col-md-4"
            ]
            [ H.label
                [ A.class "form-label" ]
                [ H.text "Preview size" ]
            , H.input
                [ A.type_ "range"
                , A.class "form-range"
                , A.min "10"
                , A.max "200"
                , A.value (String.fromInt state.fontSize)
                ]
                []
            ]
        ]


fontCategories =
    [ Serif, SansSerif, Handwriting, Display, Monospace ]


filtersView : State msg -> Html msg
filtersView state =
    -- let
    --         -- Dict.values state.families
    --         --     |> List.map .category
    --         -- |> Set.fromList
    -- in
    H.div
        [ A.class "btn-group mb-3"
        , A.attribute "role" "group"

        --, A.attribute "aria-label" "Basic checkbox toggle button group"
        ]
        (List.map
            (\category ->
                let
                    name =
                        Font.categoryName category
                in
                [ H.input
                    [ A.type_ "checkbox"
                    , A.class "btn-check"
                    , A.id name

                    --, A.attribute "autocomplete" "off"
                    ]
                    []
                , H.label
                    [ A.class "btn btn-outline-primary"
                    , A.for name
                    ]
                    [ H.text name ]
                ]
            )
            fontCategories
            |> List.concat
        )



-- [ H.input
--     [ A.type_ "checkbox"
--     , A.class "btn-check"
--     , A.id "btncheck1"
--     , A.attribute "autocomplete" "off"
--     ]
--     []
-- , H.label
--     [ A.class "btn btn-outline-primary"
--     , A.for "btncheck1"
--     ]
--     [ H.text "Checkbox 1" ]
-- , H.input
--     [ A.type_ "checkbox"
--     , A.class "btn-check"
--     , A.id "btncheck2"
--     , A.attribute "autocomplete" "off"
--     ]
--     []
-- , H.label
--     [ A.class "btn btn-outline-primary"
--     , A.for "btncheck2"
--     ]
--     [ H.text "Checkbox 2" ]
-- , H.input
--     [ A.type_ "checkbox"
--     , A.class "btn-check"
--     , A.id "btncheck3"
--     , A.attribute "autocomplete" "off"
--     ]
--     []
-- , H.label
--     [ A.class "btn btn-outline-primary"
--     , A.for "btncheck3"
--     ]
--     [ H.text "Checkbox 3" ]
--  ]


familyListView : State msg -> Html msg
familyListView state =
    let
        isSelected other =
            case state.currentFamily of
                Just family ->
                    family.id == other.id

                Nothing ->
                    False
    in
    H.div []
        [ H.div [ A.class "list-group" ]
            (Dict.toList state.families
                |> List.map
                    (\( _, family ) ->
                        H.button [ E.onClick (state.addFontMsg family), A.class "list-group-item list-group-item-action" ]
                            [ --Icons.checkSquare
                              --Icons.square
                            H.text (" " ++ family.name)
                            ]
                    )
            )
        ]
