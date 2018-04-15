module MultiSelect
    exposing
        ( Data
        , initial
        , view
        )

{-| This library is meant to be a simple multi-select component with the ability to pass in your own 'view' template


# view

@docs Data, initial, view

-}

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


{-| Opaque type to store options
-}
type Data r a
    = Data { options : Dict a (Option r a) }


type alias Option r a =
    { r
        | id : a
        , label : String
        , selected : Bool
    }


type alias ViewTemplate msg =
    { element : List (Attribute msg) -> List (Html msg) -> Html msg
    , attributes : List (Html.Attribute msg)
    , children : List (Html msg)
    }


{-| Initialize the multi-select with a set of options
-}
initial : List { r | label : String, selected : Bool, id : comparable } -> Data r comparable
initial options =
    Data
        { options =
            options
                |> List.map (\x -> ( x.id, x ))
                |> Dict.fromList
        }


type Msg a b
    = Select a
    | SelectAll
    | RemoveAll
    | External b
    | None


update msg model =
    case msg of
        Select id ->
            let
                updatedOptions =
                    Dict.update id
                        (\option ->
                            case option of
                                Just opt ->
                                    Just { opt | selected = not opt.selected }

                                Nothing ->
                                    Nothing
                        )
                        model.options
            in
            ( { model | options = updatedOptions }, Cmd.none )

        SelectAll ->
            ( model |> toggleAll True, Cmd.none )

        RemoveAll ->
            ( model |> toggleAll False, Cmd.none )

        External subMsg ->
            ( model, Cmd.none )

        None ->
            ( model, Cmd.none )


subscriptions =
    \_ -> Sub.none


{-| Render's the view for the multi-select component. The `render` parameter is a function that tells the `view`
how you want your options drawn to the screen and follows the same format as the Html library of `tag [attributes] [children]`

For example:

    let
        renderOption props =
            MultiSelect.ViewTemplate
                div [ class "option" ] [ i [ class "fas fa-map-marker-alt" ] [], text <| " " ++ props.label ]
    in
    MultiSelect.view options renderOption

-}
view :
    { e | options : Dict comparable { d | selected : Bool, id : c } }
    ->
        ({ d | selected : Bool, id : c }
         ->
            { g
                | attributes : List (Attribute (Msg c b))
                , children : f
                , element : List (Attribute (Msg c b)) -> f -> Html (Msg a b1)
            }
        )
    -> Html (Msg a b1)
view { options } render =
    let
        ( selectedOptions, availableOptions ) =
            options |> Dict.values |> List.partition (\i -> i.selected == True)
    in
    div [ class "multiselect" ]
        [ span [ class "available" ] [ span [] (List.map (viewOption render) availableOptions) ]
        , span [ class "bulk-options-container" ]
            [ span [ onClick RemoveAll, class "remove-all" ] [ i [ class "fas fa-angle-double-left" ] [] ]
            , span [ onClick SelectAll, class "select-all" ] [ i [ class "fas fa-angle-double-right" ] [] ]
            ]
        , span [ class "selected" ] [ span [] (List.map (viewOption render) selectedOptions) ]
        ]


viewOption render props =
    let
        { element, attributes, children } =
            render props
    in
    element (attributes ++ [ onClick (Select props.id) ]) children


toggleAll selected ({ options } as model) =
    { model
        | options =
            Dict.map
                (\id option -> { option | selected = selected })
                options
    }
