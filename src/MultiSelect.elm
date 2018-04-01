module MultiSelect exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Model r a =
    { options : Dict a (Option r a) }


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


initialModel options =
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
