module Index

open Elmish
open Fable.Remoting.Client
open Shared
open Sutil
open Sutil.Bulma

type Model = { Todos: Todo list; Input: string }

type Msg =
    | GotTodos of Todo list
    | SetInput of string
    | AddTodo
    | AddedTodo of Todo

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    let model = { Todos = []; Input = "" }

    let cmd = Cmd.OfAsync.perform todosApi.getTodos () GotTodos

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos -> { model with Todos = todos }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none
    | AddTodo ->
        let todo = Todo.create model.Input

        let cmd = Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo

        { model with Input = "" }, cmd
    | AddedTodo todo ->
        { model with
            Todos = model.Todos @ [ todo ]
        },
        Cmd.none


let navBrand =
    bulma.navbar [
        bulma.navbarItem.a [
            Attr.href "https://safe-stack.github.io/"
            navbarItem.isActive
            Html.img [ Attr.src "/public/favicon.png"; Attr.alt "Logo" ]
        ]
    ]

let containerBox (model: IStore<Model>) (dispatch: Msg -> unit) =
    let todos = Store.map (fun model -> model.Todos) model
    let modelInput = Store.map (fun model -> model.Input) model
    let validInput = Store.map (fun model -> model.Input |> Todo.isValid |> not) model

    bulma.box [
        bulma.content [
            Html.ol [ Bind.each (todos, (fun todo -> Html.li [ Html.text todo.Description ])) ]
        ]
        bulma.field.div [
            field.isGrouped
            bulma.control.p [
                control.isExpanded
                bulma.input.text [
                    Attr.value (modelInput, SetInput >> dispatch)
                    Attr.placeholder "What needs to be done?"
                ]
            ]
            bulma.control.p [
                bulma.button.a [
                    color.isPrimary
                    Bind.attr ("disabled", validInput)
                    Ev.onClick (fun _ -> dispatch AddTodo)
                    Html.text "Add"
                ]
            ]
        ]
    ]

let view () =
    let model, dispatch = Store.makeElmish init update ignore ()

    bulma.hero [
        Bulma.hero.isFullheight
        color.isPrimary
        Attr.style [
            Css.backgroundSize "cover"
            Css.backgroundImageUrl "https://unsplash.it/1200/900?random"
            Css.backgroundPosition "no-repeat center center fixed"
        ]
        bulma.heroHead [ bulma.navbar [ bulma.container [ navBrand ] ] ]
        bulma.heroBody [
            bulma.container [
                bulma.column [
                    column.is6
                    column.isOffset3
                    bulma.title.h1 [ text.hasTextCentered; Html.text "SafePerla" ]
                    containerBox model dispatch
                ]
            ]
        ]
    ]