module ChordParser.ChordParserView

open Elmish // provides: Cmd, Sub, .withSubscription
open System // provides: DateTime, TimeSpan

open Avalonia.FuncUI.Elmish
open Avalonia.Layout
open Avalonia.Controls
open Avalonia.FuncUI // provides: Component
open Avalonia.FuncUI.DSL // provides: .create
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.Elmish.ElmishHook // provides: .useElmish
open Avalonia.Threading // provides: DispatcherTimer

type Model = {
    InputChordChart: string
    OutputChordChart: Result<string, string>
    Transpose: int
    Accidental: string
    IsUppercase: bool
    Time: DateTime
}

type Msg =
    | ParseChart
    | TransposeUp
    | TransposeDown
    | SetInputChart of chart: string
    | SetAccidental of string
    | SetUppercase of bool // TODO: rename to be consistent with ToUpper and IsUpper etc
    | Reset
    | SetTime

let init () =
    {
        InputChordChart = SampleCharts.autumnLeaves
        OutputChordChart = Ok "" // NOTE: why not an Option? (does this have rich error messaging)
        Transpose = 0
        Accidental = "b"
        IsUppercase = false
        Time = DateTime.Now
    },
    Cmd.ofMsg ParseChart

let update msg model =
    match msg with
    | ParseChart ->
        {
            model with
                OutputChordChart =
                    ChordParser.tryProcessText model.Transpose model.Accidental model.IsUppercase model.InputChordChart
        },
        Cmd.none
    | TransposeUp ->
        match model.Transpose with
        | t when t < 11 -> { model with Transpose = model.Transpose + 1 }, Cmd.ofMsg ParseChart
        | _ -> model, Cmd.none
    | TransposeDown ->
        match model.Transpose with
        | t when t > -11 -> { model with Transpose = model.Transpose - 1 }, Cmd.ofMsg ParseChart
        | _ -> model, Cmd.none
    | SetInputChart chart -> { model with InputChordChart = chart }, Cmd.ofMsg ParseChart
    | SetAccidental acc -> { model with Accidental = acc }, Cmd.ofMsg ParseChart
    | SetUppercase isUpper -> { model with IsUppercase = isUpper }, Cmd.ofMsg ParseChart
    | Reset -> init ()
    | SetTime -> { model with Time = DateTime.Now }, Cmd.none

let private subscriptions (model: Model) : Sub<Msg> =
    let timerSub (dispatch: Msg -> unit) =
        let invoke () =
            dispatch Msg.SetTime
            true

        DispatcherTimer.Run (invoke, TimeSpan.FromMilliseconds 1000.0)

    [
        match model.Transpose with
        | 0 -> [ nameof timerSub ], timerSub
        | _ -> ()
    ]

let view () =
    Component (fun ctx ->
        let model, dispatch =
            ctx.useElmish (init, update, Program.withSubscription subscriptions)
        //let model, dispatch = ctx.useElmish(init, update) // if no subscriptions are needed

        Grid.create [
            Grid.rowDefinitions "20, *"
            Grid.columnDefinitions "*, 80, *"
            Grid.children [
                // Row labels
                TextBlock.create [ TextBlock.text "Input Chord Chart"; Grid.column 0 ]
                TextBlock.create [ TextBlock.text $"Output Chord Chart - TIME: {model.Time}"; Grid.column 2 ]

                // Input Chord Chart
                TextBox.create [
                    TextBox.text model.InputChordChart
                    TextBox.onTextChanged (fun txt -> dispatch (SetInputChart txt))
                    Grid.column 0
                    Grid.row 1
                ]

                // Middle Column (Settings)
                StackPanel.create [
                    Grid.column 1
                    Grid.row 1

                    StackPanel.children [
                        // Transpose up
                        Button.create [
                            Button.content "▲"
                            Button.horizontalAlignment HorizontalAlignment.Stretch
                            Button.horizontalContentAlignment HorizontalAlignment.Center
                            Button.onClick (fun e -> dispatch TransposeUp)
                        ]

                        // Current transpose
                        TextBlock.create [
                            TextBlock.text (string model.Transpose)
                            TextBlock.horizontalAlignment HorizontalAlignment.Center
                        ]

                        // Transpose down
                        Button.create [
                            Button.content "▼"
                            Button.horizontalAlignment HorizontalAlignment.Stretch
                            Button.horizontalContentAlignment HorizontalAlignment.Center
                            Button.onClick (fun e -> dispatch TransposeDown)
                        ]

                        // Change case
                        StackPanel.create [
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.horizontalAlignment HorizontalAlignment.Center
                            StackPanel.children [
                                CheckBox.create [
                                    CheckBox.isChecked model.IsUppercase
                                    CheckBox.onChecked (fun _ -> dispatch (SetUppercase true))
                                    CheckBox.onUnchecked (fun _ -> dispatch (SetUppercase false))
                                ]
                                TextBlock.create [
                                    TextBlock.verticalAlignment VerticalAlignment.Center
                                    TextBlock.text <| if model.IsUppercase then "A -> a" else "a -> A"
                                ]
                            ]
                        ]

                        // ♭/ ♯
                        StackPanel.create [
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.horizontalAlignment HorizontalAlignment.Center
                            StackPanel.children [
                                RadioButton.create [
                                    RadioButton.content "♭ "
                                    RadioButton.isChecked (model.Accidental = "b")
                                    RadioButton.onChecked (fun _ -> dispatch (SetAccidental "b"))
                                ]

                                RadioButton.create [
                                    RadioButton.content "♯ "
                                    RadioButton.isChecked (model.Accidental = "#")
                                    RadioButton.onChecked (fun _ -> dispatch (SetAccidental "#"))
                                ]
                            ]
                        ]

                        Button.create [
                            Button.content "Reset"
                            Button.horizontalAlignment HorizontalAlignment.Stretch
                            Button.horizontalContentAlignment HorizontalAlignment.Center
                            Button.onClick (fun _ -> dispatch Reset)
                            Button.isEnabled (
                                let initialModel, _ = init ()
                                model <> initialModel
                            )
                        ]
                    ]
                ]

                // Output Chord Chart
                TextBox.create [
                    TextBox.text
                    <| match model.OutputChordChart with
                       | Ok output -> output
                       | Error err -> err

                    TextBox.verticalAlignment VerticalAlignment.Stretch
                    TextBox.isReadOnly true

                    Grid.column 2
                    Grid.row 1
                ] // end of TextBox.create


            ]
        ]
        :> IView)