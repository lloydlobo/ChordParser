open Avalonia
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Controls
// open Avalonia.FuncUI.Diagnostics

open ChordParser.ChordParser
open ChordParser.ChordParserView // provides: view()

type MainWindow() =
    inherit HostWindow()

    do
        base.Title <- "Chord Parser"
        base.Icon <- WindowIcon (System.IO.Path.Combine ("Assets", "Icons", "icon.ico"))
        base.Width <- 800.0
        base.Height <- 450.0
        base.Content <- view ()

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add (FluentTheme ())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            let mainWindow = MainWindow ()
            //InspectorWindow(mainWindow).Show()
            //mainWindow.Renderer.DrawDirtyRects <- true
            desktopLifetime.MainWindow <- mainWindow
        | _ -> ()

module Program =

    [<EntryPoint>]
    let main (args: string[]) =
        AppBuilder //\n
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime (args)