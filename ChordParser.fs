/// Copied from
/// https://github.com/JordanMarr/FSharp.ChordParser
/// https://github.com/fsprojects/Avalonia.FuncUI/blob/master/src/Examples/Examples.ChordParser/ChordParser.fs
module ChordParser.ChordParser

// NOTE: Parser Robustness:
//       - Chord extensions like 7b5 or maj7 may fail because your extension parser only matches digits.
//       - Tonality parsing could accidentally partially match ("m" before "min"). Order seems okay, but be careful.

module Domain =
    let rootNotes = [ "A"; "A#"; "Bb"; "B"; "C"; "C#"; "Db"; "D"; "D#"; "Eb"; "E"; "F"; "F#"; "Gb"; "G"; "G#"; "Ab" ]

    type Chord = { Root: string; Tonality: string option; Extension: string option; BassNote: string option }

    let wrapAround n modulus = ((n % modulus) + modulus) % modulus // handles negative values correctly

    let transpose semitones preferredAccidental chord =
        let sharps = rootNotes |> List.filter (fun n -> not (n.EndsWith "b"))
        let flats = rootNotes |> List.filter (fun n -> not (n.EndsWith "#"))

        let notes =
            match preferredAccidental with
            | "#" -> sharps
            | "b" -> flats
            | _ -> failwith "Preferred Accidental must be either # or b."

        let transposeNote note = // NOTE: List.tryPick (List.tryFindIndex ...) |> Option.defaultWith failwith could be simplified.
            [ sharps; flats ]
            |> List.tryPick (List.tryFindIndex ((=) note))
            |> Option.defaultWith (fun () -> failwith $"Invalid note: '{note}'")
            |> fun idx -> notes.[wrapAround (idx + semitones) 12]

        { chord with Root = chord.Root |> transposeNote; BassNote = chord.BassNote |> Option.map transposeNote }

    let chordToString chord = // NOTE: Consider pattern matching for clarity.
        let tonality = chord.Tonality |> Option.defaultValue ""

        let extension = // let extension = chord.Extension |> Option.defaultValue ""
            match chord.Extension with
            | Some ext when ext.Contains (" ") -> " " + ext
            | Some ext -> ext
            | None -> ""

        let bass = chord.BassNote |> Option.map (sprintf " /%s") |> Option.defaultValue ""

        $"({chord.Root}{tonality}{extension}{bass})"
//  ~~~~~~~~~~~~~~~~~~~~~~~~~
// ^
// |
//  let chordToString chord =
// |    match chord.Tonality, chord.Extension, chord.BassNote with
// |    | Some t, Some e, Some b -> $"({chord.Root}{t}{e} /{b})"
// |    | Some t, Some e, None -> $"({chord.Root}{t}{e})"
// |    | Some t, None, Some b -> $"({chord.Root}{t} /{b})"
// |    | None, Some e, Some b -> $"({chord.Root}{e} /{b})"
// |    | Some t, None, None -> $"({chord.Root}{t})"
// |    | None, Some e, None -> $"({chord.Root}{e})"
// |    | None, None, Some b -> $"({chord.Root} /{b})"
// |    | None, None, None -> $"({chord.Root})"


module Parser =
    open FParsec
    open Domain

    type ChordChart =
        | Lyrics of string
        | Chord of Chord

    let str s = pstring s
    let strCI s = pstringCI s
    let ws = spaces

    let createChord (((root, tonality), ext), bassNote) =
        { Root = root; Tonality = tonality; Extension = ext; BassNote = bassNote }
        |> ChordChart.Chord

    let chord =
        let note =
            rootNotes
            |> List.sortByDescending _.Length // "C#" must be before "C" to avoid a "partial" consumption of "C" that prevents "C#" from matching.
            |> List.map str
            |> choice
            |>> string

        let tonality =
            [ strCI "maj"; str "M"; strCI "min"; str "m"; str "-"; strCI "dim"; str "o"; strCI "aug"; str "+"; str "+5" ] // order matters to avoid partial consumption bugs
            |> choice
            |>> string // |> opt (skipChar ' ') // NOTE[TEMP]: optional space before tonality

        // NOTE: opt (skipChar ' ') >>. tonality may skip a space before the tonality.
        // NOTE: Ensure your input has the correct spacing — might be surprising if input has no space.
        let tonality = opt (skipChar ' ') >>. tonality // `skipChar c` is an optimized implementation of `pchar c |>> ignore`.
        let extension = many1Chars digit |>> string // `many1Chars cp` parses a sequence of *one* or more chars with the char parser `cp`
        // let extension = // match digits optionally followed by letters or symbols (e.g., "7", "7b5", "maj7", "sus4")
        //     many1Satisfy (fun c -> System.Char.IsLetterOrDigit c || "+-b#/".Contains(c))
        //     |>> string // This can now parse: 7, maj7, 7b5, sus4, add9, etc.

        let bassNote = opt (skipChar ' ') >>. skipChar '/' >>. note


        skipChar '(' // skip '('
        >>. note // parse root note (e.g., "C", "G#")
        .>>. opt tonality // optional tonality (e.g., "maj", "min")
        .>>. opt extension // optional extension (e.g., "7", "9")
        .>>. opt bassNote // optional bass note (e.g., "/G")
        .>> skipChar ')' // skip ')'
        |>> createChord // create chord from parsed values

    let lyric = many1Chars (noneOf "(") .>> spaces |>> (string >> ChordChart.Lyrics)

    let chordChart = many (lyric <|> chord)

    let parseChordChart text =
        match run chordChart text with
        | Success (ast, _, _) -> ast
        | Failure (_, error, _) -> failwith (error.ToString ())



open System.Text
open Domain

/// Parses and processes the chord chart items.
let processText (semitones: int) (preferredAccidental: string) (IsUppercase: bool) (text: string) =
    Parser.parseChordChart text
    |> List.map (function
        | Parser.Lyrics lyrics -> if IsUppercase then lyrics.ToUpper () else lyrics
        | Parser.Chord chord -> chord |> transpose semitones preferredAccidental |> chordToString)
    |> List.fold (fun (sb: StringBuilder) txt -> sb.Append txt) (StringBuilder ())
    |> string

let tryProcessText (semitones: int) (preferredAccidental: string) (isUpper: bool) (text: string) =
    try
        let output = processText semitones preferredAccidental isUpper text
        Ok output
    with ex ->
        Error ex.Message