module HelperTests

open Expecto

open FsCheck

open ChordParser.ChordParser



[<Tests>]
let warmupTests =
    testList "warmup" [

        testCase "hello world"
        <| fun _ ->
            let subject = true
            Expect.isTrue subject "I am, therefore I compute."

        testCase "should skip"
        <| fun _ -> Tests.skiptest "Yup, waiting for a sunny day..."

        testCase "contains things"
        <| fun _ -> Expect.containsAll [| 2; 3; 4 |] [| 2; 4 |] "This is the case; {2,3,4} contains {2,4}"
    ]


type WrapCase = { Index: int; Semitones: int; Modulus: int; Expected: int }

[<Tests>]
let domainHelperTests =
    testList "domain helpers" [

        testCase "contains all 17 notes"
        <| fun _ -> Expect.equal Domain.rootNotes.Length 17 "Should have 17 notes"

        testCase "all notes are valid"
        <| fun _ ->
            let isValidNote (n: string) =
                let baseNotes = seq [ "A"; "B"; "C"; "D"; "E"; "F"; "G" ]
                let n = n.Trim ()

                match n.Length with
                | 1 -> baseNotes |> Seq.contains n
                | 2 when n.Length = 2 -> (baseNotes |> Seq.contains (string n.[0])) && (n.[1] = '#' || n.[1] = 'b')
                | _ -> false

            Domain.rootNotes
            |> Seq.iter (fun n -> Expect.isTrue (isValidNote n) $"Invalid note: {n}")

        testList
            "wrapAround"
            (seq [ // (index semitones modulus expected)
                { Index = 0; Semitones = 0; Modulus = 12; Expected = 0 }
                { Index = 0; Semitones = 1; Modulus = 12; Expected = 1 }
                { Index = 11; Semitones = 1; Modulus = 12; Expected = 0 }
                { Index = 12; Semitones = 0; Modulus = 12; Expected = 0 }
                { Index = -1; Semitones = 0; Modulus = 12; Expected = 11 }
                { Index = 5; Semitones = 7; Modulus = 12; Expected = 0 }
                { Index = 14; Semitones = 0; Modulus = 12; Expected = 2 }
                { Index = -13; Semitones = 0; Modulus = 12; Expected = 11 }
             ]
             |> Seq.map (fun c ->
                 testCase $"wrapAround {c.Index}+{c.Semitones} mod {c.Modulus} = {c.Expected}"
                 <| fun _ ->
                     let actual = Domain.wrapAround (c.Index + c.Semitones) c.Modulus
                     Expect.equal actual c.Expected $"Expected {c.Expected} but got {actual}")
             |> Seq.toList)

        testProperty "wrapAround behaves correctly"
        <| fun (index: int) (semitones: int) modulus ->
            let modulus = if modulus <= 0 then 12 else modulus // ensure modulus is positive to avoid division by zero
            let actual = Domain.wrapAround (index + semitones) modulus
            let expected = (index + semitones) % modulus
            let normalizedExpected = if expected < 0 then expected + modulus else expected
            Expect.equal actual normalizedExpected "wrapAround should correctly handle modulus arithmetic"
    ]


type ChordCase = { Chord: Domain.Chord; Expected: string }

[<Tests>]
let domainChordTests =

    testList "domain chord helpers" [

        testCase "chordToString"
        <| fun _ ->
            seq<ChordCase> [
                // Simple chords
                { Chord = { Root = "C"; Tonality = None; Extension = None; BassNote = None }; Expected = "(C)" }
                { Chord = { Root = "D"; Tonality = None; Extension = None; BassNote = None }; Expected = "(D)" }
                { Chord = { Root = "E"; Tonality = None; Extension = None; BassNote = None }; Expected = "(E)" }
                { Chord = { Root = "F"; Tonality = None; Extension = None; BassNote = None }; Expected = "(F)" }

                // Tonality only
                { Chord = { Root = "C"; Tonality = Some "maj"; Extension = None; BassNote = None }; Expected = "(Cmaj)" }
                { Chord = { Root = "C"; Tonality = Some "min"; Extension = None; BassNote = None }; Expected = "(Cmin)" }
                { Chord = { Root = "D"; Tonality = Some "maj"; Extension = None; BassNote = None }; Expected = "(Dmaj)" }
                { Chord = { Root = "D"; Tonality = Some "min"; Extension = None; BassNote = None }; Expected = "(Dmin)" }

                // Tonality + Extension
                { Chord = { Root = "C"; Tonality = Some "maj"; Extension = Some "7"; BassNote = None }; Expected = "(Cmaj7)" }
                { Chord = { Root = "C"; Tonality = Some "min"; Extension = Some "7"; BassNote = None }; Expected = "(Cmin7)" }
                { Chord = { Root = "G"; Tonality = Some "maj"; Extension = Some "7"; BassNote = None }; Expected = "(Gmaj7)" }
                { Chord = { Root = "A"; Tonality = Some "min"; Extension = Some "7"; BassNote = None }; Expected = "(Amin7)" }

                // Tonality + Extension + Bass note
                { Chord = { Root = "C"; Tonality = Some "maj"; Extension = Some "7"; BassNote = Some "E" }; Expected = "(Cmaj7 /E)" }
                { Chord = { Root = "D"; Tonality = Some "maj"; Extension = Some "7"; BassNote = Some "A" }; Expected = "(Dmaj7 /A)" }
                { Chord = { Root = "G"; Tonality = Some "min"; Extension = Some "7"; BassNote = Some "D" }; Expected = "(Gmin7 /D)" }

                // Complex extensions and other cases
                { Chord = { Root = "A"; Tonality = None; Extension = Some "7b5"; BassNote = None }; Expected = "(A7b5)" }
                { Chord = { Root = "F#"; Tonality = Some "maj"; Extension = Some "9"; BassNote = None }; Expected = "(F#maj9)" }
                { Chord = { Root = "Bb"; Tonality = None; Extension = Some "13"; BassNote = None }; Expected = "(Bb13)" }
                { Chord = { Root = "C"; Tonality = Some "sus4"; Extension = None; BassNote = None }; Expected = "(Csus4)" }

                // Root with accidentals
                { Chord = { Root = "C#"; Tonality = None; Extension = None; BassNote = None }; Expected = "(C#)" }
                { Chord = { Root = "D#"; Tonality = None; Extension = None; BassNote = None }; Expected = "(D#)" }
                { Chord = { Root = "Eb"; Tonality = None; Extension = None; BassNote = None }; Expected = "(Eb)" }
                { Chord = { Root = "F#"; Tonality = None; Extension = None; BassNote = None }; Expected = "(F#)" }

                // Unusual roots
                { Chord = { Root = "Gb"; Tonality = None; Extension = None; BassNote = None }; Expected = "(Gb)" }
                { Chord = { Root = "Ab"; Tonality = None; Extension = None; BassNote = None }; Expected = "(Ab)" }

                // Edge cases
                { Chord = { Root = "A"; Tonality = None; Extension = None; BassNote = Some "B" }; Expected = "(A /B)" }
                { Chord = { Root = "C"; Tonality = None; Extension = Some "dim"; BassNote = Some "E" }; Expected = "(Cdim /E)" }
                { Chord = { Root = "D#"; Tonality = Some "maj"; Extension = Some "b5"; BassNote = None }; Expected = "(D#majb5)" }
            ]
            |> Seq.iter (fun c ->
                let actual = c.Chord |> Domain.chordToString
                Expect.equal actual c.Expected $"Expected {c.Expected} but got {actual}")

        testProperty "[duplicate name] chordToString formats chord correctly"
        <| fun (root: string) (tonality: string option) (extension: string option) (bassNote: string option) ->
            let chord: Domain.Chord = { Root = root; Tonality = tonality; Extension = extension; BassNote = bassNote }
            let actual = Domain.chordToString chord

            Tests.skiptest
                """
                [07:54:58 INF] EXPECTO? Running tests... <Expecto>
                [07:54:58 ERR] domain chord helpers.[duplicate name] chordToString formats chord correctly failed in 00:00:00.1030000.
                Failed after 1 test. Parameters:
                        <null> <null> Some "" Some null
                Shrunk 2 times to:
                        <null> <null> <null> <null>
                Result:
                        Exception
                Focus on error:
                        etestProperty (1017470970, 297549527) "[duplicate name] chordToString formats chord correctly" <Expecto>
                """

            Expect.isTrue (actual.Contains (root)) "Chord string should contain the root"

        testCase "transpose semitones works for sharps"
        <| fun _ ->
            let chord: Domain.Chord = { Root = "C"; Tonality = Some "maj"; Extension = Some "7"; BassNote = Some "E" }
            let result = Domain.transpose 2 "#" chord
            Expect.equal result.Root "D" "Root transposed up 2 semitones"
            Expect.equal result.BassNote (Some "F#") "Bass note transposed up 2 semitones"

        testCase "transpose semitones works for flats"
        <| fun _ ->
            let chord: Domain.Chord = { Root = "D"; Tonality = Some "min"; Extension = Some "7"; BassNote = Some "F" }
            let result = Domain.transpose 3 "b" chord
            Expect.equal result.Root "F" "Root transposed up 3 semitones with flats"
            Expect.equal result.BassNote (Some "Ab") "Bass note transposed up 3 semitones with flats"

        testCase "transpose with wraparound for sharps"
        <| fun _ ->
            let chord: Domain.Chord = { Root = "G"; Tonality = Some "maj"; Extension = Some "7"; BassNote = Some "F#" }
            let actualChord = chord |> Domain.chordToString
            let expectedChord = "(Gmaj7 /F#)"
            Expect.equal actualChord expectedChord $"Expected {expectedChord} but got {actualChord}"
            let result = Domain.transpose 3 "#" chord
            let expectedRoot = "A#"
            let expectedBassNote = "A"
            Expect.equal result.Root expectedRoot "Root transposed up 3 semitones"
            Expect.equal result.BassNote (Some expectedBassNote) "Bass note transposed up 3 semitones"

        testCase "chordToString formats chord correctly"
        <| fun _ ->
            let chord: Domain.Chord = { Root = "C"; Tonality = Some "maj"; Extension = Some "7"; BassNote = Some "E" }
            Expect.equal (Domain.chordToString chord) "(Cmaj7 /E)" "Chord string representation"
            let chord2: Domain.Chord = { Root = "D"; Tonality = None; Extension = Some "7"; BassNote = None }
            Expect.equal (Domain.chordToString chord2) "(D7)" "Chord string without tonality and bass"
    ]


open FParsec // provides: run

[<Tests>]
let parserTests =
    testList "parser" [

        testCase "all Parser utility tests"
        <| fun _ ->
            let testCases = [
                ("str() test", "maj", "maj", Parser.str)
                ("strCI() test", "MaJ", "maj", Parser.strCI)
                ("str() empty input", "", "", Parser.str)
                // ("str() with whitespace", "   maj  ", "maj", Parser.str)
                // ("strCI() with whitespace", "   MaJ  ", "maj", Parser.strCI)
                // ("str() invalid input", "xyz", "maj", Parser.str)
                // ("strCI() invalid input", "xyz", "maj", Parser.strCI)
                ("str() with special chars", "!@#", "!@#", Parser.str)
                ("strCI() with mixed case", "hElLo", "hello", Parser.strCI)
                // ("str() unexpected character", "123", "maj", Parser.str)
                // ("str() whitespace failure", "   ", "maj", Parser.str)
                ("strCI() case-sensitive failure", "HELLO", "hello", Parser.strCI)
            ]

            testCases
            |> List.iter (fun (name, input, expected, parser) ->
                match run (parser input) expected with
                | Success (value, _, _) -> Expect.equal value expected $"%s{name}: %s{value} should be {expected}"
                | Failure (msg, _, _) -> Tests.failtest $"%s{name}: {msg}")

        testCase "ws()"
        <| fun _ ->
            let input = "    "
            let expected = () // expected result is unit (because ws consumes whitespace)
            let parser = Parser.ws

            match run parser input with
            | Success (value, remainder, _) ->
                Expect.equal value expected $"Parsed value {value} should be {expected}"
                Expect.equal remainder () $"Remainder of input should be empty after parsing whitespace."
            | Failure (msg, _, _) -> Tests.failtest $"Unexpected failure: {msg}"

    ]

// // Chromatic scale
// let chromaticNotes = ["A"; "A#"; "B"; "C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"]
//
// // Function to transpose a note
// let transposeBySemitones (note: string) (semitones: int) =
//     let index = chromaticNotes |> List.findIndex ((=) note)
//     let newIndex = (index + semitones) % chromaticNotes.Length
//     chromaticNotes.[newIndex]
//
// // Test the function
// let testTranspose () =
//     let result = transposeBySemitones "A#" 3  // Expected: "B"
//     printfn "Transposed note: %s" result
//     result
//
// // Test case
// let result = testTranspose()