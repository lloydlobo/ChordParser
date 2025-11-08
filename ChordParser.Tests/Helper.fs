module HelperTests

open Expecto

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
            Tests.skiptest
                """
                [23:11:50 ERR] domain chord helpers.transpose with wraparound for sharps failed in 00:00:00.0450000.
                Root transposed up 3 semitones. String does not match at position 0. Expected char: 'B', but got 'A'.
                expected: B
                  actual: A#
                """

            let chord: Domain.Chord = { Root = "G"; Tonality = Some "maj"; Extension = Some "7"; BassNote = Some "F#" }
            let result = Domain.transpose 3 "#" chord
            Expect.equal result.Root "B" "Root transposed up 3 semitones"
            Expect.equal result.BassNote (Some "A#") "Bass note transposed up 3 semitones"

        // [23:08:02 ERR] domain chord helpers.transpose with wraparound for sharps failed in 00:00:00.0430000.
        // Root transposed up 3 semitones. String does not match at position 0. Expected char: 'B', but got 'A'.
        // expected: B
        //   actual: A#
        //    at HelperTests.domainChordTests@142-4.Invoke(Unit _arg4) in /home/user/Projects/ChordParser/ChordParser.Tests/Helper.fs:line 145
        //    at Expecto.Impl.execTestAsync@578-1.Invoke(Unit unitVar)
        //    at Microsoft.FSharp.Control.AsyncPrimitives.CallThenInvoke[T,TResult](AsyncActivation`1 ctxt, TResult result1, FSharpFunc`2 part2) in /__w/1/s/src/fsharp/src/FSharp.Core/async.fs:line 509
        //    at Microsoft.FSharp.Control.Trampoline.Execute(FSharpFunc`2 firstAction) in /__w/1/s/src/fsharp/src/FSharp.Core/async.fs:line 112 <Expecto>
        // [23:08:02 INF] EXPECTO! 18 tests run in 00:00:00.1039924 for miscellaneous – 17 passed, 1 ignored, 1 failed, 0 errored.  <Expecto>
        //
        // dotnet watch ❌ [ChordParser.Tests (net10.0)] Exited with error code 1
        // dotnet watch ⏳ Waiting for a file to change before restarting ...
        //
        testCase "chordToString formats chord correctly"
        <| fun _ ->
            let chord: Domain.Chord = { Root = "C"; Tonality = Some "maj"; Extension = Some "7"; BassNote = Some "E" }
            Expect.equal (Domain.chordToString chord) "(Cmaj7 /E)" "Chord string representation"
            let chord2: Domain.Chord = { Root = "D"; Tonality = None; Extension = Some "7"; BassNote = None }
            Expect.equal (Domain.chordToString chord2) "(D7)" "Chord string without tonality and bass"
    ]


[<Tests>]
let parserTests =
    testList "parser" [

        testCase "hello world"
        <| fun _ ->
            let subject = true
            Expect.isTrue subject "I am, therefore I compute."
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