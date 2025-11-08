module HelperTests

open Expecto

open ChordParser.ChordParser


type WrapCase = { Index: int; Semitones: int; Modulus: int; Expected: int }

[<Tests>]
let tests =
    testList "helpers" [
        testCase "hello world"
        <| fun _ ->
            let subject = true
            Expect.isTrue subject "I am, therefore I compute."

        testCase "contains things"
        <| fun _ ->
            let subject = true
            Expect.containsAll [| 2; 3; 4 |] [| 2; 4 |] "This is the case; {2,3,4} contains {2,4}"

        testCase "should skip"
        <| fun _ -> Tests.skiptest "Yup, waiting for a sunny day..."

        testCase "should be same after wrap around for 0 semitones"
        <| fun _ ->
            let idx = 0
            let semitones = 0
            let modulus = 12
            let actual = Domain.wrapAround (idx + semitones) modulus
            let expected = 0
            Expect.equal expected actual $"This should wrap around. {actual}"

        testCase "contains all 17 notes"
        <| fun _ -> Expect.equal Domain.rootNotes.Length 17 "Should have 17 notes"

        testList
            "wrapAround"
            ([ // (index semitones modulus expected)
                { Index = 0; Semitones = 0; Modulus = 12; Expected = 0 }
                { Index = 0; Semitones = 1; Modulus = 12; Expected = 1 }
                { Index = 11; Semitones = 1; Modulus = 12; Expected = 0 }
                { Index = 12; Semitones = 0; Modulus = 12; Expected = 0 }
                { Index = -1; Semitones = 0; Modulus = 12; Expected = 11 }
                { Index = 5; Semitones = 7; Modulus = 12; Expected = 0 }
                { Index = 14; Semitones = 0; Modulus = 12; Expected = 2 }
                { Index = -13; Semitones = 0; Modulus = 12; Expected = 11 }
             ]
             |> List.map (fun c ->
                 testCase $"wrapAround {c.Index}+{c.Semitones} mod {c.Modulus} = {c.Expected}"
                 <| fun _ ->
                     let actual = Domain.wrapAround (c.Index + c.Semitones) c.Modulus
                     Expect.equal actual c.Expected $"Expected {c.Expected} but got {actual}"))
    ]