// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT License.

module Microsoft.Quantum.QsCompiler.Testing.RegexTests

open Xunit
open Microsoft.Quantum.QsCompiler.CommandLineCompiler
open Microsoft.Quantum.QsCompiler.DataTypes
open Microsoft.Quantum.QsCompiler.Testing.TestUtils
open Microsoft.Quantum.QsCompiler.Transformations.SearchAndReplace
open Microsoft.Quantum.QsCompiler.SyntaxTree


[<Fact>]
let ``Valid array bracket matching`` () =
    [
        "[]"
        "[ some text ]"
        "[ a;
           b
           c ]"
        "[[]]"
        "[a [], b sf; [a ; b c;] ]"
        "[a [], b [sf; [a ; b c;]; 4]]"
    ]
    |> List.iter (fun str -> FormatCompilation.WithinArrayBrackets.Match str |> verifyMatch str)

[<Fact>]
let ``AAAA`` () =
    let original = { Namespace = "Test"; Name = "a__8cdadee32e1d46879bbeb4779cae5c2f__Foo" }
    let gen1 = NameGenerator.GenerateCallableName(original)
    let gen2 = NameGenerator.GenerateCallableName(gen1)

    let is = NameGenerator.IsGeneratedName(original)
    let is1 = NameGenerator.IsGeneratedName(gen1)
    let is2 = NameGenerator.IsGeneratedName(gen2)

    let back = NameGenerator.OriginalCallableFromGenerated(original)
    let back1 = NameGenerator.OriginalCallableFromGenerated(gen1)
    let back2 = NameGenerator.OriginalCallableFromGenerated(gen2)

    ()

[<Fact>]
let ``BBBB`` () =
    let original = "myLabel_12__foo"
    let label = "myLabel"
    let gen1 = NameGenerator.GenerateVariableName(label, -11, original)
    let gen2 = NameGenerator.GenerateVariableName(label, -11, gen1)

    let back = NameGenerator.OriginalVariableFromGenerated(label, original)
    let back1 = NameGenerator.OriginalVariableFromGenerated(label, gen1)
    let back2 = NameGenerator.OriginalVariableFromGenerated(label, gen2)

    ()

[<Fact>]
let ``Invalid array bracket matching`` () =
    [ "["; "]"; "some text" ]
    |> List.iter (fun str -> FormatCompilation.WithinArrayBrackets.Match str |> verifyNoMatch str)

[<Fact>]
let ``Array bracket matching within text`` () =
    [
        ("some text [] more text", [ "[]" ])
        ("[ []", [ "[]" ])
        ("[[], []", [ "[]"; "[]" ])
        ("[[]]", [ "[[]]" ])
        ("[a [], b sf; [a ; b c;] ]", [ "[a [], b sf; [a ; b c;] ]" ])
        ("one [1;2] two [1], []", [ "[1;2]"; "[1]"; "[]" ])
        ("a[1;2]-aeg\[[],[1;2]]", [ "[1;2]"; "[[],[1;2]]" ])
        ("[a [], b [sf; [a ; b c;]; 4]", [ "[]"; "[sf; [a ; b c;]; 4]" ])
        ("[a [], b sf; [a ; b c;]; 4]", [ "[a [], b sf; [a ; b c;]; 4]" ])
        ("[a [], b [sf; [a ; b c;]; 4", [ "[]"; "[a ; b c;]" ])
    ]
    |> List.iter (fun (str, exp) -> FormatCompilation.WithinArrayBrackets.Matches str |> verifyMatches exp)

[<Fact>]
let ``Replace array item delimeters`` () =
    [
        ("some text", "some text")
        ("some text [1;2;] more text", "some text [1,2,] more text")
        ("[ [;]", "[ [,]")
        ("[[1;2]; []", "[[1,2]; []")
        ("[a [], b sf; [a ; b c;] ]", "[a [], b sf, [a , b c,] ]")
        ("one [1;2] two [1], []", "one [1,2] two [1], []")
        ("a[1;2]-aeg\[[],[1;2]]", "a[1,2]-aeg\[[],[1,2]]")
        ("[a [1;]; b [sf; [a ; b c;]; 4]", "[a [1,]; b [sf, [a , b c,], 4]")
        ("[a [1;]; b sf; [a ; b c;]; 4]", "[a [1,], b sf, [a , b c,], 4]")
        ("[a [1;]; b [sf; [a ; b c;]; 4", "[a [1,]; b [sf; [a , b c,]; 4")
    ]
    |> List.iter (fun (str, exp) -> FormatCompilation.UpdateArrayLiterals str |> fun got -> Assert.Equal(exp, got))


[<Fact>]
let ``Strip unique variable name resolution`` () =
    let NameResolution = new UniqueVariableNames()

    // name wrapping is added and stripped without verifying the validity of the variable name
    let origNames =
        [
            "var1"
            "__var2__"
            "3"
            "1+5"
            "some name" // the matching will fail (only) if there is a linebreak
            "'TName"
        ]

    origNames
    |> List.map (fun var -> var, UniqueVariableNames.StripUniqueName var)
    |> List.iter Assert.Equal

    origNames
    |> List.map NameResolution.SharedState.GenerateUniqueName
    |> List.map (fun unique -> unique, NameResolution.SharedState.GenerateUniqueName unique)
    |> List.map (fun (unique, twiceWrapped) -> unique, UniqueVariableNames.StripUniqueName twiceWrapped)
    |> List.iter Assert.Equal

    origNames
    |> List.map (fun var -> var, NameResolution.SharedState.GenerateUniqueName var)
    |> List.map (fun (var, unique) -> var, NameResolution.SharedState.GenerateUniqueName unique)
    |> List.map (fun (var, twiceWrapped) -> var, UniqueVariableNames.StripUniqueName twiceWrapped)
    |> List.map (fun (var, unique) -> var, UniqueVariableNames.StripUniqueName unique)
    |> List.iter Assert.Equal

    origNames
    |> List.map (fun var -> var, NameResolution.SharedState.GenerateUniqueName var)
    |> List.map (fun (var, unique) -> var, UniqueVariableNames.StripUniqueName unique)
    |> List.iter Assert.Equal
