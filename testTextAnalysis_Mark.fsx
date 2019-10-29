
(*
The following code constitutes a set of tests for the functions contained in
the textAnalysis library, for the use of ...
*)

open System
#load "textAnalysis.fs"
open textAnalysis
//#r "continuedFraction.dll"

printfn "White-box testing of textAnalysis.fs"

/// The following code tests the 'wordHistogram' function.
/// Black-box tests a variety of differently sized strings with a different
/// number of reoccouring words.
/// White-box tests a variety of unordiary strings: empty, single letter 
/// and spaces.
printfn " Unit : wordHistogram"
printfn " Black-box tests"
printfn "   Test : 1 - %b" ((wordHistogram "hello").Length = 1 &&
                            (wordHistogram "hello").[0] = ("hello", 1))
printfn "   Test : 2 - %b" ((wordHistogram "hello world").Length = 2 &&
                            (wordHistogram "hello world").[0] = ("hello", 1) &&
                            (wordHistogram "hello world").[1] = ("world", 1))
printfn "   Test : 3 - %b" ((wordHistogram "hello world world").Length = 2 &&
                      (wordHistogram "hello world world").[0] = ("hello", 1) &&
                      (wordHistogram "hello world world").[1] = ("world", 2))
printfn "   Test : 4 - %b" (
        (wordHistogram "hello there world who who world").Length = 4 &&
        (wordHistogram "hello there world who who world").[0] = ("hello", 1) &&
        (wordHistogram "hello there world who who world").[1] = ("there", 1) &&
        (wordHistogram "hello there world who who world").[2] = ("world", 2) &&
        (wordHistogram "hello there world who who world").[3] = ("who", 2))
printfn " White-box tests"
printfn "   Branch : 1a - %b" ((wordHistogram "").Length = 1 &&
                            (wordHistogram "").[0] = ("", 0))
printfn "   Branch : 1b - %b" ((wordHistogram "a").Length = 1 &&
                            (wordHistogram "a").[0] = ("a", 1))
printfn "   Branch : 1c - %b" ((wordHistogram " ").Length = 1 &&
                            (wordHistogram " ").[0] = ("", 0))
printfn "   Branch : 1d - %b" ((wordHistogram "  ").Length = 1 &&
                            (wordHistogram "  ").[0] = ("", 0))

/// The following code tests the '...' function.
/// This tests a variety of ...
/// Note that ...
//printfn " Unit : diffw"
//printfn " Black-box tests"
//printfn "   Test : 1 - %b" false
//printfn " White-box tests"
//printfn "   Branch : 1a - %b" false

/// The following code tests the '...' function.
/// This tests a variety of ...
/// Note that ...
//printfn " Unit : ..."
//printfn " Black-box tests"
//printfn "   Test : 1 - %b" false
//printfn " White-box tests"
//printfn "   Branch : 1a - %b" false