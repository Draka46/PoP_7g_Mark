
(*
The following code constitutes a set of tests for the functions contained in
the textAnalysis library, for the use of ...
*)

open System
#load "textAnalysis.fs"
open textAnalysis
//#r "continuedFraction.dll"

let theStory = readText "littleClausAndBigClaus.txt"
let convertedText = convertText theStory
let storyWordHist = wordHistogram convertedText
let wordsInStory = ((List.ofArray (convertedText.Split ' ')).Length - 1)
let rndStrOfWords = randomWords storyWordHist wordsInStory
let rndWordHist = wordHistogram (randomWords storyWordHist wordsInStory)

let str_h = "hello"
let str_hw = "hello world"
let str_hww = "hello world world"
let str_htwwww = "hello there world who who world"

printfn "White-box testing of textAnalysis.fs"

/// The following code tests the 'wordHistogram' function.
/// Black-box tests a variety of differently sized strings with a different
/// number of reoccouring words.
/// White-box tests a variety of unordiary strings: empty, single letter 
/// and spaces.
printfn " Unit : wordHistogram"
printfn " Black-box tests"
printfn "   Test : 1 - %b" ((wordHistogram str_h).Length = 1 &&
                            (wordHistogram str_h).[0] = ("hello", 1))
printfn "   Test : 2 - %b" ((wordHistogram str_hw).Length = 2 &&
                            (wordHistogram str_hw).[0] = ("hello", 1) &&
                            (wordHistogram str_hw).[1] = ("world", 1))
printfn "   Test : 3 - %b" ((wordHistogram str_hww).Length = 2 &&
                            (wordHistogram str_hww).[0] = ("hello", 1) &&
                            (wordHistogram str_hww).[1] = ("world", 2))
printfn "   Test : 4 - %b" ((wordHistogram str_htwwww).Length = 4 &&
                            (wordHistogram str_htwwww).[0] = ("hello", 1) &&
                            (wordHistogram str_htwwww).[1] = ("there", 1) &&
                            (wordHistogram str_htwwww).[2] = ("world", 2) &&
                            (wordHistogram str_htwwww).[3] = ("who", 2))
printfn " White-box tests"
printfn "   Branch : 1a - %b" ((wordHistogram "").Length = 1 &&
                            (wordHistogram "").[0] = ("", 0))
printfn "   Branch : 1b - %b" ((wordHistogram "a").Length = 1 &&
                            (wordHistogram "a").[0] = ("a", 1))
printfn "   Branch : 1c - %b" ((wordHistogram " ").Length = 1 &&
                            (wordHistogram " ").[0] = ("", 0))
printfn "   Branch : 1d - %b" ((wordHistogram "  ").Length = 1 &&
                            (wordHistogram "  ").[0] = ("", 0))

/// The following code tests the 'diffw' function.
/// This tests a variety of histograms with variably less words than the 
/// base historgram they're being compared with.
/// Test 1 is to assure functionality.
/// Test 2-3 and 5-7 test limits for our threshold on respectively large
/// and small histograms.
/// Test 4 tests the diffw method against a randomized histogram.
/// Note that we have set out threshold at 1.0, as ... (NO EXPLANATION YET)
printfn " Unit : diffw"
printfn " Black-box tests"
printfn "   Test : 1 - %b" (diffw storyWordHist storyWordHist = 0.0)
printfn "   Test : 2 - %b" (diffw storyWordHist storyWordHist.[0 .. 
                                  storyWordHist.Length - 50] > 1.0)
printfn "   Test : 3 - %b" (diffw storyWordHist storyWordHist.[0 .. 
                                  storyWordHist.Length - 100] > 1.0)
printfn "   Test : 4 - %b" (diffw storyWordHist rndWordHist < 1.0)
printfn "   Test : 5 - %b" (diffw (wordHistogram str_h) (wordHistogram str_h) < 1.0)
printfn "   Test : 6 - %b" (diffw (wordHistogram str_hw) (wordHistogram str_h) < 1.0)
printfn "   Test : 7 - %b" (diffw (wordHistogram str_htwwww) (wordHistogram str_h) > 1.0)

/// The following code tests the '...' function.
/// This tests a variety of ...
/// Note that ...
//printfn " Unit : ..."
//printfn " Black-box tests"
//printfn "   Test : 1 - %b" false
//printfn " White-box tests"
//printfn "   Branch : 1a - %b" false