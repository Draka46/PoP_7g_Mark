
open System
#load "textAnalysis.fs"
open textAnalysis

let text = readText "littleClausAndBigClaus.txt"
let convertedText = convertText text

//printfn "%A" text.[0 .. 200]
//printfn "%A" convertedText.[0 .. 200]
//printfn "%A" (histogram convertedText.[0 .. 100])
//printfn "%A" (histogram convertedText)
//printfn "%A" (diff (histogram convertedText) (histogram convertedText))
//printfn "%A" (diff (histogram convertedText.[0 .. (convertedText.Length - 2)]) (histogram convertedText))
//printfn "%A" (diff (histogram convertedText.[0 .. (convertedText.Length - 3)]) (histogram convertedText))
//printfn "%A" (diff (histogram convertedText.[0 .. (convertedText.Length - 30)]) (histogram convertedText))

let randomHist = histogram (randomString (histogram convertedText) convertedText.Length)
printfn "%A" (histogram convertedText)
printfn "%A" randomHist
printfn "%A" (diff randomHist (histogram convertedText))

let wordHistrogram1 = wordHistogram convertedText
printfn "%A" (wordHistrogram1)

let randomWH = (randomWords wordHistrogram1 4250)
//printfn "%A" (randomWH).[0 .. 400]
//printfn "%A" (diffw wordHistrogram1 wordHistrogram1)
//printfn "%A" (diffw wordHistrogram1 wordHistrogram1.[0 .. (wordHistrogram1.Length - 50)])
printfn "%A" (diffw wordHistrogram1 (wordHistogram randomWH))