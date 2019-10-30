module textAnalysis

/// <summary> Calculate the cumulative sum of a list of integers from the first
/// to the last element. First element is the first number in the original list,
/// last element is the sum of all integers in the original list. </summary>
/// <param name = "lst"> A list </param>
/// <returns> A cumulative summed list. E.g., for lst = [e1; e2; e3],
/// [e1; e1+e2; e1+e2+e3] is returned. </returns>
let cumSum (lst : int list) : int list =
  List.tail (List.scan (+) 0 lst)

/// <summary> Given a monotonic function and an index into its value set, find
/// the corresponding value on its definition set. </summary>
/// <param name = "monotonic"> A list of samples of a monotonically increasing
/// function. E.g., if monotonic = [e1; e2; e3] then e1 <= e2 <= e3 </param>
/// <param name = "v"> A value in the codomain of monotonic </param>
/// <returns> A value in the domain of monotonic approximately corresponding to
/// v. E.g., if monotonic.[i] = v then reverseLookup v = i </returns>
let reverseLookup (monotonic : 'a list) (v : 'a) : int =
    // `findIndex` will throw an exception if `v` is larger than all elements in
    // `monotonic`. The try-with expression will default to the with-clause if
    // such an exception is thrown.
    try
      List.findIndex (fun w -> w > v) monotonic
    with
      _ -> monotonic.Length - 1

// The random generator is created outside any function call. This is done to
// use a single seed for all random numbers, thus avoiding using the same seed
// more than once.
let rnd = System.Random()

// The remaining functions are specialized to work with the following alphabet
let alphabet = ['a'..'z']@[' ']

/// <summary> Generate a random character according to a histogram. </summary>
/// <param name = "hist"> A list of histogram values with count hist.[0] being
/// the value for 'a', hist.[1] for 'b' etc. </param>
/// <returns> A character randomly drawn from a distribution resembling
/// hist. </returns>
let randomChar (hist : int list) : char =
  let cumHist = cumSum hist
  let v = rnd.Next(cumHist.[cumHist.Length-1])
  let i = reverseLookup cumHist v
  alphabet.[i] // Warning, this may cause an index out-of-bound exception

/// <summary> Generate a string of random characters each distributed according
/// to a histogram. </summary>
/// <param name = "hist"> A list of histogram values </param>
/// <param name = "len"> The length of the resulting string </param>
/// <returns> A string of lenth len whose values are independently drawn from a
/// distribution resembling hist </returns>
let randomString (hist : int list) (len : int) : string =
  String.init len (fun _ -> string (randomChar hist))

/// <summary> Generate a histogram of the characters 'a'..'z' in a given
/// string. </summary>
/// <param name = "str"> Any string consisting of the characters: 'a'..'z' and
/// ' ' in any order. </param>
/// <returns> A list of character counts, with the first element is the count of
/// 'a's in str, second the count of 'b's etc. </returns>
let histogram (str : string) : int list =
  List.init alphabet.Length (fun c -> (str.Split alphabet.[c]).Length - 1)

let readText (filename : string) =
  let text = 
    try
      let reader = System.IO.File.OpenText filename
      reader.ReadToEnd ()
    with
      _ -> "" // The file cannot be read, so we return an empty string
  text

let convertText (src : string) : string =
  let mutable text = src.ToLower()
  text <- String.filter (fun x -> List.contains x alphabet) text
  text

let diff (h1 : int list) (h2 : int list) : double =
  double (List.sum (List.init alphabet.Length
           (fun c -> pown (h1.[c] - h2.[c]) 2))) / double alphabet.Length


// ===  Del 3 ===

// The following functions are designed to work with this representation
// of a word histogram.
type wordHistogram = (string * int) list

// Using list initilization, the function creates a list of all letters
// preceeded or proceeded by an empty space, using the Split function.
// Multiples are ignored, do to using the distinct function.
// Word occourneces are counted similarly, by counting the resulting number
// of elements in list created from splitting the string according to each
// word (minus one, as initial split creates two elements).
/// <summary> Generate a histogram of all words in a given string. </summary>
/// <remark> We define words as any collection of letters preceeded or
/// proceeded by a space. </remark>
/// <param name = "src"> Any string consisting of letters and ' ' in any 
/// order </param>
/// <returns> A list of word counts, consisting of (string * int) tubles
/// where the first element is the word and the second is the number of
/// occourences of that word. </returns>
let wordHistogram (src : string) : wordHistogram =
  let wordList = List.distinct (List.ofArray (src.Split ' '))
  List.init wordList.Length (fun c -> 
       (wordList.[c], (src.Split wordList.[c]).Length - 1))

// Sums together the occourences of words in wHist, until the sum exceeds
// the occo value (histogram occourence sum that specifies the randomly chosen
// word from wHist) and then returns the wHist according index.
/// <summary> Identifies the index of the word in a histogram, according to
/// the summed occourence. </summary>
/// <param name = "wHist"> Any word histogram </param>
/// <param name = "occo"> Histogram sum of word </param>
/// <param name = "sum"> Recursively increasing sum of histogram </param>
/// <returns> Index of specific word in wHist </returns>
let rec wordLookUp (wHist : wordHistogram) (occo : int) (sum : int) 
                   (index : int) : int =

  let newSum = sum + (snd wHist.[0])
  if occo <= newSum then
    index
  else
    (wordLookUp wHist.Tail occo newSum (index + 1))

// Sums all the counted occourences of words in the histogram and uses 
// rnd (random) function to get a random value equal or less to the sum,
// thus making the word to be chosen based on weighted-randomness, acorrding
// to words' number of occourences.
/// <summary> Generates a random word, using a historgram. </summary>
/// <param name = "wHist"> Any word histogram </param>
/// <returns> A random word count from wHist. </returns>
let randomWordSelect (wHist : wordHistogram) : (string * int) =
  let occoSum = List.sum (List.init wHist.Length (fun c -> snd wHist.[c]))
  let occoN = rnd.Next (occoSum + 1)
  let index = wordLookUp wHist occoN 0 0
  wHist.[index]

// Calls randomWordSelect method to generate a random word from wHist and
// then recursively builds the string of random words.
/// <summary> Generate a string of random words, using a histogram. </summary>
/// <remark> Histogram is used to determine likelihood for indevidual random 
/// word's occourence in the string. </remark>
/// <param name = "wHist"> Any word histogram </param>
/// <param name = "nWords"> Number of words that string should contain </param>
/// <returns> A string, consisting of random words. </returns>
let rec randomWords (wHist : wordHistogram) (nWords : int) : string =
  let word = fst (randomWordSelect wHist)

  if nWords = 1 then
    word
  else
    word + " " + randomWords wHist (nWords - 1)

// Tries to search for the refered word in w1, in w2. It it occours (doesn't
// return 'None'), returns the word count as it is in w2. It it doesn't occour,
// returns a word count of the word, with occourence set as '0'.
/// <summary> Finds word in word histogram if it's there or returns a '0' value
/// word count if it isn't there. </summary>
/// <param name = "w1"> Any word histogram </param>
/// <param name = "w2"> Word histogram subset of w1 </param>
/// <param name = "c"> Index of word in w2, to find in w2 </param>
/// <returns> Word count of word in w2 or '0' value word count. </returns>
let sortWHist (w1 : wordHistogram) (w2 : wordHistogram) (c : int) =
  let w1E = w1.[c]

  if ((List.tryFind (fun k -> fst k = fst w1.[c]) w2) <> None) then 
    List.find (fun k -> fst k = fst w1.[c]) w2 
  else 
    (fst w1E, 0)

// Using sortWHist function, generates a variation of w2 that is sorted for
// appropriate comparison with w1 and calculates the result of the diffw value
// (from 7g.pdf), using the number of word occourences.
/// <summary> Generates a value representing size of difference between
/// two histograms. </summary>
/// <remark> w1 is the base-line histogram and w2 the one it is being compared
/// to, meaning that we assume w1 to hold any words not in w2. </remark>
/// <param name = "w1"> Any word histogram </param>
/// <param name = "w2"> Word histogram subset of w1 </param>
/// <returns> Difference between histograms, represented as a value. </returns>
let diffw (w1 : wordHistogram) (w2 : wordHistogram) : double =
  let newW2 = List.init w1.Length (fun c -> sortWHist w1 w2 c)
  
  // For comparison
  //printfn "w1: %A" w1
  //printfn "w1 l: %A" w1.Length
  //printfn "w2: %A" w2
  //printfn "w2 l: %A" w2.Length
  //printfn "newW2: %A" newW2
  //printfn "newW2 l: %A" newW2.Length

  double (List.sum (List.init w1.Length
           (fun c -> pown (snd w1.[c] - snd newW2.[c]) 2))) / double w1.Length