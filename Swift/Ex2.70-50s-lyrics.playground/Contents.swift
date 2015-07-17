import Cocoa

//: ## Exercise 2.70
//: The following eight-symbol slaphabet with associated relative frequencies was designed to efficiently encode the lyrics of 1950s rock songs. (Note that the "symbols" of an "alphabet" need not be individual letters.)
//:
//:    A    2   GET  2   SHA  3   WAH  1
//:    BOOM 1   JOB  2   NA  16   YIP  9
//:
//: Use *generateHuffmanTree* (Exercise 2.69) to generate a corresponding Huffman tree, and use encode (Exercise 2.68) to encode the following message:
//:
//:    Get a job
//:    Sha na na na na na na na na
//:    Get a job
//:    Sha na na na na na na na na
//:    Wah yip yip yip yip yip yip yip yip yip
//:    Sha boom
//: 
//: How many bits are required for the encoding? 
//:
//: What is the smallest number of bits that would be needed to encode this song if we used a fixed-length code for the eight-symbol alphabet?

let pairs = [("WAH",1),("A",2),("GET",2),("SHA",4),("BOOM",1),("JOB",2),("NA",16),("YIP",9)]
println(makeLeafSet(pairs))

let lyricTree = generateHuffmanTree(pairs)
println(lyricTree)

let lyrics = ["GET","A","JOB","SHA","NA","NA","NA","NA","NA","NA","NA","NA","GET","A","JOB","SHA","NA","NA","NA","NA","NA","NA","NA","NA","WAH","YIP","YIP","YIP","YIP","YIP","YIP","YIP","YIP","YIP","SHA","BOOM"]
let encodedLyrics = encode(lyrics, lyricTree)
println("\(encodedLyrics) requires \(encodedLyrics.count) bits of information")

println("Using a fixed-length code would require \(lyrics.count * 3) bits of information to encode the song")

let decoded = decode(encodedLyrics, lyricTree)

println(decoded)

for lyric in Array(Set(lyrics)) {
    println("\(lyric):\(encode([lyric],lyricTree))")
}
