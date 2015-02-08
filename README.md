# SICP Exercises in Racket and Swift

This is just a personal project to work through the exercises in the Structure and Interpretation of Computer Programs textbook in a dialect of Lisp and Swift.

## Thoughts on Chapter 1
Chapter 1 has been very interesting. This book is nothing like any other programming book I have ever read. In a lot of programming texts the bulk of the book is devoted to demonstrating the syntax of the language. SICP appears to go out of its way to demonstrate as much as possible with the least amount of syntax learned. For example Chapter one has you writing algorithms for the following tasks

- Square root
- Cube root
- Factorial
- Addition using only increment and decrement functions
- Ackermann's function
- Fibonacci
- Pascal's triangle
- Sine
- Exponential
- Multiply using only double and halve functions
- Greatest common divisor
- Testing for Prime numbers

Using only
- Basic arithmetic operators (+ - / * %)
- If statements
- Condition statements (Switch/Case in Swift)
- and recursive function definitions

For most of the algorithms mentioned above the performance implications of multiple implementation techniques are explored as well. 

Very enlightening so far.

I have been using [DrRacket](http://racket-lang.org) as my lisp environment. I have seen it recommended that [MIT Scheme](http://www.gnu.org/software/mit-scheme/) is a better choice. For now I'll be sticking with DrRacket and to date I haven't run into any issues. There is a [SICP compatibility mode for DrRacket](http://www.neilvandyke.org/racket-sicp/), I haven't tried it out yet though it is apparently required for the later chapters.

Using Swift to carry out the same exercises has been interesting. Racket makes Swift feel verbose. When I started this project I found Lisp incredibly hard to read. I really couldn't make head nor tail of it. Now that I am more familiar with I find that I like its terseness. This might all be due to the fact that I am working on concise, short-lived puzzles so far. 

Swift playgrounds are very useful for this sort of experimentation. It is far easier than any other option that XCode provides. Performance is an issue though. Swift Playgrounds are much, much slower than DrRacket. To the point of not being useable for some algorithms. The execution count in the playground gutter to show how often a line of code has been called is very useful for exploring the growth of calculations.