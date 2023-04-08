# Advent of Code 2022

## Overview

This repo contains solutions to [Advent of Code 2022](https://adventofcode.com/2022) done in Common Lisp. Each day is in its own file and defined in its own package. Doing it this way makes every day self contained and easy to work on. Load the system using quicklisp. You will also need to tell lisp where to find the inputs for each day. They are in the inputs directory. For example:

```Common Lisp
(ql:quickload "advent-of-code-2022")
(setf utils:*input-directory* "/my/development/directory/advent-of-code-2022/inputs/")
```

Change the path to match what's on your machine.

Each day is in its own package called `day-n`, replace n with the day number. Each day also has two functions which show the solutions for that day named `part-1` and `part-2`. So if you want to see the solutions for day 1:

```Common Lisp
(in-package :day-1)
(part-1)
(part-2)
```

Run all of the above commands inside your common lisp environment. And your common lisp environemnt SHOULD be a properly set up to use SLIME. The easiest way to do that is to load [quicklisp](https://www.quicklisp.org/beta/) and then let quicklisp set everything up for you.

## [Day 1](src/day-01.lisp) Put Title Here

## [Day 10](src/day-10.lisp) I Hate Confusing Instructions

Part 1 was fine. However, it took forever to figure out what part 2 was saying. While this may provide practice in reading ambiguous and poorly specified requirements, this is the least favorite part of my day job. This may be my least favorite type of AOC challenge.

## [Day 11](src/day-11.lisp) Chinese Remainder Theorem

There were two challenges for this day: 1) Parse the inputs to be able to produce the correct initial monkey states and 2) figure out how to keep the numbers from growing exponentially in part 2.

Part 1 is super easy if you are using lisp. Remove the `:` and `,` characters, allow lisp to read them as symbols, then pick out the interesting parts from the lists of symbols. The only real tricky part is constructing the function for each monkey to compute. I ended up just using a dynamic variable and `eval`-ing a dynamically constructed expression. I originally thought a macro would get the job done. This however was dumb of me because macros run at compile time while the expression needed to be made at runtime (since we are parsing a file and all macros had already been run).

Part 2 was using the [Chinese Remainder Theorem](https://en.wikipedia.org/wiki/Chinese_remainder_theorem), a solution strategy that comes up semi-regularly. I ended up guessing that replacing the computed value with the mod of all the co-prime divisors multiplied together would get the job done, which it did. It sort of makes sense from the definition of the Chinese Remainder Theorem. However, at this point I should probably stop AOC for a little bit and attempt to understand why it worked. In any case, modular arithmetic is _stil_ confusing to me, and attempting to remedy that will probably pay dividends down the road.

