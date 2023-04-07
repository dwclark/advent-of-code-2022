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

