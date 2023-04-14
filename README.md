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

At the very least I should probably rename the `*residues*` variable...just as soon as I can figure out something better.

## [Day 12](src/day-12.lisp) First Dijkstra of 2022

First path-finding challenge of 2022. I initially tried using a normal priority queue and a visited set. Like always, I bungled it and switched to using a tracking map + Fibonacci heap. Anyway, straighforward use of Dijkstra's algorithm.

**Addendum (same day)** I felt like I wussed out of using the normal Dijkstra's algorithm, with a visited queue and a priority queue. I went ahead and implemented it for this problem. So, when can you use the normal Dijkstra's algorithm? When you never have to revisit a position because there's no way that a revisit would give you a lower total path cost. This is only the case when each move costs the same amount. When does it pay to use an updatable priority queue? Whenever you may have to revisit a path because the total cost may be lower on a subsequent visit. This is almost always the case when you have multiple moving pieces. In this case you are really tracking total cost of board states, and there are several ways to arrive at the same board state (albeit with different costs).

## [Day 13](src/day-13.lisp) Use Ternary Logic

At first I tried a purely recursive approach with binary logic. Then I tried a mixed iterative/recursive approach with binary logic. Finally, I used the mixed iterative/recursive approach with ternary logic. It's shocking to me I still fail to see ternary logic problems at first, given successes I've had at work applying ternary logic uniformly.

I liked the fact that I could use part-1 without any modifications in part-2, I just needed a translation layer to go from ternary to binary logic in order to use the standard lisp sequence functions.

## [Day 15](src/day-13.lisp) You Have To Do The Math

The numbers are too big here to just track things in a set/map and do counts at the end. For both parts you have to extract ranges of valid values. For part 1 I just did a loop to see if which x values were neither beacons nor in the ranges. For part 2 you have to coalesce the ranges row by row. If you have one range after coalescing, that can't be it. So if there are two ranges, then the non-overlapping value of those two ranges are the right answer, provided it's not also a beacon. Lots of fiddly math. It took me far too long to get the `coalese-ranges` code right. I kept trying to get a purely recursive one to work correctly, but writing an iterative/mutable turned out to be easier.

## [Day 16](src/day-16.lisp) BFS, Then Use Dynamic Programming

First, use BFS to find the shortest paths between all of the nodes that have a valve with pressure to release. Then start at AA, try all the paths, recursively trying all of the paths from each of those nodes. Track current minutes and position in a visited list. When you are done, you have found the highest pressure release.

Part 2 is slow, but it's almost identical to part 1. In this case, just track whether to move the man or the elephant next. Whoever has the lowest number of minutes is the next to move. It takes much longer because there's much fewer early terminations. Possibilities for improving are there. One improvement is to stop using lists for everything. All of that searching take a lot of time. Another would be to look for opportunities to cache things. 

**Addendum** I was not very happy with the original solution. I was hoping to find a super fast memoizable solution. I did make the code about 20 lines shorter and much faster, though not as fast as I had hoped. Perhaps there is a memoizable solution, or some other dynamic programming trick, but I can't see it.
