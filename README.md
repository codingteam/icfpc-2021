Codingteam ICFP Contest 2021 Submission
=======================================

Solution Overview
-----------------

We've used semi-automated approach using two UI tools: one written in Python (aptly named `temp/*.py`) and another written in Scala (`src/**/*`).

During the contest, we've tried a bunch of algorithms.

One of the widely used solutions was a force-based algorithm, where too small or too long edges create forces and attract/detract each other; additional forces are added by edges and nodes being outside the hole boundaries.

We have also tried to use a [self-organizing map](https://en.wikipedia.org/wiki/Self-organizing_map) (see `SOMSolver` class), a genetic algorithm, and even tried to consider this problem as a boolean satisfiability problem, and apply a SAT solver (minisat).

Last but not least, we applied a bit of smarts and simple brute-force optimizations to the results of automatic algorithms.

Team Members
------------

(in no particular order):

- Minoru
- Akon32
- ForNeVeR
- portnov
- pink-snow
- sergevp
