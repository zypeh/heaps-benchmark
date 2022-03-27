### Day 1 : Implementing a Pairing Heap
The fairly simpler heap data structure. I also implemented the list / vector version of it. The lesson learnt: DO NOT UNDERESTIMATE THE POWER OF CONSING IN LINKED LIST!!!

```
benchmarked Numbers/Z.Data.ListHeap
time                 5.529 ms   (5.454 ms .. 5.602 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 5.391 ms   (5.348 ms .. 5.498 ms)
std dev              196.9 μs   (106.4 μs .. 362.2 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarked Numbers/Z.Data.Heap
time                 7.491 ms   (7.427 ms .. 7.575 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 7.436 ms   (7.391 ms .. 7.508 ms)
std dev              173.4 μs   (115.0 μs .. 291.8 μs)
```
