# Summary

Data structure intended for accumulating a sequence of elements
for later traversal or folding.
A great basis for implementing many custom monoids,
most notably of the Builder pattern.

It shines with its Monoid instance,
which relieves the user from caring about from which side to append.
This is important because,
different data-structures exhibit very different performance depending on that.
Most notably List.
Acc on the other hand is neutral and performs well in all scenarios.

For such purposes it is common to use Seq or DList.
The benchmark results below show that Acc is a better fit.

# Benchmark results

These benchmarks compare the performance of acc vs. various other structures
as used for aggregation with intent of reduction.

In other words a two-step process of the following structure is measured as a whole:

1. Construct the measured data-structure of from data of a given size using cons or snoc or fromList
2. Fold the data-structure using sum or length

```
sum/cons/10000/acc                       402.9 μs
sum/cons/10000/list                      598.8 μs
sum/cons/10000/dlist                     1.088 ms
sum/cons/10000/sequence                  872.6 μs
sum/snoc/10000/acc                       604.2 μs
sum/snoc/10000/dlist                     846.6 μs
sum/snoc/10000/sequence                  876.6 μs
sum/fromList/10000/acc                   494.7 μs
sum/fromList/10000/list                  426.5 μs
sum/fromList/10000/dlist                 626.2 μs
sum/fromList/10000/sequence              187.6 μs
length/cons/10000/acc                    399.6 μs
length/cons/10000/list                   128.5 μs
length/cons/10000/dlist                  546.7 μs
length/cons/10000/sequence               790.3 μs
```
