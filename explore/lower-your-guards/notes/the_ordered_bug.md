# RESOLVED!!!

I was thinking about the term equality constraints backwards.
I was thinking of them as (x, HerebyBe y), when they are actually (x, OriginallyWas y)

I think I was coaxed into believing the HerebyBe version because it made some small
simplification in the code at one point.

A poignant warning, to not be drawn to everything beautiful, without thought


# Good Case
Expected behavior

```disc
foo : , Int Bool -> Bool
foo , 0 n = False
foo , n True = False
```
outputs: 
```
Uncoverd Pattern: , (Not 0) False
Redundant Clauses: []
```

# Bad Case 1
Is exactly the same as the good case, but with the clauses swapped.
Forgets about the zero for some reason

```disc
foo : , Int Bool -> Bool
foo , n True = False
foo , 0 n = False
```

outputs:
```
Uncovered Pattern: , _ False
Redundant Clauses: []
```

# Bad Case 2

Is exactly the same as the good case, but with the fist clause added to the end.
But this isn't reported as redundant!!!

```
foo : , Int Bool -> Bool
foo , 0 n = False
foo , n True = False
foo , 0 n = False
```

outputs:
```
Uncoverd Pattern: , (Not 0) False
Redundant Clauses: []
```

# Thoughts

These two clauses in this order seem to cause problems
```
foo : , Int Bool -> Bool
...
foo , n True = False
foo , 0 n = False
```

And, this configuration always seems to not report (3) as a redundancy
```
foo : , Int Bool -> Bool
...
foo , 0 n = False
foo , n True = False
foo , 0 n = False        (3)
...
```

# Possible

UA bug?

No.
Tested on the old uncovered function, same deal
