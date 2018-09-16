Grammar for WHILE programs, supported by `while-rs`
===================================================

```
P ::= xI := xJ + C
    | xI := xJ - C
    | P;P
    | LOOP xI DO P END
    | WHILE xI != 0 DO P END
```

where `I` and `J` are unsigend integer and `C` is a signed integer.

