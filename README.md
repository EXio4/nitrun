# nitrun

Run Haskell programs without having to compile[1] them! 

---

```haskell
#!/usr/bin/env nitrun

import System.Environment

factorial n = product [1..n]

main = print . factorial . read . head =<< getArgs

```

---

[1] actually compiles the program on-demand, and caches the binary
