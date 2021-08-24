# visualization

## To Profile
Setup what you want to profile in main, then run:
```
stack run --profile -- visualization +RTS -p -l-au
ghc-prof-flamegraph visualization.prof
```

You can get [ghc-prof-flamegraph][ghc-prof-flamegraph] where linked.

[ghc-prof-flamegraph]: https://github.com/fpco/ghc-prof-flamegraph
