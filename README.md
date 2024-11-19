# Tyche Adapter for QuickCheck

This library provides a wrapper for QuickCheck properties that allows them to be analyzed using
_Tyche_ on [on the web](https://tyche-pbt.github.io/tyche-extension/) or in the
[VSCode extension](https://marketplace.visualstudio.com/items?itemName=HarrisonGoldstein.tyche).

## Installing

For the moment, the library needs to be installed from GitHub. Add the following lines to your
`cabal.project` file:
```
source-repository-package
  type: git
  location: https://github.com/tyche-pbt/tyche-quickcheck
```
and then add `tyche` to your `build-depends` as usual:
```
  build-depends:
    ...
    tyche,
    ...
```

To use this wrapper, simply follow the recipe here:
```haskell
prop_insertPost :: (Double, Double) -> Int -> Tree -> Property
prop_insertPost (d1, d2) x t =
  Tyche.visualize "prop_insert_post" $
    labelNumber "size" (size t) $
    labelContinuous "d" d1 $
    labelPair "p" ("d1", d1) ("d2", d2) $
    labelNumber "value" x $
    labelCategory "isBST" (show (isBST t)) $
      isBST t ==> member x (insert x t)
```
wrap your property with `Tyche.visualize`, adding a string to name it, and then within the property
call `labelNumber` or `labelCategory` to label different features for analysis. The full worked
example can be found in the `test/` directory.

Once the property runs, the data will be placed in `.quickcheck/observations/`. If Tyche is
installed in VSCode, the interface should immediately appear. If using the in-browser version of the
tool, simply navigate to these files from the browser.