# parttree 0.0.1.9005

To be released as 0.1.0

#### Breaking changes

* Move ggplot2 to Suggests, following the addition of native (base R)
`plot.parttree` method. The `geom_parttree()` function now checks whether
ggplot2 is available on the user's system before executing any code. (#18)
* The `flipaxes` argument has been renamed to `flip`, e.g.
`parttree(..., flip = TRUE)`. (#18)

#### Improvements

* Parttree objects now have their own class with a dedicated `plot.parttree`
method, powered by tinyplot. (#18)  
* Major speed-up for extracting parttree nodes and coordinates on complicated
trees. (#15)
* Add method for tidymodels workflows objects fitted with `"rpart"` engine. (#7
by @juliasilge).

#### Bug fixes

* Support for negative values. (#6 by @pjgeens)
* Better handling of single-level factors and `flip(axes)`. (#5)

#### Internals

* Several dependency adjustments, e.g. tinyplot to Imports and ggplot2 to
Suggests. (#18)
* Added SVG snapshots for image-based tests. (#18)
* Bump ggplot2 version dependency to match deprecated functions from 3.4.0.
* Switched to "main" as primary GitHub branch for development.
* Added two dedicated vignettes.

# parttree 0.0.1

* Create `parttree()` generic. (again, #3 by @zeileis)
* Support for partykit objects (i.e. `constparty` class), which in turn allows support for base plot methods. (#3 by @zeileis)
* ggplot2 moves from imports to depends and is now automatically loaded with parttree.
* Add `flipaxes` argument for easy switching in case or mismatch plot orientation. (#2 by @brhkim)
* Added a `NEWS.md` file to track changes to the package.

# parttree 0.0.0.9000

* Initial set of functions.
