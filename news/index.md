# Changelog

## parttree 0.1.3

CRAN release: 2026-03-31

No user-facing changes.

##### Internals

- Update maintainer email address.

## parttree 0.1.2

CRAN release: 2026-01-15

##### New features

- Bundle low-res versions of the images used in the Abstract Art
  vignette to bypass potential download issues and CRAN warnings.
  ([\#25](https://github.com/grantmcdermott/parttree/issues/25))

## parttree 0.1.1

CRAN release: 2025-05-10

No user-facing changes.

##### Internals

- Bump svglite to v2.2.0 and update test snapshots to fix CRAN errors.

## parttree 0.1.0

CRAN release: 2025-01-16

Our first CRAN submission. 🎉🎉

##### Breaking changes

- Move ggplot2 to Enhances, following the addition of a dedicated
  (base R) `plot.parttree` method. The
  [`geom_parttree()`](https://grantmcdermott.com/parttree/reference/geom_parttree.md)
  function is still available, but requires that ggplot2 is already been
  installed on the user’s system.
  ([\#18](https://github.com/grantmcdermott/parttree/issues/18))
- The `flipaxes` argument has been renamed to `flip`, e.g.
  `parttree(..., flip = TRUE)`.
  ([\#18](https://github.com/grantmcdermott/parttree/issues/18))

##### Improvements

- Parttree objects now have their own class with a dedicated
  `plot.parttree` method, powered by tinyplot.
  ([\#18](https://github.com/grantmcdermott/parttree/issues/18))  
- Major speed-up for extracting parttree nodes and coordinates on
  complicated trees.
  ([\#15](https://github.com/grantmcdermott/parttree/issues/15))
- Add method for tidymodels workflows objects fitted with `"rpart"`
  engine. ([\#7](https://github.com/grantmcdermott/parttree/issues/7) by
  [@juliasilge](https://github.com/juliasilge)).

##### Bug fixes

- Support for negative values.
  ([\#6](https://github.com/grantmcdermott/parttree/issues/6) by
  [@pjgeens](https://github.com/pjgeens))
- Better handling of single-level factors and `flip(axes)`.
  ([\#5](https://github.com/grantmcdermott/parttree/issues/5))
- Handling of complex formula expressions.
  ([\#17](https://github.com/grantmcdermott/parttree/issues/17))

##### Internals

- Several dependency adjustments, e.g. tinyplot to Imports and ggplot2
  to Suggests.
  ([\#18](https://github.com/grantmcdermott/parttree/issues/18))
- Added SVG snapshots for image-based tests.
  ([\#18](https://github.com/grantmcdermott/parttree/issues/18))
- Bump ggplot2 version dependency to match deprecated functions from
  3.4.0.
- Switched to “main” as primary GitHub branch for development.
- Added two dedicated vignettes.

## parttree 0.0.1

- Create
  [`parttree()`](https://grantmcdermott.com/parttree/reference/parttree.md)
  generic. (again,
  [\#3](https://github.com/grantmcdermott/parttree/issues/3) by
  [@zeileis](https://github.com/zeileis))
- Support for partykit objects (i.e. `constparty` class), which in turn
  allows support for base plot methods.
  ([\#3](https://github.com/grantmcdermott/parttree/issues/3) by
  [@zeileis](https://github.com/zeileis))
- ggplot2 moves from imports to depends and is now automatically loaded
  with parttree.
- Add `flipaxes` argument for easy switching in case or mismatch plot
  orientation.
  ([\#2](https://github.com/grantmcdermott/parttree/issues/2) by
  [@brhkim](https://github.com/brhkim))
- Added a `NEWS.md` file to track changes to the package.

## parttree 0.0.0.9000

- Initial set of functions.
