## Comparison data
## Note that tree from partykit::ctree is slightly different to rpart::rpart
d_ct =
  data.frame(
    node = c(2, 4, 5),
    Species = factor(c("setosa", "versicolor", "virginica")),
    path = c("Petal.Length <= 1.9",
             "Petal.Length > 1.9 --> Petal.Width <= 1.7",
             "Petal.Length > 1.9 --> Petal.Width > 1.7"),
    xmin = c(-Inf, 1.9, 1.9),
    xmax = c(1.9, Inf, Inf),
    ymin = c(-Inf, -Inf, 1.7),
    ymax = c(Inf, 1.7, Inf)
    )

## Formula
fml = formula(Species ~ Petal.Length + Petal.Width)

## partykit
if (require(partykit)) {
  ct = ctree(fml,
             control = ctree_control(maxdepth = 2),
             data = iris)
  pt_ct = parttree(ct)
  row.names(pt_ct) = NULL
  expect_equal(d_ct, pt_ct)
}
