## Comparison data
d =
  data.frame(
    node = c(2, 6, 7),
    Species = factor(c("setosa", "versicolor", "virginica")),
    path = c("Petal.Length < 2.45",
             "Petal.Length >= 2.45 --> Petal.Width < 1.75",
             "Petal.Length >= 2.45 --> Petal.Width >= 1.75"),
    xmin = c(-Inf, 2.45, 2.45),
    xmax = c(2.45, Inf, Inf),
    ymin = c(-Inf, -Inf, 1.75),
    ymax = c(Inf, 1.75, Inf)
  )

## Formula
fml = formula(Species ~ Petal.Length + Petal.Width)

## rpart
rp = rpart::rpart(fml, data = iris)
expect_equal(d, parttree(rp))

## partykit
if (require(partykit)) {
  rp2 = as.party(rp)
  pt_rp2 = parttree(rp2)
  row.names(pt_rp2) = NULL
  expect_equal(d[, setdiff(names(d), 'node')],
               pt_rp2[, setdiff(names(pt_rp2), 'node')])
}

