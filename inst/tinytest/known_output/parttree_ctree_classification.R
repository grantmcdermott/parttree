# parttree(partykit::ctree(Sepal.Length ~ Petal.Length + Sepal.Width,
#                          control = ctree_control(maxdepth = 2),
#                          data = iris))
pt_ct_cl_known =
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
