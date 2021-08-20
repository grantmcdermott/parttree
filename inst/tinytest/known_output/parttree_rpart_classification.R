## parttree(rpart::rpart(Sepal.Length ~ Petal.Length + Sepal.Width, data = iris))
pt_cl_known =
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
