## parttree(rpart::rpart(Sepal.Length ~ Petal.Length + Sepal.Width, data = iris))
pt_reg_known =
  data.frame(
    node = c(5, 7, 8, 9, 13, 24, 25),
    Sepal.Length = c(5.640000, 7.577778, 4.735000, 5.169697, 6.604000, 6.054545, 6.530000),
    path = c("Petal.Length < 4.25 --> Petal.Length >= 3.4" ,
             "Petal.Length >= 4.25 --> Petal.Length >= 6.05",
             "Petal.Length < 4.25 --> Petal.Length < 3.4 --> Sepal.Width < 3.25",
             "Petal.Length < 4.25 --> Petal.Length < 3.4 --> Sepal.Width >= 3.25",
             "Petal.Length >= 4.25 --> Petal.Length < 6.05 --> Petal.Length >= 5.15",
             "Petal.Length >= 4.25 --> Petal.Length < 6.05 --> Petal.Length < 5.15 --> Sepal.Width < 3.05",
             "Petal.Length >= 4.25 --> Petal.Length < 6.05 --> Petal.Length < 5.15 --> Sepal.Width >= 3.05"),
    xmin = c(3.40, 6.05, -Inf, -Inf, 5.15, 4.25, 4.25),
    xmax = c(4.25, Inf, 3.40, 3.40, 6.05, 5.15, 5.15),
    ymin = c(-Inf, -Inf, -Inf, 3.25, -Inf, -Inf, 3.05),
    ymax = c(Inf, Inf, 3.25, Inf, Inf, 3.05, Inf)
    )
