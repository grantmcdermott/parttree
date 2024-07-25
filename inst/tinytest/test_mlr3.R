
if (require(mlr3)) {

  # Classification
  source('known_output/parttree_rpart_classification.R')
  # task_cl = tsk("iris", target = "Species") # simpler (but less precise?) version of below
  task_cl = TaskClassif$new("iris", iris, target = "Species")
  task_cl$formula(rhs = "Petal.Length + Petal.Width")
  fit_cl = lrn("classif.rpart", keep_model = TRUE)
  fit_cl$train(task_cl)
  fit_cl_pt = parttree(fit_cl)
  attr(fit_cl_pt, "parttree") = NULL
  class(fit_cl_pt) = "data.frame"
  expect_equal(pt_cl_known, fit_cl_pt)

  # Regression
  source('known_output/parttree_rpart_regression.R')
  task_reg = TaskRegr$new("iris", iris, target = "Sepal.Length")
  task_reg$formula(rhs = "Petal.Length + Sepal.Width")
  fit_reg = lrn("regr.rpart", keep_model = TRUE)
  fit_reg$train(task_reg)
  fit_reg_pt = parttree(fit_reg)
  attr(fit_reg_pt, "parttree") = NULL
  class(fit_reg_pt) = "data.frame"
  expect_equal(pt_reg_known, fit_reg_pt, tolerance = 1e-7)

}
