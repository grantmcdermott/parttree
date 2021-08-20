
if (require(mlr3)) {

  # Classification
  source('known_output/parttree_rpart_classification.R')
  # task_cl = tsk("iris", target = "Species") # simpler (but less precise?) version of below
  task_cl = TaskClassif$new("iris", iris, target = "Species")
  task_cl$formula(rhs = "Petal.Length + Petal.Width")
  fit_cl = lrn("classif.rpart")
  fit_cl$train(task_cl)
  expect_equal(pt_cl_known, parttree(fit_cl))

  # Regression
  source('known_output/parttree_rpart_regression.R')
  task_reg = TaskRegr$new("iris", iris, target = "Sepal.Length")
  task_reg$formula(rhs = "Petal.Length + Sepal.Width")
  fit_reg = lrn("regr.rpart")
  fit_reg$train(task_reg)
  expect_equal(pt_reg_known, parttree(fit_reg), , tolerance = 1e-7)

}
