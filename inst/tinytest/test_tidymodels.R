#
# Classification
#

# Comparison data
source('known_output/parttree_rpart_classification.R')

if (require(tidymodels)) {
  fml_cl = Species ~ Petal.Length + Petal.Width

  # parsnip
  ps_cl_pt = decision_tree() |>
    set_engine("rpart") |>
    set_mode("classification") |>
    fit(fml_cl, data = iris) |>
    parttree()
  attr(ps_cl_pt, "parttree") = NULL
  class(ps_cl_pt) = "data.frame"
  expect_equal(pt_cl_known, ps_cl_pt)

  # workflows
  wf_spec_cl = decision_tree() |> set_mode("classification")
  wf_tree_cl = workflow(fml_cl, spec = wf_spec_cl)
  wf_cl = fit(wf_tree_cl, iris)
  wf_cl_pt = parttree(wf_cl)
  attr(wf_cl_pt, "parttree") = NULL
  class(wf_cl_pt) = "data.frame"
  expect_equal(pt_cl_known, wf_cl_pt)
}

#
# Regression
#

# Comparison data
source('known_output/parttree_rpart_regression.R')

if (require(tidymodels)) {
  fml_reg = Sepal.Length ~ Petal.Length + Sepal.Width

  # parsnip
  ps_reg_pt = decision_tree() |>
    set_engine("rpart") |>
    set_mode("regression") |>
    fit(fml_reg, data = iris) |>
    parttree()
  attr(ps_reg_pt, "parttree") = NULL
  class(ps_reg_pt) = "data.frame"
  expect_equal(pt_reg_known, ps_reg_pt, tolerance = 1e-7)

  # workflows
  wf_spec_reg = decision_tree() |> set_mode("regression")
  wf_tree_reg = workflow(fml_reg, spec = wf_spec_reg)
  wf_reg = fit(wf_tree_reg, iris)
  wf_reg_pt = parttree(wf_reg)
  attr(wf_reg_pt, "parttree") = NULL
  class(wf_reg_pt) = "data.frame"
  expect_equal(pt_reg_known, wf_reg_pt, tolerance = 1e-7)
}
