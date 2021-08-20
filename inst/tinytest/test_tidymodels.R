#
# Classification
#

# Comparison data
source('known_output/parttree_rpart_classification.R')

if (require(tidymodels)) {
  fml_cl = Species ~ Petal.Length + Petal.Width

  # parsnip
  ps_cl = decision_tree() %>%
    set_engine("rpart") %>%
    set_mode("classification") %>%
    fit(fml_cl, data = iris)
  expect_equal(pt_cl_known, parttree(ps_cl))

  # workflows
  wf_spec_cl = decision_tree() %>% set_mode("classification")
  wf_tree_cl = workflow(fml_cl, spec = wf_spec_cl)
  wf_cl = fit(wf_tree_cl, iris)
  expect_equal(pt_cl_known, parttree(wf_cl))
}

#
# Regression
#

# Comparison data
source('known_output/parttree_rpart_regression.R')

if (require(tidymodels)) {
  fml_reg = Sepal.Length ~ Petal.Length + Sepal.Width

  # parsnip
  ps_reg = decision_tree() %>%
    set_engine("rpart") %>%
    set_mode("regression") %>%
    fit(fml_reg, data = iris)
  expect_equal(pt_reg_known, parttree(ps_reg), tolerance = 1e-7)

  # workflows
  wf_spec_reg = decision_tree() %>% set_mode("regression")
  wf_tree_reg = workflow(fml_reg, spec = wf_spec_reg)
  wf_reg = fit(wf_tree_reg, iris)
  expect_equal(pt_reg_known, parttree(wf_reg), tolerance = 1e-7)
}
