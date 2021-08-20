## Comparison data
source('known_output/parttree_ctree_classification.R')

## partykit
if (require(partykit)) {
  ct = ctree(Species ~ Petal.Length + Petal.Width,
             control = ctree_control(maxdepth = 2),
             data = iris)
  ct = parttree(ct)
  row.names(ct) = NULL
  expect_equal(ct, pt_ct_cl_known)
}
