#
# Classification
#

# Comparison data
source('known_output/parttree_rpart_classification.R')

# rpart
rp = rpart::rpart(Species ~ Petal.Length + Petal.Width, data = iris)
rp_pt = parttree(rp)
attr(rp_pt, "parttree") = NULL
class(rp_pt) = "data.frame"
expect_equal(pt_cl_known, rp_pt)

# partykit
if (require(partykit)) {
  rp2 = as.party(rp)
  rp2_pt = parttree(rp2)
  row.names(rp2_pt) = NULL
  attr(rp2_pt, "parttree") = NULL
  class(rp2_pt) = "data.frame"
  expect_equal(pt_cl_known[, setdiff(names(pt_cl_known), 'node')],
               rp2_pt[, setdiff(names(rp2_pt), 'node')])
}


#
# flip
#

# Comparison data
source('known_output/parttree_rpart_classification_flip.R')
rp_pt_flip = parttree(rp, flip = TRUE)
attr(rp_pt_flip, "parttree") = NULL
class(rp_pt_flip) = "data.frame"
expect_equal(pt_cl_known_flip, rp_pt_flip)


#
# Regression
#

# Comparison data
source('known_output/parttree_rpart_regression.R')

rp_reg = rpart::rpart(Sepal.Length ~ Petal.Length + Sepal.Width, data = iris)
rp_reg_pt = parttree(rp_reg)
attr(rp_reg_pt, "parttree") = NULL
class(rp_reg_pt) = "data.frame"
expect_equal(pt_reg_known, rp_reg_pt, tolerance = 1e-7)
