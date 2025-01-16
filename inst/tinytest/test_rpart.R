# For tinysnapshot
source("helpers.R")
using("tinysnapshot")

#
# Classification
#

# Comparison data
source('known_output/parttree_rpart_classification.R')

# rpart
rp = rpart::rpart(Species ~ Petal.Length + Petal.Width, data = iris)
rp_pt = parttree(rp)

# plot method
f = function() {plot(rp_pt)}
expect_snapshot_plot(f, label = "iris_classification")

# ## uncomment once tinyplot 0.3.0 comes out
# f = function() {plot(rp_pt, flip = TRUE)}
# expect_snapshot_plot(f, label = "iris_classification_flip")

# now strip attributes and compare data frames
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

# plot method
f = function() {plot(rp_pt_flip)}
expect_snapshot_plot(f, label = "iris_classification_flip")

# now strip attributes and compare data frames
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
# plot method
f = function() {plot(rp_reg_pt)}
expect_snapshot_plot(f, label = "iris_regression")
# now strip attributes and compare data frames
attr(rp_reg_pt, "parttree") = NULL
class(rp_reg_pt) = "data.frame"
expect_equal(pt_reg_known, rp_reg_pt, tolerance = 1e-7)

