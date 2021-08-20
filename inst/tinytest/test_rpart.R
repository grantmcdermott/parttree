#
# Classification
#

# Comparison data
source('known_output/parttree_rpart_classification.R')

# rpart
rp = rpart::rpart(Species ~ Petal.Length + Petal.Width, data = iris)
expect_equal(pt_cl_known, parttree(rp))

# partykit
if (require(partykit)) {
  rp2 = as.party(rp)
  rp2 = parttree(rp2)
  row.names(rp2) = NULL
  expect_equal(pt_cl_known[, setdiff(names(pt_cl_known), 'node')],
               rp2[, setdiff(names(rp2), 'node')])
}


#
# flipaxes
#

# Comparison data
source('known_output/parttree_rpart_classification_flip.R')
expect_equal(pt_cl_known_flip, parttree(rp, flipaxes = TRUE))


#
# Regression
#

# Comparison data
source('known_output/parttree_rpart_regression.R')

rp = rpart::rpart(Sepal.Length ~ Petal.Length + Sepal.Width, data = iris)
expect_equal(pt_reg_known, parttree(rp), tolerance = 1e-7)
