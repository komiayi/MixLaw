

normdist <- melange_normal(c(0.2,0.8),c(.1,.8))

set.seed(1)

normrdist <- rand(normdist, n=10000)


test_that("melange_normal", {

  expect_identical(round(mean(normrdist),1), 0.5*0.2+0.5*0.8)
  expect_error(melange_normal(c(0.2,2),c(-2,.8)))
  expect_equal(normdist$Poids,rep(0.5, length(normdist$Poids)))
  expect_s3_class(normdist, "melange_normal")
  expect_s3_class(normdist, "melange_dist")
})
