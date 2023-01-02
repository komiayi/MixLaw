
mdist <- melange_dist(list(rnorm, rexp, \(n) rgamma(n, 10)), c(0.1,0.2, 0.5))

rdist <- rand(mdist)


test_that("melange_dist", {

  expect_error(melange_dist(list(\(n) rexp(n, 10), \(n) rexp(n, 20), rnorm), c(3, 4)))
  expect_length(rdist,100L)
  expect_equal(sum(mdist$Poids), 1)
  expect_invisible(plot(mdist))
  expect_s3_class(mdist, "melange_dist")
})
