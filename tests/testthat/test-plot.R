mdist <- melange_dist(list(rnorm, rexp, \(n) rgamma(n, 10)), c(0.1, 0.2, 0.7))


normdist <- melange_normal(c(2,0.8,1),c(0.4,0.2,0.5))

plot_mdist <- function(n) {
  file = "mdist.png"
  png(file)
  set.seed(1)
  plot(mdist,n=n, color ="blue")
  dev.off()

  file
}

plot_normdist <- function(n) {
  file = "normdist.png"
  png(file)
  set.seed(1)
  plot(normdist,n=n, color="red")
  dev.off()

  file
}

test_that("plot", {
  set.seed(6)
  expect_snapshot_file(plot_mdist(10000))
  expect_snapshot_file(plot_normdist(100000))
})
