#' Moyenne arithmétique
#'
#' Moyenne des observations issus d'un objet de classe \code{melange_dist}.
#'
#' @param u objet de classe \code{melange_dist}.
#'
#' @param \dots arguments supplémentaires.
#'
#' @return Moyenne des réalisations.
#'
#' @examples
#' M <- melange_dist(list(rnorm, rexp, \(n) rgamma(n, 10)), c(0.1, 0.2, 0.7))
#' mean(M)
#' N <- melange_normal(c(1,.2),c(.1,.8))
#' mean(N)
#'
#'
#' @export
mean.melange_dist <- function(x,...){
  mean(rand(x))
}

