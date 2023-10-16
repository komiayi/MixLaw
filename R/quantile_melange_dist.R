#' Quantile
#'
#' Calcule le quantile des observations issus d'un objet de classe \code{melange_dist}.
#'
#' @param x objet de classe \code{melange_dist}.
#' @param probs vecteur numérique des probabilités.
#' @param \dots arguments supplémentaires.
#'
#' @return Quantile des réalisations.
#'
#' @examples
#' M <- melange_dist(list(rnorm, rexp, \(n) rgamma(n, 10)), c(0.1, 0.2, 0.7))
#' quantile(M)
#' N <- melange_normal(c(5,.2),c(.1,.8))
#' quantile(N)
#'
#' @export
quantile.melange_dist <- function(x, probs = seq(0, 1, 0.25),...){
  quantile(rand(x), probs = probs)
}

