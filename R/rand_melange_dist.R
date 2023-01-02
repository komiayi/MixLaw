#' Réalisations de mélange de lois
#'
#' Génere des réalisations de lois d'un objet de classe \code{melange_dist}.
#'
#' @param x objet de classe \code{melange_dist}.
#'
#' @param n nombre d'observations.
#'
#' @param \dots arguments supplémentaires.
#'
#'
#' @return \code{n} réalisations du mélange de lois.
#'
#' @examples
#' M <- melange_dist(list(rnorm, rexp, \(n) rgamma(n, 10)), c(0.1, 0.2, 0.7))
#' rand(M)
#'
#' @export
rand <- function(x, ...) UseMethod("rand")
#'
#' @rdname rand
#' @import stats
#' @export
rand.melange_dist <- function(x, n=100L,...){
    out <- replicate(n,{
                         choice <- sample(1:length(x$Lois), size =1L, prob = x$Poids)
                         x$Lois[[choice]](1)
                        },
                      simplify = "array")

   out
}

