#' Graphique
#'
#' Affiche la densité des réalisations d'un objet de classe \code{melange_dist}.
#'
#' @param x objet de classe \code{melange_dist}.
#' @param n nombre d'observations tracées.
#' @param type caractère pour désigner le type de tracer
#' @param xlim les limites x (x1, x2) du tracé.
#' @param ylim les limites y du tracé.
#' @param main titre du tracé, see also \code{\link{plot}}.
#' @param xlab label de l'axe des x.
#' @param ylab label de l'axe des y.
#' @param color couleur du tracé.
#'
#' @param \dots arguments supplémentaires.
#'
#' @return Graphique de la densité du mélange de lois.
#'
#' @examples
#' M <- melange_dist(list(rnorm, rexp, \(n) rgamma(n, 10)), c(0.1, 0.2, 0.7))
#' plot(M, color="red")
#' N <- melange_normal(c(1,.2),c(1,.8))
#' plot(N, main="Mélange de lois normales")
#'
#' @importFrom graphics plot
#'
#' @export
plot.melange_dist <- function(x,n=100L,type = "l",  xlim = NULL, ylim = NULL,
                               main = NULL, xlab = NULL,
                              ylab = NULL, color="black",...){

    out <- plot(density(rand(x,n)), type = type, xlim = xlim, ylim = ylim,
          main = main , xlab = xlab, ylab = ylab, col= color)
    invisible(out)
}

