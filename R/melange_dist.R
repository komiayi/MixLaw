#' Mélange de distributions
#'
#' Classe \code{S3} d'un objet de mélange de lois.
#'
#' @param x liste de lois. Chacune doit être une fonction d’un seul argument,\code{n},
#' permettant de générer \code{n} réalisations d’une variable aléatoire
#'
#' @param prob poids du mélange.
#'
#' @param \dots arguments supplémentaires.
#'
#' @return Liste des lois à mélanger avec leurs poids de mélanges respectives normalisés et la classe \code{melange_dist}.
#'
#' @examples
#' melange_dist(list(rnorm, rexp, \(n) rgamma(n, 10)), c(0.1, 0.2, 0.7))
#' melange_dist(list(\(n) rnorm(n, 10, 5), \(n) rnorm(n, 3, 2)), c(3, 4))
#' melange_dist(list(\(n) rexp(n, 10), \(n) rexp(n, 20)), c(3, 4), "Mélange de deux exponentielles")
#'
#' @export
melange_dist <- function(x = list(), prob = vector(),...){

  argsList <- as.list(sys.call()[-c(1,2,3)])

  if (!is.list(x)) {
    stop( " x doit \\u00eatre une liste de fonctions", call. = FALSE )
  }

  if (!is.vector(prob)) {
    stop( " prob doit \\u00eatre un vecteur", call. = FALSE )
  }

  if (!is.numeric(prob)) {
    stop( "prob doit \\u00eatre un vecteur num\\u00e9rique", call. = FALSE )
  }

  if (length(x) != length(prob)) {
    stop( "Longueur du vecteur de poids est diff\\u00e9rents de la longeur de la liste de
          fonctions  ", call. = FALSE )
  }

  structure(list( Lois = x, Poids = prob/sum(prob), Autres = argsList, ois = as.list(sys.call()[2])),
            class=c("melange_dist")
            )
}
