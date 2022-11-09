#' Mélange de lois normales
#'
#'  Classe  \code{S3} d'un objet de mélange de lois normales. Les méthodes de la classe \code{melange_dist}
#'  sont aussi applicables pour cette classe.
#'
#' @param moy vecteur de moyennes.
#'
#' @param ect vecteur d'écart-type.
#'
#' @param prob vecteur de poids du mélange.
#'
#' @param \dots arguments supplémentaires.
#'
#' @return Vecteurs de moyenne, d'écart-type et poids normalisés des lois normales à mélanger
#'  et la classe \code{melange_normal}.
#'
#'
#' @examples
#' melange_normal(c(1,1),c(.1,.8))
#' melange_normal( c(3, 4),c(2,2),prob= c(.2,.6),"Mélange de deux exponentielles")
#'
#' @export
melange_normal <- function(moy = vector(), ect = vector(), prob=NULL,...){

  argsList <- as.list(sys.call()[-c(1,2,3)])

  stopifnot(all(ect>0))

  if (!is.vector(moy)) {
    stop( " moy doit \\u00eatre un vecteur", call. = FALSE )
  }

  if (!is.vector(ect)) {
    stop( " ect doit \\u00eatre un vecteur", call. = FALSE )
  }

  if (!is.numeric(moy)) {
    stop( "moy doit \\u00eatre un vecteur num\\u00e9rique", call. = FALSE )
  }

  if (!is.numeric(ect)) {
    stop( "ect doit \\u00eatre un vecteur num\\u00e9rique", call. = FALSE )
  }

  if (length(moy) != length(ect)) {
    stop( "Longueur des moyennes doit \\u00eatre \\u00e9gal \\u00e0 la longeur des \\u00e9cart-types",
          call. = FALSE )
  }

  if (is.null(prob))
  {
    poids <- rep(1,length(moy))/length(moy)
  }else{ poids <- prob/sum(prob) }

  structure(list(Lois=lapply(1:length(moy), FUN= function(x){
                            formals(rnorm)$mean = moy[x]
                            formals(rnorm)$sd = ect[x]
                            rnorm}),
                 Moy = moy, Ect = ect, Poids = poids, Autres = argsList),
                 class=c("melange_normal","melange_dist")
  )

}





