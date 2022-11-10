#' Affichage informations
#'
#' Affiche un tableau qui renseigne sur les informations d'une instance de classe \code{melange_dist}.
#'
#' @param x objet de classe \code{melange_dist}.
#'
#' @param \dots arguments supplémentaires.
#'
#' @return Les informations sur l'instance sur laquelle elle est appliquée.
#'
#' @examples
#' M <- melange_dist(list(rnorm, rexp, \(n) rgamma(n, 10)), c(0.1, 0.2, 0.7))
#' print(M)
#' N <- melange_normal(c(1,.2,4),c(.1,.8,8))
#' print(N)
#'
#' @export
print.melange_dist <- function(x,...){

  if(class(x)[1]== "melange_dist"){
    cat("\n",...)
    cat("Informations sur les lois \\u00e0 m\\u00e9langer: ","\n",...)
    tab <- round(t(x$Poids),3)
    colnames(tab) <- as.character(x$ois[[1]][-1])
    rownames(tab) <- "Poids"
  }else if(class(x)[1]== "melange_normal"){
    cat("\n",...)
    cat("Informations sur les lois normales \\u00e0 m\\u00e9langer: ","\n",...)
    tab <- round(rbind(x$Moy,x$Ect,x$Poids),3)
    rownames(tab) <- c("Moyenne","Ecart-type","Poids")
  }
  print(tab)
  cat("\n",...)
  cat("class:","\n")
  print(class(x))

}


