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
#' @importFrom kableExtra kbl kable_classic add_header_above '%>%'
#'
#' @export
print.melange_dist <- function(x,...){

  if(class(x)[1]== "melange_dist"){
    tab <- t(x$Poids)
    colnames(tab) <- as.character(x$ois[[1]][-1])
    rownames(tab) <- "Poids"
    kbl(tab, align = "c") %>%
      kable_classic( full_width = F) %>%
      add_header_above(c(" ","Lois" =ncol(tab)))
  }else if(class(x)[1]== "melange_normal"){
    tab <- rbind(x$Moy,x$Ect,x$Poids)
    rownames(tab) <- c("Moyenne","Ecart-type","Poids")
    kbl(round(tab,3), align = "c") %>%
      kable_classic( full_width = F) %>%
      add_header_above(c(" ","Lois normales" =ncol(tab)))
  }
}


