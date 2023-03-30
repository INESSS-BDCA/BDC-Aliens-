#' Utils
#'
#' FROM clause de SQL
#'
#' @param bd Nom de la base de données. Souvent `PROD`.
#' @param vue Nom de la vue.
#'
#' @encoding UTF-8
#' @return "from `bd`.`vue`"
#' @keywords internal
from_bd.vue <- function(bd = "PROD", vue) {
  ### Provenance des données
  return(paste0("from ",bd,".",vue))
}


#' Utils
#'
#' Indentation du code
#'
#' @param niv Niveau d'indentation.
#'
#' @encoding UTF-8
#' @return Quatre (4) espaces répétés `niv` fois.
#' @keywords internal
indent <- function(niv = 1) {
  if (niv < 0) {
    stop("indent() : niv doit être plus grand ou égal à zéro.")
  } else if (niv == "select") {
    return("       ")
  } else if (niv == 0) {
    return("")
  } else {
    return(paste0(rep("    ", niv), collapse = ""))
  }
}


#' Utils
#'
#' Converti un vecteur R en code SQL
#'
#' @param x Vecteur
#' @keywords internal
#' @encoding UTF-8
#' @return c("x", "y") -> "'x','y'"
#' @export
qu <- function(x) {
  return(paste(paste0("'",x,"'"), collapse = ", "))
}


#' Utils
#'
#' supprimer les NA
#'
#' @param x data table
#' @keywords internal
#' @encoding UTF-8
#' @return data table
#' @export
rmNA <- function(x) {
  if (anyNA(x)) {
    return(x[!is.na(x)])
  } else {
    return(x)
  }
}

#' Astuce
#'
#' Combinaison de `sort()` et `unique()`.
#'
#' @param x Vecteur à trier et supprimer doublons.
#' @param decreasing Ordre décroissant = `TRUE`, sinon `FALSE`.
#' @param na.last Afficher les `NA` à la fin = `TRUE`, sinon `FALSE`. `NA` n'affiche pas les valeurs `NA`.
#'
#' @encoding UTF-8
#' @export
#' @examples
#' x <- sample(c(1:10, NA, NaN))
#' x
#'
#' sunique(x)
#' sunique(x, na.last = TRUE)
#' sunique(x, decreasing = TRUE, na.last = NA)
sunique <- function(x, decreasing = FALSE, na.last = FALSE) {

  return(sort(unique(x), decreasing = decreasing, na.last = na.last))

}


#' Astuce
#'
#' Remplace les `NA`s dans un tableau par `by`.
#'
#' @param dt Tableau contenant des `NA`s.
#' @param by Valeur de remplacement.
#'
#' @encoding UTF-8
#' @export
replace_NA_in_dt <- function(dt, by) {

  if (!data.table::is.data.table(dt)) {
    dt <- data.table::as.data.table(dt)
  }

  if (is.character(by)) {
    to_char <- TRUE
  } else {
    to_char <- FALSE
  }

  for (j in 1:ncol(dt)) {
    if (to_char && !is.character(dt[[j]])) {
      dt[[j]] <- as.character(dt[[j]])
    }
    data.table::set(dt, which(is.na(dt[[j]])), j, by)
  }

  return(dt)

}
