#' @title Reconstitution des épisodes de soins
#'
#' @description La fonction `combine_epi_soins` permet de combiner et fusionner les épisodes de soins dont les périodes se chevauchent dans le temps.
#'
#' @param dt Data d'analyse.
#' @param id Nom de la colonne indiquant l'identifiant de l'individu.
#' @param debut Nom de la colonne indiquant le début de la période.
#' @param fin Nom de la colonne indiquant la fin de la période.
#' @param par_cols Nom des autres colonnes qui doivent être incluses dans l'analyse. Par exemple des codes de médicaments ou des conditions médicales.
#' Afin de pouvoir utiliser le paramètre `par_cols`, la base de données doit être sous forme long en fonction des ids et des variables incluses dans `par_cols`.Par défaut `NULL`.
#' @param njours Nombre de jours max entre le début et la fin précédente pour effectuer une combinaison.
#'
#' @import data.table
#' @importFrom lubridate as_date
#'
#' @encoding UTF-8
#' @export

#' @examples
#'  \dontrun{
#'  # DT <- data.table(
#   ID = c(rep(1, 3), rep(2, 2)),
#   DIN = c(rep(555, 3), rep(999, 2)),
#   DENOM = c(rep(159, 3), rep(357, 2)),
#   DatAdm = as.Date(c("2022-03-01","2022-03-15","2022-03-19",
#                      "2022-06-01", "2022-06-15")),
#   DatDep = as.Date(c("2022-03-25","2022-03-17","2022-03-30",
#                      "2022-06-15", "2022-06-19"))
# )
# Hospit<-combine_epi_soins (dt=DT, id='ID', debut='DatAdm', fin='DatDep', par_cols = 'condition', njours = 1)
#'}

combine_epi_soins <- function(dt, id, debut, fin, par_cols = NULL, njours = 1) {
  ############################# #
  # Serait inclut dans le package inesss
  rmNA <- function(x) {
    if (anyNA(x)) {
      return(x[!is.na(x)])
    } else {
      return(x)
    }
  }
  ############################ #

  # Convertir data.table au besoin
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }

  # Sélectionner et renommer les colonnes nécessaires
  if (is.null(par_cols)) {
    dt <- dt[, c(id, debut, fin), with = FALSE]
  } else {
    dt <- dt[, c(id, par_cols, debut, fin), with = FALSE]
  }
  setnames(dt, c(id, debut, fin), c("id", "debut", "fin"))

  # Trier en ordre croissant les périodes
  if (is.null(par_cols)) {
    setorder(dt, id, debut, fin)
  } else {
    setorderv(dt, c("id", par_cols, "debut", "fin"))
  }

  # Combiner les périodes qui se chevauchent
  if (is.null(par_cols)) {
    dt[, diff := as.integer(debut - shift(fin)), .(id)]  # nombre de jours entre début{i} et fin{i-1}
    idx <- rmNA(dt[, .I[diff <= njours]])  # vérifier si une combinaison des périodes doit être faite
    while (length(idx)) {
      dt[is.na(diff), diff := 0L]
      dt[, per := 0L][diff > 1, per := 1L]  # 0=même période, 1=changement de période
      dt[, per := cumsum(per) + 1L, .(id)]  # numéroter les périodes de 1 à N
      dt <- dt[  # combiner les mêmes périodes en conservant le min et le max
        , .(debut = min(debut),
            fin = max(fin)),
        .(id, per)
      ][, per := NULL]  # supprimer la colonne

      # Vérifier si on doit refaire l'algorithme
      dt[, diff := as.integer(debut - shift(fin)), .(id)]  # nombre de jours entre début{i} et fin{i-1}
      idx <- rmNA(dt[, .I[diff <= njours]])  # vérifier si une combinaison des périodes doit être faite
    }
  } else {
    dt[, diff := as.integer(debut - shift(fin)), by = c("id", par_cols)]  # nombre de jours entre début{i} et fin{i-1}
    idx <- rmNA(dt[, .I[diff <= njours]])  # vérifier si une combinaison des périodes doit être faite
    while (length(idx)) {  # appliquer l'algorithme à chaque fois qu'il y a au moins un diff <= njours
      dt[is.na(diff), diff := 0L]
      dt[, per := 0L][diff > 1, per := 1L]  # 0=même période, 1=changement de période
      dt[, per := cumsum(per) + 1L, by = c("id", par_cols)]  # numéroter les périodes de 1 à N
      dt <- dt[  # combiner les mêmes périodes en conservant le min et le max
        , .(debut = min(debut),
            fin = max(fin)),
        by = c("id", par_cols, "per")
      ][, per := NULL]  # supprimer la colonne

      # Vérifier si on doit refaire l'algorithme
      dt[, diff := as.integer(debut - shift(fin)), by = c("id", par_cols)]  # nombre de jours entre début{i} et fin{i-1}
      idx <- rmNA(dt[, .I[diff <= njours]])  # vérifier si une combinaison des périodes doit être faite
    }
  }

  # Arranger la table finale
  dt[, diff := NULL]
  setnames(dt, c("id", "debut", "fin"), c(id, debut, fin))

  return(dt)

}



