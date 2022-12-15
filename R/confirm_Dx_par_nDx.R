#' confirm_Dx_par_nDx
#'
#' confirm_Dx_par_nDx indique la liste des diagnostics qui sont confirmés par `nDx` dans un intervalle de \[`n1`, `n2`\] jours. La fonction confirm_Dx_par_nDx est utilisée par la fonction \code{\link{SQL_reperage_cond_med}}
#'
#' @param dt Table d'analyse.
#' @param ID Nom de la colonne indiquant le numéro de l'usager, de l'individu.
#' @param DIAGN Nom de la colonne indiquant le numéro de l'usager, de l'individu.
#' @param DATE_DX Nom de la colonne indiquant la date du diagnostic.
#' @param nDx Nombre de diagnostics qui doivent confirmer une date précédente.
#' @param n1,n2 Nombre de jours requis pour confirmer le diagnostic
#' @param keep_all Afficher toutes les dates qui sont suivies de `nDx` date(s) dans un intervalle de \[`n1`, `n2`\] jours (`TRUE`), ou conserver seulement la première date (`FALSE`).
#'
#' @import data.table
#' @importFrom lubridate as_date
#'
#' @return `data.table`
#' @encoding UTF-8
#' @export
confirm_Dx_par_nDx <- function(
    dt,
    ID, DIAGN, DATE_DX,
    nDx = 1,
    n1 = 30, n2 = 730,
    keep_all = FALSE
) {

  # Arranger dataset
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)  # convertir data.table au besoin
  }
  dt <- dt[, c(ID, DIAGN, DATE_DX), with = FALSE]  # conserver les colonnes nécessaires
  setnames(dt, names(dt), c("ID", "DIAGN", "DATE_DX"))  # renommer les colonnes pour avoir "ID" et "DIAGN"
  setkey(dt, ID, DIAGN, DATE_DX)
  dt[, DATE_DX := as.integer(DATE_DX)]  # convertir date en integer = plus rapide
  dt <- dt[dt[, .I[.N > nDx], .(ID, DIAGN)]$V1]  # supprimer les Dx qui n'ont pas assez d'obs pour être confirmé par nDx Dx
  if (!nrow(dt)) {
    message(paste0("Il n'y a pas assez d'observations par diagnostic pour permettre une analyse avec nDx=",nDx,"."))
    return(NULL)
  }
  nloop <- max(dt[, .N, .(ID, DIAGN)]$N) - nDx  # nombre de loop nécessaire au maximum
  DT_final <- vector("list", nloop)  # table résultante

  for (i in 1:nloop) {
    dt_confirm <- copy(dt)
    dt_confirm[, diff := DATE_DX - shift(DATE_DX), .(ID, DIAGN)][is.na(diff), diff := 0L]  # nbre jours entre la date et celle précédente
    dt_confirm[, cumul := cumsum(diff), .(ID, DIAGN)]  # nombre de jours entre la date du diagnostic et la date d'analyse
    dt_confirm <- dt_confirm[
      diff == 0  # date d'analyse qui doit être confirmée par d'autres dates
      | (n1 <= cumul & cumul <= n2)  # dates qui confirment la date d'analyse selon l'intervalle [n1,n2]
    ]
    dt_confirm <- dt_confirm[dt_confirm[, .I[.N >= nDx + 1], .(ID, DIAGN)]$V1]  # conserver les dates qui sont confirmées par au moins nDx autres
    if (nrow(dt_confirm)) {
      dt_confirm <- dt_confirm[dt_confirm[, .I[1:(nDx + 1)], .(ID, DIAGN)]$V1]  # conserver les nDx+1 premières obs
      dt_confirm[, DATE_CONF := rank(DATE_DX) - 1, .(ID, DIAGN)]
      dt_confirm <- dcast(dt_confirm, ID + DIAGN ~ DATE_CONF, value.var = "DATE_DX")  # convertir lignes en colonnes
      DT_final[[i]] <- dt_confirm  # save dans table finale
      if (!keep_all) {
        dt <- dt[!dt_confirm[, .(ID, DIAGN)], on = .(ID, DIAGN)]  # supprimer les ID+DIAGN qui sont déjà dans la table finale.
      }
    }
    dt <- dt[!dt[, .I[1], .(ID, DIAGN)]$V1]  # supprimer la 1re ligne de chaque groupe -> on passe à la prochaine date à analyser
    dt <- dt[dt[, .I[.N > nDx], .(ID, DIAGN)]$V1]  # supprimer les Dx qui n'ont pas assez d'obs pour être confirmé par nDx Dx
    if (!nrow(dt)) {
      break  # sortir de la loop s'il n'y a plus de donnéess
    }
    if (i == nloop && nrow(dt)) {
      stop("Erreur dans le nombre d'itérations de la boucle FOR.")  # back up au cas où l'algorithme aurait une erreur
    }
  }

  # Arranger la table finale
  DT_final <- rbindlist(DT_final)
  setkey(DT_final)
  setnames(DT_final, "0", "DATE_REF")  # Date de référence
  DT_final[, DATE_REF := as_date(DATE_REF)]
  for (col in as.character(1:nDx)) {
    DT_final[, (col) := as_date(get(col))]
    setnames(DT_final, col, paste0("DATE_CONF", col))
  }

  return(DT_final)

}
