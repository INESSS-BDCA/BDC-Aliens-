#' Astuce
#'
#' Calcul des indicateurs de *Charlson*, *Elixhauser* et la combinaison des deux.
#'
#' \strong{\code{confirm_sourc} :} Dans l'exemple `confirm_sourc = list(source1=1, source2=2, source3=2, ...)`, la `source3` pourrait confirmer la `source2` et vice-versa.\cr\cr
#' \strong{\code{Dx_table} :} Il n'est pas nécessaire d'inclure un pourcentage à la fin de chaque code. Le programme le fait automatiquement s'il n'y en a pas. Par exemple inscrire le code `I25` et `I25%` revient au même.
#'
#' @param dt Dataset ayant au moins les quatre (4) colonnes `ID`, `DIAGN`, `DATE_DX` et `SOURCE`.
#' @param ID Nom de la colonne indiquant le numéro de l'usager, de l'individu.
#' @param DIAGN Nom de la colonne indiquant le code d'un diagnostic.\cr Voir `names(inesss::Comorbidity_diagn_codes)`.
#' @param DATE_DX Nom de la colonne indiquant la date du diagnostic.
#' @param SOURCE Nom de la colonne indiquant la provenance du diagnostic.
#' @param n1,n2 Nombre de jours dans le but de construire l'intervalle `[n1,n2]`. Pour qu'un code de diagnostic soit confirmé, il faut que *DIAGN{i}* soit suivi de *DIAGN{j}* (où i < j) et que le nombre de jours entre les deux soit dans l'intervalle `[n1,n2]`.
#' @param Dx_table `list` personnelle contenant les codes de diagnostics ou nom du dataset (de la librairie *inesss*) contenant la liste des codes de diagnostics à l'étude.
#' * `'Combine_Dx_CCI_INSPQ18'`
#' * `'Charlson_Dx_CCI_INSPQ18'`
#' * `'Elixhauser_Dx_CCI_INSPQ18'`
#' * `'Charlson_Dx_UManitoba16'`
#' @param scores Nom de la table à utiliser pour le calcul des indicateurs. Voir les éléments de la liste `ComorbidityWeights`.
#' * `'CCI_INSPQ_2018_CIM9'`
#' * `'CCI_INSPQ_2018_CIM10'`
#' * `'UManitoba_2016'`
#' @param confirm_sourc `list` indiquant la *confiance* des `SOURCE`. Si une `SOURCE` doit être confirmée par une autre dans l'intervalle `[n1,n2]`, inscrire `2`, sinon `1`. Inscrire les sources sous le format : `confirm_sourc = list(source1 = 1, source2 = 2, source3 = 2, ...)`. `confirm_sourc` doit contenir toutes les valeurs uniques de la colonne `SOURCE`.
#' @param exclu_diagn Vecteur contenant le nom du ou des diagnostics à exclure de l'analyse. Voir la liste de `Dx_table` pour connaître les valeurs permises.
#' @param keep_confirm_data `TRUE` ou `FALSE`. Place en attribut (voir fonction `base::attributes`) le data `confirm_data` qui indique la date de repérage et la date de confirmation d'un diagnostic.
#'
#' @return `data.table`
#' @import data.table
#' @encoding UTF-8
#' @export
comorbidity <- function(
  dt, ID, DIAGN, DATE_DX, SOURCE,
  n1 = 30, n2 = 730,
  Dx_table = 'Comorbidity_Dx_CCI_INSPQ18', scores = 'CCI_INSPQ_2018_CIM10',
  confirm_sourc = list(MEDECHO = 1, BDCU = 2, SMOD = 2),
  exclu_diagn = NULL,
  keep_confirm_data = FALSE
) {

  ### Arranger dataset à analyser
  if (!is.data.table(dt)) {  # convertir data.table au besoin
    dt <- as.data.table(dt)
  }
  dt <- dt[, c(ID, DIAGN, DATE_DX, SOURCE), with = FALSE]  # sélection des colonnes
  setnames(dt, names(dt), c("ID", "DIAGN", "DATE_DX", "SOURCE"))  # renommer les colonnes
  # DATE_DX est une date
  if (!lubridate::is.Date(dt$DATE_DX)) {
    dt[, DATE_DX := lubridate::as_date(DATE_DX)]  # convertir au format date au besoin
    if (anyNA(dt$DATE_DX)) {  # erreur s'il y a des NA après convertion
      stop("DATE_DX contient au moins une valeur qui n'est pas une date au format 'AAAA-MM-JJ'.")
    }
  }
  dt[, DATE_DX := as.integer(DATE_DX)]  # convertir en integer -> memory efficient

  ### Confirmation des codes de diagnostiques
  dt <- comorbidity.confirm_diagn(dt, n1, n2, confirm_sourc)
  if (keep_confirm_data) {  # conserver ce data à mettre en attribut
    confirm_data <- copy(dt)
  }

  ### Ajouter le score aux diagn
  dt <- inesss::ComorbidityWeights[[scores]][, .(DIAGN = DIAGN_CODE, POIDS)][dt, on = .(DIAGN)]

  ### Compter le nombre de Dx par ID
  Dx_par_id <- dt[, .(nDx = .N), .(ID)]

  ### Mettre une colonne par diagn
  dt <- dcast(dt, ID ~ DIAGN, value.var = "POIDS")
  dt <- replace_NA_in_dt(dt, 0L)

  ### Calculer les scores
  dt <- comorbidity.scores(dt, Dx_table, exclu_diagn)

  ### Ajouter le nombre de Dx par ID
  dt <- Dx_par_id[dt, on = .(ID)]

  ### Information dans les attributs
  attr(dt, "infos") <- list(
    CreateDate = Sys.Date(),  # date de création
    Dx_table = Dx_table,  # table utilisée
    Scores = scores,  # score utilisé
    ConfirmSources = confirm_sourc,  # confiance des sources
    nJours = sort(c(n1, n2))  # jours utilisés pour l'intervalle de confirmation
  )
  if (keep_confirm_data) {  # ajouter le data des confirmations de diagn en attribut
    attr(dt, "confirm_data") <- confirm_data
  }

  return(dt)

}

#' @title comorbidity
#' @import data.table
#' @encoding UTF-8
#' @keywords internal
#' @return `data.table` de 6 colonnes :
#' * ID : Numéro d'identification de l'usager.
#' * DIAGN : Code de diagnostique.
#' * DATE_REP : Première date de diagnostique confirmée.
#' * SOURCE_REP : Provenance de `DATE_REP`.
#' * DATE_CONF : Date qui confirme `DATE_REP`.
#' * SOURCE_CONF : Provenance de `SOURCE_CONF`.
comorbidity.confirm_diagn <- function(dt, n1, n2, confirm_sourc) {
  ### Confirmation des codes s'il y a au moins un 2e code qui le suit dans
  ### l'intervale [n1, n2].

  if (n1 > n2) {  # safe
    n1_copy <- n1
    n1 <- n2
    n2 <- n1_copy
  }

  ### Trier les données selon l'importance des sources
  for (desc in names(confirm_sourc)) {
    dt[SOURCE == desc, tri := confirm_sourc[[desc]]]
  }
  setkey(dt, ID, DIAGN, DATE_DX, tri, SOURCE)
  dt[, tri := NULL]

  ### Conserver un data pour les informations
  infos <- unique(dt[, .(ID, DIAGN, DATE_DX, SOURCE)])
  infos <- infos[, .SD[1], .(ID, DIAGN, DATE_DX)]

  ### Confirmer les diagn
  idx <- rmNA(dt[, .I[.N > 1], .(ID, DIAGN)]$V1)  # no lignes ayant 2 obs ou plus
  if (length(idx)) {
    dt[  # nombres de jours entre DATE_DX et la valeur qui la précède
      idx,
      diff := DATE_DX - shift(DATE_DX),
      .(ID, DIAGN)
    ][is.na(diff), diff := 0L]
  } else {
    dt[, diff := 0L]
  }
  dt[  # somme cumulative de diff qui recommence à 0 si dépasse n2
    , cdiff := Reduce(function (x, y) {
      z <- x + y
      if (z > n2) return(0L) else return(z)
    }, x = diff, accumulate = TRUE),
    .(ID, DIAGN)
  ]
  # Indiquer si les dates confirment une date précédente
  dt[, `:=` (diff_conf = FALSE, cdiff_conf = FALSE)]
  sourc1 <- comorbidity.confirm_sourc_names(confirm_sourc, 1)  # sources n'ayant pas besoin de confirmation
  if (length(sourc1)) {
    dt[  # confirmation automatique -> TRUE
      SOURCE %in% sourc1,
      `:=` (diff_conf = TRUE,
            cdiff_conf = TRUE)
    ]
  }
  sourc2 <- comorbidity.confirm_sourc_names(confirm_sourc, 2)  # sources ayant besoin d'une confirmation dans le future [n1, n2]
  if (length(sourc2)) {
    dt[  # vérifier le nombre de jours à partir de diff
      SOURCE %in% sourc2 &
        n1 <= diff & diff <= n2,
      diff_conf := TRUE
    ][  # vérifier le nombre de jours à partir de cdiff
      SOURCE %in% sourc2 &
        n1 <= cdiff & cdiff <= n2,
      cdiff_conf := TRUE
    ]
  }
  # Conserver les confirmations de diagnostiques seulement
  dt <- dt[diff_conf == TRUE | cdiff_conf == TRUE]
  dt <- dt[, .SD[1], .(ID, DIAGN)]  # conserver la première confirmation seulement
  # Indiquer le nombre de jours à conserver entre diff ou cdiff pour retrouver la date à utiliser
  dt[
    diff_conf == TRUE & cdiff_conf == TRUE,
    njours := min(diff, cdiff),
    .(ID, DIAGN)
  ]
  dt[
    diff_conf == TRUE & cdiff_conf == FALSE,
    njours := diff
  ]
  dt[
    diff_conf == FALSE & cdiff_conf == TRUE,
    njours := cdiff
  ]
  dt <- dt[, .(ID, DIAGN, DATE_CONF = DATE_DX, SOURCE_CONF = SOURCE, DATE_REP = DATE_DX - njours)]

  ### Arranger le dataset pour présentation finale
  infos <- infos[ID %in% dt$ID]  # conserver les ID existant dans dt
  dt <- infos[, .(ID, DIAGN, DATE_REP = DATE_DX, SOURCE_REP = SOURCE)][dt, on = .(ID, DIAGN, DATE_REP)]

  ### Dates au format AAAA-MM-JJ
  dt[, `:=` (DATE_REP = lubridate::as_date(DATE_REP),
             DATE_CONF = lubridate::as_date(DATE_CONF))]

  return(dt)

}
comorbidity.confirm_sourc_names <- function(confirm_sourc, val) {
  ### Indique le ou les noms des confirm_sourc qui ont la valeur 'val'.

  return(unlist(lapply(names(confirm_sourc), function(x) {
    if (confirm_sourc[[x]] == val) {
      return(x)
    } else {
      return(NULL)
    }
  })))

}
#' @import data.table
#' @encoding UTF-8
#' @keywords internal
comorbidity.scores <- function(dt, Dx_table, exclu_diagn) {
  Dx_table_name <- Dx_table
  Dx_table <- SQL_comorbidity_diagn.select_Dx_table(Dx_table)

  ### Ajouter les colonnes manquantes - sauf exclusions
  cols <- names(Dx_table)
  if (!is.null(exclu_diagn)) {
    cols <- cols[!cols %in% exclu_diagn]
  }
  for (col in cols) {
    if (!col %in% names(dt)) {
      dt[, (col) := 0L]
    }
  }

  ### Calcul du score
  if (Dx_table_name == "Combine_Dx_CCI_INSPQ18") {
    charl_cols <- names(inesss::Charlson_Dx_CCI_INSPQ18)
    elix_cols <- names(inesss::Elixhauser_Dx_CCI_INSPQ18)
    comb_cols <- names(inesss::Combine_Dx_CCI_INSPQ18)
    if (!is.null(exclu_diagn)) {
      charl_cols <- charl_cols[!charl_cols %in% exclu_diagn]
      elix_cols <- elix_cols[!elix_cols %in% exclu_diagn]
      comb_cols <- comb_cols[!comb_cols %in% exclu_diagn]
    }
    dt[, Charlson_Dx_CCI_INSPQ18 := rowSums(dt[, charl_cols, with = FALSE])]
    dt[, Elixhauser_Dx_CCI_INSPQ18 := rowSums(dt[, elix_cols, with = FALSE])]
    dt[, Combine_Dx_CCI_INSPQ18 := rowSums(dt[, comb_cols, with = FALSE])]
    setcolorder(dt, c("ID", "Combine_Dx_CCI_INSPQ18", "Charlson_Dx_CCI_INSPQ18", "Elixhauser_Dx_CCI_INSPQ18", cols))
  } else {
    score_cols <- names(Dx_table)
    if (is.null(exclu_diagn)) {
      score_cols <- score_cols[!score_cols %in% exclu_diagn]
    }
    dt[, (Dx_table_name) := rowSums(dt[, score_cols, with = FALSE])]
    setcolorder(dt, c("ID", Dx_table_name, cols))
  }

  return(dt)
}
