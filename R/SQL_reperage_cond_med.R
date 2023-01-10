#' Requête complexe: Repérage condition médicale
#'
#' Repérage d'une condition médicale basé sur les codes de diagnostic cim-9 et cim-10 dans SMOD, MEDECHO-diag, MEDECHO-sej et BDCU.
#'
#' **by_Dx :**\cr
#' Supposons `Dx_table = list(Angoisse = [...], Trouble = [...], Deficience = [...])`. Si `TRUE`, il y aura la colonne `DIAGN` qui indiquera des dates pour chaque élément, soit *Angoisse*, *Trouble* et *Deficience*. Si `FALSE`, la colonne `DIAGN` est absente et l'algorithme est appliqué sur tous les codes de chaque élément. Cela reviendrait à écrire tous les codes sur une même ligne.
#'
#' @inheritParams SQL_diagn
#' @inheritParams confirm_Dx_par_nDx
#' @param by_Dx `TRUE` ou `FALSE`. Distinction entre les diagnostics (`TRUE`) ou pas (`FALSE`). Si `TRUE`, on considère chaque élément de `Dx_table` où chaque élément peut contenir plusieurs codes. Le nombre d'éléments sera donc le nombre maximal d'observations (lignes résultats) par individu.\cr Si `FALSE`, on considère tous les éléments de `Dx_Table` comme un seul, on aura donc au maximum une ligne résultat par individu.\cr Voir Détails.
#' @param nDx Nombre de diagnostics qui doivent confirmer la 1re date. Par défaut 1.
#'
#' @return `data.table` :
#' * `ID` : Identifiant de l'individu.
#' * `DIAGN` : Nom du diagnostic. Seulement si `by_Dx=TRUE`.
#' * `DI_Finale` : Date d'incidence retenue.
#' * `DI_Hospit` : Date d'incidence d'hospitalisation.
#' * `DI_Acte` : Date d'incidence acte.
#' * `DC_Acte` : Date de confirmation de `DI_Acte`.
#' * `D_Recent` : Date du diagnostic la plus récente **sans tenir compte de l'algorithme**.
#' @encoding UTF-8
#' @import data.table
#' @export
#' @examples
#' EX1_default <- SQL_reperage_cond_med(
#'   conn = conn, debut = "2020-01-01", fin = "2020-12-31",
#'   Dx_table = list(
#'     diabete = list(
#'       CIM9 = c("2500%", "2501%", "2502%"),
#'       CIM10 = c("E100%", "E101%", "E109%", "E110%", "E111%", "E119%", "E130%",
#'                 "E131%", "E139%", "E140%", "E141%", "E149%")
#'     ),
#'     diabete_complication = list(
#'       CIM9 = paste0(2503:2509, "%"),
#'       CIM10 = paste0("E", c(102:108, 112:118, 132:138, 142:148), "%")
#'     )
#'   ),
#'   CIM = c("CIM9", "CIM10"), by_Dx = TRUE, date_dx_var = "admis",
#'   n1 = 30, n2 = 730,
#'   nDx = 1
#' )
#' EX2_nDx0 <- SQL_reperage_cond_med(
#'   conn = conn, debut = "2020-01-01", fin = "2020-12-31",
#'   Dx_table = list(
#'     diabete = list(
#'       CIM9 = c("2500%", "2501%", "2502%"),
#'       CIM10 = c("E100%", "E101%", "E109%", "E110%", "E111%", "E119%", "E130%",
#'                 "E131%", "E139%", "E140%", "E141%", "E149%")
#'     ),
#'     diabete_complication = list(
#'       CIM9 = paste0(2503:2509, "%"),
#'       CIM10 = paste0("E", c(102:108, 112:118, 132:138, 142:148), "%")
#'     )
#'   ),
#'   nDx = 0
#' )
#' EX3_nDx2 <- SQL_reperage_cond_med(
#'   conn = conn, debut = "2020-01-01", fin = "2020-12-31",
#'   Dx_table = list(
#'     diabete = list(
#'       CIM9 = c("2500%", "2501%", "2502%"),
#'       CIM10 = c("E100%", "E101%", "E109%", "E110%", "E111%", "E119%", "E130%",
#'                 "E131%", "E139%", "E140%", "E141%", "E149%")
#'     ),
#'     diabete_complication = list(
#'       CIM9 = paste0(2503:2509, "%"),
#'       CIM10 = paste0("E", c(102:108, 112:118, 132:138, 142:148), "%")
#'     )
#'   ),
#'   nDx = 2
#' )
#' EX4_byDxFALSE_nDx0 <- SQL_reperage_cond_med(
#'   conn = conn, debut = "2020-01-01", fin = "2020-12-31",
#'   Dx_table = list(
#'     diabete = list(
#'       CIM9 = c("2500%", "2501%", "2502%"),
#'       CIM10 = c("E100%", "E101%", "E109%", "E110%", "E111%", "E119%", "E130%",
#'                 "E131%", "E139%", "E140%", "E141%", "E149%")
#'     ),
#'     diabete_complication = list(
#'       CIM9 = paste0(2503:2509, "%"),
#'       CIM10 = paste0("E", c(102:108, 112:118, 132:138, 142:148), "%")
#'     )
#'   ),
#'   by_Dx = FALSE, nDx = 0
#' )
#' EX5_byDxFALSE_nDx2 <- SQL_reperage_cond_med(
#'   conn = conn, debut = "2020-01-01", fin = "2020-12-31",
#'   Dx_table = list(
#'     diabete = list(
#'       CIM9 = c("2500%", "2501%", "2502%"),
#'       CIM10 = c("E100%", "E101%", "E109%", "E110%", "E111%", "E119%", "E130%",
#'                 "E131%", "E139%", "E140%", "E141%", "E149%")
#'     ),
#'     diabete_complication = list(
#'       CIM9 = paste0(2503:2509, "%"),
#'       CIM10 = paste0("E", c(102:108, 112:118, 132:138, 142:148), "%")
#'     )
#'   ),
#'   by_Dx = FALSE, nDx = 2
#' )
SQL_reperage_cond_med <- function(
    conn = SQL_connexion(),
    debut,fin,
    Dx_table,
    CIM = c("CIM9", "CIM10"),
    nDx = 1,
    by_Dx = TRUE,
    date_dx_var = "admis",
    n1 = 30, n2 = 730,code_stat_decis=code_stat_decis,
    ...
) {

  ### Arguments qui sont peut-etre dans <...> : les creer si ce n'est pas le cas
  dotargs <- list(...)
  if (any(names(dotargs) == "keep_all")) {
    keep_all <- dotargs$keep_all
  } else {
    keep_all <- FALSE
  }
  if (any(names(dotargs) == "verbose")) {
    verbose <- dotargs$verbose
  } else {
    verbose <- TRUE
  }
  if (any(names(dotargs) == "cohort")) {
    cohort <- dotargs$cohort
  } else {
    cohort <- NULL
  }

  ### Algorithme de repérage d'une condition médicale - étape 1 - MEDECHO
  # Extraction de tous les Dx selon les critères de l'étape 1
  if (nDx > 0) {
    if (verbose) {
      cat("Étape 1 :\n")  # indiquer que l'étape 1 commence
    }
    Dx_etape1 <- SQL_diagn(
      conn = conn, cohort = NULL,
      debut = debut, fin = fin,
      Dx_table = Dx_table, CIM = CIM,
      dt_source = c("V_DIAGN_SEJ_HOSP_CM", "V_SEJ_SERV_HOSP_CM"),
      dt_desc = list(V_DIAGN_SEJ_HOSP_CM = "MEDECHO", V_SEJ_SERV_HOSP_CM = "MEDECHO"),
      date_dx_var = date_dx_var,
      typ_diagn = c("P", "S", "D"),
      exclu_diagn = NULL, verbose = verbose,code_stat_decis=code_stat_decis
    )
    if (verbose) {
      cat("\n")  # retour de ligne pour l'affichage de l'étape 2
    }
    if (!is.null(Dx_etape1)) {
      # Conserver la dernière date - besoin d'indiquer la date la plus récente plus tard
      if (by_Dx) {
        Dx_etape1_last <- Dx_etape1[Dx_etape1[, .I[.N], .(ID, DIAGN)]$V1, .(ID, DIAGN, DATE_DX)]
      } else {
        Dx_etape1_last <- Dx_etape1[Dx_etape1[, .I[.N], .(ID)]$V1, .(ID, DATE_DX)]
      }
      # Conserver la 1re observation de chaque individu - 1er Dx
      if (by_Dx) {
        Dx_etape1 <- Dx_etape1[
          Dx_etape1[, .I[1], .(ID, DIAGN)]$V1,
          .(ID, DIAGN, DI_Hospit = DATE_DX)  # sélection + nommer les colonnes
        ]
      } else {
        Dx_etape1 <- Dx_etape1[
          Dx_etape1[, .I[1], .(ID)]$V1,
          .(ID, DI_Hospit = DATE_DX)  # sélection + nommer les colonnes
        ]
      }
    }
  } else {
    if (verbose) {
      Dx_etape1 <- NULL
      Dx_etape1_last <- NULL
      cat("nDx=0 : Extraction des résultats (Pas de confirmation nécessaire).\n")
    }
  }

  ### Algorithme de repérage d'une condition médicale - étape 2
  # Extraction de tous les Dx selon les critères de l'étape 2
  if (nDx > 0 && verbose) {
    cat("Étape 2 :\n")  # indiquer que l'étape 2 commence
  }
  Dx_etape2 <- SQL_diagn(
    conn = conn, cohort = NULL,
    debut = debut, fin = fin,
    Dx_table = Dx_table, CIM = CIM,
    dt_source = c("V_DIAGN_SEJ_HOSP_CM", "V_SEJ_SERV_HOSP_CM",
                  "V_EPISO_SOIN_DURG_CM", "I_SMOD_SERV_MD_CM"),
    dt_desc = list(V_DIAGN_SEJ_HOSP_CM = "MEDECHO", V_SEJ_SERV_HOSP_CM = "MEDECHO",
                   V_EPISO_SOIN_DURG_CM = "BDCU", I_SMOD_SERV_MD_CM = "SMOD"),
    date_dx_var = date_dx_var,
    typ_diagn = c("A", "P", "S", "D"),
    exclu_diagn = NULL, verbose = verbose,code_stat_decis=code_stat_decis
  )
  if (nDx > 0 && verbose) {
    cat("\n")
  }
  if (nrow(Dx_etape2)) {
    if (by_Dx) {
      Dx_etape2 <- unique(Dx_etape2[, .(ID, DIAGN, DATE_DX)])
      Dx_etape2_last <- Dx_etape2[  # conserver la dernière date - besoin d'indiquer la date la plus récente plus tard
        Dx_etape2[, .I[.N], .(ID, DIAGN)]$V1,
        .(ID, DIAGN, DATE_DX)
      ]
    } else {
      Dx_etape2 <- unique(Dx_etape2[, .(ID, DATE_DX)])
      Dx_etape2_last <- Dx_etape2[  # conserver la dernière date - besoin d'indiquer la date la plus récente plus tard
        Dx_etape2[, .I[.N], .(ID)]$V1,
        .(ID, DATE_DX)
      ]
    }

    ### Confirmation des Dx de l'étape 2
    if (nDx > 0 && verbose) {
      cat("Confirmation des diagnostics...\n")
      # Ajouter une colonne substitut si by_Dx = FALSE
      if (!by_Dx) {
        Dx_etape2[, DIAGN2 := 1L]
      }
      # Confirmation des diagnostics
      Dx_etape2 <- confirm_Dx_par_nDx(
        dt = Dx_etape2,
        ID = "ID",
        DIAGN = {if (by_Dx) "DIAGN" else "DIAGN2"},
        DATE_DX = "DATE_DX",
        nDx = nDx, n1 = n1, n2 = n2,
        keep_all = FALSE
      )
      if (!by_Dx) {  # supprimer la colonne substitut si elle a ete cree precedemment
        Dx_etape2[, DIAGN := NULL]
      }
      if (nrow(Dx_etape2)) {  # renommer les colonnes
        setnames(Dx_etape2, c("DATE_REF", paste0("DATE_CONF", 1:nDx)), c("DI_Acte", paste0("DC_Acte", 1:nDx)))
      }
    } else {
      # Si pas de confirmation, on conserve la 1re date = la plus ancienne
      if (by_Dx) {
        Dx_etape2 <- Dx_etape2[
          Dx_etape2[, .I[1], .(ID, DIAGN)]$V1,
          .(ID, DIAGN, DI_Acte = DATE_DX)
        ]
      } else {
        Dx_etape2 <- Dx_etape2[
          Dx_etape2[, .I[1], .(ID)]$V1,
          .(ID, DI_Acte = DATE_DX)
        ]
      }
    }
  } else {
    if (nDx == 0) {
      stop("Aucune données à la suite de l'extraction.")
    }
  }

  if (verbose) {
    cat("Arrangement de la table finale...\n")
  }
  ### Dernière date Dx de chaque ID
  if (!is.null(Dx_etape1) && !is.null(Dx_etape2)) {
    Dx_recent <- rbind(Dx_etape1_last, Dx_etape2_last)
  } else if (!is.null(Dx_etape1)) {
    Dx_recent <- copy(Dx_etape1_last)
  } else if (!is.null(Dx_etape2)) {
    Dx_recent <- copy(Dx_etape2_last)
  } else {
    Dx_recent <- NULL
  }

  if (!is.null(Dx_recent)) {
    setkey(Dx_recent)
    if (by_Dx) {
      Dx_recent <- Dx_recent[
        Dx_recent[, .I[.N], .(ID, DIAGN)]$V1,
        .(ID, DIAGN, D_Recent = DATE_DX)
      ]
    } else {
      Dx_recent <- Dx_recent[
        Dx_recent[, .I[.N], .(ID)]$V1,
        .(ID, D_Recent = DATE_DX)
      ]
    }
  }

  ### Combiner les datasets pour table finale
  if (is.null(Dx_recent)) {
    return(NULL)
  } else {
    dt_final <- copy(Dx_recent)
    if (by_Dx) {
      if (!is.null(Dx_etape1)) {
        dt_final <- Dx_etape1[dt_final, on = .(ID, DIAGN)]
      }
      if (!is.null(Dx_etape2)) {
        dt_final <- Dx_etape2[dt_final, on = .(ID, DIAGN)]
      }
    } else {
      if (!is.null(Dx_etape1)) {
        dt_final <- Dx_etape1[dt_final, on = .(ID)]
      }
      if (!is.null(Dx_etape2)) {
        dt_final <- Dx_etape2[dt_final, on = .(ID)]
      }
    }
    # Ajouter les colonnes manquantes (si Dx_etape 1 ou 2 n'existe pas)
    if (nDx > 0) {
      for (col in c("ID", "DIAGN", "DI_Finale", "DI_Hospit", "DI_Acte", paste0("DC_Acte", 1:nDx), "D_Recent")) {
        if (!any(names(dt_final) == col)) {
          dt_final[, (col) := NA]
        }
      }
    }
    dt_final[, DI_Finale := DI_Acte]
    if (nDx > 0) {
      dt_final[is.na(DI_Acte), DI_Finale := DI_Hospit]
      dt_final[DI_Hospit < DI_Acte, DI_Finale := DI_Hospit]
    } else {
      dt_final[, DI_Acte := NULL]
    }

    if (nDx > 0) {
      if (by_Dx) {
        setcolorder(dt_final, c("ID", "DIAGN", "DI_Finale", "DI_Hospit", "DI_Acte", paste0("DC_Acte", nDx), "D_Recent"))
      } else {
        setcolorder(dt_final, c("ID", "DI_Finale", "DI_Hospit", "DI_Acte", paste0("DC_Acte", 1:nDx), "D_Recent"))
      }
    } else {
      if (by_Dx) {
        setcolorder(dt_final, c("ID", "DIAGN", "DI_Finale", "D_Recent"))
      } else {
        setcolorder(dt_final, c("ID", "DI_Finale", "D_Recent"))
      }
    }

    if (verbose) {
      cat("FIN de création de la cohorte.\n")
    }

    if (keep_all) {
      return(dt_final)
    } else {
      return(dt_final[!is.na(DI_Finale)])
    }

  }

}
