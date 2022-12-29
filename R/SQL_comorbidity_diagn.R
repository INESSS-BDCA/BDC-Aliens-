#' SQL_comorbidity_diagn
#'
#' SQL_comorbidity_diagn est une fonction intermédiaire utilisée par la fonction \code{\link{SQL_diag}} afin d'extraire des diagnostics cim-9 et cim-10 pour la création d'une cohorte.
#' SQL_comorbidity_diagn fait appel aux différentes fonctions: \code{\link{query_I_SMOD_SERV_MD_CM}}, \code{\link{query_V_DIAGN_SEJ_HOSP_CM}}, \code{\link{query_V_SEJ_SERV_HOSP_CM}}, \code{\link{query_V_EPISO_SOIN_DURG_CM}}.
#'
#' @details \strong{\code{dt_source} :}
#' * \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=6721&NomVue=V%5FDIAGN%5FSEJ%5FHOSP%5FCM+%28Diagnostic+s%E9jour+hospitalier%29}{`V_DIAGN_SEJ_HOSP_CM`} : Cette structure contient tous les diagnostics associés à un séjour hospitalier.
#' * \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=6724&NomVue=V%5FSEJ%5FSERV%5FHOSP%5FCM+%28S%E9jour+service+hospitalier%29}{`V_SEJ_SERV_HOSP_CM`} : Cette structure contient les séjours dans un service effectués par l'individu hospitalisé.
#' * \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=6687&NomVue=V%5FEPISO%5FSOIN%5FDURG%5FCM+%28%C9pisodes+de+soins+en+D%E9partement+d%27urgence%29}{`V_EPISO_SOIN_DURG_CM`} : Cette structure contient les épisodes de soins des départements d'urgence de la province.
#' * \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=1797&NomVue=I%5FSMOD%5FSERV%5FMD%5FCM}{`I_SMOD_SERV_MD_CM`} : Cette vue retourne différentes informations se rapportant aux Services rendus à l'acte par des médecins.
#' \strong{\code{Dx_table} :} Il n'est pas nécessaire d'inclure un pourcentage à la fin de chaque code. Le programme le fait automatiquement s'il n'y en a pas. Par exemple inscrire le code `I25` et `I25%` revient au même.
#'
#' @inheritParams comorbidity
#' @param conn Variable contenant la connexion entre R et Teradata. Voir \code{\link{SQL_connexion}}.
#' @param cohort Cohorte d'étude. Vecteur comprenant les numéros d'identification des individus à conserver.
#' @param debut Date de début de la période d'étude au format `AAAA-MM-JJ`.
#' @param fin Date de fin de la période d'étude au format `AAAA-MM-JJ`.
#' @param Dx_table `list` contenant les codes à analyser ou le nom d'une table qui est inclue dans la librairie.
#' @param CIM `'CIM9'`, `'CIM10'` ou les deux. Permet de filtrer les codes de diagnostics selon le numéro de révision de la *Classification statistique internationale des maladies et des problèmes de santé connexes* (CIM).
#' @param dt_source Vecteur comprenant la ou les bases de données où aller chercher l'information. Voir *Details*.
#' @param dt_desc `list` décrivant les bases de données demandées dans `dt_source` au format `list(BD = 'MaDescription')`. Voir *Details*.
#' @param date_dx_var `'admis` ou `'depar'`. Indique si on utilise la date d'admission ou la date de départ comme date de diagnostic pour l'étude dans les vues V_DIAGN_SEJ_HOSP_CM, V_SEJ_SERV_HOSP_CM et V_EPISO_SOIN_DURG_CM.
#' @param typ_diagn Type de disgnostic permettant de préciser le genre de diagnostic posé pendant le séjour hospitalier. `A = Admission`, `D = Décès`, `P = Principal` et `S = Secondaire`. Voir la variable `SHOP_TYP_DIAGN_SEJ_HOSP` de la vue `V_DIAGN_SEJ_HOSP_CM`.
#' @param verbose `TRUE` ou `FALSE`. Affiche le temps qui a été nécessaire pour extraire les diagnostics d'une source (`dt_source`). Utile pour suivre le déroulement de l'extraction.
#'
#' @return `data.table` de 4 variables :
#' * **`ID`** : Numéro d'identification de l'usager.
#' * **`DATE_DX`** : Date de diagnostic.
#' * **`DIAGN`** : Code descriptif des diagnostics provenant de `diagn_codes`.
#' * **`SOURCE`** : Indique d'où provient l'information. Une valeur parmi `dt_source`.
#' @encoding UTF-8
#' @export
#' @examples
#' ### Inscription des codes de diagnostics dans Dx_table
#' # 1 code CIM9 ou CIM10
#' liste1 = list(CIM9 = "413")
#' liste2 = list(CIM10 = "I20")
#' # 1 code de chaque
#' liste3 = list(CIM9 = "413", CIM10 = "I20")
#' # Plusieurs codes qui se suivent
#' liste4 = list(CIM9 = 410:414, CIM10 = paste0("I", 21:25))
#' # Codes qui ne se suivent pas
#' liste5 = list(CIM9 = c(410, 412, 414), CIM10 = paste0("I", 21, 23, 25))
#' # Codes qui se suivent et qui ne se suivent pas
#' liste6 = list(CIM9 = c(405:410, 412:414),
#'               CIM10 = c(paste0("I", c(18:21, 23:25)),
#'                         paste0("K0", c(5:9)), paste0("K", 10:15)))
SQL_comorbidity_diagn <- function(
    conn = SQL_connexion(), cohort, debut, fin,
    Dx_table = 'Combine_Dx_CCI_INSPQ18', CIM = c('CIM9', 'CIM10'),
    dt_source = c('V_DIAGN_SEJ_HOSP_CM', 'V_SEJ_SERV_HOSP_CM',
                  'V_EPISO_SOIN_DURG_CM', 'I_SMOD_SERV_MD_CM'),
    dt_desc = list(V_DIAGN_SEJ_HOSP_CM = 'MEDECHO', V_SEJ_SERV_HOSP_CM = 'MEDECHO',
                   V_EPISO_SOIN_DURG_CM = 'BDCU', I_SMOD_SERV_MD_CM = 'SMOD'),
    date_dx_var = "depar", typ_diagn = c('A', 'P', 'S'),
    exclu_diagn = NULL, verbose = TRUE
) {

  ### Extraction des diagn
  if (!"info" %in% names(attributes(conn))) {  # si encore NULL = Erreur
    stop("Erreur de connexion.")
  } else {

    Dx_table <- inesss:::SQL_comorbidity_diagn.select_Dx_table(Dx_table)
    # Exclusion des diagnostiques
    if (!is.null(exclu_diagn)) {
      Dx_table <- Dx_table[!names(Dx_table) %in% exclu_diagn]
    }
    # Ajouter des pourcentages aux codes s'il n'y en a pas
    for (i in 1:length(Dx_table)) {
      for (j in 1:2) {
        if (length(Dx_table[[i]][[j]])) {
          Dx_table[[i]][[j]] <- sapply(Dx_table[[i]][[j]], function(x) {
            if (!stringr::str_detect(x, "%")) {
              return(paste0(x, "%"))
            } else {
              return(x)
            }
          })
        }
      }
    }
    # CIM9 vs CIM10 -- Filtrer les types de codes si on veut seulement une version
    # de classification de code.
    Dx_table_V_DIAGN_SEJ <- Dx_table  # conserver une copie pour l'exception de la table V_DIAGN_SEJ_HOSP_CM
    if (length(CIM) == 1) {
      for (i in names(Dx_table)) {
        Dx_table_V_DIAGN_SEJ[[i]] <- Dx_table_V_DIAGN_SEJ[[i]][[CIM]]
        Dx_table[[i]] <- Dx_table[[i]][[CIM]]  # conserver seulement CIM9 ou CIM10
      }
    } else {
      for (i in names(Dx_table)) {
        Dx_table[[i]] <- unlist(Dx_table[[i]], use.names = FALSE)  # grouper CIM9 et CIM10 en un seul vecteur
      }
    }

    # Extraction des diagnostics
    DT <- vector("list", length(dt_source) * length(Dx_table))  # contiendra toutes les requêtes
    i <- 1L
    for (sour in dt_source) {
      if (verbose) {
        cat(sour, "\n")
      }
      # Différencier V_DIAGN_SEJ_HOSP_CM des autres sources
      if (sour == "V_DIAGN_SEJ_HOSP_CM") {
        for (dia in names(Dx_table_V_DIAGN_SEJ)) {
          t1 <- Sys.time()
          DT[[i]] <- SQL_comorbidity_diagn.V_DIAGN_SEJ_HOSP_CM(
            conn = conn, ids = cohort, diagn = Dx_table_V_DIAGN_SEJ[[dia]],
            debut = debut, fin = fin,
            diag_desc = dia, sourc_desc = dt_desc[[sour]],
            date_dx_var = date_dx_var, typ_diagn = typ_diagn
          )
          t2 <- Sys.time()
          i <- i + 1L
          # Afficher le temps d'exécution
          if (verbose) {
            cat(" - ",dia,
                " (",round(as.numeric(difftime(t2, t1)), 2),
                " ",attr(difftime(t2, t1), "units"), ")\n",
                sep = "")

          }
        }
      } else {
        fct <- get(paste0("SQL_comorbidity_diagn.",sour))  # fonction d'extraction selon la source
        for (dia in names(Dx_table)) {
          t1 <- Sys.time()
          DT[[i]] <- fct(  # tableau provenant de la requête
            conn = conn, ids = cohort, diagn = Dx_table[[dia]],
            debut = debut, fin = fin,
            diag_desc = dia, sourc_desc = dt_desc[[sour]],
            date_dx_var = date_dx_var
          )
          t2 <- Sys.time()
          i <- i + 1L
          # Afficher le temps d'exécution
          if (verbose) {
            cat(" - ",dia,
                " (",round(as.numeric(difftime(t2, t1)), 2),
                " ",attr(difftime(t2, t1), "units"), ")\n",
                sep = "")

          }
        }
      }
    }
    DT <- data.table::rbindlist(DT)  # regrouper tous les tableaux en un seul

    if (nrow(DT)) {
      setcolorder(DT, c("ID", "DIAGN", "DATE_DX", "SOURCE"))  # ordre des colonnes
      setkey(DT)  # tri
      return(DT)
    } else {
      return(NULL)
    }

  }

}

#' @title SQL_comorbidity_diagn
#' @description Sélection du data contenant les codes à analyser.
#' @encoding UTF-8
#' @keywords internal
SQL_comorbidity_diagn.select_Dx_table <- function(Dx_table) {
  if (is.list(Dx_table)) {
    return(Dx_table)
  } else if (Dx_table == "Combine_Dx_CCI_INSPQ18") {
    return(inesss::Combine_Dx_CCI_INSPQ18)
  } else if (Dx_table == "Charlson_Dx_CCI_INSPQ18") {
    return(inesss::Charlson_Dx_CCI_INSPQ18)
  } else if (Dx_table == "Elixhauser_Dx_CCI_INSPQ18") {
    return(inesss::Elixhauser_Dx_CCI_INSPQ18)
  } else if (Dx_table == "Charlson_Dx_UManitoba16") {
    return(inesss::Charlson_Dx_UManitoba16)
  } else if (Dx_table == "Charlson_Dx_CCI_INSPQ_Manitoba") {
    return(inesss::Charlson_Dx_CCI_INSPQ_Manitoba)
  } else {
    stop("SQL_comorbidity_diagn.select_Dx_table(): Dx_table ne contient pas une valeur permise.")
  }
}
#' @title SQL_comorbidity_diagn
#' @description Requête SQL pour extraire les diagnostics de la vue V_DIAGN_SEJ_HOSP_CM
#' @param diagn Comparativement aux autres fonctions d'extraction, celle-ci conserve la liste des diagnostics par CIM9 et CIM10 parce qu'il est possible de les séparer. La variable `SHOP_NO_SEQ_SYS_CLA` indique si c'est un CIM9 ou un CIM10, car la vue `V_DIAGN_SEJ_HOSP_CM` peut contenir deux fois le même code de diagnostic appartenant à des CIM différents.
#' @keywords internal
#' @import data.table
SQL_comorbidity_diagn.V_DIAGN_SEJ_HOSP_CM <- function(
    conn, ids, diagn, debut, fin,
    diag_desc, sourc_desc, date_dx_var, typ_diagn
) {

  yr_deb <- year(lubridate::as_date(debut))  # 1ere année à extraire
  yr_fin <- year(lubridate::as_date(fin))  # dernière année à extraire
  DT <- vector("list", yr_fin - yr_deb + 1L)  # contiendra toutes les années
  i <- 1L
  for (yr in yr_deb:yr_fin) {
    # Ajuster les dates
    if (yr == yr_deb) {
      deb <- debut
    } else {
      deb <- paste0(yr,"-01-01")
    }
    if (yr == yr_fin) {
      fi <- fin
    } else {
      fi <- paste0(yr,"-12-31")
    }

    # Extraction des diagn selon l'année
    DT[[i]] <- as.data.table(odbc::dbGetQuery(
      conn = conn, statement = query_V_DIAGN_SEJ_HOSP_CM(
        debut = deb, fin = fi,
        diagn = diagn,
        date_dx_var = date_dx_var,
        typ_diagn = typ_diagn
      )
    ))
    if (!is.null(ids)) {
      DT[[i]] <- DT[[i]][ID %in% ids]  # filtrer les IDs demandés
    }
    i <- i + 1L
  }
  DT <- rbindlist(DT)  # regrouper les tableaux

  ### Ajouter les descriptions et forcer les classes
  if (nrow(DT)) {
    DT[, `:=` (ID = as.integer(ID),
               DATE_DX = lubridate::as_date(DATE_DX),
               DIAGN = diag_desc,
               SOURCE = sourc_desc)]
  } else {
    DT <- NULL
  }

  return(DT)

}
#' @title SQL_comorbidity_diagn
#' @description Requête SQL pour extraire les diagnostics de la vue V_SEJ_SERV_HOSP_CM
#' @keywords internal
#' @import data.table
SQL_comorbidity_diagn.V_SEJ_SERV_HOSP_CM <- function(
    conn, ids, diagn, debut, fin,
    diag_desc, sourc_desc, date_dx_var
) {

  yr_deb <- year(lubridate::as_date(debut))  # 1ere année à extraire
  yr_fin <- year(lubridate::as_date(fin))  # dernière année à extraire
  DT <- vector("list", yr_fin - yr_deb + 1L)  # contiendra toutes les années
  i <- 1L
  for (yr in yr_deb:yr_fin) {
    # Ajuster les dates
    if (yr == yr_deb) {
      deb <- debut
    } else {
      deb <- paste0(yr,"-01-01")
    }
    if (yr == yr_fin) {
      fi <- fin
    } else {
      fi <- paste0(yr,"-12-31")
    }
    # Extraction des diagn selon l'année
    DT[[i]] <- as.data.table(odbc::dbGetQuery(
      conn = conn, statement = query_V_SEJ_SERV_HOSP_CM(
        debut = deb, fin = fi,
        diagn = diagn, date_dx_var = date_dx_var
      )
    ))
    if (!is.null(ids)) {
      DT[[i]] <- DT[[i]][ID %in% ids]  # filtrer les IDs demandés
    }
    i <- i + 1L
  }
  DT <- rbindlist(DT)  # regrouper les tableaux

  ### Ajouter les descriptions et forcer les classes
  if (nrow(DT)) {
    DT[, `:=` (ID = as.integer(ID),
               DATE_DX = lubridate::as_date(DATE_DX),
               DIAGN = diag_desc,
               SOURCE = sourc_desc)]
  } else {
    DT <- NULL
  }

  return(DT)

}
#' @title SQL_comorbidity_diagn
#' @description Requête SQL pour extraire les diagnostics de la vue I_SMOD_SERV_MD_CM
#' @details L'argument `date_dx_var` n'est pas utilisé dans cette fonction. Il est là pour permettre une boucle avec les autres fonctions de requêtes.
#' @keywords internal
#' @import data.table
SQL_comorbidity_diagn.I_SMOD_SERV_MD_CM <- function(
    conn, ids, diagn, debut, fin,
    diag_desc, sourc_desc, date_dx_var
) {

  yr_deb <- year(lubridate::as_date(debut))  # 1ere année à extraire
  yr_fin <- year(lubridate::as_date(fin))  # dernière année à extraire
  DT <- vector("list", yr_fin - yr_deb + 1L)  # contiendra toutes les années
  i <- 1L
  for (yr in yr_deb:yr_fin) {
    # Ajuster les dates
    if (yr == yr_deb) {
      deb <- debut
    } else {
      deb <- paste0(yr,"-01-01")
    }
    if (yr == yr_fin) {
      fi <- fin
    } else {
      fi <- paste0(yr,"-12-31")
    }
    # Extraction des diagn selon l'année
    DT[[i]] <- as.data.table(odbc::dbGetQuery(
      conn = conn, statement = query_I_SMOD_SERV_MD_CM_AG(
        query="extraction_Dx",
        debut = deb,
        fin = fi,
        diagn = diagn,
        omni_spec,
        CodeActe

      )
    ))
    if (!is.null(ids)) {
      DT[[i]] <- DT[[i]][ID %in% ids]  # filtrer les IDs demandés
    }
    i <- i + 1L
  }
  DT <- rbindlist(DT)  # regrouper les tableaux

  ### Ajouter les descriptions et forcer les classes
  if (nrow(DT)) {
    DT[, `:=` (ID = as.integer(ID),
               DATE_DX = lubridate::as_date(DATE_DX),
               DIAGN = diag_desc,
               SOURCE = sourc_desc)]
  } else {
    DT <- NULL
  }

  return(DT)

}
#' @title SQL_comorbidity_diagn
#' @description Requête SQL pour extraire les diagnostics de la vue V_EPISO_SOIN_DURG_CM
#' @keywords internal
#' @import data.table
SQL_comorbidity_diagn.V_EPISO_SOIN_DURG_CM <- function(
    conn, ids, diagn, debut, fin,
    diag_desc, sourc_desc, date_dx_var
) {

  yr_deb <- year(lubridate::as_date(debut))  # 1ere année à extraire
  yr_fin <- year(lubridate::as_date(fin))  # dernière année à extraire
  DT <- vector("list", yr_fin - yr_deb + 1L)  # contiendra toutes les années
  i <- 1L
  for (yr in yr_deb:yr_fin) {
    # Ajuster les dates
    if (yr == yr_deb) {
      deb <- debut
    } else {
      deb <- paste0(yr,"-01-01")
    }
    if (yr == yr_fin) {
      fi <- fin
    } else {
      fi <- paste0(yr,"-12-31")
    }
    # Extraction des diagn selon l'année
    DT[[i]] <- as.data.table(odbc::dbGetQuery(
      conn = conn, statement = query_V_EPISO_SOIN_DURG_CM(
        debut = deb, fin = fi,
        diagn = diagn, date_dx_var = date_dx_var
      )
    ))
    if (!is.null(ids)) {
      DT[[i]] <- DT[[i]][ID %in% ids]  # filtrer les IDs demandés
    }
    i <- i + 1L
  }
  DT <- rbindlist(DT)  # regrouper les tableaux

  ### Ajouter les descriptions et forcer les classes
  if (nrow(DT)) {
    DT[, `:=` (ID = as.integer(ID),
               DATE_DX = lubridate::as_date(DATE_DX),
               DIAGN = diag_desc,
               SOURCE = sourc_desc)]
  } else {
    DT <- NULL
  }

  return(DT)

}
