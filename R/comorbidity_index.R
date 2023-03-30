#' @title comorbidity_index
#' @description Extraction des codes de diagnostics CIM pour ensuite calculer les indicateurs de Charlson et Elixhauser.
#'
#' \strong{dt :} Si un `ID` a plus d'une date index, seule la première, la plus ancienne, sera conservée.\cr\cr
#' \strong{obstetric_exclu :} Lorsqu'un cas de diabète ou d'hypertension a lieu 120 jours avant ou 180 jours après un évènement obstétrique, on les considère de type gestationnel. Ces cas sont alors exclus de l'analyse.\cr\cr
#' \strong{dt_source :}
#' * \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=6721&NomVue=V%5FDIAGN%5FSEJ%5FHOSP%5FCM+%28Diagnostic+s%E9jour+hospitalier%29}{`V_DIAGN_SEJ_HOSP_CM`} : Cette structure contient tous les diagnostics associés à un séjour hospitalier.
#' * \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=6724&NomVue=V%5FSEJ%5FSERV%5FHOSP%5FCM+%28S%E9jour+service+hospitalier%29}{`V_SEJ_SERV_HOSP_CM`} : Cette structure contient les séjours dans un service effectués par l'individu hospitalisé.
#' * \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=6687&NomVue=V%5FEPISO%5FSOIN%5FDURG%5FCM+%28%C9pisodes+de+soins+en+D%E9partement+d%27urgence%29}{`V_EPISO_SOIN_DURG_CM`} : Cette structure contient les épisodes de soins des départements d'urgence de la province.
#' * \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=1797&NomVue=I%5FSMOD%5FSERV%5FMD%5FCM}{I_SMOD_SERV_MD_CM} : Cette vue retourne différentes informations se rapportant aux Services rendus à l'acte par des médecins.
#'
#' @inheritParams comorbidity
#' @inheritParams SQL_comorbidity_diagn
#' @param dt Tableau ayant au moins deux colonnes : `ID` et `DATE_INDEX`.
#' @param ID Nom de la colonne contenant l’identifiant unique de l’usager.
#' @param DATE_INDEX Nom de la colonne contenant la date index de chaque usager.
#' @param lookup Nombre entier. Années à analyser avant la date indexe de chaque individu.
#' @param obstetric_exclu `TRUE` ou `FALSE`. Si l'on doit exclure (`TRUE`) les diabètes et les hypertensions de type gestationnel. Voir Détails.
#'
#' @return `data.table` :
#' * `ID` : Colonne contenant l’identifiant unique de l’usager.
#' * `nDx` : Nombre de diagnostics associé à l'individu.
#' * `Charlson` : Indicateur, seulement si `method` contient `'Charlson'`.
#' * `Elixhauser` : Indicateur, seulement si `method` contient `'Elixhauser'`.
#' * `Combined` : Indicateur, seulement si `method` contient `'Charlson'` et `'Elixhauser'`.
#' * Tous les diagnostics ainsi que leur poids (score).
#' @import data.table
#' @importFrom lubridate %m-%
#' @encoding UTF-8
#' @export

comorbidity_index <- function(
    conn = SQL_connexion(),
    dt,
    ID,
    DATE_INDEX,
    Dx_table = 'Combine_Dx_CCI_INSPQ18',
    CIM = c('CIM9', 'CIM10'),
    scores = 'CCI_INSPQ_2018_CIM10',
    lookup = 2,
    n1 = 30, n2 = 730,

    # dt_source = c('V_DIAGN_SEJ_HOSP_CM', 'V_SEJ_SERV_HOSP_CM', 'V_EPISO_SOIN_DURG_CM', 'I_SMOD_SERV_MD_CM'),
    # dt_desc = list(V_DIAGN_SEJ_HOSP_CM = 'MEDECHO', V_SEJ_SERV_HOSP_CM = 'MEDECHO',
    #                V_EPISO_SOIN_DURG_CM = 'BDCU', I_SMOD_SERV_MD_CM = 'SMOD'),
    # confirm_sourc = list(MEDECHO = 1, BDCU = 2, SMOD = 2),


    date_dx_var = "depar",
    obstetric_exclu = TRUE,
    exclu_diagn = NULL,
    verbose = TRUE,
    keep_confirm_data = FALSE,
    ...
) {

  dotargs <- list(...)
  if (any(names(dotargs) == "dt_source")) {
    dt_source <- dotargs$dt_source
    } else {
      dt_source <- c('V_DIAGN_SEJ_HOSP_CM', 'V_SEJ_SERV_HOSP_CM', 'V_EPISO_SOIN_DURG_CM', 'I_SMOD_SERV_MD_CM')
    }
  if (any(names(dotargs) == "dt_desc")) {
    dt_desc <- dotargs$dt_desc
    } else {
      dt_desc <- list(V_DIAGN_SEJ_HOSP_CM = 'MEDECHO', V_SEJ_SERV_HOSP_CM = 'MEDECHO',
                    V_EPISO_SOIN_DURG_CM = 'BDCU', I_SMOD_SERV_MD_CM = 'SMOD')
    }
  if (any(names(dotargs) == "confirm_sourc")) {
    confirm_sourc <- dotargs$confirm_sourc
    } else {
      confirm_sourc <- list(MEDECHO = 1, BDCU = 2, SMOD = 2)
    }



  if (!"info" %in% names(attributes(conn))) {
    stop("Erreur de connexion.")
  } else {

    if (n1 > n2) {  # safe
      n1_copy <- n1
      n1 <- n2
      n2 <- n1_copy
    }

    ### Arranger dataset
    if (!is.data.table(dt)) {  # convertir data.table
      dt <- setDT(dt)
    }
    dt <- dt[, c(ID, DATE_INDEX), with = FALSE]  # sélection des colonnes
    setnames(dt, names(dt), c("ID", "DATE_INDEX"))  # renommer les colonnes
    setkey(dt)  # tri ID+DATE_INDEX
    if (!lubridate::is.Date(dt$DATE_INDEX)) {  # Convertir au format DATE la colonne DATE_INDEX
      dt[, DATE_INDEX := lubridate::as_date(DATE_INDEX)]
    }
    dt <- dt[complete.cases(dt)]  # Supprimer les NAs
    # Conserver la première date index de chaque ID s'ils ne sont pas unique
    idx <- rmNA(dt[, .I[.N > 1], .(ID)]$V1)
    if (length(idx)) {
      dt <- dt[dt[, .I[1], .(ID)]$V1]
    }

    ### Cohorte d'étude
    cohort <- sort(dt$ID)

    ### Extraction des diagnostics dans les années désirées
    DIAGN <- RequeteGeneriqueBDCA:::SQL_comorbidity_diagn(
      conn,
      cohort = cohort,
      debut = lubridate::as_date(ifelse(
        2 %in% unlist(confirm_sourc),
        min(dt$DATE_INDEX) %m-% months(lookup*12) - n2,
        min(dt$DATE_INDEX) %m-% months(lookup*12)
      )),
      fin = max(dt$DATE_INDEX),
      Dx_table = Dx_table, CIM = CIM,
      dt_source = dt_source, dt_desc = dt_desc,
      date_dx_var = date_dx_var, typ_diagn = c("A", "P", "S"),
      exclu_diagn = exclu_diagn, verbose = verbose,
      code_stat_decis = c("PAY", "PPY")
    )

    ### Filtrer dt pour en faire l'analyse
    # Supprimer les diagnostics qui ne sont pas dans l'intervalle [DATE_INDEX - lookup - n1; DATE_INDEX]
    dt <- DIAGN[dt, on = .(ID), nomatch = 0]  # ajouter les diagn aux dates index en conservant seulement les id présent dans DIAGN et dt
    dt <- dt[DATE_INDEX %m-% months(lookup*12) - n2 <= DATE_DX & DATE_DX <= DATE_INDEX]  # [index-X{ans}-n2{jours}; index]
    setkey(dt, ID, DIAGN, DATE_DX)
    # Supprimer les dates < (DATE_INDEX - lookup) dont la source a une confirmation = 1
    sourc <- RequeteGeneriqueBDCA:::comorbidity.confirm_sourc_names(confirm_sourc, 1)
    if (length(sourc)) {
      idx <- intersect(
        dt[, .I[SOURCE %in% sourc]],
        dt[, .I[DATE_DX < DATE_INDEX %m-% months(lookup*12)]]
      )
      if (length(idx)) {
        dt <- dt[!idx]
      }
    }

    ### Exclusion des cas gestationnels
    if (verbose) {
        cat("Exclusion des cas gestationnels\n")
      }
    if (obstetric_exclu) {
      dt <- RequeteGeneriqueBDCA:::SQL_comorbidity.exclu_diab_gross(conn, dt, CIM, dt_source, dt_desc, date_dx_var, typ_diagn = c("A", "P", "S"),verbose)
    }

    ### Dx confirmé par un Dx dans l'intervalle qui précède la période d'étude
    for (desc in names(confirm_sourc)) {  # Trier les données selon l'importance des sources
      dt[SOURCE == desc, tri := confirm_sourc[[desc]]]
    }
    setkey(dt, ID, DIAGN, DATE_DX, tri, SOURCE)
    dt[, tri := NULL]
    dt[, row := 1:nrow(dt)]
    dates_b4 <- dt[DATE_DX < DATE_INDEX %m-% months(lookup*12)]
    if (nrow(dates_b4)) {
      dates_after <- dt[DATE_DX >= DATE_INDEX %m-% months(lookup*12), .(ID, DATE_DX2 = DATE_DX, DIAGN, row2 = row)]
      dates_b4 <- dates_after[dates_b4, on = .(ID, DIAGN), allow.cartesian = TRUE, nomatch = 0]
      dates_b4 <- dates_b4[DATE_DX + n2 >= DATE_DX2]
      dates_b4[, confirm := DATE_DX2 - DATE_DX]
      dates_b4 <- dates_b4[n1 <= confirm & confirm <= n2]
      if (nrow(dates_b4)) {
        dates_b4 <- dates_b4[dates_b4[, .I[1], .(ID, DIAGN)]$V1]
        dt <- dt[sunique(c(dates_after$row2, dates_b4$row))]
        dt[, row := NULL]
      }
    }

    ### Calcul des scores
    dt <- comorbidity(
      dt=dt, "ID", "DIAGN", "DATE_DX", "SOURCE", n1=n1, n2=n2,
      Dx_table=Dx_table, scores=scores, confirm_sourc=confirm_sourc,
      exclu_diagn=exclu_diagn, keep_confirm_data=keep_confirm_data
    )
    # Conserver les attributs pour plus tard
    attr_infos <- attr(dt, "infos")
    attr_confirm_data <- attr(dt, "confirm_data")

    ### Ajouter les ID manquants
    ids_2_add <- cohort[!cohort %in% dt$ID]
    if (length(ids_2_add)) {
      dt <- rbind(dt, data.table(ID = ids_2_add), fill = TRUE)
      # Remplacer les NA par 0
      for (col in names(dt)[names(dt) != "ID"]) {
        set(dt, which(is.na(dt[[col]])), col, 0L)
      }
    }
    setkey(dt, ID)

    ### Insérer les attributs dans le data final
    attr(dt, "infos") <- attr_infos
    if (keep_confirm_data) {
      attr(dt, "confirm_data") <- attr_confirm_data
    }

    return(dt)

  }

}



#' @title Extraction - Codes diagn obstetriques
#' @description Extraction des événements obstétriques.
#' @import data.table
#' @encoding UTF-8
SQL_obstetric <- function(
    conn = SQL_connexion(),
    cohort, debut, fin,
    CIM = c('CIM9', 'CIM10'),
    dt_source = c('V_DIAGN_SEJ_HOSP_CM', 'V_SEJ_SERV_HOSP_CM',
                  'V_EPISO_SOIN_DURG_CM', 'I_SMOD_SERV_MD_CM'),
    dt_desc = list(V_DIAGN_SEJ_HOSP_CM = 'MED-ECHO', V_SEJ_SERV_HOSP_CM = 'MED-ECHO',
                   V_EPISO_SOIN_DURG_CM = 'BDCU', I_SMOD_SERV_MD_CM = 'SMOD'),
    date_dx_var = "depar", typ_diagn = c('A', 'P', 'S'), verbose = TRUE
) {

  ### Arranger les arguments
  # Arguments possiblement manquants
  if (missing(conn)) {
    conn <- NULL
  }

  ### Extraction des diagn
  if (!"info" %in% names(attributes(conn))) {  # si encore NULL = Erreur
    stop("Erreur de connexion. Vérifier l'identifiant (uid) et le mot de passe (pwd).")
  } else {

    # CIM9 vs CIM10 -- Filtrer les types de codes si on veut seulement une version
    # de classification de code.
    diagn_codes <- inesss::Obstetrics_Dx
    if (length(CIM) == 1 && CIM == "CIM9") {
      diagn_codes$obstetric$CIM10 <- NULL
    } else if (length(CIM) == 1 && CIM == "CIM10") {
      diagn_codes$obstetric$CIM9 <- NULL
    }

    # Extraction des diagnostiques
    DT <- vector("list", length(dt_source) * length(diagn_codes))  # contiendra toutes les requêtes
    i <- 1L
    for (sour in dt_source) {
      if (verbose) {
        cat(sour,"\n")
      }
      fct <- get(paste0("SQL_comorbidity_diagn.",sour))  # fonction d'extraction selon la source
      for (dia in names(diagn_codes)) {
        t1 <- Sys.time()
        DT[[i]] <- fct(  # tableau provenant de la requête
          conn = conn, ids = sunique(cohort), diagn = diagn_codes[[dia]],
          debut = debut, fin = fin,
          diag_desc = dia, sourc_desc = dt_desc[[sour]],
          date_dx_var = date_dx_var,
          typ_diagn = typ_diagn
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
    DT <- data.table::rbindlist(DT)  # regrouper tous les tableaux en un seul

    if (nrow(DT)) {
      setkey(DT)  # tri
    }
    return(DT)

  }

}


#' @title SQL_comorbidity
#' @description Exclusion des cas de diabète et d'hypertension gestationnel.
#' @keywords internal
#' @import data.table
#' @encoding UTF-8
SQL_comorbidity.exclu_diab_gross <- function(conn, dt, CIM, dt_source, dt_desc, date_dx_var,typ_diagn, verbose) {
  ### Supprimer les cas de diabètes de grosses. Un diagnostic de diabète sera
  ### supprimé s'il se trouve 120 jours avant le diagnostic et 180 jours après.

  ### Cas de diabète & hypertension
  dt_diab_hyp <- unique(dt[  # un seul cas par ID + DIAGN + DATE
    DIAGN %in% c("diab", "diabwc", "hyp"),  # cas de diabète ou d'hypertension
    .(ID, DATE_DX, DIAGN)  # colonnes
  ])

  if (nrow(dt_diab_hyp)) {
    ### Extraction des cas de grossesses
    dt_gross <-RequeteGeneriqueBDCA:::SQL_obstetric(
      conn,
      cohort = sunique(dt_diab_hyp$ID),
      debut = min(dt_diab_hyp$DATE_DX) - 180, fin = max(dt_diab_hyp$DATE_DX) + 120,
      CIM, dt_source, dt_desc, date_dx_var, typ_diagn,verbose
    )

    ### Arranger le data pour exclusion des diabètes et hypertension de grossesses
    dt_gross <- unique(dt_gross[, .(ID, DATE_OBSTE = DATE_DX)])  # un seul cas par ID + DATE
    dt_diab_hyp <- dt_gross[dt_diab_hyp, on = .(ID), nomatch = 0]  # combinaison {diab, hyp} + {obstetric}
    dt_diab_hyp <- dt_diab_hyp[  # supprimer les diagn qui ont au moins un cas de grossesse [-120; 180] jours.
      !is.na(DATE_OBSTE) &  # n'a pas de cas de grosseses
        DATE_OBSTE + 180 >= DATE_DX & DATE_DX >= DATE_OBSTE - 120  # cas où l'obstetric annule le diab ou l'hyp
    ]
    dt_diab_hyp <- unique(dt_diab_hyp[, .(ID, DATE_DX, DIAGN)])  # un seul cas par date et diagn
    dt <- dt[!dt_diab_hyp, on = .(ID, DATE_DX, DIAGN)]  # exclure de dt les observations qui sont présentes dans dt_diab_hyp
  }

  return(dt)

}


#' @title SQL_comorbidity
#' @description Indiquer la source de confirmation: MEDECHO, BDCU, SMOD
#' @keywords internal
#' @import data.table
#' @encoding UTF-8
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
