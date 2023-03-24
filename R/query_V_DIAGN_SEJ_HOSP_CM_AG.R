#' @title Code SQL: Extraction de diagnostics dans MEDECHO-diag
#'
#' @description Générateur de code SQL pour l'extraction de diagnostics de la vue `V_DIAGN_SEJ_HOSP_CM`.
#'
#' @param debut_cohort Date de début de la période d'étude.
#' @param fin_cohort Date de fin_cohort de la période d'étude.
#' @param diagn `list` indiquant les codes de diagnostics *CIM9* et *CIM10* à extraire.\cr `list(CIM9=c(...),CIM10=c(...))`.
#' @param date_dx_var `'admis'` ou `'depar'`. Indique si on utilise la date d'admission ou la date de départ comme date de diagnostic pour l'étude.
#' @param typ_diagn Type de disgnostic permettant de préciser le genre de diagnostic posé pendant le séjour hospitalier. `A = Admission`, `D = Décès`, `P = Principal` et `S = Secondaire`. Voir la variable `SHOP_TYP_DIAGN_SEJ_HOSP` de la vue `V_DIAGN_SEJ_HOSP_CM`.
#'
#' @return chaîne de caractères, code SQL.
#' @keywords internal
#' @encoding UTF-8
#' @export
#'
query_V_DIAGN_SEJ_HOSP_CM_AG <- function(
    query,
    debut_periode,
    fin_periode,
    debut_cohort,
    fin_cohort,
    diagn,
    date_dx_var = 'admis',
    typ_diagn = c('A', 'P', 'S', 'D')) {
  switch (query,
          "extraction_MEDECHO_DIAGN_SEJ"={
            return(paste0(
              "SELECT\n",
              "SHOP_NO_INDIV_BEN_BANLS as ID,\n",
              query_V_DIAGN_SEJ_HOSP_CM.date_dx_var(date_dx_var)," as DATE_DX,\n",
              "SHOP_COD_DIAGN_MDCAL_CLINQ AS DX,\n",
              "SHOP_DAT_ADMIS_SEJ_HOSP,\n",
              "SHOP_DAT_DEPAR_SEJ_HOSP,\n",
              "SHOP_NO_SEQ_SEJ_HOSP,\n",
              "SHOP_NO_ETAB_MSSS,\n",
              "SHOP_TYP_DIAGN_SEJ_HOSP,\n",
              "SHOP_TYP_SOIN_SEJ_HOSP,\n",
              "SHOP_NO_SEQ_SYS_CLA \n",

              "FROM RES_SSS.V_DIAGN_SEJ_HOSP_CM\n",
              "WHERE ",query_V_DIAGN_SEJ_HOSP_CM.date_dx_var(date_dx_var)," between '",debut_periode,"' and '",fin_periode,"'\n",
              query_V_DIAGN_SEJ_HOSP_CM.where_diagn_AG(diagn),
              query_V_DIAGN_SEJ_HOSP_CM.where_typ_diagn(typ_diagn),
              ";"))},
          "extraction_Dx"={
            return(paste0(
              "SELECT
              ID,
              DATE_DX
              FROM (
              SELECT
              DISTINCT
              SHOP_NO_INDIV_BEN_BANLS as ID,\n",
              indent("select"),query_V_DIAGN_SEJ_HOSP_CM.date_dx_var(date_dx_var)," as DATE_DX,\n",
              "SHOP_COD_DIAGN_MDCAL_CLINQ AS DX\n",
              "from RES_SSS.V_DIAGN_SEJ_HOSP_CM\n",
              "where ",query_V_DIAGN_SEJ_HOSP_CM.date_dx_var(date_dx_var)," between '",debut_cohort,"' and '",fin_cohort,"'\n",
              query_V_DIAGN_SEJ_HOSP_CM.where_diagn(diagn),
              query_V_DIAGN_SEJ_HOSP_CM.where_typ_diagn(typ_diagn),
              ") AS A ;"))}
  )

}

#' @title query_V_DIAGN_SEJ_HOSP_CM.date_dx_var
#' @description Retourne le nom de la variable à utiliser selon la valeur de la variable `date_dx_var`.
#' @encoding UTF-8
#' @return "SHOP_DAT_ADMIS_SEJ_HOSP" ou "SHOP_DAT_DEPAR_SEJ_HOSP"
#' @keywords internal
query_V_DIAGN_SEJ_HOSP_CM.date_dx_var <- function(date_dx_var) {
  if (date_dx_var == "admis") {
    return("SHOP_DAT_ADMIS_SEJ_HOSP")
  } else if (date_dx_var == "depar") {
    return("SHOP_DAT_DEPAR_SEJ_HOSP")
  } else {
    stop("query_V_DIAGN_SEJ_HOSP_CM.date_dx_var : date_dx_var ne contient pas une valeur permise.")
  }
}

#' @title query_V_DIAGN_SEJ_HOSP_CM.where_diagn
#' @description Section `where` du code SQL où on demande les codes de diagnostics.
#' @encoding UTF-8
#' @keywords internal
query_V_DIAGN_SEJ_HOSP_CM.where_diagn <- function(diagn) {

  cim9 <- diagn$CIM9
  cim10 <- diagn$CIM10

  if (length(cim9) && length(cim10)) {
    # CIM9 et CIM10
    return(paste0(
      indent(),
      "and ((SHOP_COD_DIAGN_MDCAL_CLINQ like any (",qu(cim10),") and SHOP_NO_SEQ_SYS_CLA = 1)\n",
      indent(),
      "     or\n",
      indent(),
      "     (SHOP_COD_DIAGN_MDCAL_CLINQ like any (",qu(cim9),") and SHOP_NO_SEQ_SYS_CLA = 4))\n"
    ))
  } else if (length(cim9) && !length(cim10)) {
    # CIM9 seulement
    return(paste0(
      indent(),
      "and SHOP_COD_DIAGN_MDCAL_CLINQ like any (",qu(cim9),") and SHOP_NO_SEQ_SYS_CLA = 4\n"
    ))
  } else if (!length(cim9) && length(cim10)) {
    # CIM10 seulement
    return(paste0(
      indent(),
      "and SHOP_COD_DIAGN_MDCAL_CLINQ like any (",qu(cim10),") and SHOP_NO_SEQ_SYS_CLA = 1\n"
    ))
  } else {
    stop("query_V_DIAGN_SEJ_HOSP_CM.where_diagn : diagn est vide.")
  }
}

#' @title query_V_DIAGN_SEJ_HOSP_CM.where_diagn_AG
#' @description Section `where` du code SQL où on demande les codes de diagnostics.
#' @encoding UTF-8
#' @keywords internal
query_V_DIAGN_SEJ_HOSP_CM.where_diagn_AG <- function(diagn) {
  for (dia in names(diagn)) {
    cim9 <- diagn[[dia]]$CIM9
    cim10 <- diagn[[dia]]$CIM10
    cim9
    cim10
  }

  if (length(cim9) && length(cim10)) {
    # CIM9 et CIM10
    return(paste0(
      indent(),
      "and ((SHOP_COD_DIAGN_MDCAL_CLINQ like any (",qu(cim10),") and SHOP_NO_SEQ_SYS_CLA = 1)\n",
      indent(),
      "or\n",
      indent(),
      "(SHOP_COD_DIAGN_MDCAL_CLINQ like any (",qu(cim9),") and SHOP_NO_SEQ_SYS_CLA = 4))\n"
    ))
  } else if (length(cim9) && !length(cim10)) {
    # CIM9 seulement
    return(paste0(
      indent(),
      "and SHOP_COD_DIAGN_MDCAL_CLINQ like any (",qu(cim9),") and SHOP_NO_SEQ_SYS_CLA = 4\n"
    ))
  } else if (!length(cim9) && length(cim10)) {
    # CIM10 seulement
    return(paste0(
      indent(),
      "and SHOP_COD_DIAGN_MDCAL_CLINQ like any (",qu(cim10),") and SHOP_NO_SEQ_SYS_CLA = 1\n"
    ))
  } else {
    stop("query_V_DIAGN_SEJ_HOSP_CM.where_diagn : diagn est vide.")
  }
}

#' @title query_V_DIAGN_SEJ_HOSP_CM.where_typ_diagn
#' @description Section `where` du code SQL où on demande les types de codes de diagnostics. Voir variable `SHOP_TYP_DIAGN_SEJ_HOSP`.
#' @encoding UTF-8
#' @keywords internal
query_V_DIAGN_SEJ_HOSP_CM.where_typ_diagn <- function(typ_diagn) {
  if (length(typ_diagn)) {
    return(paste0(
      indent(),
      "and SHOP_TYP_DIAGN_SEJ_HOSP in (",qu(typ_diagn),")"
    ))
  } else {
    return("")
  }
}














