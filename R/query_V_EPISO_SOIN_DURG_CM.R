#' Code SQL: Extraction de diagnostics dans BDCU
#'
#' Générateur de code SQL pour l'extraction de diagnostics de la vue `V_EPISO_SOIN_DURG_CM`.
#'
#' @param debut Date de début de la période d'étude.
#' @param fin Date de fin de la période d'étude.
#' @param diagn `vector` indiquant les codes de diagnostics *CIM9* et/ou *CIM10* à extraire.
#' @param date_dx_var `'admis'` ou `'depar'`. Indique si on utilise la date d'admission ou la date de départ comme date de diagnostic pour l'étude.
#'
#' @return chaîne de caractères, code SQL.
#' @keywords internal
#' @encoding UTF-8
#' @export
#'
query_V_EPISO_SOIN_DURG_CM <- function(debut,fin, diagn, date_dx_var) {

  return(paste0(
    "SELECT
    ID,
    DATE_DX
    FROM (
    SELECT
    DISTINCT
    SURG_NO_INDIV_BEN_BANLS as ID,\n",
    indent("select"),query_V_EPISO_SOIN_DURG_CM.date_dx_var(date_dx_var)," as DATE_DX,\n",
    "SURG_COD_DIAGN_MDCAL_CLINQ AS DX\n",
    "from RES_SSS.V_EPISO_SOIN_DURG_CM\n",
    "where ",query_V_EPISO_SOIN_DURG_CM.date_dx_var(date_dx_var)," between to_date('",debut,"') and to_date('",fin,"')\n",
    indent(),"and SURG_COD_DIAGN_MDCAL_CLINQ like any (",qu(diagn),")",
    ") AS A;"
  ))

}

#' @title query_V_EPISO_SOIN_DURG_CM
#' @description Retourne le nom de la variable à utiliser selon la valeur de la variable `date_dx_var`.
#' @encoding UTF-8
#' @keywords internal
query_V_EPISO_SOIN_DURG_CM.date_dx_var <- function(date_dx_var) {
  if (date_dx_var == "admis") {
    return("SURG_DHD_EPISO_SOIN_DURG")
  } else if (date_dx_var == "depar") {
    return("SURG_DH_DEPAR_USAG_DURG")
  } else {
    stop("query_V_EPISO_SOIN_DURG_CM.date_dx_var : date_dx_var ne contient pas une valeur permise.")
  }
}
