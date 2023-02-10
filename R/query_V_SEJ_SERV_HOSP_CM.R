#' Code SQL: Extraction de diagnostics dans MEDECHO-sej
#'
#' Générateur de code SQL pour l'extraction de diagnostics de la vue `V_SEJ_SERV_HOSP_CM`.
#'
#' @param debut_cohort Date de début de la période d'étude.
#' @param fin_cohort Date de fin de la période d'étude.
#' @param diagn `vector` indiquant les codes de diagnostics *CIM9* et/ou *CIM10* à extraire.
#' @param date_dx_var `'admis'` ou `'depar'`. Indique si on utilise la date d'admission ou la date de départ comme date de diagnostic pour l'étude.
#'
#' @return chaîne de caractères, code SQL.
#' @keywords internal
#' @encoding UTF-8
#' @export
query_V_SEJ_SERV_HOSP_CM <- function(debut_cohort,fin_cohort, diagn, date_dx_var) {

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
    "from RES_SSS.V_SEJ_SERV_HOSP_CM\n",
    "where ",query_V_DIAGN_SEJ_HOSP_CM.date_dx_var(date_dx_var)," between '",debut_cohort,"' and '",fin_cohort,"'\n",
    indent(),"and SHOP_COD_DIAGN_MDCAL_CLINQ like any (",qu(diagn),")",
    ") AS A;"
  ))

}
