#' @title Code SQL: Extraction de diagnostics dans SMOD
#'
#' @description  Générateur de code SQL pour l'extraction de diagnostics de la vue `I_SMOD_SERV_MD_CM`.
#'
#' @param debut Date de début de la période d'étude.
#' @param fin Date de fin de la période d'étude.
#' @param diagn `vector` indiquant les codes de diagnostics *CIM9* et/ou *CIM10* à extraire.
#'
#' @return chaîne de caractères, code SQL.
#' @keywords internal
#' @encoding UTF-8
#' @export
#'

query_I_SMOD_SERV_MD_CM <- function(debut, fin, diagn) {
  return(paste0(
    "SELECT
     ID,
     DATE_DX
     FROM (
     SELECT
     DISTINCT\n",
    "SMOD_NO_INDIV_BEN_BANLS as ID,\n",
    "SMOD_DAT_SERV as DATE_DX,\n",
    "SMOD_COD_DIAGN_PRIMR AS DX\n",
    "from PROD.I_SMOD_SERV_MD_CM\n",
    "where SMOD_DAT_SERV between '",debut,"' and '",fin,"'\n",
    "    and SMOD_COD_DIAGN_PRIMR like any (",qu(diagn),")\n",
    "    and SMOD_COD_STA_DECIS = 'PAY'",
    ") AS A;"
  ))
}
