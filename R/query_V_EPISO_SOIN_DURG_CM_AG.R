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
query_V_EPISO_SOIN_DURG_CM <- function(query,debut,fin, diagn, date_dx_var) {
  switch (query,
          "extraction_BDCU"={
            return(paste0(
            "SELECT
              BD_BDCU.SURG_NO_INDIV_BEN_BANLS AS ID,
              BD_BDCU.SURG_NO_EPISO_SOIN_DURG,
              BD_BDCU.SURG_AN_PER_TEMPS --Année période temps
              BD_BDCU.SURG_DHD_EPISO_SOIN_DURG, --Date et heure de début d'un épisode de soins
              BD_BDCU.SURG_DH_DEPAR_USAG_DURG, --DateHeure de départ d'un usager
              BD_BDCU.SURG_DH_PREM_TRIAG_DURG, --Date heure du premier triage d'urgence
              BD_BDCU.SURG_DH_PRIS_CHARG_DURG, --Date et heure de prise en charge
              BD_BDCU.SURG_DH_DEM_ADMIS_DURG, --Date et heure de la demande d'admission en CH
              BD_BDCU.SURG_DH_DECES_DURG, --Date et heure du décès
              BD_BDCU.SURG_COD_PRIOR_TRIAG_DURG, --Code de priorité du premier triage
              BD_BDCU.SURG_COD_AUTNM_PREM_TRIAG_DURG, --Code d'autonomie après le premier triage
              BD_BDCU.SURG_COD_RAIS_VISIT_DURG, --Code de raison de visite en urgence
              BD_BDCU.SURG_CATG_MAJR_DIAGN_DURG, --Catégorie majeure de diagnostic
              BD_BDCU.SURG_COD_DIAGN_MDCAL_CLINQ, --Code de diagnostic médical clinique
              BD_BDCU.SURG_NBR_CNSUL_DURG, --Nombre de consultations en département d'urgence
              BD_BDCU.SURG_COD_MODE_ARRIV_DURG,--Code du mode d'arrivée de l'usager en urgence
              BD_BDCU.SURG_TYP_ORITN_USAG_DEPAR_DURG,--Type d'orientation de l'usager à son départ
              BD_BDCU.SURG_NO_ETAB_MSSS,--Numero de l'etablissement attribue par MSSS \n",
              query_V_EPISO_SOIN_DURG_CM.date_dx_var(date_dx_var)," as DATE_DX,\n",
              "FROM RES_SSS.V_EPISO_SOIN_DURG_CM AS BD_BDCU\n",
              "WHERE ",query_V_EPISO_SOIN_DURG_CM.date_dx_var(date_dx_var)," between to_date('",debut,"') and to_date('",fin,"')\n",
              "AND SURG_COD_PRIOR_TRIAG_DURG IN ('1','2','3','4','5');"))}
  )

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





