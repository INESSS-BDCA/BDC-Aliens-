#' SQL_diagn
#'
#' SQL_diagn est une fonction intermédiaire utilisée par la fonction \code{\link{SQL_reperage_cond_med}} afin d'extraire des diagnostics cim-9 et cim-10 pour la création d'une cohorte.
#' SQL_diagn fait appel à la fonction: \code{\link{SQL_comorbidity_diagn}}.
#'
#' @inherit SQL_comorbidity_diagn details
#'
#' @inheritParams SQL_comorbidity_diagn
#'
#' @inherit SQL_comorbidity_diagn return
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @inherit SQL_comorbidity_diagn examples
SQL_diagn <- function(
    conn = SQL_connexion(), cohort = NULL, debut, fin,
    Dx_table, CIM = c('CIM9', 'CIM10'),
    dt_source = c('V_DIAGN_SEJ_HOSP_CM', 'V_SEJ_SERV_HOSP_CM',
                  'V_EPISO_SOIN_DURG_CM', 'I_SMOD_SERV_MD_CM'),
    dt_desc = list(V_DIAGN_SEJ_HOSP_CM = 'MEDECHO', V_SEJ_SERV_HOSP_CM = 'MEDECHO',
                   V_EPISO_SOIN_DURG_CM = 'BDCU', I_SMOD_SERV_MD_CM = 'SMOD'),
    date_dx_var = 'admis', typ_diagn = c('A', 'P', 'S', 'D'),
    exclu_diagn = NULL, verbose = TRUE
) {
  ### Même fonction que SQL_comorbidity_diagn
  return(SQL_comorbidity_diagn(
    conn, cohort, debut, fin, Dx_table, CIM,
    dt_source, dt_desc,
    date_dx_var, typ_diagn,
    exclu_diagn, verbose
  ))
}
