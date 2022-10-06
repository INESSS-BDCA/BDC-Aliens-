#' @title SQL_connexion
#'
#' @descriptio Connexion entre R et SQL Teradata.
#'
#' @param uid Identifiant. Si `NULL`, le *user* est demandé lors de l'exécution.
#' @param pwd Mot de passe. Si `NULL`, le mot de passe est demandé lors de l'exécution.
#' @param dsn **D**ata **S**ource **N**ame. Par défaut `'PEI_PRD'`.
#' @param encoding `'latin1'` ou `'UTF-8'`. Encodage de la base de données. Par défaut `'latin1'`.
#'
#' @encoding UTF-8
#' @return Connexion Teradata, sinon `NULL`.
#' @export
#' @examples
#' \dontrun{
#' conn <- SQL_connexion('abc007')
#' conn <- SQL_connexion(uid = 'abc007', pwd = 'MonMotDePasse', dsn = 'PEI_PRD')
#' }
SQL_connexion <- function(uid = NULL, pwd = NULL, dsn = 'PEI_PRD', encoding = 'latin1') {

  ### Demander user s'il n'est pas inscrit
  if (is.null(uid)) {
    uid <- askpass::askpass("Teradata : User")
  }

  ### Demander le mot de passe s'il n'est pas inscrit
  if (is.null(pwd)) {
    pwd <- askpass::askpass("Teradata : Mot de passe")
  }

  ### Effectuer la connexion et conserver message erreur s'il y en a
  is_error <- testthat::capture_error({
    sql_conn <- odbc::dbConnect(odbc::odbc(), dsn, uid = uid, pwd = pwd, encoding = encoding)
  })

  ### Connexion, sinon NULL si erreur
  if (is.null(is_error)) {
    return(sql_conn)
  } else {
    return(NULL)
  }

}
