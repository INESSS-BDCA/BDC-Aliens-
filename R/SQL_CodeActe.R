#' @title SQL_CodeActe
#'
#' @description La fonction `SQL_CodeActe` permet d'extraire les actes entre une date de début et de fin d'étude.
#' La fonction `SQL_CodeActe` utilise la fonction générique `query_SQL_CodeActe`. Voir \code{\link{query_SQL_CodeActe}}
#'
#' @inheritParams query_SQL_CodeActe
#' @param conn Variable contenant la connexion entre R et Teradata. Voir \code{\link{SQL_connexion}}.
#' @param statement Fait appel à la fonction `query_SQL_CodeActe`. Voir \code{\link{query_SQL_CodeActe}}.
#'
#' @return `data.table` de 31 variables :
#' * **`BenBanls`** : Numéro d'identification du bénéficiaire.
#' * **`AnCivil`** : L'année civil de l'acte.
#' * **`DateActe`** : La date de l'acte au format `AAAA-MM-JJ`.
#' * **`CodeActe`** : Les actes demandés dans l'argument `CodeActe`.
#' * **`CoutActe`** : Le coût de de l'acte.
#' * **`DxActe`**: Num de diagnostique associé à l'acte.
#' * **`LieuDisp`**: Code du lieu de dispensation d'un service (C:Service rendu en cabinet ou à domicile; E:Service rendu en établissement; X:Indéterminé).
#' * **`SecActiv`**: Identifie de façon unique chacun des secteurs d'activités dans lesquels les différents établissements peuvent oeuvrer.
#' * **`Num_ETAB_USUEL`**: Numéro de l'établissement.
#' * **`CodEntente`**: Code d'entente. Il utile pour associer les dispensateurs à un groupe spécifique.
#' * **`Sexe`**: Sexe du bénéficiaire.
#' * **`DatNais`**: Date de naissance du bénéficiaire au format `AAAA-MM-JJ`.
#' * **`datdeces`**: Date de décès du bénéficiaire au format `AAAA-MM-JJ`.
#' * **`Age`**: Age du bénéficiaire calculé jusqu'à la date de l'acte.
#' * **`NumDispSp`**: Numéro de dispensateur.
#' * **`SPDisps`**: Spécialité de dispensateur.
#' * **`NumDispRef`**: Numéro de dispensateur référent.
#' * **`SPDispRefs`**: Spécialité de dispensateur référent.
#' * **`NO_ETAB`**: Numéro de l'établissement.
#' * **`Grp_Cat_Etab`**: Catégorie de l'établissement.
#' * **`Cat_Etab`** Catégorie de l'établissement: .
#' * **`Nom_Grp_Cat_Etab`**: Nom de la catégorie de l'établissement.
#' * **`NOM_ETAB`**: Nom de l'établissement.
#' * **`RSS_Benf`**: Régions sociosanitaires (RSS) du bénéficiaire.
#' * **`RLS_Benef`**: Réseaux locaux de services (RLS) du bénéficiaire.
#' * **`NomRLS_Benef`**: Nom réseaux locaux de services (RLS) du bénéficiaire.
#' * **`NomRSS_Benef`**: Nom régions sociosanitaires (RSS) du bénéficiaire.
#' * **`RSS_Etab`**: Régions sociosanitaires (RSS) de l'établissement.
#' * **`RLS_Etab`**: Réseaux locaux de services (RLS) de l'établissement.
#' * **`NomRLS_Etab`**: Nom réseaux locaux de services (RLS) de l'établissement.
#' * **`NomRSS_Etab`**: Nom régions sociosanitaires (RSS) de l'établissement.
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' \dontrun{
#'tmp<-SQL_CodeActe(conn=conn,
#'                  statement=query_SQL_CodeActe(),
#'                  DateDebut="2019-03-01",
#'                  DateFin="2019-05-31",
#'                  CodeActe=c(07122, 07237, 07800, 07089, 0780))
#' }
#'
SQL_CodeActe<-function(conn,statement,DateDebut, DateFin,CodeActe){
  DT<- as.data.table(odbc::dbGetQuery(
    conn = SQL_connexion(),
    statement = query_SQL_CodeActe(DateDebut, DateFin,CodeActe)

  ))
  return(DT)
  if (nrow(DT)) {

  } else {
    cat("Aucune observation n'a été trouvée. Veuillez-svp vérifier si les codes d'acte sont corrects")
    return(NULL)
  }
}
