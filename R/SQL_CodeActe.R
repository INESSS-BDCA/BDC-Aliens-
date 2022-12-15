#' @title SQL_CodeActe
#'
#' @description La fonction `SQL_CodeActe` permet d'extraire les actes entre une date de début et de fin d'étude.
#' La fonction `SQL_CodeActe` utilise plusieurs autres fonctions génériques telles que:
#' `SQL_reperage_cond_med` Voir \code{\link{SQL_reperage_cond_med}}, `query_SQL_CodeActe` Voir \code{\link{query_SQL_CodeActe}}, et `query_V_FICH_ID_BEN_CM` Voir \code{\link{query_V_FICH_ID_BEN_CM}}
#'
#' @inheritParams SQL_reperage_cond_med
#' @inheritParams query_SQL_CodeActe
#' @inheritParams query_V_FICH_ID_BEN_CM
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
#'DT_final<-SQL_CodeActe(conn=SQL_connexion(),
#'                       debut = "2021-04-01",
#'                       fin = "2022-03-31",
#'                       CodeActe = c('06452','06148','06154','06265','06251','06216','06208','06164','06270','06276','06274','06273','06913','06099',
#'                                    '06400','06405','06406','06408','06420','06410','06411','06414','6421','6426','20017'),
#'                       Dx_table = list(Prolapsus = list(CIM9 = c('5995','6180','6181','6183','6184','6185','6186','6187','6188','6189','6204'),
#'                                                        CIM10 = c('N810','N811','N812','N813','N814','N815','N816','N818','N819','N993','N834','O345'))),
#'
#'                       CIM =  c("CIM9", "CIM10"),
#'                       by_Dx = FALSE,
#'                       date_dx_var = "admis",
#'                       n1 = 30, n2 = 730,
#'                       nDx=0 ,
#'                       date_age="2022-03-31",
#'                       verbose = TRUE)
#'
SQL_CodeActe<-function(conn=SQL_connexion(),
                       debut,
                       fin,
                       CodeActe,
                       Dx_table,
                       CIM,
                       by_Dx,
                       nDx,
                       date_dx_var,
                       n1,n2,
                       date_age,
                       verbose){
  # Création de la cohorte en utilisant les Dx cim-9 et/ou cim-10 ####
  if (verbose) {
    cat("Création de la cohorte:\n")
  }

  DX<-SQL_reperage_cond_med (
    conn = conn,
    debut=debut,
    fin=fin,
    Dx_table=Dx_table,
    CIM = CIM,
    nDx = nDx,
    by_Dx = by_Dx,
    date_dx_var=date_dx_var,
    n1 = n1,
    n2 = n2,
    verbose = verbose

  )

  # Extraction d'acte ####
  if (verbose) {
    cat("Extraction des actes:\n")
  }

  t1 <- Sys.time()

  DT<-as.data.table(odbc::dbGetQuery(conn=conn,
                                     statement=query_SQL_CodeActe(debut=debut,fin=fin,CodeActe=CodeActe)))

  t2 <- Sys.time()
  if (verbose) {
    cat(" - ","Temps d'exécution",
        " (",round(as.numeric(difftime(t2, t1)), 2),
        " ",attr(difftime(t2, t1), "units"), ")\n",
        sep = "")
  }

  # Ajout caractéristiques Benef ####
  if (verbose) {
    cat("Ajout des caractéristiques des bénéficiaires qui n'ont pas eu d'acte dans la période d'étude:\n")
  }
  t1 <- Sys.time()
  DT1<-as.data.table(odbc::dbGetQuery(conn=conn,
                                      statement=query_V_FICH_ID_BEN_CM(debut=debut, fin=fin, date_age=date_age)
  ))
  t2 <- Sys.time()
  if (verbose) {
    cat(" - ","Temps d'exécution",
        " (",round(as.numeric(difftime(t2, t1)), 2),
        " ",attr(difftime(t2, t1), "units"), ")\n",
        sep = "")
  }

  # Préparation de la base de données finale ####
  if (verbose) {
    cat("Préparation de la base de données finale:\n")
  }

  t1 <- Sys.time()
  DT_final<-left_join(DX,DT, by="ID")

  DT_final <- left_join(DT_final, DT1, by="ID") %>%
    mutate(Sexe=ifelse(is.na(Sexe.x), Sexe.y, Sexe.x),
           DatNais =as.Date(ifelse(is.na(DatNais.x), DatNais.y, DatNais.x),origin="1970-01-01"),
           DatDeces=as.Date(ifelse(is.na(DatDeces.x), DatDeces.y, DatDeces.x),origin="1970-01-01"),
           Age=ifelse(is.na(as.integer(as.character(Age.x))), as.integer(as.character(Age.y)), as.integer(as.character(Age.x))),
           RSS_Benf=ifelse(is.na(RSS_Benf.x), RSS_Benf.y, RSS_Benf.x),
           NomRSS_Benef=ifelse(is.na(NomRSS_Benef.x), NomRSS_Benef.y, NomRSS_Benef.x),
           RLS_Benef=ifelse(is.na(RLS_Benef.x), RLS_Benef.y, RLS_Benef.x),
           NomRLS_Benef=ifelse(is.na(NomRLS_Benef.x), NomRLS_Benef.y, NomRLS_Benef.x)) %>%
    mutate(NumDispSp=bit64::as.integer64(as.character(NumDispSp)),
           NumDispRef=bit64::as.integer64(as.character(NumDispRef))) %>%
    select(-c(Sexe.x,Sexe.y,DatNais.x,DatNais.y,DatDeces.y, DatDeces.x,Age.y, Age.x,RSS_Benf.y, RSS_Benf.x,NomRSS_Benef.y, NomRSS_Benef.x,RLS_Benef.y, RLS_Benef.x,NomRLS_Benef.y, NomRLS_Benef.x))

  t2 <- Sys.time()
  if (verbose) {
    cat(" - ","Temps d'exécution",
        " (",round(as.numeric(difftime(t2, t1)), 2),
        " ",attr(difftime(t2, t1), "units"), ")\n",
        sep = "")
  }

  return(DT_final)
  if (nrow(DT_final)) {

  } else {
    cat("Aucune observation n'a été trouvée. Veuillez-svp vérifier si les codes d'acte sont corrects")
    return(NULL)
  }
}


