#' @title SQL_CodeActe
#'
#' @description La fonction `SQL_CodeActe` permet d'extraire les actes entre une date de début et de fin d'étude.
#' La fonction `SQL_CodeActe` utilise plusieurs autres fonctions génériques telles que:
#' `SQL_reperage_cond_med` pour la création de la cohorte. Voir \code{\link{SQL_reperage_cond_med}}, `query_I_SMOD_SERV_MD_CM_AG` pour l'extraction des actes,
#' ainsi que les caractéristiques de bénéficiaire et de l'établissement de soins. Voir \code{\link{query_I_SMOD_SERV_MD_CM_AG}}, et finalement, `query_V_FICH_ID_BEN_CM`
#' pour l'extraction des caractéristiques de bénéficiaire faisant partie de la cohorte, mais n'ayant pas eu d'acte dans la période d'étude. Voir \code{\link{query_V_FICH_ID_BEN_CM}}
#'
#' @inheritParams SQL_reperage_cond_med
#' @inheritParams query_I_SMOD_SERV_MD_CM_AG
#' @inheritParams query_V_FICH_ID_BEN_CM
#' @param cohort `vecteur` indiquant les numéros d'identification des bénéficiaires définisant la cohorte de l'étude.
#' @param verbose `TRUE` ou `FALSE`. Affiche le temps qui a été nécessaire pour extraire les diagnostics. Utile pour suivre le déroulement de l'extraction.
#' @param keep_all `TRUE` ou `FALSE`. Par défaut `FALSE`, soit filter les observations ayant eu un DX et un code d'acte. Si `TRUE` garder toutes les observations, soit ceux qui ont eu un DX et qui ont eu ou pas un code d'acte.
#' Si `cohort` est fournit, keep_all `TRUE` garde toutes les observations demandées par `cohort`. Si keep_all `FALSE` filtrer les observations demandées par `cohort`qui ont eu un code d'acte.
#'
#' @return `data.table` de 32 variables :
#' * **`ID`** : Numéro d'identification du bénéficiaire.
#' * **`DI_Finale`** : Date d'incidence retenue.
#' * **`D_Recent`** : Date la plus récente observée.
#' * **`AnFinan`** : Année financière de l'acte.
#' * **`AnCivil`** : Année civile de l'acte.
#' * **`DateActe`** : Date  de l'acte au format `AAAA-MM-JJ`.
#' * **`CodeActe`** : Actes demandés dans l'argument CodeActe.
#' * **`CoutActe`** : Cout de l'acte.
#' * **`DxActe`** : Diagnostique associé à l'acte.
#' * **`Num_ETAB_USUEL`** : Numéro de l'établissement.
#' * **`CodEntente`** : Code d'entente sert à associer les dispensateurs à un groupe spécifique.
#' * **`SecActiv`** : Identifie de façon unique chacun des secteurs d'activités dans lesquels les différents établissements peuvent oeuvrer.
#' * **`NumDispSp`** : Numéro de dispensateur.
#' * **`SPDisp_smod`** : Spécialité de dispensateur indiqué dans `SMOD`.
#' * **`SPDisp_fip`** : Spécialité de dispensateur indiqué dans `FIP`.
#' * **`NumDispRef`** : Numéro de dispensateur référent.
#' * **`SPDispRefs`** : Spécialité de dispensateur référent.
#' * **`NO_ETAB`** : Numéro de l'établissement.
#' * **`Cat_Etab`** : Catégorie de l'établissement.
#' * **`NOM_ETAB`** : Nom de l'établissement.
#' * **`RSS_Etab`** : Régions sociosanitaires (RSS) de l'établissement.
#' * **`RLS_Etab`** : Réseaux locaux de services (RLS) de l'établissement.
#' * **`NomRSS_Etab`** : Nom régions sociosanitaires (RSS) de l'établissement.
#' * **`NomRLS_Etab`** : Nom réseaux locaux de services (RLS) de l'établissement.
#' * **`Sexe`** : Sexe du bénéficiaire.
#' * **`DatNais`** : Date de naissance du bénéficiaire au format `AAAA-MM-JJ`.
#' * **`datdeces`** : Date de décès du bénéficiaire au format `AAAA-MM-JJ`.
#' * **`Age`** : Age du bénéficiaire calculé jusqu'à la date de l'acte.
#' * **`RSS_Benf`** : Régions sociosanitaires (RSS) du bénéficiaire.
#' * **`RLS_Benef`** : Réseaux locaux de services (RLS) du bénéficiaire.
#' * **`NomRSS_Benef`** : Nom régions sociosanitaires (RSS) du bénéficiaire.
#' * **`NomRLS_Benef`** : Nom réseaux locaux de services (RLS) du bénéficiaire.
#'
#' @encoding UTF-8
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#'DT_final<-SQL_CodeActe(conn=SQL_connexion("ms069a"),
#'                       cohort = NULL,
#'                       debut = "2020-01-01",
#'                       fin = "2020-12-31",
#'                       CodeActe = c('06452','06148','06154','06265','06251','06216','06208'),
#'                       omni_spec="all",
#'                       Dx_table = list(Prolapsus = list(CIM9 = c('5995','6180','6181','6183'),
#'                                                        CIM10 = c('N810','N811','N812','N813'))),
#'
#'                       CIM =  c("CIM9", "CIM10"),
#'                       by_Dx = FALSE,
#'                       date_dx_var = "admis",
#'                       n1 = 30, n2 = 730,
#'                       nDx=0 ,
#'                       date_age="2022-03-31",
#'                       verbose = TRUE,
#'                       keep_all=FALSE)
#'}
SQL_CodeActe<-function(conn=SQL_connexion(),
                       cohort=NULL,
                       debut,
                       fin,
                       CodeActe,
                       omni_spec,
                       Dx_table,
                       CIM,
                       by_Dx,
                       nDx,
                       date_dx_var,
                       n1,n2,
                       date_age,
                       verbose=TRUE,
                       keep_all=FALSE
                       ){
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
                                     statement=query_I_SMOD_SERV_MD_CM_AG(query="extraction_acte",
                                                                          debut=debut,
                                                                          fin=fin,
                                                                          diagn,
                                                                          CodeActe=CodeActe,
                                                                          omni_spec=omni_spec)))

  t2 <- Sys.time()
  if (verbose) {
    cat(" - ","Temps d'exécution",
        " (",round(as.numeric(difftime(t2, t1)), 2),
        " ",attr(difftime(t2, t1), "units"), ")\n",
        sep = "")
  }

  DT_final<-left_join(DX,DT, by="ID")


  # Ajout caractéristiques Benef ####

  if(keep_all==TRUE){

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

  }

  if (verbose && keep_all==FALSE) {
    if (length(cohort)>0) {
      cat("Garder les IDs demandés dans la cohorte\n")
    }
    if (is.null(cohort)){
    cat("Garder les IDs qui ont eu un Dx et un code d'acte\n")
    }
  }


  # Préparation de la base de données finale ####
  if (verbose) {
    cat("Préparation de la base de données finale:\n")
  }

  t1 <- Sys.time()

  if(is.null(cohort) && keep_all==FALSE){
    DT_final<-DT_final %>% filter(!is.na(DateActe))
  }
  else if(length(cohort) > 0 && keep_all==FALSE){
    cohort<-cohort %>% data.frame() %>% rename(ID=".") %>% left_join(DT_final,by="ID") %>% filter(!is.na(DateActe))
    DT_final<-cohort
  }
  else if (length(cohort) > 0 && keep_all== TRUE){
    cohort<-cohort %>% data.frame() %>% rename(ID=".") %>% left_join(DT_final,by="ID")
    DT_final<-cohort
  }
  else{
    DT_final
 }

  t2 <- Sys.time()
  if (verbose) {
    cat(" - ","Temps d'exécution",
        " (",round(as.numeric(difftime(t2, t1)), 2),
        " ",attr(difftime(t2, t1), "units"), ")\n",
        sep = "")
  }

  return(DT_final)

  if (nrow(DT_final) == 0) {
    cat("Aucune observation n'a été trouvée. Veuillez-svp vérifier si les codes d'acte sont corrects")
    return(NULL)
  }

}


