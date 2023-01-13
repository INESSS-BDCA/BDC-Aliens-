#' @title user_I_SMOD_SERV_MD_CM_AG
#'
#' @description La fonction `user_I_SMOD_SERV_MD_CM_AG` est destinée aux utilisateurs. Elle permet d'exécuter la rêquette SQL retournée par la fonction `query_I_SMOD_SERV_MD_CM_AG`.\cr
#'
#' Elle permet: \cr
#' 1) d'extraire les variables pertinantes de la base de données SMOD.\cr
#' 2) d'extraire les variables pertinantes de la base de données SMOD, jumlées avec d'autres bases de données pour rajouter les caractéristiques de dispensateur, du bénéficiaire et de l'établissement de soins.\cr
#' 3) d'extraire les dates de visites (services).\cr
#' 4) de calculer le nombre d'acte par bénéficiaire entre la date de début et de fin d'une étude.\cr
#'
#' La fonction `user_I_SMOD_SERV_MD_CM_AG` utilise plusieurs autres fonctions génériques telles que:\cr
#'
#' `SQL_reperage_cond_med` pour la création de la cohorte. Voir \code{\link{SQL_reperage_cond_med}}\cr
#' `query_V_FICH_ID_BEN_CM` pour l'extraction des caractéristiques de bénéficiaire. Voir \code{\link{query_V_FICH_ID_BEN_CM}}
#'
#' @inheritParams SQL_reperage_cond_med
#' @inheritParams query_I_SMOD_SERV_MD_CM_AG
#' @inheritParams query_V_FICH_ID_BEN_CM
#' @param cohort `vecteur` indiquant les numéros d'identification des bénéficiaires définisant la cohorte de l'étude.
#' @param verbose `TRUE` ou `FALSE`. Affiche le temps qui a été nécessaire pour extraire les diagnostics. Utile pour suivre le déroulement de l'extraction.
#' @param keep_all `TRUE` ou `FALSE`. Par défaut `FALSE`, soit filter les observations ayant eu un DX et un code d'acte. Si `TRUE` garder toutes les observations, soit ceux qui ont eu un DX et qui ont eu ou pas un code d'acte.
#' Si `cohort` est fournit, keep_all `TRUE` garde toutes les observations demandées par `cohort`. Si keep_all `FALSE` filtrer les observations demandées par `cohort`qui ont eu un code d'acte.
#'
#' @return
#' **Extraction des variables pertinantes de SMOD:**
#' `data.table` de 19 variables si la cohorte est fournie par l'analyste, sinon 21 variables s'il y a une création de cohorte.
#'
#' **Extraction des variables pertinantes de SMOD combinées avec d'autres bases de données:**
#' `data.table` de 36 variables si la cohorte est fournie par l'analyste, sinon 38 variables s'il y a une création de cohorte.
#'
#' **Extraction des dates de visites:**
#' `data.table` de 10 variables si la cohorte est fournie par l'analyste, sinon 12 variables s'il y a une création de cohorte.
#'
#' **Calcul du nombre d'acte par bénéficiaire:**
#' `data.table` de 10 variables si la cohorte est fournie par l'analyste, sinon 12 variables s'il y a une création de cohorte.
#'
#' @encoding UTF-8
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#'
#' **Extraction des variables pertinantes de SMOD:**
#' Si la cohorte est fournie par l'analyste:
#'
#' DT_final<-user_I_SMOD_SERV_MD_CM_AG(task="extraction_SMOD",
#'                       conn=SQL_connexion("ms069a"),
#'                       cohort = cohort,
#'                       debut = "2020-01-01",
#'                       fin = "2020-12-31",
#'                       CodeActe = c('06452','06148','06154','06265','06251','06216','06208'),
#'                       code_stat_decis="PAY-PPY",
#'                       verbose = TRUE,
#'                       keep_all=FALSE)
#'
#' Si on veut créer une nouvelle cohorte: (ceci est applicable pour les autres task)
#'
#' DT_final<-user_I_SMOD_SERV_MD_CM_AG(task="extraction_SMOD",
#'                       conn=SQL_connexion("ms069a"),
#'                       cohort = NULL,
#'                       debut = "2020-01-01",
#'                       fin = "2020-12-31",
#'                       CodeActe = c('06452','06148','06154','06265','06251','06216','06208'),
#'                       code_stat_decis="PAY-PPY",
#'
#'                       Dx_table = list(Prolapsus = list(CIM9 = c('5995','6180','6181','6183'),CIM10 = c('N810','N811','N812','N813'))),
#'                       CIM =  c("CIM9", "CIM10"),
#'                       by_Dx = FALSE,
#'                       date_dx_var = "admis",
#'                       n1 = 30, n2 = 730,
#'                       nDx=0 ,
#'
#'                       verbose = TRUE,
#'                       keep_all=FALSE)
#'
#' **Extraction des variables pertinantes de SMOD combinées avec d'autres bases de données:**
#' DT_final<-user_I_SMOD_SERV_MD_CM_AG(task="extraction_SMOD_combin",
#'                       conn=SQL_connexion("ms069a"),
#'                       cohort = cohort,
#'                       debut = "2020-01-01",
#'                       fin = "2020-12-31",
#'                       CodeActe = c('06452','06148','06154','06265','06251','06216','06208'),
#'                       code_stat_decis="PAY-PPY",
#'                       mni_spec="all",
#'                       catg_etab="all",
#'                       date_age="2020-07-01",
#'                       verbose = TRUE,
#'                       keep_all=FALSE)
#'
#' **Extraction des dates de visites:**
#' DT_final<-user_I_SMOD_SERV_MD_CM_AG(task="date_visite",
#'                       conn=SQL_connexion("ms069a"),
#'                       cohort = cohort,
#'                       debut = "2020-01-01",
#'                       fin = "2020-12-31",
#'                       CodeActe = c('06452','06148','06154','06265','06251','06216','06208'),
#'                       code_stat_decis="PAY-PPY",
#'                       omni_spec="all",
#'                       catg_etab="all",
#'                       date_age="2020-07-01",
#'                       verbose = TRUE,
#'                       keep_all=FALSE)
#'
#' **Calcul du nombre d'acte par bénéficiaire:**
#' DT_final<-user_I_SMOD_SERV_MD_CM_AG(task="calcul_Nb_acte",
#'                       conn=SQL_connexion("ms069a"),
#'                       cohort = cohort,
#'                       debut = "2020-01-01",
#'                       fin = "2020-12-31",
#'                       CodeActe = c('06452','06148','06154','06265','06251','06216','06208'),
#'                       code_stat_decis="PAY-PPY",
#'                       omni_spec="all",
#'                       catg_etab="all",
#'                       date_age="2020-07-01",
#'                       verbose = TRUE,
#'                       keep_all=FALSE)
#'
#'}


user_I_SMOD_SERV_MD_CM_AG<-function(task,
                                    conn=SQL_connexion(),
                                    cohort=NULL,
                                    debut,
                                    fin,
                                    CodeActe,
                                    omni_spec,
                                    catg_etab,
                                    code_stat_decis,
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
  switch (task,
          "extraction_SMOD"={
            # Création de la cohorte en utilisant les Dx cim-9 et/ou cim-10 et Extraction d'acte ####
            if(is.null(cohort) && !is.null(Dx_table)){
              if (verbose) {
                cat("Création de la cohorte avec la fonction SQL_reperage_cond_med:\n")
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
                verbose = verbose,
                code_stat_decis=code_stat_decis)

              # Extraction d'acte ####
              if (verbose) {
                cat("Extraction des actes avec la fonction query_I_SMOD_SERV_MD_CM_AG:\n")
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_I_SMOD_SERV_MD_CM_AG(query="extraction_SMOD",
                                                                                                  debut=debut,
                                                                                                  fin=fin,
                                                                                                  diagn,
                                                                                                  CodeActe=CodeActe,
                                                                                                  omni_spec=omni_spec,
                                                                                                  catg_etab = catg_etab,
                                                                                                  code_stat_decis=code_stat_decis)))

              t2 <- Sys.time()
              if (verbose) {
                cat(" - ","Temps d'exécution",
                    " (",round(as.numeric(difftime(t2, t1)), 2),
                    " ",attr(difftime(t2, t1), "units"), ")\n",
                    sep = "")
              }

              DT_final<-left_join(DX,DT, by="ID")
            }
            else {
              # Extraction d'acte ####
              if (verbose) {
                cat("Extraction des actes avec la fonction query_I_SMOD_SERV_MD_CM_AG pour la cohorte fournie par l'analyste:\n")
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_I_SMOD_SERV_MD_CM_AG(query="extraction_SMOD",
                                                                                                  debut=debut,
                                                                                                  fin=fin,
                                                                                                  diagn,
                                                                                                  CodeActe=CodeActe,
                                                                                                  omni_spec=omni_spec,
                                                                                                  catg_etab = catg_etab,
                                                                                                  code_stat_decis=code_stat_decis)))

              t2 <- Sys.time()
              if (verbose) {
                cat(" - ","Temps d'exécution",
                    " (",round(as.numeric(difftime(t2, t1)), 2),
                    " ",attr(difftime(t2, t1), "units"), ")\n",
                    sep = "")
              }

              DT_final<-DT
            }


            # Préparation de la base de données finale ####
            if (verbose) {
              cat("Préparation de la base de données finale:\n")
            }

            if (verbose && keep_all==FALSE) {
              if (length(cohort)>0) {
                cat("Garder les IDs de la cohorte fournie par l'analyste qui ont eu un Dx et un code d'act\n")
              }
              if (is.null(cohort)){
                cat("Garder les IDs de la cohorte crée qui ont eu un Dx et un code d'acte\n")
              }
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

            # Add Etiquettes SMOD_COD_ROLE, SMOD_COD_SPEC, ETAB_COD_SECT_ACTIV_ETAB, SMOD_COD_ENTEN, Dx ####
            DT_final<-left_join(DT_final,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_ROLE,Desc_SMOD_COD_ROLE) %>% mutate(SMOD_COD_ROLE=as.character(SMOD_COD_ROLE)) %>% na.omit(),by="SMOD_COD_ROLE") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_SPEC,Desc_SMOD_COD_SPEC) %>% na.omit(),by="SMOD_COD_SPEC") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(ETAB_COD_SECT_ACTIV_ETAB,Desc_ETAB_COD_SECT_ACTIV_ETAB) %>% na.omit(),by="ETAB_COD_SECT_ACTIV_ETAB") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_ENTEN,Desc_SMOD_COD_ENTEN) %>% mutate(SMOD_COD_ENTEN=as.character(SMOD_COD_ENTEN)) %>% na.omit(),by="SMOD_COD_ENTEN") %>%
              mutate(DxActe=trimws(DxActe))


            cim9_cim10<-intersect(DT_final[["DxActe"]], c(intersect(RequeteGeneriqueBDCA::Etiquettes$CIM9,RequeteGeneriqueBDCA::Etiquettes$CIM10)))

            cim9 <- RequeteGeneriqueBDCA::Etiquettes$CIM9
            cim10 <- RequeteGeneriqueBDCA::Etiquettes$CIM10
            cim9desc <- RequeteGeneriqueBDCA::Etiquettes$Desc_CIM9
            cim10desc <- RequeteGeneriqueBDCA::Etiquettes$Desc_CIM10
            DxActe <- c(cim9, cim10) %>% data.frame()%>% rename(DxActe=".")
            Desc_DxActe <- c(cim9desc, cim10desc) %>% data.frame()%>% rename(Desc_DxActe=".")
            DT_DxActe<-cbind(DxActe,Desc_DxActe)

            if(length(cim9_cim10)==0){
              DT_final<-left_join(DT_final,DT_DxActe,by="DxActe") %>%
                mutate(NumDispSp=bit64::as.integer64(as.character(NumDispSp)),
                       NumDispRef=bit64::as.integer64(as.character(NumDispRef)))
            }
            else{
              DT_final<-left_join(DT_final,DT_DxActe,by="DxActe")
              DT_final<-DT_final %>% mutate(Desc_DxActe=ifelse(substr(DxActe,1,1)=="V",NA,Desc_DxActe)) %>%
                mutate(NumDispSp=bit64::as.integer64(as.character(NumDispSp)),
                       NumDispRef=bit64::as.integer64(as.character(NumDispRef)))
              warning(paste0("Il y a des Dx qui commencent avec la lettre 'V' qui pourraient être des cim9 ou cim10. L'étiquette de ces Dx n'a pas été rajoutée.",
                             "Veuillez-svp les rajouter manuellement en utilisant la BD 'RequeteGeneriqueBDCA::Etiquettes'.", "Voici les Dx à vérifier: ", paste(cim9_cim10, collapse = ","),"\n"))
            }

            t2 <- Sys.time()
            if (verbose) {
              cat(" - ","Temps d'exécution",
                  " (",round(as.numeric(difftime(t2, t1)), 2),
                  " ",attr(difftime(t2, t1), "units"), ")\n",
                  sep = "")
            }

            if (verbose) {
              if (nrow(DT_final) == 0) {
                cat("Aucune observation n'a été trouvée. Veuillez-svp vérifier si les Dx et les codes d'acte sont corrects ou si les Dx match avec les actes.")
                return(NULL)
              }
            }

            return(DT_final)

          },
          "extraction_SMOD_combin"={
            # Création de la cohorte en utilisant les Dx cim-9 et/ou cim-10 et Extraction d'acte ####
            if(is.null(cohort) && !is.null(Dx_table)){
              if (verbose) {
                cat("Création de la cohorte avec la fonction SQL_reperage_cond_med:\n")
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
                verbose = verbose,
                code_stat_decis=code_stat_decis)

              # Extraction d'acte ####
              if (verbose) {
                cat("Extraction des actes avec la fonction query_I_SMOD_SERV_MD_CM_AG:\n")
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_I_SMOD_SERV_MD_CM_AG(query="extraction_SMOD_combin",
                                                                                                  debut=debut,
                                                                                                  fin=fin,
                                                                                                  diagn,
                                                                                                  CodeActe=CodeActe,
                                                                                                  omni_spec=omni_spec,
                                                                                                  catg_etab = catg_etab,
                                                                                                  code_stat_decis=code_stat_decis)))

              t2 <- Sys.time()
              if (verbose) {
                cat(" - ","Temps d'exécution",
                    " (",round(as.numeric(difftime(t2, t1)), 2),
                    " ",attr(difftime(t2, t1), "units"), ")\n",
                    sep = "")
              }

              DT_final<-left_join(DX,DT, by="ID")
            }
            else {
              # Extraction d'acte ####
              if (verbose) {
                cat("Extraction des actes avec la fonction query_I_SMOD_SERV_MD_CM_AG pour la cohorte fournie par l'analyste:\n")
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_I_SMOD_SERV_MD_CM_AG(query="extraction_SMOD_combin",
                                                                                                  debut=debut,
                                                                                                  fin=fin,
                                                                                                  diagn,
                                                                                                  CodeActe=CodeActe,
                                                                                                  omni_spec=omni_spec,
                                                                                                  catg_etab = catg_etab,
                                                                                                  code_stat_decis=code_stat_decis)))

              t2 <- Sys.time()
              if (verbose) {
                cat(" - ","Temps d'exécution",
                    " (",round(as.numeric(difftime(t2, t1)), 2),
                    " ",attr(difftime(t2, t1), "units"), ")\n",
                    sep = "")
              }

              DT_final<-DT
            }


            # Ajout caractéristiques des Benef n'ayant pas eu de code d'acte ####

            if(keep_all==TRUE){

              if (verbose) {
                cat("Ajout des caractéristiques des bénéficiaires avec la fonction query_V_FICH_ID_BEN_CM:\n")
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


            # Préparation de la base de données finale ####
            if (verbose) {
              cat("Préparation de la base de données finale:\n")
            }

            if (verbose && keep_all==FALSE) {
              if (length(cohort)>0) {
                cat("Garder les IDs de la cohorte fournie par l'analyste qui ont eu un Dx et un code d'act\n")
              }
              if (is.null(cohort)){
                cat("Garder les IDs de la cohorte crée qui ont eu un Dx et un code d'acte\n")
              }
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

            # Add Etiquettes SMOD_COD_ROLE, SMOD_COD_SPEC, ETAB_COD_SECT_ACTIV_ETAB, SMOD_COD_ENTEN, Dx ####
            DT_final<-left_join(DT_final,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_ROLE,Desc_SMOD_COD_ROLE) %>% mutate(SMOD_COD_ROLE=as.character(SMOD_COD_ROLE)) %>% na.omit(),by="SMOD_COD_ROLE") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_SPEC,Desc_SMOD_COD_SPEC) %>% na.omit(),by="SMOD_COD_SPEC") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(ETAB_COD_SECT_ACTIV_ETAB,Desc_ETAB_COD_SECT_ACTIV_ETAB) %>% na.omit(),by="ETAB_COD_SECT_ACTIV_ETAB") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_ENTEN,Desc_SMOD_COD_ENTEN) %>% mutate(SMOD_COD_ENTEN=as.character(SMOD_COD_ENTEN)) %>% na.omit(),by="SMOD_COD_ENTEN") %>%
              mutate(DxActe=trimws(DxActe))


            cim9_cim10<-intersect(DT_final[["DxActe"]], c(intersect(RequeteGeneriqueBDCA::Etiquettes$CIM9,RequeteGeneriqueBDCA::Etiquettes$CIM10)))

            cim9 <- RequeteGeneriqueBDCA::Etiquettes$CIM9
            cim10 <- RequeteGeneriqueBDCA::Etiquettes$CIM10
            cim9desc <- RequeteGeneriqueBDCA::Etiquettes$Desc_CIM9
            cim10desc <- RequeteGeneriqueBDCA::Etiquettes$Desc_CIM10
            DxActe <- c(cim9, cim10) %>% data.frame()%>% rename(DxActe=".")
            Desc_DxActe <- c(cim9desc, cim10desc) %>% data.frame()%>% rename(Desc_DxActe=".")
            DT_DxActe<-cbind(DxActe,Desc_DxActe)

            if(length(cim9_cim10)==0){
              DT_final<-left_join(DT_final,DT_DxActe,by="DxActe") %>%
                mutate(NumDispSp=bit64::as.integer64(as.character(NumDispSp)),
                       NumDispRef=bit64::as.integer64(as.character(NumDispRef)))
            }
            else{
              DT_final<-left_join(DT_final,DT_DxActe,by="DxActe")
              DT_final<-DT_final %>% mutate(Desc_DxActe=ifelse(substr(DxActe,1,1)=="V",NA,Desc_DxActe)) %>%
                mutate(NumDispSp=bit64::as.integer64(as.character(NumDispSp)),
                       NumDispRef=bit64::as.integer64(as.character(NumDispRef)))
              warning(paste0("Il y a des Dx qui commencent avec la lettre 'V' qui pourraient être des cim9 ou cim10. L'étiquette de ces Dx n'a pas été rajoutée.",
                             "Veuillez-svp les rajouter manuellement en utilisant la BD 'RequeteGeneriqueBDCA::Etiquettes'.", "Voici les Dx à vérifier: ", paste(cim9_cim10, collapse = ","),"\n"))
            }

            t2 <- Sys.time()
            if (verbose) {
              cat(" - ","Temps d'exécution",
                  " (",round(as.numeric(difftime(t2, t1)), 2),
                  " ",attr(difftime(t2, t1), "units"), ")\n",
                  sep = "")
            }

            if (verbose) {
              if (nrow(DT_final) == 0) {
                cat("Aucune observation n'a été trouvée. Veuillez-svp vérifier si les Dx et les codes d'acte sont corrects ou si les Dx match avec les actes.")
                return(NULL)
              }
            }

            return(DT_final)

          },
          "date_visite"={
            # Création de la cohorte en utilisant les Dx cim-9 et/ou cim-10 ####
            if(is.null(cohort) && !is.null(Dx_table)){
              if (verbose) {
                cat("Création de la cohorte avec la fonction SQL_reperage_cond_med:\n")
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
                verbose = verbose,
                code_stat_decis=code_stat_decis)

              # extraction des dates de visites ####
              if (verbose) {
                cat("Extraction des dates de vistes (services):\n")
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_I_SMOD_SERV_MD_CM_AG(query="date_visite",
                                                                                                  debut=debut,
                                                                                                  fin=fin,
                                                                                                  diagn,
                                                                                                  CodeActe=CodeActe,
                                                                                                  omni_spec=omni_spec,
                                                                                                  catg_etab = catg_etab,
                                                                                                  code_stat_decis=code_stat_decis)))

              t2 <- Sys.time()
              if (verbose) {
                cat(" - ","Temps d'exécution",
                    " (",round(as.numeric(difftime(t2, t1)), 2),
                    " ",attr(difftime(t2, t1), "units"), ")\n",
                    sep = "")
              }

              DT_final<-left_join(DX,DT, by="ID")
            }

            else {
              # extraction des dates de visites ###
              if (verbose) {
                cat("Extraction des dates de visites avec la fonction query_I_SMOD_SERV_MD_CM_AG pour la cohorte fournie par l'analyste:\n")
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_I_SMOD_SERV_MD_CM_AG(query="date_visite",
                                                                                                  debut=debut,
                                                                                                  fin=fin,
                                                                                                  diagn,
                                                                                                  CodeActe=CodeActe,
                                                                                                  omni_spec=omni_spec,
                                                                                                  catg_etab = catg_etab,
                                                                                                  code_stat_decis=code_stat_decis)))

              t2 <- Sys.time()
              if (verbose) {
                cat(" - ","Temps d'exécution",
                    " (",round(as.numeric(difftime(t2, t1)), 2),
                    " ",attr(difftime(t2, t1), "units"), ")\n",
                    sep = "")
              }

              DT_final<-DT
            }


            # Ajout caractéristiques Benef ####

            if(keep_all==TRUE){

              if (verbose) {
                cat("Ajout des caractéristiques des bénéficiaires qui ont eu un Dx et qui ont eu ou non une date de visite dans la période d'étude:\n")
              }
            } else{
              if (verbose) {
                cat("Ajout des caractéristiques des bénéficiaires qui ont eu un Dx et une date de visite dans la période d'étude:\n")
              }
            }

            t1 <- Sys.time()
            DT1<-as.data.table(odbc::dbGetQuery(conn=conn,
                                                statement=query_V_FICH_ID_BEN_CM(debut=debut, fin=fin, date_age=date_age)))
            t2 <- Sys.time()
            if (verbose) {
              cat(" - ","Temps d'exécution",
                  " (",round(as.numeric(difftime(t2, t1)), 2),
                  " ",attr(difftime(t2, t1), "units"), ")\n",
                  sep = "")
            }

            DT_final <- left_join(DT_final, DT1, by="ID")


            # Préparation de la base de données finale ####
            if (verbose) {
              cat("Préparation de la base de données finale:\n")
            }

            if (verbose && keep_all==FALSE) {
              if (length(cohort)>0) {
                cat("Garder les IDs demandés dans la cohorte\n")
              }
              if (is.null(cohort)){
                cat("Garder les IDs qui ont eu un Dx et une date de visite\n")
              }
            }

            t1 <- Sys.time()

            if(is.null(cohort) && keep_all==FALSE){
              DT_final<-DT_final %>% filter(!is.na(Date_visite))
            }
            else if(length(cohort) > 0 && keep_all==FALSE){
              cohort<-cohort %>% data.frame() %>% rename(ID=".") %>% left_join(DT_final,by="ID") %>% filter(!is.na(Date_visite))
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

            if (verbose) {
              if (nrow(DT_final) == 0) {
                cat("Aucune observation n'a été trouvée. Veuillez-svp vérifier si les Dx et les codes d'acte sont corrects ou si les Dx match avec les actes")
                return(NULL)
              }
            }

            return(DT_final)

          },
          "calcul_Nb_acte"={
            # Création de la cohorte en utilisant les Dx cim-9 et/ou cim-10 ####
            if(is.null(cohort) && !is.null(Dx_table)){
              if (verbose) {
                cat("Création de la cohorte avec la fonction SQL_reperage_cond_med:\n")
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
                verbose = verbose,
                code_stat_decis=code_stat_decis)

              # Extraction d'acte ####
              if (verbose) {
                cat("Extraction des actes et calcul du Nb actes avec la fonction query_I_SMOD_SERV_MD_CM_AG:\n")
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_I_SMOD_SERV_MD_CM_AG(query="calcul_Nb_acte",
                                                                                                  debut=debut,
                                                                                                  fin=fin,
                                                                                                  diagn,
                                                                                                  CodeActe=CodeActe,
                                                                                                  omni_spec=omni_spec,
                                                                                                  catg_etab = catg_etab,
                                                                                                  code_stat_decis=code_stat_decis)))

              t2 <- Sys.time()
              if (verbose) {
                cat(" - ","Temps d'exécution",
                    " (",round(as.numeric(difftime(t2, t1)), 2),
                    " ",attr(difftime(t2, t1), "units"), ")\n",
                    sep = "")
              }

              DT_final<-left_join(DX,DT, by="ID")
            }

            else {
              # Extraction d'acte ####
              if (verbose) {
                cat("Extraction des actes et calcul du Nb actes avec la fonction query_I_SMOD_SERV_MD_CM_AG:\n")
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_I_SMOD_SERV_MD_CM_AG(query="calcul_Nb_acte",
                                                                                                  debut=debut,
                                                                                                  fin=fin,
                                                                                                  diagn,
                                                                                                  CodeActe=CodeActe,
                                                                                                  omni_spec=omni_spec,
                                                                                                  catg_etab = catg_etab,
                                                                                                  code_stat_decis=code_stat_decis)))

              t2 <- Sys.time()
              if (verbose) {
                cat(" - ","Temps d'exécution",
                    " (",round(as.numeric(difftime(t2, t1)), 2),
                    " ",attr(difftime(t2, t1), "units"), ")\n",
                    sep = "")
              }

              DT_final<-DT
            }

            # Ajout caractéristiques Benef ####

            if(keep_all==TRUE){

              if (verbose) {
                cat("Ajout des caractéristiques des bénéficiaires qui ont eu un Dx et qui ont eu ou non un acte dans la période d'étude:\n")
              }
            } else{
              if (verbose) {
                cat("Ajout des caractéristiques des bénéficiaires qui ont eu un Dx et un acte dans la période d'étude:\n")
              }
            }

            t1 <- Sys.time()
            DT1<-as.data.table(odbc::dbGetQuery(conn=conn,
                                                statement=query_V_FICH_ID_BEN_CM(debut=debut, fin=fin, date_age=date_age)))
            t2 <- Sys.time()
            if (verbose) {
              cat(" - ","Temps d'exécution",
                  " (",round(as.numeric(difftime(t2, t1)), 2),
                  " ",attr(difftime(t2, t1), "units"), ")\n",
                  sep = "")
            }

            DT_final <- left_join(DT_final, DT1, by="ID")


            # Préparation de la base de données finale ####
            if (verbose) {
              cat("Préparation de la base de données finale:\n")
            }

            if (verbose && keep_all==FALSE) {
              if (length(cohort)>0) {
                cat("Garder les IDs demandés dans la cohorte\n")
              }
              if (is.null(cohort)){
                cat("Garder les IDs qui ont eu un Dx et un code d'acte\n")
              }
            }

            t1 <- Sys.time()

            if(is.null(cohort) && keep_all==FALSE){
              DT_final<-DT_final %>% filter(!is.na(nb_actes))
            }
            else if(length(cohort) > 0 && keep_all==FALSE){
              cohort<-cohort %>% data.frame() %>% rename(ID=".") %>% left_join(DT_final,by="ID") %>% filter(!is.na(nb_actes))
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

            if (verbose) {
              if (nrow(DT_final) == 0) {
                cat("Aucune observation n'a été trouvée. Veuillez-svp vérifier si les Dx et les codes d'acte sont corrects ou si les Dx match avec les actes")
                return(NULL)
              }
            }

            return(DT_final)

          },
          "Projet_Factuartion_Acte"={
            # Création de la cohorte en utilisant les Dx cim-9 et/ou cim-10 ####
            if(is.null(cohort) && !is.null(Dx_table)){
              if (verbose) {
                cat("Création de la cohorte avec la fonction SQL_reperage_cond_med:\n")
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
                verbose = verbose,
                code_stat_decis=code_stat_decis)

              # Extraction d'acte ####
              if (verbose) {
                cat("Extraction des actes avec la fonction query_I_SMOD_SERV_MD_CM_AG:\n")
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_I_SMOD_SERV_MD_CM_AG(query="extraction_SMOD_combin",
                                                                                                  debut=debut,
                                                                                                  fin=fin,
                                                                                                  diagn,
                                                                                                  CodeActe=CodeActe,
                                                                                                  omni_spec=omni_spec,
                                                                                                  catg_etab = catg_etab,
                                                                                                  code_stat_decis=code_stat_decis)))

              t2 <- Sys.time()
              if (verbose) {
                cat(" - ","Temps d'exécution",
                    " (",round(as.numeric(difftime(t2, t1)), 2),
                    " ",attr(difftime(t2, t1), "units"), ")\n",
                    sep = "")
              }

              DT_final<-left_join(DX,DT, by="ID")
            }

            else {
              # Extraction d'acte ####
              if (verbose) {
                cat("Extraction des actes avec la fonction query_I_SMOD_SERV_MD_CM_AG pour la cohorte fournie par l'analyste:\n")
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_I_SMOD_SERV_MD_CM_AG(query="extraction_SMOD_combin",
                                                                                                  debut=debut,
                                                                                                  fin=fin,
                                                                                                  diagn,
                                                                                                  CodeActe=CodeActe,
                                                                                                  omni_spec=omni_spec,
                                                                                                  catg_etab = catg_etab,
                                                                                                  code_stat_decis=code_stat_decis)))

              t2 <- Sys.time()
              if (verbose) {
                cat(" - ","Temps d'exécution",
                    " (",round(as.numeric(difftime(t2, t1)), 2),
                    " ",attr(difftime(t2, t1), "units"), ")\n",
                    sep = "")
              }

              DT_final<-DT
            }


            # Ajout caractéristiques Benef ####

            if(keep_all==TRUE){

              if (verbose) {
                cat("Ajout des caractéristiques des bénéficiaires avec la fonction query_V_FICH_ID_BEN_CM:\n")
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


            # Préparation de la base de données finale ####
            if (verbose) {
              cat("Préparation de la base de données finale:\n")
            }

            if (verbose && keep_all==FALSE) {
              if (length(cohort)>0) {
                cat("Garder les IDs de la cohorte fournie par l'analyste qui ont eu un Dx et un code d'act\n")
              }
              if (is.null(cohort)){
                cat("Garder les IDs de la cohorte crée qui ont eu un Dx et un code d'acte\n")
              }
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


            # Creation d'une variable combinant les codes d'acte avant la suppression des doublants d'acte (mëme: bene,  date, Disp, Etab) ####
            if (verbose) {
              cat("Creation d'une variable combinant les codes d'acte avant la suppression des doublants d'acte (mëme: bene,  date, Disp, Etab):\n")
            }

            t1 <- Sys.time()
            tmp<-DT_final %>%
              dplyr::select(ID,DateActe,CodeActe,NumDispSp,Num_ETAB_USUEL)
            tmp<-data.table::setDT(tmp)[, lapply(.SD, paste, collapse="-") , by = c("ID","DateActe","NumDispSp","Num_ETAB_USUEL")]
            tmp<-tmp %>%
              dplyr::rename(Combine_CodeActe=CodeActe)

            DT_final<-left_join(DT_final,tmp, by=c("ID","DateActe","NumDispSp","Num_ETAB_USUEL"))

            t2 <- Sys.time()
            if (verbose) {
              cat(" - ","Temps d'exécution",
                  " (",round(as.numeric(difftime(t2, t1)), 2),
                  " ",attr(difftime(t2, t1), "units"), ")\n",
                  sep = "")
            }
            # calucl du Nb acte avant la suppression des doublant (meme bene, meme date, meme code acte) ####
            if (verbose) {
              cat("Creation d'une variable calculant le nombre d'acte avant la suppression des doublants d'acte (mëme: bene,  date, Disp, Etab):\n")
            }
            t1 <- Sys.time()
            DT_final<-setDT(DT_final)
            setDT(DT_final)[,n:=1]
            setDT(DT_final)[,Nb_Acte:=sum(n), .(ID,DateActe,NumDispSp,Num_ETAB_USUEL)]
            t2 <- Sys.time()
            if (verbose) {
              cat(" - ","Temps d'exécution",
                  " (",round(as.numeric(difftime(t2, t1)), 2),
                  " ",attr(difftime(t2, t1), "units"), ")\n",
                  sep = "")
            }
            # calcul somme cout acte avant la suppression des doublant (meme bene, meme date, meme code acte) ####
            if (verbose) {
              cat("Creation d'une variable calculant la somme des coût d'acte avant la suppression des doublants d'acte (mëme: bene,  date, Disp, Etab):\n")
            }
            t1 <- Sys.time()
            DT_final<-DT_final %>%
              arrange(ID,DateActe,NumDispSp,Num_ETAB_USUEL) %>%
              group_by(ID,DateActe,NumDispSp,Num_ETAB_USUEL) %>%
              mutate(Nb_Acte=ifelse(is.na(CodeActe),NA,Nb_Acte),
                     Combine_CodeActe=ifelse(is.na(CodeActe),NA,Combine_CodeActe),
                     Sum_coutActe=sum(CoutActe, na.rm = T),
                     Sum_coutActe=ifelse(is.na(CodeActe),NA,Sum_coutActe))
            t2 <- Sys.time()
            if (verbose) {
              cat(" - ","Temps d'exécution",
                  " (",round(as.numeric(difftime(t2, t1)), 2),
                  " ",attr(difftime(t2, t1), "units"), ")\n",
                  sep = "")
            }
            # Exclusion les doublants actes (meme bene, meme date, meme code acte) ####
            if (verbose) {
              cat("Exclusion des doublants d'acte (mëme: bene,  date, Disp, Etab):\n")
            }
            t1 <- Sys.time()
            DT_final<-DT_final %>%
              group_by(ID,DateActe,NumDispSp,Num_ETAB_USUEL) %>%
              filter (! duplicated(CodeActe)) %>%
              ungroup()
            t2 <- Sys.time()
            if (verbose) {
              cat(" - ","Temps d'exécution",
                  " (",round(as.numeric(difftime(t2, t1)), 2),
                  " ",attr(difftime(t2, t1), "units"), ")\n",
                  sep = "")
            }

            # Add Etiquettes SMOD_COD_ROLE, SMOD_COD_SPEC, ETAB_COD_SECT_ACTIV_ETAB, SMOD_COD_ENTEN, Dx ####
            if (verbose) {
              cat("Add Etiquettes SMOD_COD_ROLE, SMOD_COD_SPEC, ETAB_COD_SECT_ACTIV_ETAB, SMOD_COD_ENTEN, Dx:\n")
            }
            t1 <- Sys.time()

            DT_final<-left_join(DT_final,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_ROLE,Desc_SMOD_COD_ROLE) %>% mutate(SMOD_COD_ROLE=as.character(SMOD_COD_ROLE)) %>% na.omit(),by="SMOD_COD_ROLE") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_SPEC,Desc_SMOD_COD_SPEC) %>% na.omit(),by="SMOD_COD_SPEC") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(ETAB_COD_SECT_ACTIV_ETAB,Desc_ETAB_COD_SECT_ACTIV_ETAB) %>% na.omit(),by="ETAB_COD_SECT_ACTIV_ETAB") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_ENTEN,Desc_SMOD_COD_ENTEN) %>% mutate(SMOD_COD_ENTEN=as.character(SMOD_COD_ENTEN)) %>% na.omit(),by="SMOD_COD_ENTEN") %>%
              mutate(DxActe=trimws(DxActe))


            cim9_cim10<-intersect(DT_final[["DxActe"]], c(intersect(RequeteGeneriqueBDCA::Etiquettes$CIM9,RequeteGeneriqueBDCA::Etiquettes$CIM10)))

            cim9 <- RequeteGeneriqueBDCA::Etiquettes$CIM9
            cim10 <- RequeteGeneriqueBDCA::Etiquettes$CIM10
            cim9desc <- RequeteGeneriqueBDCA::Etiquettes$Desc_CIM9
            cim10desc <- RequeteGeneriqueBDCA::Etiquettes$Desc_CIM10
            DxActe <- c(cim9, cim10) %>% data.frame()%>% rename(DxActe=".")
            Desc_DxActe <- c(cim9desc, cim10desc) %>% data.frame()%>% rename(Desc_DxActe=".")
            DT_DxActe<-cbind(DxActe,Desc_DxActe)

            if(length(cim9_cim10)==0){
              DT_final<-left_join(DT_final,DT_DxActe,by="DxActe") %>%
                mutate(NumDispSp=bit64::as.integer64(as.character(NumDispSp)),
                       NumDispRef=bit64::as.integer64(as.character(NumDispRef)))
            }
            else{
              DT_final<-left_join(DT_final,DT_DxActe,by="DxActe")
              DT_final<-DT_final %>% mutate(Desc_DxActe=ifelse(substr(DxActe,1,1)=="V",NA,Desc_DxActe)) %>%
                mutate(NumDispSp=bit64::as.integer64(as.character(NumDispSp)),
                       NumDispRef=bit64::as.integer64(as.character(NumDispRef)))
              warning(paste0("Il y a des Dx qui commencent avec la lettre 'V' qui pourraient être des cim9 ou cim10. L'étiquette de ces Dx n'a pas été rajoutée.",
                             "Veuillez-svp les rajouter manuellement en utilisant la BD 'RequeteGeneriqueBDCA::Etiquettes'.", "Voici les Dx à vérifier: ", paste(cim9_cim10, collapse = ","),"\n"))
            }

            t2 <- Sys.time()
            if (verbose) {
              cat(" - ","Temps d'exécution",
                  " (",round(as.numeric(difftime(t2, t1)), 2),
                  " ",attr(difftime(t2, t1), "units"), ")\n",
                  sep = "")
            }

            # Sil n' y a aucune obsérvation trouvée

            if (verbose) {
              if (nrow(DT_final) == 0) {
                cat("Aucune observation n'a été trouvée. Veuillez-svp vérifier si les Dx et les codes d'acte sont corrects ou si les Dx match avec les actes.")
                return(NULL)
              }
            }

            return(DT_final)

          }

)
}




