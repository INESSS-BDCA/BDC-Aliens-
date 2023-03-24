#' @title user_MEDECHO_DIAGN_SEJ_HOSP_CM
#'
#' @description La fonction `user_MEDECHO_DIAGN_SEJ_HOSP_CM` est destinée aux utilisateurs. Elle permet d'exécuter la rêquette SQL retournée par les fonctions:\cr
#'  `query_V_DIAGN_SEJ_HOSP_CM_AG`\cr
#'  `query_V_SEJ_SERV_HOSP_CM_AG`\cr
#'  `query_V_SEJ_HOSP_CM`\cr
#'
#' Elle permet: \cr
#' 1) d'extraire les variables pertinantes des bases de données MEDECHO: SEJ_HOSP, DIAGN_SEJ_HOSP et SEJ_SERV_HOSP, jumlées avec d'autres bases de données pour rajouter les caractéristiques de dispensateur, du bénéficiaire et de l'établissement de soins.\cr
#'
#' La fonction `user_MEDECHO_DIAGN_SEJ_HOSP_CM` utilise plusieurs autres fonctions génériques telles que:\cr
#'
#' `SQL_reperage_cond_med` pour la création de la cohorte. Voir \code{\link{SQL_reperage_cond_med}}\cr
#' `query_V_FICH_ID_BEN_CM` pour l'extraction des caractéristiques de bénéficiaire. Voir \code{\link{query_V_FICH_ID_BEN_CM}}
#'
#' @inheritParams SQL_reperage_cond_med
#' @inheritParams query_V_DIAGN_SEJ_HOSP_CM_AG
#' @inheritParams query_V_SEJ_SERV_HOSP_CM_AG
#' @inheritParams query_V_SEJ_HOSP_CM
#' @inheritParams query_V_FICH_ID_BEN_CM
#' @param cohort `vecteur` indiquant les numéros d'identification des bénéficiaires définisant la cohorte de l'étude.
#' @param verbose `TRUE` ou `FALSE`. Affiche le temps qui a été nécessaire pour extraire les diagnostics. Utile pour suivre le déroulement de l'extraction.
#' @param keep_all `TRUE` ou `FALSE`. Par défaut `FALSE`, soit filter les observations ayant eu un DX et un code d'acte. Si `TRUE` garder toutes les observations, soit ceux qui ont eu un DX et qui ont eu ou pas un code d'acte.
#' Si `cohort` est fournit, keep_all `TRUE` garde toutes les observations demandées par `cohort`. Si keep_all `FALSE` filtrer les observations demandées par `cohort`qui ont eu un code d'acte.
#'
#' @return
#' **Extraction des variables pertinantes de MEDECHO combinées avec d'autres bases de données:**
#' `data.table` de -- variables si la cohorte est fournie par l'analyste, sinon 39 variables s'il y a une création de cohorte.
#'
#' @encoding UTF-8
#' @import dplyr ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#'
#' DT_final<-user_MEDECHO_DIAGN_SEJ_HOSP_CM(
#'                       task=c("extraction_SMOD","projet_facturation_acte"),
#'                       conn=SQL_connexion("ms069a"),
#'                       cohort = c(NULL, cohort),
#'                       debut_periode = "2020-01-01",
#'                       fin_periode = "2020-12-31",
#'                       CodeActe = c('06452','06148','06154','06265','06251','06216','06208'),
#'                       omni_spec="all",
#'                       catg_etab="all",
#'                       code_stat_decis="PAY-PPY",
#'                       benef_adr="dernière_adresse",
#'                       date_adr="2020-07-01",
#'                       date_age="2020-07-01",
#'                       Dx_table = list(Prolapsus = list(CIM9 = c('5995','6180','6181','6183'),CIM10 = c('N810','N811','N812','N813'))),
#'                       debut_cohort="1998-12-31",
#'                       fin_cohort="2020-12-31",
#'                       CIM =c("CIM9", "CIM10"),
#'                       by_Dx = FALSE,
#'                       date_dx_var = "admis",
#'                       n1 = 30, n2 = 730,
#'                       nDx=0 ,
#'                       setwd="V:/GI/Projets/4.Projets_en_cours/..."
#'                       verbose = TRUE,
#'                       keep_all=FALSE)
#'
#'}


user_MEDECHO_DIAGN_SEJ_HOSP_CM<-function(task,
                                       conn=SQL_connexion(),
                                       cohort=NULL,
                                       debut_periode,
                                       fin_periode,
                                       benef_adr,
                                       date_adr,
                                       date_age,
                                       diagn,
                                       typ_diagn,

                                       Dx_table,
                                       debut_cohort,
                                       fin_cohort,
                                       CIM,
                                       by_Dx,
                                       date_dx_var,
                                       n1,n2,
                                       nDx,
                                       setwd,
                                       keep_all=FALSE,
                                       verbose=TRUE
){
  switch (task,
          "extraction_MEDECHO_SEJ_HOSP"={
            # Création de la cohorte en utilisant les Dx cim-9 et/ou cim-10 et Extraction d'acte ####
            if(is.null(cohort) && !is.null(Dx_table)){
              if (verbose) {
                cat("Étape 1: Création d'une cohorte en utilisant les Dx cim-9 et/ou cim-10:\n")
              }


              DX<-SQL_reperage_cond_med (
                conn = conn,
                debut_cohort=debut_cohort,
                fin_cohort=fin_cohort,
                Dx_table=Dx_table,
                CIM = CIM,
                nDx = nDx,
                by_Dx = by_Dx,
                date_dx_var=date_dx_var,
                n1 = n1,
                n2 = n2,
                verbose = verbose,
                code_stat_decis=c('PAY','PPY'))

              # Extraction d'acte ###
              if (verbose) {
                cat("Étape 2: Extraction des variables MEDECHO_SEJ_HOSP pour la cohorte crée à l'étape 1:\n")
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_V_SEJ_HOSP_CM(query="extraction_MEDECHO_SEJ_HOSP",
                                                                                                     debut_periode=debut_periode,
                                                                                                     fin_periode=fin_periode,
                                                                                                     benef_adr=benef_adr,
                                                                                                     date_adr=date_adr,
                                                                                                     date_dx_var=date_dx_var)))

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
              # Extraction d'acte ###
              if (verbose) {
                if(is.null(cohort)){
                  cat("Étape 1: Extraction des variables MEDECHO_SEJ_HOSP pour les bénéficiaires qui ont eu un séjour hospitalier à l'intérieur de la période de l'étude:\n")
                }
                else{
                  cat("Étape 1: Extraction des variables MEDECHO_SEJ_HOSP pour la cohorte fournie par l'analyste:\n")
                }
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_V_SEJ_HOSP_CM(query="extraction_MEDECHO_SEJ_HOSP",
                                                                                           debut_periode=debut_periode,
                                                                                           fin_periode=fin_periode,
                                                                                           benef_adr=benef_adr,
                                                                                           date_adr=date_adr,
                                                                                           date_dx_var=date_dx_var)))

              t2 <- Sys.time()
              if (verbose) {
                cat(" - ","Temps d'exécution",
                    " (",round(as.numeric(difftime(t2, t1)), 2),
                    " ",attr(difftime(t2, t1), "units"), ")\n\n",
                    sep = "")

              }

              DT_final<-DT
            }

            # Préparation de la base de données finale ####
            if (verbose) {
              if (is.null(cohort) && !is.null(Dx_table)) {
                cat("Étape 3: Préparation de la base de données finale:\n")
              } else {
                cat("Étape 2: Préparation de la base de données finale:\n")
              }
            }


            if (verbose && keep_all==FALSE) {
              if (length(cohort)>0) {
                cat("  Garder les IDs de la cohorte fournie par l'analyste qui ont eu un séjour hospitalier à l'intérieur de la période de l'étude\n\n")
              }
              if (is.null(cohort) && !is.null(Dx_table)){
                cat("  Garder les IDs de la cohorte crée à l'étape 1 qui ont eu un séjour hospitalier à l'intérieur de la période de l'étude\n\n")
              }
              if (is.null(cohort) && is.null(Dx_table)){
                cat("  Garder les IDs qui ont un séjour hospitalier à l'intérieur de la période de l'étude\n\n")
              }
            } else if (verbose && keep_all==TRUE){
              if (length(cohort)>0) {
                cat("  Garder les IDs de la cohorte fournie par l'analyste qui ont eu ou non un séjour hospitalier à l'intérieur de la période de l'étude\n\n")
              }
              if (is.null(cohort) && !is.null(Dx_table)){
                cat("  Garder les IDs de la cohorte crée à l'étape 1 qui ont eu ou non un séjour hospitalier à l'intérieur de la période de l'étude\n\n")
              }
            }

            t1 <- Sys.time()

            if(is.null(cohort) && keep_all==FALSE){ #
              DT_final<-DT_final %>% filter(!is.na(SHOP_DAT_ADMIS_SEJ_HOSP ))
            }
            else if(length(cohort) > 0 && keep_all==FALSE){
              cohort<-cohort %>% data.frame() %>% rename(ID=".") %>% left_join(DT_final,by="ID") %>% filter(!is.na(SHOP_DAT_ADMIS_SEJ_HOSP))
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
                  " ",attr(difftime(t2, t1), "units"), ")\n\n",
                  sep = "")
            }

            # Ajout caractéristiques des Benef n'ayant pas eu de code d'acte ####

            if(keep_all==TRUE){
              if (verbose) {
                if (is.null(cohort) && !is.null(Dx_table)){
                  cat("Étape 4: Ajouter les caractéristiques des bénéficiaires ayant eu un Dx, mais qui n'ont pas eu un séjour hospitalier à l'intérieure de la période de l'étude:\n")
                }
                else{
                  cat("Étape 3: Ajouter les caractéristiques des bénéficiaires ayant eu un Dx, mais qui n'ont pas eu un séjour hospitalier à l'intérieure de la période de l'étude:\n")
                }
              }

              t1 <- Sys.time()
              DT1<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                              statement=query_V_FICH_ID_BEN_CM(debut_periode=debut_periode, fin_periode=fin_periode, date_age=date_age)
              ))


              DT_final <- left_join(DT_final, DT1, by="ID") %>%
                mutate(Sexe=ifelse(is.na(Sexe.x), Sexe.y, Sexe.x),
                       DatNais =as.Date(ifelse(is.na(DatNais.x), DatNais.y, DatNais.x),origin="1970-01-01"),
                       DatDeces=as.Date(ifelse(is.na(DatDeces.x), DatDeces.y, DatDeces.x),origin="1970-01-01"),
                       #Age=ifelse(is.na(as.integer(as.character(Age.x))), as.integer(as.character(Age.y)), as.integer(as.character(Age.x))),
                       RSS_Benf=ifelse(is.na(RSS_Benf.x), RSS_Benf.y, RSS_Benf.x),
                       NomRSS_Benef=ifelse(is.na(NomRSS_Benef.x), NomRSS_Benef.y, NomRSS_Benef.x),
                       RLS_Benef=ifelse(is.na(RLS_Benef.x), RLS_Benef.y, RLS_Benef.x),
                       NomRLS_Benef=ifelse(is.na(NomRLS_Benef.x), NomRLS_Benef.y, NomRLS_Benef.x)) %>%
                select(-c(Sexe.x,Sexe.y,DatNais.x,DatNais.y,DatDeces.y, DatDeces.x,RSS_Benf.y, RSS_Benf.x,NomRSS_Benef.y, NomRSS_Benef.x,RLS_Benef.y, RLS_Benef.x,NomRLS_Benef.y, NomRLS_Benef.x)) #,Age.y, Age.x

            } else{
              DT_final
            }

            if(!is.null(setwd)){
              if (verbose) {
                if (keep_all == TRUE) {
                  if (is.null(cohort) && !is.null(Dx_table)) {
                    cat(paste0("Étape 5: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                  } else {
                    cat(paste0("Étape 4: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                  }
                } else {
                  if (is.null(cohort) && !is.null(Dx_table)) {
                    cat(paste0("Étape 4: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                  } else {
                    cat(paste0("Étape 3: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                  }
                }
              }



              write.table(DT_final, file = paste0(setwd,"/DT_final","_",as.Date(Sys.time()),".txt"), sep = ";", dec = ".",row.names = F, col.names = T,quote = FALSE)

              return(DT_final)
            }

            else{
              if (verbose) {
                if (nrow(DT_final) == 0) {
                  cat("Aucune observation n'a été trouvée. Veuillez-svp vérifier si les Dx et les codes d'acte sont corrects ou si les Dx match avec les actes.")
                  return(NULL)
                }
              }
              return(DT_final)
            }


            # if (verbose) {
            #   if (nrow(DT_final) == 0) {
            #     cat("Aucune observation n'a été trouvée. Veuillez-svp vérifier si les Dx et les codes d'acte sont corrects ou si les Dx match avec les actes.")
            #     return(NULL)
            #   }
            # }
            #
            # return(DT_final)


          },
          "extraction_MEDECHO_DIAGN_SEJ"={
            # Création de la cohorte en utilisant les Dx cim-9 et/ou cim-10 et Extraction d'acte ####
            if(is.null(cohort) && !is.null(Dx_table)){
              if (verbose) {
                cat("Étape 1: Création d'une cohorte en utilisant les Dx cim-9 et/ou cim-10:\n")
              }


              DX<-SQL_reperage_cond_med (
                conn = conn,
                debut_cohort=debut_cohort,
                fin_cohort=fin_cohort,
                Dx_table=Dx_table,
                CIM = CIM,
                nDx = nDx,
                by_Dx = by_Dx,
                date_dx_var=date_dx_var,
                n1 = n1,
                n2 = n2,
                verbose = verbose,
                code_stat_decis=c('PAY','PPY'))

              # Extraction d'acte ###
              if (verbose) {
                cat("Étape 2: Extraction des variables MEDECHO_DIAGN_SEJ pour la cohorte crée à l'étape 1:\n")
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_V_DIAGN_SEJ_HOSP_CM_AG(query="extraction_MEDECHO_DIAGN_SEJ",
                                                                                           debut_periode=debut_periode,
                                                                                           fin_periode=fin_periode,
                                                                                           debut_cohort=NULL,
                                                                                           fin_cohort=NULL,
                                                                                           diagn=diagn,
                                                                                           date_dx_var=date_dx_var,
                                                                                           typ_diagn=typ_diagn)))

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
              # Extraction d'acte ###
              if (verbose) {
                if(is.null(cohort)){
                  cat("Étape 1: Extraction des variables MEDECHO_DIAGN_SEJ pour les bénéficiaires qui ont un séjour hospitalier à l'intérieur de la période de l'étude:\n")
                }
                else{
                  cat("Étape 1: Extraction des variables MEDECHO_DIAGN_SEJ pour la cohorte fournie par l'analyste:\n")
                }
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_V_DIAGN_SEJ_HOSP_CM_AG(query="extraction_MEDECHO_DIAGN_SEJ",
                                                                                                    debut_periode=debut_periode,
                                                                                                    fin_periode=fin_periode,
                                                                                                    debut_cohort=NULL,
                                                                                                    fin_cohort=NULL,
                                                                                                    diagn=diagn,
                                                                                                    date_dx_var=date_dx_var,
                                                                                                    typ_diagn=typ_diagn)))

              t2 <- Sys.time()
              if (verbose) {
                cat(" - ","Temps d'exécution",
                    " (",round(as.numeric(difftime(t2, t1)), 2),
                    " ",attr(difftime(t2, t1), "units"), ")\n\n",
                    sep = "")

              }

              DT_final<-DT
            }

            # Préparation de la base de données finale ####
            if (verbose) {
              if (is.null(cohort) && !is.null(Dx_table)) {
                cat("Étape 3: Préparation de la base de données finale:\n")
              } else {
                cat("Étape 2: Préparation de la base de données finale:\n")
              }
            }


            if (verbose && keep_all==FALSE) {
              if (length(cohort)>0) {
                cat("  Garder les IDs de la cohorte fournie par l'analyste qui ont eu un séjour hospitalier à l'intérieur de la période de l'étude\n\n")
              }
              if (is.null(cohort) && !is.null(Dx_table)){
                cat("  Garder les IDs de la cohorte crée à l'étape 1 qui ont eu un séjour hospitalier à l'intérieur de la période de l'étude\n\n")
              }
              if (is.null(cohort) && is.null(Dx_table)){
                cat("  Garder les IDs qui ont eu un séjour hospitalier à l'intérieur de la période de l'étude\n\n")
              }
            } else if (verbose && keep_all==TRUE){
              if (length(cohort)>0) {
                cat("  Garder les IDs de la cohorte fournie par l'analyste qui ont eu ou non un séjour hospitalier à l'intérieur de la période de l'étude\n\n")
              }
              if (is.null(cohort) && !is.null(Dx_table)){
                cat("  Garder les IDs de la cohorte crée à l'étape 1 qui ont eu ou non un séjour hospitalier à l'intérieur de la période de l'étude\n\n")
              }
            }

            t1 <- Sys.time()

            if(is.null(cohort) && keep_all==FALSE){ #
              DT_final<-DT_final %>% filter(!is.na(SHOP_DAT_ADMIS_SEJ_HOSP ))
            }
            else if(length(cohort) > 0 && keep_all==FALSE){
              cohort<-cohort %>% data.frame() %>% rename(ID=".") %>% left_join(DT_final,by="ID") %>% filter(!is.na(SHOP_DAT_ADMIS_SEJ_HOSP))
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
                  " ",attr(difftime(t2, t1), "units"), ")\n\n",
                  sep = "")
            }

            # Ajout caractéristiques des Benef n'ayant pas eu de code d'acte ####

            if(keep_all==TRUE){
              if (verbose) {
                if (is.null(cohort) && !is.null(Dx_table)){
                  cat("Étape 4: Ajouter les caractéristiques des bénéficiaires ayant eu un Dx, mais qui n'ont pas eu un séjour hospitalier à l'intérieure de la période de l'étude:\n")
                }
                else{
                  cat("Étape 3: Ajouter les caractéristiques des bénéficiaires ayant eu un Dx, mais qui n'ont pas eu un séjour hospitalier à l'intérieure de la période de l'étude:\n")
                }
              }

              t1 <- Sys.time()
              DT1<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                              statement=query_V_FICH_ID_BEN_CM(debut_periode=debut_periode, fin_periode=fin_periode, date_age=date_age)
              ))


              DT_final <- left_join(DT_final, DT1, by="ID")

            } else{
              DT_final
            }

            if(!is.null(setwd)){
              if (verbose) {
                if (keep_all == TRUE) {
                  if (is.null(cohort) && !is.null(Dx_table)) {
                    cat(paste0("Étape 5: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                  } else {
                    cat(paste0("Étape 4: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                  }
                } else {
                  if (is.null(cohort) && !is.null(Dx_table)) {
                    cat(paste0("Étape 4: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                  } else {
                    cat(paste0("Étape 3: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                  }
                }
              }



              write.table(DT_final, file = paste0(setwd,"/DT_final","_",as.Date(Sys.time()),".txt"), sep = ";", dec = ".",row.names = F, col.names = T,quote = FALSE)

              return(DT_final)
            }

            else{
              if (verbose) {
                if (nrow(DT_final) == 0) {
                  cat("Aucune observation n'a été trouvée. Veuillez-svp vérifier si les Dx et les codes d'acte sont corrects ou si les Dx match avec les actes.")
                  return(NULL)
                }
              }
              return(DT_final)
            }


            # if (verbose) {
            #   if (nrow(DT_final) == 0) {
            #     cat("Aucune observation n'a été trouvée. Veuillez-svp vérifier si les Dx et les codes d'acte sont corrects ou si les Dx match avec les actes.")
            #     return(NULL)
            #   }
            # }
            #
            # return(DT_final)

          },
          "extraction_MEDECHO_SEJ_SERV"={
            # Création de la cohorte en utilisant les Dx cim-9 et/ou cim-10 et Extraction d'acte ####
            if(is.null(cohort) && !is.null(Dx_table)){
              if (verbose) {
                cat("Étape 1: Création d'une cohorte en utilisant les Dx cim-9 et/ou cim-10:\n")
              }


              DX<-SQL_reperage_cond_med (
                conn = conn,
                debut_cohort=debut_cohort,
                fin_cohort=fin_cohort,
                Dx_table=Dx_table,
                CIM = CIM,
                nDx = nDx,
                by_Dx = by_Dx,
                date_dx_var=date_dx_var,
                n1 = n1,
                n2 = n2,
                verbose = verbose,
                code_stat_decis=c('PAY','PPY'))

              # Extraction d'acte ###
              if (verbose) {
                cat("Étape 2: Extraction des variables MEDECHO_SEJ_SERV pour la cohorte crée à l'étape 1:\n")
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_V_SEJ_SERV_HOSP_CM_AG(query="extraction_MEDECHO_SEJ_SERV",
                                                                                                   debut_periode=debut_periode,
                                                                                                   fin_periode=fin_periode,
                                                                                                   date_dx_var=date_dx_var,
                                                                                                   debut_cohort=NULL,
                                                                                                   fin_cohort=NULL,
                                                                                                   diagn=diagn)))

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
              # Extraction d'acte ###
              if (verbose) {
                if(is.null(cohort)){
                  cat("Étape 1: Extraction des variables MEDECHO_SEJ_SERV pour les bénéficiaires qui ont un séjour hospitalier à l'intérieur de la période de l'étude:\n")
                }
                else{
                  cat("Étape 1: Extraction des variables MEDECHO_SEJ_SERV pour la cohorte fournie par l'analyste:\n")
                }
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_V_SEJ_SERV_HOSP_CM_AG(query="extraction_MEDECHO_SEJ_SERV",
                                                                                                   debut_periode=debut_periode,
                                                                                                   fin_periode=fin_periode,
                                                                                                   debut_cohort=NULL,
                                                                                                   fin_cohort=NULL,
                                                                                                   date_dx_var=date_dx_var,
                                                                                                   diagn=diagn)))

              t2 <- Sys.time()
              if (verbose) {
                cat(" - ","Temps d'exécution",
                    " (",round(as.numeric(difftime(t2, t1)), 2),
                    " ",attr(difftime(t2, t1), "units"), ")\n\n",
                    sep = "")

              }

              DT_final<-DT
            }

            # Préparation de la base de données finale ####
            if (verbose) {
              if (is.null(cohort) && !is.null(Dx_table)) {
                cat("Étape 3: Préparation de la base de données finale:\n")
              } else {
                cat("Étape 2: Préparation de la base de données finale:\n")
              }
            }


            if (verbose && keep_all==FALSE) {
              if (length(cohort)>0) {
                cat("  Garder les IDs de la cohorte fournie par l'analyste qui ont eu un séjour hospitalier à l'intérieur de la période de l'étude\n\n")
              }
              if (is.null(cohort) && !is.null(Dx_table)){
                cat("  Garder les IDs de la cohorte crée à l'étape 1 qui ont eu un séjour hospitalier à l'intérieur de la période de l'étude\n\n")
              }
              if (is.null(cohort) && is.null(Dx_table)){
                cat("  Garder les IDs qui ont eu un séjour hospitalier à l'intérieur de la période de l'étude\n\n")
              }
            } else if (verbose && keep_all==TRUE){
              if (length(cohort)>0) {
                cat("  Garder les IDs de la cohorte fournie par l'analyste qui ont eu ou non un séjour hospitalier à l'intérieur de la période de l'étude\n\n")
              }
              if (is.null(cohort) && !is.null(Dx_table)){
                cat("  Garder les IDs de la cohorte crée à l'étape 1 qui ont eu ou non un séjour hospitalier à l'intérieur de la période de l'étude\n\n")
              }
            }

            t1 <- Sys.time()

            if(is.null(cohort) && keep_all==FALSE){ #
              DT_final<-DT_final %>% filter(!is.na(SHOP_DAT_ADMIS_SEJ_HOSP ))
            }
            else if(length(cohort) > 0 && keep_all==FALSE){
              cohort<-cohort %>% data.frame() %>% rename(ID=".") %>% left_join(DT_final,by="ID") %>% filter(!is.na(SHOP_DAT_ADMIS_SEJ_HOSP))
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
                  " ",attr(difftime(t2, t1), "units"), ")\n\n",
                  sep = "")
            }

            # Ajout caractéristiques des Benef n'ayant pas eu de code d'acte ####

            if(keep_all==TRUE){
              if (verbose) {
                if (is.null(cohort) && !is.null(Dx_table)){
                  cat("Étape 4: Ajouter les caractéristiques des bénéficiaires ayant eu un Dx, mais qui n'ont pas eu un séjour hospitalier à l'intérieure de la période de l'étude:\n")
                }
                else{
                  cat("Étape 3: Ajouter les caractéristiques des bénéficiaires ayant eu un Dx, mais qui n'ont pas eu un séjour hospitalier à l'intérieure de la période de l'étude:\n")
                }
              }

              t1 <- Sys.time()
              DT1<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                              statement=query_V_FICH_ID_BEN_CM(debut_periode=debut_periode, fin_periode=fin_periode, date_age=date_age)
              ))


              DT_final <- left_join(DT_final, DT1, by="ID")

            } else{
              DT_final
            }

            if(!is.null(setwd)){
              if (verbose) {
                if (keep_all == TRUE) {
                  if (is.null(cohort) && !is.null(Dx_table)) {
                    cat(paste0("Étape 5: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                  } else {
                    cat(paste0("Étape 4: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                  }
                } else {
                  if (is.null(cohort) && !is.null(Dx_table)) {
                    cat(paste0("Étape 4: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                  } else {
                    cat(paste0("Étape 3: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                  }
                }
              }



              write.table(DT_final, file = paste0(setwd,"/DT_final","_",as.Date(Sys.time()),".txt"), sep = ";", dec = ".",row.names = F, col.names = T,quote = FALSE)

              return(DT_final)
            }

            else{
              if (verbose) {
                if (nrow(DT_final) == 0) {
                  cat("Aucune observation n'a été trouvée. Veuillez-svp vérifier si les Dx et les codes d'acte sont corrects ou si les Dx match avec les actes.")
                  return(NULL)
                }
              }
              return(DT_final)
            }


            # if (verbose) {
            #   if (nrow(DT_final) == 0) {
            #     cat("Aucune observation n'a été trouvée. Veuillez-svp vérifier si les Dx et les codes d'acte sont corrects ou si les Dx match avec les actes.")
            #     return(NULL)
            #   }
            # }

            return(DT_final)}
  )
}
