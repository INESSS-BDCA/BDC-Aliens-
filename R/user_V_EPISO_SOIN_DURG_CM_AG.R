#' @title user_V_EPISO_SOIN_DURG_CM_AG
#'
#' @description La fonction `user_V_EPISO_SOIN_DURG_CM_AG` est destinée aux utilisateurs. Elle permet d'exécuter la rêquette SQL retournée par la fonction `query_V_EPISO_SOIN_DURG_CM_AG`.\cr
#'
#' Elle permet: \cr
#' 1) d'extraire les variables pertinantes de la base de données BDCU, jumlées avec d'autres bases de données pour rajouter les caractéristiques de dispensateur, du bénéficiaire et de l'établissement de soins.\cr
#'
#' La fonction `user_V_EPISO_SOIN_DURG_CM_AG` utilise plusieurs autres fonctions génériques telles que:\cr
#'
#' `SQL_reperage_cond_med` pour la création de la cohorte. Voir \code{\link{SQL_reperage_cond_med}}\cr
#' `query_V_FICH_ID_BEN_CM` pour l'extraction des caractéristiques de bénéficiaire. Voir \code{\link{query_V_FICH_ID_BEN_CM}}
#'
#' @inheritParams SQL_reperage_cond_med
#' @inheritParams query_V_EPISO_SOIN_DURG_CM_AG
#' @inheritParams query_V_FICH_ID_BEN_CM
#' @param cohort `vecteur` indiquant les numéros d'identification des bénéficiaires définisant la cohorte de l'étude.
#' @param verbose `TRUE` ou `FALSE`. Affiche le temps qui a été nécessaire pour extraire les diagnostics. Utile pour suivre le déroulement de l'extraction.
#' @param keep_all `TRUE` ou `FALSE`. Par défaut `FALSE`, soit filter les observations ayant eu un DX et qui ont eu recours aux services d'urgence. Si `TRUE` garder toutes les observations, soit ceux qui ont eu un DX et qui ont eu ou pas recours aux services d'urgence.
#' Si `cohort` est fournit, keep_all `TRUE` garder toutes les observations demandées par `cohort`. Si keep_all `FALSE` filtrer les observations demandées par `cohort`qui ont eu recours aux services d'urgence.
#'
#' @return
#' **Extraction des variables pertinantes de BDCU combinées avec d'autres bases de données:**
#' `data.table` de 33 variables si la cohorte est fournie par l'analyste, sinon 35 variables s'il y a une création de cohorte.
#'
#' @encoding UTF-8
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#'
#' DT_final<-user_V_EPISO_SOIN_DURG_CM_AG(
#'                       task=c("extraction_BDCU"),
#'                       conn=SQL_connexion("ms069a"),
#'                       cohort = c(NULL, cohort),
#'                       debut_periode = "2020-01-01",
#'                       fin_periode = "2020-12-31",
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



user_V_EPISO_SOIN_DURG_CM_AG<-function(task,
                                    conn=SQL_connexion(),
                                    cohort=NULL,
                                    debut_periode,
                                    fin_periode,
                                    benef_adr,
                                    date_adr,
                                    date_age,

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
          "extraction_BDCU"={
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
                cat("Étape 2: Extraction des variables BDCU pour la cohorte crée à l'étape 1:\n")
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_V_EPISO_SOIN_DURG_CM_AG(query="extraction_BDCU",
                                                                                                  debut_periode=debut_periode,
                                                                                                  fin_periode=fin_periode,
                                                                                                  debut_cohort=debut_cohort,
                                                                                                  fin_cohort=fin_cohort,
                                                                                                  diagn=NULL,
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
                  cat("Étape 1: Extraction des variables BDCU pour les bénéficiaires qui ont eu recours aux services d'urgence à l'intérieur de la période de l'étude:\n")
                }
                else{
                  cat("Étape 1: Extraction des variables BDCU pour la cohorte fournie par l'analyste:\n")
                }
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_V_EPISO_SOIN_DURG_CM_AG(query="extraction_BDCU",
                                                                                                     debut_periode=debut_periode,
                                                                                                     fin_periode=fin_periode,
                                                                                                     debut_cohort=debut_cohort,
                                                                                                     fin_cohort=fin_cohort,
                                                                                                     diagn=NULL,
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
                cat("  Garder les IDs de la cohorte fournie par l'analyste qui ont eu recours aux services d'urgence à l'intérieur de la période de l'étude\n\n")
              }
              if (is.null(cohort) && !is.null(Dx_table)){
                cat("  Garder les IDs de la cohorte crée à l'étape 1 qui ont eu recours aux services d'urgence à l'intérieur de la période de l'étude\n\n")
              }
              if (is.null(cohort) && is.null(Dx_table)){
                cat("  Garder les IDs qui ont eu recourd aux services d'urgence à l'intérieur de la période de l'étude\n\n")
              }
            } else if (verbose && keep_all==TRUE){
              if (length(cohort)>0) {
                cat("  Garder les IDs de la cohorte fournie par l'analyste qui ont eu ou non recours aux services d'urgence à l'intérieur de la période de l'étude\n\n")
              }
              if (is.null(cohort) && !is.null(Dx_table)){
                cat("  Garder les IDs de la cohorte crée à l'étape 1 qui ont eu ou non recourd aux services d'urgence à l'intérieur de la période de l'étude\n\n")
              }
            }

            t1 <- Sys.time()

            if(is.null(cohort) && keep_all==FALSE){ #
              DT_final<-DT_final %>% filter(!is.na(DD_SOIN_DURG ))
            }
            else if(length(cohort) > 0 && keep_all==FALSE){
              cohort<-cohort %>% data.frame() %>% rename(ID=".") %>% left_join(DT_final,by="ID") %>% filter(!is.na(DD_SOIN_DURG))
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
                  cat("Étape 4: Ajouter les caractéristiques des bénéficiaires ayant eu un Dx, mais qui n'ont pas eu recours aux services d'urgence à l'intérieure de la période de l'étude:\n")
                }
                else{
                  cat("Étape 3: Ajouter les caractéristiques des bénéficiaires ayant eu un Dx, mais qui n'ont pas eu recours aux services d'urgence à l'intérieure de la période de l'étude:\n")
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

