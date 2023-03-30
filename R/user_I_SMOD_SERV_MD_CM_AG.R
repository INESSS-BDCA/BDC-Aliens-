#' @title user_I_SMOD_SERV_MD_CM_AG
#'
#' @description La fonction `user_I_SMOD_SERV_MD_CM_AG` est destinée aux utilisateurs. Elle permet d'exécuter la rêquette SQL retournée par la fonction `query_I_SMOD_SERV_MD_CM_AG`.\cr
#'
#' Elle permet: \cr
#' 1) d'extraire les variables pertinantes de la base de données SMOD, jumlées avec d'autres bases de données pour rajouter les caractéristiques de dispensateur, du bénéficiaire et de l'établissement de soins.\cr
#' 2) d'extraire les variables pertinantes de la base de données SMOD et d'effectuer des analyses discriptives afin d'orienter la prise de décision.\cr
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
#' **Extraction des variables pertinantes de SMOD combinées avec d'autres bases de données:**
#' `data.table` de 37 variables si la cohorte est fournie par l'analyste, sinon 39 variables s'il y a une création de cohorte.
#'
#' **Extraction des variables et analyses discriptives:**
#' `data.table` de 37 variables si la cohorte est fournie par l'analyste, sinon 39 variables s'il y a une création de cohorte.
#' `Tableaux` 5 tableaux: Tab1_Nb_Acte_annee, Tab2_Nb_Acte_spec, Tab3_Nb_Acte_lieu, Tab4_Combin_Acte_Freq, Tab5_Nb_Acte_rep
#' `Figures` 2 figures: variation des coûts d'acte en fonction des spécialités principales (Plot_Cout_acte_spec) et d'anesthésistes (Plot_Cout_acte_Anesth)
#'
#' @encoding UTF-8
#' @import dplyr ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#'
#' DT_final<-user_I_SMOD_SERV_MD_CM_AG(
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


user_I_SMOD_SERV_MD_CM_AG<-function(task,
                                    conn=SQL_connexion(),
                                    cohort=NULL,
                                    debut_periode,
                                    fin_periode,
                                    CodeActe,
                                    omni_spec,
                                    catg_etab,
                                    code_stat_decis,
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
          "extraction_SMOD"={
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
                code_stat_decis=code_stat_decis)

              # Extraction d'acte ###
              if (verbose) {
                cat("Étape 2: Extraction des actes pour la cohorte crée à l'étape 1:\n")
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_I_SMOD_SERV_MD_CM_AG(query="extraction_SMOD",
                                                                                                  debut_periode=debut_periode,
                                                                                                  fin_periode=fin_periode,
                                                                                                  debut_cohort=debut_cohort,
                                                                                                  fin_cohort=fin_cohort,
                                                                                                  diagn=NULL,
                                                                                                  CodeActe=CodeActe,
                                                                                                  omni_spec=omni_spec,
                                                                                                  catg_etab = catg_etab,
                                                                                                  code_stat_decis=code_stat_decis,
                                                                                                  benef_adr=benef_adr,
                                                                                                  date_adr=date_adr)))

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
                  cat("Étape 1: Extraction des actes pour les bénéficiaires ayant eu un acte à l'intérieure de la péride de l'étude:\n")
                }
                else{
                  cat("Étape 1: Extraction des actes pour la cohorte fournie par l'analyste:\n")
                }
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_I_SMOD_SERV_MD_CM_AG(query="extraction_SMOD",
                                                                                                  debut_periode=debut_periode,
                                                                                                  fin_periode=fin_periode,
                                                                                                  debut_cohort=debut_cohort,
                                                                                                  fin_cohort=fin_cohort,
                                                                                                  diagn=NULL,
                                                                                                  CodeActe=CodeActe,
                                                                                                  omni_spec=omni_spec,
                                                                                                  catg_etab = catg_etab,
                                                                                                  code_stat_decis=code_stat_decis,
                                                                                                  benef_adr=benef_adr,
                                                                                                  date_adr=date_adr)))

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
              #if (keep_all == TRUE) {
                if (is.null(cohort) && !is.null(Dx_table)) {
                  cat("Étape 3: Préparation de la base de données finale:\n")
                } else {
                  cat("Étape 2: Préparation de la base de données finale:\n")
                }
              # } else {
              #   if (is.null(cohort) && !is.null(Dx_table)) {
              #     cat("Étape 3: Préparation de la base de données finale:\n")
              #   } else {
              #     cat("Étape 2: Préparation de la base de données finale:\n")
              #   }
              #}
            }


            if (verbose && keep_all==FALSE) {
              if (length(cohort)>0) {
                cat("  Garder les IDs de la cohorte fournie par l'analyste qui ont eu un Dx et un code d'acte\n\n")
              }
              if (is.null(cohort) && !is.null(Dx_table)){
                cat("  Garder les IDs de la cohorte crée à l'étape 1 qui ont eu un Dx et un code d'acte\n\n")
              }
              if (is.null(cohort) && is.null(Dx_table)){
                cat("  Garder les IDs qui ont eu un code d'acte à l'intérieure de la période de l'étude\n\n")
              }
            } else if (verbose && keep_all==TRUE){
              if (length(cohort)>0) {
                cat("  Garder les IDs de la cohorte fournie par l'analyste qui ont eu ou non un code d'acte\n\n")
              }
              if (is.null(cohort) && !is.null(Dx_table)){
                cat("  Garder les IDs de la cohorte crée à l'étape 1 qui ont eu ou non un code d'acte\n\n")
              }
            }

            t1 <- Sys.time()

            if(is.null(cohort) && keep_all==FALSE){ #
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

            # Ajout caractéristiques des Benef n'ayant pas eu de code d'acte ####

            if(keep_all==TRUE){
              if (verbose) {
                if (is.null(cohort) && !is.null(Dx_table)){
                  cat("Étape 4: Ajouter les caractéristiques des bénéficiaires ayant eu un Dx, mais pas d'acte à l'intérieure de la période de l'étude:\n")
                }
                else{
                  cat("Étape 3: Ajouter les caractéristiques des bénéficiaires ayant eu un Dx, mais pas d'acte à l'intérieure de la période de l'étude:\n")
                }
              }

              t1 <- Sys.time()
              DT1<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                              statement=query_V_FICH_ID_BEN_CM(debut_periode=debut_periode, fin_periode=fin_periode, date_age=date_age)
              ))
              t2 <- Sys.time()
              if (verbose) {
                cat(" - ","Temps d'exécution",
                    " (",round(as.numeric(difftime(t2, t1)), 2),
                    " ",attr(difftime(t2, t1), "units"), ")\n\n",
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

            } else{
              DT_final
            }

            # Add Etiquettes SMOD_COD_ROLE, SMOD_COD_SPEC, ETAB_COD_SECT_ACTIV_ETAB, SMOD_COD_ENTEN, Dx ####

            if (verbose) {
              if (keep_all == TRUE) {
                if (is.null(cohort) && !is.null(Dx_table)) {
                  cat("Étape 5: Rajouter les étiquettes pour `SMOD_COD_ROLE`, `SMOD_COD_SPEC`, `ETAB_COD_SECT_ACTIV_ETAB`, `SMOD_COD_ENTEN`:\n") #et `Dx`
                } else {
                  cat("Étape 4: Rajouter les étiquettes pour `SMOD_COD_ROLE`, `SMOD_COD_SPEC`, `ETAB_COD_SECT_ACTIV_ETAB`, `SMOD_COD_ENTEN`:\n")# et `Dx`
                }
              } else {
                if (is.null(cohort) && !is.null(Dx_table)) {
                  cat("Étape 4: Rajouter les étiquettes pour `SMOD_COD_ROLE`, `SMOD_COD_SPEC`, `ETAB_COD_SECT_ACTIV_ETAB`, `SMOD_COD_ENTEN`:\n") # et `Dx`
                } else {
                  cat("Étape 3: Rajouter les étiquettes pour `SMOD_COD_ROLE`, `SMOD_COD_SPEC`, `ETAB_COD_SECT_ACTIV_ETAB`, `SMOD_COD_ENTEN`:\n") # et `Dx`
                }
              }
            }


            t1 <- Sys.time()

            DT_final<-DT_final %>% mutate(ETAB_COD_SECT_ACTIV_ETAB=ifelse(!is.na(DateActe) & is.na(ETAB_COD_SECT_ACTIV_ETAB),0,ETAB_COD_SECT_ACTIV_ETAB))

            DT_final<-left_join(DT_final,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_ROLE,Desc_SMOD_COD_ROLE) %>% mutate(SMOD_COD_ROLE=as.character(SMOD_COD_ROLE)) %>% na.omit(),by="SMOD_COD_ROLE") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_SPEC,Desc_SMOD_COD_SPEC) %>% na.omit(),by="SMOD_COD_SPEC") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_SPEC,Desc_SMOD_COD_SPEC) %>% rename(SPDisp_fip=SMOD_COD_SPEC,Desc_SMOD_COD_SPECFip=Desc_SMOD_COD_SPEC) %>% na.omit(),by="SPDisp_fip") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_SPEC,Desc_SMOD_COD_SPEC) %>% rename(SPDispRefs=SMOD_COD_SPEC,Desc_SMOD_COD_SPECref=Desc_SMOD_COD_SPEC) %>% na.omit(),by="SPDispRefs") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(ETAB_COD_SECT_ACTIV_ETAB,Desc_ETAB_COD_SECT_ACTIV_ETAB,Secteur_regroupe) %>% na.omit(),by="ETAB_COD_SECT_ACTIV_ETAB") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_ENTEN,Desc_SMOD_COD_ENTEN) %>% mutate(SMOD_COD_ENTEN=as.character(SMOD_COD_ENTEN)) %>% na.omit(),by="SMOD_COD_ENTEN") %>%
              mutate(DxActe=trimws(DxActe))


            DT_final<-DT_final %>% mutate (Desc_SMOD_COD_SPECref=ifelse(!is.na(NumDispRef) & is.na(SPDispRefs), "Med_fam",Desc_SMOD_COD_SPECref))

            # cim9_cim10<-intersect(DT_final[["DxActe"]], c(intersect(RequeteGeneriqueBDCA::Etiquettes$CIM9,RequeteGeneriqueBDCA::Etiquettes$CIM10)))
            #
            # cim9 <- RequeteGeneriqueBDCA::Etiquettes$CIM9
            # cim10 <- RequeteGeneriqueBDCA::Etiquettes$CIM10
            # cim9desc <- RequeteGeneriqueBDCA::Etiquettes$Desc_CIM9
            # cim10desc <- RequeteGeneriqueBDCA::Etiquettes$Desc_CIM10
            # DxActe <- c(cim9, cim10) %>% data.frame()%>% rename(DxActe=".")
            # Desc_DxActe <- c(cim9desc, cim10desc) %>% data.frame()%>% rename(Desc_DxActe=".")
            # DT_DxActe<-cbind(DxActe,Desc_DxActe)
            #
            # if(length(cim9_cim10)==0){
            #   DT_final<-left_join(DT_final,DT_DxActe,by="DxActe") %>%
            #     mutate(NumDispSp=bit64::as.integer64(as.character(NumDispSp)),
            #            NumDispRef=bit64::as.integer64(as.character(NumDispRef)))
            # }
            # else{
            #   DT_final<-left_join(DT_final,DT_DxActe,by="DxActe")
            #   DT_final<-DT_final %>% mutate(Desc_DxActe=ifelse(substr(DxActe,1,1)=="V",NA,Desc_DxActe)) %>%
            #     mutate(NumDispSp=bit64::as.integer64(as.character(NumDispSp)),
            #            NumDispRef=bit64::as.integer64(as.character(NumDispRef)))
            #   warning(paste0("Il y a des Dx qui commencent avec la lettre 'V' qui pourraient être des cim9 ou cim10. L'étiquette de ces Dx n'a pas été rajoutée.",
            #                  "Veuillez-svp les rajouter manuellement en utilisant la BD 'RequeteGeneriqueBDCA::Etiquettes'.", "Voici les Dx à vérifier: ", paste(cim9_cim10, collapse = ","),"\n"))
            # }

            DT_final<-DT_final %>%
              rename(Code_RSS_Ben=RSS_Benf,
                     Desc_RSS_Ben=NomRSS_Benef,
                     Code_RLS_Ben=RLS_Benef,
                     Desc_RLS_Ben=NomRLS_Benef,
                     Code_Acte=CodeActe,
                     Cout_Acte=CoutActe,
                     Code_DxActe=DxActe,
                     Code_Role=SMOD_COD_ROLE,
                     Desc_Role=Desc_SMOD_COD_ROLE,
                     Code_Entente=SMOD_COD_ENTEN,
                     Desc_Entente=Desc_SMOD_COD_ENTEN,
                     Code_SpecSmod=SMOD_COD_SPEC,
                     Desc_SpecSmod=Desc_SMOD_COD_SPEC,
                     Code_SpecFip=SPDisp_fip,
                     Desc_SpecFip=Desc_SMOD_COD_SPECFip,
                     Code_SpecDispRef=SPDispRefs,
                     Desc_SpecDispRef=Desc_SMOD_COD_SPECref,
                     NumEtabUsuel=Num_ETAB_USUEL,
                     NumEtab=NO_ETAB,
                     NomEtab=NOM_ETAB,
                     Code_SectActiv=ETAB_COD_SECT_ACTIV_ETAB,
                     Desc_SectActiv=Desc_ETAB_COD_SECT_ACTIV_ETAB,
                     Desc_SectActivRegroup=Secteur_regroupe,
                     Code_CatEtab=Cat_Etab,
                     Code_RSS_Etab=RSS_Etab,
                     Desc_RSS_Etab=NomRSS_Etab,
                     Code_RLS_Etab=RLS_Etab,
                     Desc_RLS_Etab=NomRLS_Etab) %>%
              select(ID,Sexe,DatNais,DatDeces,Age,Code_RSS_Ben,Desc_RSS_Ben,Code_RLS_Ben,Desc_RLS_Ben,
                     AnFinan,AnCivil,DateActe,Code_Acte,Cout_Acte,Code_DxActe,Code_Role, #Desc_DxActe
                     Desc_Role,Code_Entente,Desc_Entente,NumDispSp,Code_SpecSmod,Desc_SpecSmod,Code_SpecFip,
                     Desc_SpecFip,NumDispRef,Code_SpecDispRef,Desc_SpecDispRef,NumEtabUsuel,NumEtab,NomEtab,
                     Code_SectActiv,Desc_SectActiv,Desc_SectActivRegroup,Code_CatEtab,Code_RSS_Etab,Desc_RSS_Etab,
                     Code_RLS_Etab,Desc_RLS_Etab)

            t2 <- Sys.time()
            if (verbose) {
              cat(" - ","Temps d'exécution",
                  " (",round(as.numeric(difftime(t2, t1)), 2),
                  " ",attr(difftime(t2, t1), "units"), ")\n",
                  sep = "")
            }


            if(!is.null(setwd)){
              if (verbose) {
                if (keep_all == TRUE) {
                  if (is.null(cohort) && !is.null(Dx_table)) {
                    cat(paste0("Étape 6: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                  } else {
                    cat(paste0("Étape 5: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                  }
                } else {
                  if (is.null(cohort) && !is.null(Dx_table)) {
                    cat(paste0("Étape 5: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                  } else {
                    cat(paste0("Étape 4: Sauvegarde des BD et figures dans: ",setwd,"\n"))
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
          "projet_facturation_acte"={
            # Création de la cohorte en utilisant les Dx cim-9 et/ou cim-10 ####
            if(is.null(cohort) && !is.null(Dx_table)){
              if (verbose) {
                cat("Étape 1: Création d'une cohorte en utilisant les Dx cim-9 et/ou cim-10:\n")
              }

              t1 <- Sys.time()
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
                code_stat_decis=code_stat_decis)

              t2 <- Sys.time()
              if (verbose) {
                cat(" - ","Temps d'exécution",
                    " (",round(as.numeric(difftime(t2, t1)), 2),
                    " ",attr(difftime(t2, t1), "units"), ")\n \n",
                    sep = "")
              }

              # Extraction d'acte ####
              if (verbose) {
                cat("Étape 2: Extraction des actes pour la cohorte crée à l'étape 1:\n")
              }

              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_I_SMOD_SERV_MD_CM_AG(query="extraction_SMOD",
                                                                                                  debut_periode=debut_periode,
                                                                                                  fin_periode=fin_periode,
                                                                                                  diagn=NULL,
                                                                                                  CodeActe=CodeActe,
                                                                                                  omni_spec=omni_spec,
                                                                                                  catg_etab = catg_etab,
                                                                                                  code_stat_decis=code_stat_decis,
                                                                                                  benef_adr=benef_adr,
                                                                                                  date_adr=date_adr)))

              t2 <- Sys.time()
              if (verbose) {
                cat(" - ","Temps d'exécution",
                    " (",round(as.numeric(difftime(t2, t1)), 2),
                    " ",attr(difftime(t2, t1), "units"), ")\n \n",
                    sep = "")
              }

              DT_final<-left_join(DX,DT, by="ID")
            }

            else {
              # Extraction d'acte ####
              if (verbose) {
                if(is.null(cohort)){
                cat("Étape 1: Extraction des actes pour les bénéficiaires ayant eu un acte à l'intérieure de la péride de l'étude:\n")
                }
                else{
                  cat("Étape 1: Extraction des actes pour la cohorte fournie par l'analyste:\n")
                }
              }


              t1 <- Sys.time()

              DT<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                             statement=query_I_SMOD_SERV_MD_CM_AG(query="extraction_SMOD",
                                                                                                  debut_periode=debut_periode,
                                                                                                  fin_periode=fin_periode,
                                                                                                  diagn=NULL,
                                                                                                  CodeActe=CodeActe,
                                                                                                  omni_spec=omni_spec,
                                                                                                  catg_etab = catg_etab,
                                                                                                  code_stat_decis=code_stat_decis,
                                                                                                  benef_adr=benef_adr,
                                                                                                  date_adr=date_adr)))

              t2 <- Sys.time()
              if (verbose) {
                cat(" - ","Temps d'exécution",
                    " (",round(as.numeric(difftime(t2, t1)), 2),
                    " ",attr(difftime(t2, t1), "units"), ")\n \n",
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
                cat("  Garder les IDs de la cohorte fournie par l'analyste qui ont eu un Dx et un code d'acte\n\n")
              }
              if (is.null(cohort) && !is.null(Dx_table)){
                cat("  Garder les IDs de la cohorte crée à l'étape 1 qui ont eu un Dx et un code d'acte\n\n")
              }
              if (is.null(cohort) && is.null(Dx_table)){
                cat("  Garder les IDs qui ont eu un code d'acte à l'intérieure de la période de l'étude\n\n")
              }
            } else if (verbose && keep_all==TRUE){
              if (length(cohort)>0) {
                cat("  Garder les IDs de la cohorte fournie par l'analyste qui ont eu ou non un code d'acte\n\n")
              }
              if (is.null(cohort) && !is.null(Dx_table)){
                cat("  Garder les IDs de la cohorte crée à l'étape 1 qui ont eu ou non un code d'acte\n\n")
              }
            }

            t1 <- Sys.time()

            if(is.null(cohort) && keep_all==FALSE){ #
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


            # Ajout caractéristiques Benef pour ceux qui n'ont pas eu d'acte facturé dans la période de l'étude ####

            if(keep_all==TRUE){
              if (verbose) {
                if (is.null(cohort) && !is.null(Dx_table)){
                  cat("Étape 4: Ajouter les caractéristiques des bénéficiaires ayant eu un Dx, mais pas d'acte à l'intérieure de la période de l'étude:\n")
                }
                else{
                  cat("Étape 3: Ajouter les caractéristiques des bénéficiaires ayant eu un Dx, mais pas d'acte à l'intérieure de la période de l'étude:\n")
                }
              }

              t1 <- Sys.time()
              DT1<-data.table::as.data.table(odbc::dbGetQuery(conn=conn,
                                                              statement=query_V_FICH_ID_BEN_CM(debut_periode=debut_periode, fin_periode=fin_periode, date_age=date_age)
              ))
              t2 <- Sys.time()
              if (verbose) {
                cat(" - ","Temps d'exécution",
                    " (",round(as.numeric(difftime(t2, t1)), 2),
                    " ",attr(difftime(t2, t1), "units"), ")\n\n",
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

            } else{
              DT_final
            }



            # Add Etiquettes SMOD_COD_ROLE, SMOD_COD_SPEC, ETAB_COD_SECT_ACTIV_ETAB, SMOD_COD_ENTEN, Dx ####
            if (verbose) {
              if (keep_all == TRUE) {
                if (is.null(cohort) && !is.null(Dx_table)) {
                  cat("Étape 5: Rajouter les étiquettes pour `SMOD_COD_ROLE`, `SMOD_COD_SPEC`, `ETAB_COD_SECT_ACTIV_ETAB`, `SMOD_COD_ENTEN`:\n")# et `Dx`
                } else {
                  cat("Étape 4: Rajouter les étiquettes pour `SMOD_COD_ROLE`, `SMOD_COD_SPEC`, `ETAB_COD_SECT_ACTIV_ETAB`, `SMOD_COD_ENTEN`:\n")# et `Dx`
                }
              } else {
                if (is.null(cohort) && !is.null(Dx_table)) {
                  cat("Étape 4: Rajouter les étiquettes pour `SMOD_COD_ROLE`, `SMOD_COD_SPEC`, `ETAB_COD_SECT_ACTIV_ETAB`, `SMOD_COD_ENTEN`:\n")# et `Dx`
                } else {
                  cat("Étape 3: Rajouter les étiquettes pour `SMOD_COD_ROLE`, `SMOD_COD_SPEC`, `ETAB_COD_SECT_ACTIV_ETAB`, `SMOD_COD_ENTEN`:\n")# et `Dx`
                }
              }
            }



            t1 <- Sys.time()

            DT_final<-DT_final %>% mutate(ETAB_COD_SECT_ACTIV_ETAB=ifelse(!is.na(DateActe) & is.na(ETAB_COD_SECT_ACTIV_ETAB),0,ETAB_COD_SECT_ACTIV_ETAB))

            DT_final<-left_join(DT_final,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_ROLE,Desc_SMOD_COD_ROLE) %>% mutate(SMOD_COD_ROLE=as.character(SMOD_COD_ROLE)) %>% na.omit(),by="SMOD_COD_ROLE") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_SPEC,Desc_SMOD_COD_SPEC) %>% na.omit(),by="SMOD_COD_SPEC") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_SPEC,Desc_SMOD_COD_SPEC) %>% rename(SPDisp_fip=SMOD_COD_SPEC,Desc_SMOD_COD_SPECFip=Desc_SMOD_COD_SPEC) %>% na.omit(),by="SPDisp_fip") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_SPEC,Desc_SMOD_COD_SPEC) %>% rename(SPDispRefs=SMOD_COD_SPEC,Desc_SMOD_COD_SPECref=Desc_SMOD_COD_SPEC) %>% na.omit(),by="SPDispRefs") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(ETAB_COD_SECT_ACTIV_ETAB,Desc_ETAB_COD_SECT_ACTIV_ETAB,Secteur_regroupe) %>% na.omit(),by="ETAB_COD_SECT_ACTIV_ETAB") %>%
              left_join(.,RequeteGeneriqueBDCA::Etiquettes %>% data.frame() %>% select(SMOD_COD_ENTEN,Desc_SMOD_COD_ENTEN) %>% mutate(SMOD_COD_ENTEN=as.character(SMOD_COD_ENTEN)) %>% na.omit(),by="SMOD_COD_ENTEN") %>%
              mutate(DxActe=trimws(DxActe))

            DT_final<-DT_final %>% mutate (Desc_SMOD_COD_SPECref=ifelse(!is.na(NumDispRef) & is.na(SPDispRefs), "Med_fam",Desc_SMOD_COD_SPECref))




            # cim9_cim10<-intersect(DT_final[["DxActe"]], c(intersect(RequeteGeneriqueBDCA::Etiquettes$CIM9,RequeteGeneriqueBDCA::Etiquettes$CIM10)))
            #
            # cim9 <- RequeteGeneriqueBDCA::Etiquettes$CIM9
            # cim10 <- RequeteGeneriqueBDCA::Etiquettes$CIM10
            # cim9desc <- RequeteGeneriqueBDCA::Etiquettes$Desc_CIM9
            # cim10desc <- RequeteGeneriqueBDCA::Etiquettes$Desc_CIM10
            # DxActe <- c(cim9, cim10) %>% data.frame()%>% rename(DxActe=".")
            # Desc_DxActe <- c(cim9desc, cim10desc) %>% data.frame()%>% rename(Desc_DxActe=".")
            # DT_DxActe<-cbind(DxActe,Desc_DxActe)
            #
            # if(length(cim9_cim10)==0){
            #   DT_final<-left_join(DT_final,DT_DxActe,by="DxActe") %>%
            #     mutate(NumDispSp=bit64::as.integer64(as.character(NumDispSp)),
            #            NumDispRef=bit64::as.integer64(as.character(NumDispRef)))
            # }
            # else{
            #   DT_final<-left_join(DT_final,DT_DxActe,by="DxActe")
            #   DT_final<-DT_final %>% mutate(Desc_DxActe=ifelse(substr(DxActe,1,1)=="V",NA,Desc_DxActe)) %>%
            #     mutate(NumDispSp=bit64::as.integer64(as.character(NumDispSp)),
            #            NumDispRef=bit64::as.integer64(as.character(NumDispRef)))
            #   warning(paste0("Il y a des Dx qui commencent avec la lettre 'V' qui pourraient être des cim9 ou cim10. L'étiquette de ces Dx n'a pas été rajoutée.",
            #                  "Veuillez-svp les rajouter manuellement en utilisant la BD 'RequeteGeneriqueBDCA::Etiquettes'.", "Voici les Dx à vérifier: ", paste(cim9_cim10, collapse = ","),"\n"))
            # }

           t2 <- Sys.time()
            if (verbose) {
              cat(" - ","Temps d'exécution",
                  " (",round(as.numeric(difftime(t2, t1)), 2),
                  " ",attr(difftime(t2, t1), "units"), ")\n \n",
                  sep = "")
            }

            # Analyses des actes médicaux ####
            if (verbose) {
              if (keep_all == TRUE) {
                if (is.null(cohort) && !is.null(Dx_table)) {
                  cat("Étape 6: Préparation des tableaux et des figures pour les analyses discriptives des actes:\n")
                } else {
                  cat("Étape 5: Préparation des tableaux et des figures pour les analyses discriptives des actes:\n")
                }
              } else {
                if (is.null(cohort) && !is.null(Dx_table)) {
                  cat("Étape 5: Préparation des tableaux et des figures pour les analyses discriptives des actes:\n")
                } else {
                  cat("Étape 4: Préparation des tableaux et des figures pour les analyses discriptives des actes:\n")
                }
              }
            }

            # variation des actes par année ####
            if (verbose) {
              cat("  Tableau 1: Variation du nombre d'acte par année civille \n")
            }
            t1 <- Sys.time()
            Acte_ann<-expss::cro(DT_final$CodeActe,DT_final$AnCivil)
            Acte_ann$row_labels <- stringr::str_sub(Acte_ann$row_labels, 19, 100)
            original_colnames <- colnames(Acte_ann %>% data.frame())
            modified_colnames <- stringr::str_sub(original_colnames, start = 18, end = 100)
            colnames(Acte_ann) <- modified_colnames
            Tab1_Nb_Acte_annee<-Acte_ann
            rm(Acte_ann)
            t2 <- Sys.time()
            if (verbose) {
              cat(" - ","Temps d'exécution",
                  " (",round(as.numeric(difftime(t2, t1)), 2),
                  " ",attr(difftime(t2, t1), "units"), ")\n \n",
                  sep = "")
            }

            # Variation des actes par spécialité de disp ####
            if (verbose) {
              cat("  Tableau 2: Variation du nombre d'acte par spécialité de dispensateur \n")
            }
            t1 <- Sys.time()
            Acte_spec<-expss::cro(DT_final$CodeActe,DT_final$Desc_SMOD_COD_SPEC)
            Acte_spec$row_labels <- stringr::str_sub(Acte_spec$row_labels, 19, 100)
            original_colnames <- colnames(Acte_spec %>% data.frame())
            modified_colnames <- stringr::str_sub(original_colnames, start = 29, end = 100)
            colnames(Acte_spec) <- modified_colnames
            Tab2_Nb_Acte_spec<-Acte_spec
            rm(Acte_spec)
            t2 <- Sys.time()
            if (verbose) {
              cat(" - ","Temps d'exécution",
                  " (",round(as.numeric(difftime(t2, t1)), 2),
                  " ",attr(difftime(t2, t1), "units"), ")\n \n",
                  sep = "")
            }

            # Variation des actes par lieu ####
            if (verbose) {
              cat("  Tableau 3: Variation du nombre d'acte par lieu de service \n")
            }
            t1 <- Sys.time()
            Acte_lieu<-expss::cro(DT_final$CodeActe,DT_final$Desc_ETAB_COD_SECT_ACTIV_ETAB)
            Acte_lieu$row_labels <- stringr::str_sub(Acte_lieu$row_labels, 19, 100)
            original_colnames <- colnames(Acte_lieu %>% data.frame())
            modified_colnames <- stringr::str_sub(original_colnames, start = 40, end = 1000)
            colnames(Acte_lieu) <- modified_colnames
            Tab3_Nb_Acte_lieu<-Acte_lieu
            rm(Acte_lieu)
            t2 <- Sys.time()
            if (verbose) {
              cat(" - ","Temps d'exécution",
                  " (",round(as.numeric(difftime(t2, t1)), 2),
                  " ",attr(difftime(t2, t1), "units"), ")\n \n",
                  sep = "")
            }

            # Freq combinaison des actes facturés par le même disp, à la même date pour le même benef ####
            if (verbose) {
              cat("  Tableau 4: Fréquence des combinaisons d'actes facturés par le même disp, à la même date pour le même benef \n")
            }
            t1 <- Sys.time()
            tmp<-DT_final %>%
              dplyr::select(ID,DateActe,CodeActe,NumDispSp)
            tmp<-data.table::setDT(tmp)[, lapply(.SD, paste, collapse="-") , by = c("ID","DateActe","NumDispSp")] #,"Desc_SMOD_COD_ROLE","Desc_SMOD_COD_SPEC"
            tmp<-tmp %>%
              dplyr::rename(Combine_CodeActe=CodeActe)


            tmp1<-epiDisplay::tab1(tmp$Combine_CodeActe, cum.percent = F,graph=FALSE) %>%
              data.frame() %>%
              select(-1) %>%
              arrange(desc(output.table.Frequency)) %>% tibble::rownames_to_column() %>%
              rename(Combine_CodeActe=rowname,
                     Frequence="output.table.Frequency",
                     pourcentage="output.table.Percent")
            Tab4_Combin_Acte_Freq<-tmp1
            rm(tmp,tmp1)

            t2 <- Sys.time()
            if (verbose) {
              cat(" - ","Temps d'exécution",
                  " (",round(as.numeric(difftime(t2, t1)), 2),
                  " ",attr(difftime(t2, t1), "units"), ")\n \n",
                  sep = "")
            }

            # calucl du Nb Rep acte avant la suppression des doublant (meme bene, date, disp, code acte) ####
            # Rep acte by ID,DateActe,NumDispSp
            if (verbose) {
              cat("  Tableau 5: Nombre de répétion d'acte  par (ID, Date Acte, Numéro Disp Spec) et (ID, Date Acte) \n")
            }
            t1 <- Sys.time()
            Nb_Acte<-DT_final %>%
              select(ID,CodeActe,DateActe,NumDispSp) %>%
              mutate(n=1) %>%
              group_by(ID,CodeActe,DateActe,NumDispSp) %>%
              mutate(Nb_Acte_Id_Date_Disp=sum(n)) %>%
              distinct(ID,CodeActe,DateActe,NumDispSp,.keep_all = T)

            Nb_Acte_Id_Date_Disp<-epiDisplay::tab1(Nb_Acte$Nb_Acte_Id_Date_Disp, cum.percent = F,graph=FALSE) %>%
              data.frame() %>%
              select(-1) %>%
              tibble::rownames_to_column() %>%
              rename(Nb_repetition=rowname)%>%
              tibble::rownames_to_column("Titre") %>%
              mutate(`Nombre Répétition d'acte par Ids, date et disp` = NA)


            # Rep acte by ID,DateActe

            Nb_Acte<-DT_final %>%
              select(ID,CodeActe,DateActe) %>%
              mutate(n=1) %>%
              group_by(ID,CodeActe,DateActe) %>%
              mutate(Nb_Acte_Id_Date_Disp=sum(n)) %>%
              distinct(ID,CodeActe,DateActe,.keep_all = T)

            Nb_Acte_Id_Date<-epiDisplay::tab1(Nb_Acte$Nb_Acte_Id_Date_Disp, cum.percent = F,graph=FALSE) %>%
              data.frame() %>%
              select(-1) %>%
              tibble::rownames_to_column() %>%
              rename(Nb_repetition=rowname)%>%
              tibble::rownames_to_column("Titre") %>%
              mutate(`Nombre d'acte par Ids et date` = NA)


            Nb_Acte_rep<-bind_rows(Nb_Acte_Id_Date_Disp,Nb_Acte_Id_Date,.id = "source")
            Nb_Acte_rep<-Nb_Acte_rep %>%
              mutate(Titre=ifelse(source==1,"Nombre Répétition d'acte par Ids, date et disp",
                                  "Nombre Répétition d'acte par Ids et date")) %>%
              rename(Frequence=`output.table.Frequency`,
                     Pourcentage=`output.table.Percent`) %>%
              select(2,3,4,5) %>%
              mutate(Titre = ifelse(duplicated(Titre),NA,Titre))
            Tab5_Nb_Acte_rep<-Nb_Acte_rep
            rm(Nb_Acte,Nb_Acte_Id_Date,Nb_Acte_Id_Date_Disp,Nb_Acte_rep)
            t2 <- Sys.time()
            if (verbose) {
              cat(" - ","Temps d'exécution",
                  " (",round(as.numeric(difftime(t2, t1)), 2),
                  " ",attr(difftime(t2, t1), "units"), ")\n \n",
                  sep = "")
            }

            # variation de cout d'acte ####
            if (verbose) {
              cat("  Figure 1: Variation des coûts de facturation par acte  \n \n")
            }
            t1 <- Sys.time()

            # Define a function to filter and plot the data for each CodeActe
            list_code <- unique(DT_final$CodeActe)
            filter_and_plot <- function(codeActe) {
              data <- DT_final %>%
                select(CodeActe, CoutActe, Desc_SMOD_COD_SPEC) %>%
                filter(CodeActe == codeActe, Desc_SMOD_COD_SPEC != "Anesthésiologie")

              # Calculate statistics for the CodeActe
              max_count <- max(table(data$CoutActe))
              mean_cout <- mean(data$CoutActe)
              median_cout <- median(data$CoutActe)
              range_cout <- range(data$CoutActe)

              # Create the plot
              plot <-ggplot(data, aes(x = CoutActe)) +
                geom_histogram(aes(fill = Desc_SMOD_COD_SPEC), bins = 30) +
                theme(legend.position = "top",legend.text = element_text(size = 12))+
                scale_x_continuous(breaks = pretty(range(DT_final$CoutActe))) +
                scale_y_continuous(labels = scales::number_format(scale = 1, accuracy = 1)) +
                labs(x = "Coût Acte", y = "Fréquence",
                     title = paste0("Code Acte: ", codeActe)) +
                theme(axis.text = element_text(size = 12),
                      axis.title = element_text(size = 14))+
                geom_vline(xintercept = mean_cout, color = "red", size = 1, linetype = "dashed") +
                geom_vline(xintercept = median_cout, color = "blue", size = 1, linetype = "dashed") +
                annotate("text", x = mean_cout - (mean_cout*0.15), y = max_count,
                         label = paste0("Mean = ", format(round(mean_cout, 1), nsmall = 1)), color = "red",size=6)+
                annotate("text", x = mean_cout - (mean_cout*0.15), y = max_count - (max_count*0.05),
                         label = paste0("Median = ", format(round(median_cout, 1), nsmall = 1)), color = "blue",size=6)+
                annotate("text", x = mean_cout - (mean_cout*0.15), y = max_count - (max_count*0.1),
                         label = paste0("Range = ", format(round(range_cout[1], 1), nsmall = 1), " - ", format(round(range_cout[2], 1), nsmall = 1)), color = "black",size=6)

              #plot<- plotly::ggplotly(plot)
              return(plot)
            }

            # Apply the function to each CodeActe and save the plots
            plots<- lapply(unique(DT_final$CodeActe), filter_and_plot)
            # Arrange the plots using grid.arrange
            Plot_Cout_acte_spec <- gridExtra::grid.arrange(grobs = plots, ncol = length(list_code))


            # coût Anesthésiologie
            # Define a function to filter and plot the data for each CodeActe
            list_code <- unique(DT_final$CodeActe)
            filter_and_plot <- function(codeActe) {
              data <- DT_final %>%
                select(CodeActe, CoutActe, Desc_SMOD_COD_SPEC) %>%
                filter(CodeActe == codeActe, Desc_SMOD_COD_SPEC == "Anesthésiologie")

              # Calculate statistics for the CodeActe
              max_count <- max(table(data$CoutActe))
              mean_cout <- mean(data$CoutActe)
              median_cout <- median(data$CoutActe)
              range_cout <- range(data$CoutActe)

              # Create the plot
              plot <- ggplot(data, aes(x = CoutActe)) +
                geom_histogram(aes(fill = Desc_SMOD_COD_SPEC), bins = 30) +
                theme(legend.position = "top",legend.text = element_text(size = 12))+
                scale_x_continuous(breaks = pretty(range(DT_final$CoutActe))) +
                scale_y_continuous(labels = scales::number_format(scale = 1, accuracy = 1)) +
                labs(x = "Coût Acte", y = "Fréquence",
                     title = paste0("Code Acte: ", codeActe)) +
                theme(axis.text = element_text(size = 12),
                      axis.title = element_text(size = 14))+
                geom_vline(xintercept = mean_cout, color = "red", size = 1, linetype = "dashed") +
                geom_vline(xintercept = median_cout, color = "blue", size = 1, linetype = "dashed") +
                annotate("text", x = mean_cout - (mean_cout*0.15), y = max_count,
                         label = paste0("Mean = ", format(round(mean_cout, 1), nsmall = 1)), color = "red",size=6)+
                annotate("text", x = mean_cout - (mean_cout*0.15), y = max_count - (max_count*0.05),
                         label = paste0("Median = ", format(round(median_cout, 1), nsmall = 1)), color = "blue",size=6)+
                annotate("text", x = mean_cout - (mean_cout*0.15), y = max_count - (max_count*0.1),
                         label = paste0("Range = ", format(round(range_cout[1], 1), nsmall = 1), " - ", format(round(range_cout[2], 1), nsmall = 1)), color = "black",size=6)

              #plot<- plotly::ggplotly(plot)


              return(plot)
            }

            # Apply the function to each CodeActe and save the plots
            plots<- lapply(unique(DT_final$CodeActe), filter_and_plot)

            Plot_Cout_acte_Anesth<-gridExtra::grid.arrange(grobs = plots, ncol = length(list_code))


            # changer les noms de variables et reorganiser l'ordre ####
            DT_final<-DT_final %>%
              rename(Code_RSS_Ben=RSS_Benf,
                     Desc_RSS_Ben=NomRSS_Benef,
                     Code_RLS_Ben=RLS_Benef,
                     Desc_RLS_Ben=NomRLS_Benef,
                     Code_Acte=CodeActe,
                     Cout_Acte=CoutActe,
                     Code_DxActe=DxActe,
                     Code_Role=SMOD_COD_ROLE,
                     Desc_Role=Desc_SMOD_COD_ROLE,
                     Code_Entente=SMOD_COD_ENTEN,
                     Desc_Entente=Desc_SMOD_COD_ENTEN,
                     Code_SpecSmod=SMOD_COD_SPEC,
                     Desc_SpecSmod=Desc_SMOD_COD_SPEC,
                     Code_SpecFip=SPDisp_fip,
                     Desc_SpecFip=Desc_SMOD_COD_SPECFip,
                     Code_SpecDispRef=SPDispRefs,
                     Desc_SpecDispRef=Desc_SMOD_COD_SPECref,
                     NumEtabUsuel=Num_ETAB_USUEL,
                     NumEtab=NO_ETAB,
                     NomEtab=NOM_ETAB,
                     Code_SectActiv=ETAB_COD_SECT_ACTIV_ETAB,
                     Desc_SectActiv=Desc_ETAB_COD_SECT_ACTIV_ETAB,
                     Desc_SectActivRegroup=Secteur_regroupe,
                     Code_CatEtab=Cat_Etab,
                     Code_RSS_Etab=RSS_Etab,
                     Desc_RSS_Etab=NomRSS_Etab,
                     Code_RLS_Etab=RLS_Etab,
                     Desc_RLS_Etab=NomRLS_Etab) %>%
              select(ID,Sexe,DatNais,DatDeces,Age,Code_RSS_Ben,Desc_RSS_Ben,Code_RLS_Ben,Desc_RLS_Ben,
                     AnFinan,AnCivil,DateActe,Code_Acte,Cout_Acte,Code_DxActe,Code_Role, #Desc_DxActe
                     Desc_Role,Code_Entente,Desc_Entente,NumDispSp,Code_SpecSmod,Desc_SpecSmod,Code_SpecFip,
                     Desc_SpecFip,NumDispRef,Code_SpecDispRef,Desc_SpecDispRef,NumEtabUsuel,NumEtab,NomEtab,
                     Code_SectActiv,Desc_SectActiv,Desc_SectActivRegroup,Code_CatEtab,Code_RSS_Etab,Desc_RSS_Etab,
                     Code_RLS_Etab,Desc_RLS_Etab)


            # Save data and graphs ####
            if(!is.null(setwd)){
              if (verbose) {
                if (keep_all == TRUE) {
                  if (is.null(cohort) && !is.null(Dx_table)) {
                  cat(paste0("Étape 7: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                } else {
                  cat(paste0("Étape 6: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                }
              } else {
                if (is.null(cohort) && !is.null(Dx_table)) {
                  cat(paste0("Étape 6: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                } else {
                  cat(paste0("Étape 5: Sauvegarde des BD et figures dans: ",setwd,"\n"))
                }
              }
            }


            list_of_datasets <- list("Tab1_Nb_Acte_annee" = Tab1_Nb_Acte_annee,
                                     "Tab2_Nb_Acte_spec" = Tab2_Nb_Acte_spec,
                                     "Tab3_Nb_Acte_lieu" = Tab3_Nb_Acte_lieu,
                                     "Tab4_Combin_Acte_Freq" = Tab4_Combin_Acte_Freq,
                                     "Tab5_Nb_Acte_rep" = Tab5_Nb_Acte_rep)

            write.table(DT_final, file = paste0(setwd,"/DT_final","_",as.Date(Sys.time()),".txt"), sep = ";", dec = ".",row.names = F, col.names = T,quote = FALSE)
            openxlsx::write.xlsx(list_of_datasets, file = paste0(setwd,"/Analyse_descript_Acte","_",as.Date(Sys.time()),".xlsx"))
            ggplot2::ggsave(filename = paste0(setwd,"/Cout_acte","_",as.Date(Sys.time()),".png"), plot = Plot_Cout_acte_spec, width = 30, height = 20, dpi = 300, units = "in")
            ggplot2::ggsave(filename = paste0(setwd,"/Cout_acte","_",as.Date(Sys.time()),".png"), plot = Plot_Cout_acte_Anesth, width = 30, height = 20, dpi = 300, units = "in")

            # dev.off()
            return_list <- list(DT_final,Tab1_Nb_Acte_annee,Tab2_Nb_Acte_spec,Tab3_Nb_Acte_lieu,Tab4_Combin_Acte_Freq,Tab5_Nb_Acte_rep)
            names(return_list) <- c("DT_final", "Tab1_Nb_Acte_annee", "Tab2_Nb_Acte_spec", "Tab3_Nb_Acte_lieu","Tab4_Combin_Acte_Freq","Tab5_Nb_Acte_rep")
            return(return_list)
            }

            else{
            if (verbose) {
              if (nrow(DT_final) == 0) {
                cat("Aucune observation n'a été trouvée. Veuillez-svp vérifier si les Dx et les codes d'acte sont corrects ou si les Dx match avec les actes.")
                return(NULL)
              }
            }
            return_list <- list(DT_final,Tab1_Nb_Acte_annee,Tab2_Nb_Acte_spec,Tab3_Nb_Acte_lieu,Tab4_Combin_Acte_Freq,Tab5_Nb_Acte_rep,Plot_Cout_acte_spec,Plot_Cout_acte_Anesth)
            names(return_list) <- c("DT_final", "Tab1_Nb_Acte_annee", "Tab2_Nb_Acte_spec", "Tab3_Nb_Acte_lieu","Tab4_Combin_Acte_Freq","Tab5_Nb_Acte_rep","Plot_Cout_acte_spec","Plot_Cout_acte_Anesth")
            return(return_list)
            }
          }
          )
}






