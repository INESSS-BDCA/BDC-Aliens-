#' @title RequeteGeneriqueBDCA_shiny
#'
#' @description L'application `RequeteGeneriqueBDCA_shiny` est destinée aux utilisateurs.
#'
#' #' Elle permet: \cr
#' 1) d'extraire les variables pertinantes de différentes vues de données.\cr
#' 2) de creéer une cohorte en utilisant les Dx cim9 et cim10.\cr
#' 3) d'appliquer une standardisation directe ou indirect. \cr
#'
#'
#' @encoding UTF-8
#' @import shiny shinyAce bslib shinycssloaders shinyjs plotly ggplot2 dplyr stringr odbc expss epiDisplay shinyWidgets
#' @export

RequeteGeneriqueBDCA_shiny<-function(){

  # Ui- Define the user interface for the Shiny app ####
  ui <- fluidPage(
    titlePanel("Requête Générique BDCA"),
    #theme = bs_theme(version = 4, bootswatch = "yeti"),
    tags$head(
      tags$style(
        HTML('.navlist-wrapper {width: 1000px;}')
      )
    ),
    div(class="navlist-wrapper",
        navlistPanel(
          # Extraction des données ####
          tabPanel("Extraction des données",style='width: 1500px; height: 1000px',
                   sidebarLayout(
                     sidebarPanel(

                         # Base de données - Requêt ####
                       wellPanel(
                         tags$h4("Base de données - Requête", style = "font-weight: bold;"),
                         textInput("fct", label = "Fonction générique exécutée",value = ""),
                         selectInput("DT", label = HTML(paste0("Banque de données", div(helpText("Indiquer la banque de données"), class = "pull-below"))),
                                     choices = c("SMOD", "BDCU", "MEDECHO")),
                         selectInput("task", label = HTML(paste0("Requête", div(helpText("Indiquer la requêt à exécuter"), class = "pull-below"))),
                                     choices = NULL),
                         style = "background-color: #e1f1f7;"),


                       wellPanel(
                         tags$h4("Paramètres de la fonction", style = "font-weight: bold;"),
                         # Connexion ####
                         dropdownButton(
                           inputId = "mydropdown_connexion",
                           label = "Connexion",
                           icon = icon("plug"),
                           status = "primary",
                           circle = FALSE,
                           wellPanel(
                             #h4("Connexion"),
                             textInput("sql_user", label = HTML(paste0("Identifiant", div(helpText("Indiquer votre identifiant teradata (ms0xx)"), class = "pull-below"))), value = "ms069a"),
                             passwordInput("sql_pwd", label = HTML(paste0("Mot de passe", div(helpText("Indiquer votre mot de passe teradata"), class="pull-below"))), value = "Kong2051"),
                             style = "background-color: #e1f1f7;")),
                         tags$br(),
                         # Cohorte ####
                         dropdownButton(
                           inputId = "mydropdown_cohort",
                           label = "Cohorte",
                           icon = icon("users"),
                           status = "primary",
                           circle = FALSE,
                           wellPanel(
                             #h4("Cohorte"),
                             helpText("* Si cohorte = NULL (il n'y a pas de cohorte prédéfinie) : extraction d'acte pour tous ceux ayant eu une date d'acte à l'intérieure de la période d'étude.",tags$br(),
                                      "* Si cohorte = cohorte (il y a une cohorte prédéfinie): extraction d'acte pour les individus de la cohorte prédéfinie ayant eu une date d'acte à l'intérieure de la période d'étude.",tags$br(),
                                      "* Si CRÉATION D'UNE COHORTE ✅ :", tags$br() ,
                                      "Étape 1 - création d'une cohorte en utilisant l'algorithme SQL_reperage_cond_med.", tags$br(),
                                      "Étape 2 - extraction d'acte pour les individus de la cohorte crée ayant une date d'acte à l'intérieure de la période de l'étude.", tags$br(),
                                      "* Si vous avez une cohorte prédéfinie ou une cohorte crée, vous devez mentionner si vous voulez selectionner tous les individus de la cohorte ayant eu ou non un acte
                                          à l'intérieure de la période d'étude 'Cas retenus'✅ .",tags$br(), tags$br()),

                             selectInput("cohort", label = HTML(paste0("Cohorte prédéfinie", div(helpText("Indiquer si vous avez une cohorte prédéfinie (défaut=NULL)"), class = "pull-below"))),
                                         choices=c("NULL", "cohort")),
                             uiOutput("fileInput"),
                             checkboxInput("creation_cohort", label = HTML(paste0("<b>CRÉATION D'UNE COHORTE</b>", div(helpText("Selectionner pour afficher les paramétres pour la création d'une cohorte.",tags$br(),
                                                                                                                                "La création de la cohorte est effectuée avec l'algorithme de la fonction : SQL_reperage_cond_med"), class = "pull-below"))),
                                           value=FALSE),
                             uiOutput("creation_cohort_select"),
                             checkboxInput("keep_all", label = HTML(paste0("<b>Cas retenus</b>", div(helpText("Selection des Cas retenus (défaut=selectionner seulement les personnes avec un acte).",tags$br(),
                                                                                                              "Si 'Cas retenue'✅ : selectionner toutes les personnes de la cohorte prédéfinie ou crée"), class = "pull-below"))),
                                           value=FALSE),
                             style = "background-color: #e1f1f7;")),
                         tags$br(),
                         # Période de l'étude ####
                         dropdownButton(
                           inputId = "mydropdown_periode_etude",
                           label = "Période de l'étude",
                           icon = icon("calendar"),
                           status = "primary",
                           circle = FALSE,
                           wellPanel(
                             #tags$h4("Période de l'étude", style = "font-weight: bold;"),
                             textInput("debut_periode", label = HTML(paste0("Début de la période à l'étude", div(helpText("Indiquer la date de début de l'étude (AAAA-MM-JJ)"), class = "pull-below"))),
                                       value="2020-01-01"),
                             textInput("fin_periode", label = HTML(paste0("Fin de la période à l'étude", div(helpText("Indiquer la date de fin de l'étude (AAAA-MM-JJ)"), class = "pull-below"))),
                                       value="2020-12-31"),
                             style = "background-color: #e1f1f7;")),
                         tags$br(),
                         # Paramètres spécifiques de requête ####
                         dropdownButton(
                           inputId = "mydropdown_param_requete",
                           label = "Paramètres spécifiques de requête",
                           icon = icon("list"),
                           status = "primary",
                           circle = FALSE,
                           wellPanel(
                             #tags$h4("Paramètres spécifiques de requête", style = "font-weight: bold;"),
                             uiOutput("DTInput"),
                             style = "background-color: #e1f1f7;")),
                         tags$br(),
                         # Caractéristiques bénéficiaire ####
                         dropdownButton(
                           inputId = "mydropdown_carac_benef",
                           label = "Caractéristiques bénéficiaire",
                           icon = icon("venus-mars"),
                           status = "primary",
                           circle = FALSE,
                           wellPanel(
                             #h4("Caractéristiques bénéficiaire"),
                             selectInput("benef_adr", label = HTML(paste0("Adresse du bénéficiaire", div(helpText("Indiquer la méthode d'identification de l'adresse du bénéficiaire (défaut=dernière_adresse).",tags$br(),
                                                                                                                  "Si Adresse du bénéficiaire = date_fixe, Indiquer une date (AAAA-MM-JJ) dans 'Date adresse'"), class = "pull-below"))),
                                         choices = c("date_fixe","dernière_adresse","première_adresse","long_adresse","date_acte"),selected = "dernière_adresse"),
                             textInput("date_adr", label = HTML(paste0("Date adresse", div(helpText("Indiquer une date (AAAA-MM-JJ) pour identifier l'adresse du bénéficiaire"), class = "pull-below"))),
                                       value="NULL"),
                             textInput("date_age", label = HTML(paste0("Date âge", div(helpText("Si 'Cas retenus' est selectionné ✅ indiquer la date (AAAA-MM-JJ) pour le calcul de l’âge des personnes sans acte au cours de la période à l’étude (l’âge de bénéficiaires qui ont eu un acte à l’intérieur de la période de l’étude est calculé à la date de l’acte)"), class = "pull-below"))),
                                       value="NULL"),
                             style = "background-color: #e1f1f7;")),
                         tags$br(),
                         # Paramètres optionnels ####
                         dropdownButton(
                           inputId = "mydropdown_param_optionel",
                           label = "Paramètres optionnels",
                           icon = icon("gear"),
                           status = "primary",
                           circle = FALSE,
                           wellPanel(
                             checkboxInput("verbose", label = HTML(paste0("verbose", div(helpText("Par défault verbose=TRUE, un message de progression est affiché sous l'onglet résultats: Message de progression et warnings:"), class = "pull-below"))),
                                           value=TRUE),
                             checkboxInput("Code_sql", label = HTML(paste0("Paramétres d'affichage du script sql", div(helpText("Selectionner ✅ pour afficher les paramétres d'affichage de script sql"), class = "pull-below"))),
                                           value=FALSE),
                             uiOutput("Code_sql_select"),
                             selectInput("fileType", label=HTML(paste0("Format du fichier",div(helpText("Indiquer le format du fichier à télécharger"),class = "pull-below"))), choices = c("csv", "txt","png")),
                             style = "background-color: #e1f1f7;")),
                         tags$br(),
                         actionButton("Executer", "Exécuter la requête",style = "background-color: green;color: white;"),
                         actionButton("reset", "Réinitialiser les champs"),
                         style = "background-color: #e1f1f7;"),
                       tags$br(),
                         # Paramètres Teradata studio ####
                       wellPanel(
                         tags$h4("Teradata Studio", style = "font-weight: bold;"),
                         dropdownButton(
                           inputId = "mydropdown_Teradata_studio",
                           label = "Paramètres Teradata studio",
                           icon = icon("gear"),
                           status = "primary",
                           circle = FALSE,
                           wellPanel(
                             textInput("sql_user_tera", label = HTML(paste0("Identifiant", div(helpText("Indiquer votre identifiant teradata (ms0xx)"), class = "pull-below"))), value = "ms069a"),
                             passwordInput("sql_pwd_tera", label = HTML(paste0("Mot de passe", div(helpText("Indiquer votre mot de passe teradata"), class="pull-below"))), value = "Kong2051"),
                             checkboxInput("Code_sql_tera", label = HTML(paste0("Paramétres d'affichage du script sql", div(helpText("Selectionner ✅ pour afficher les paramétres d'affichage de script sql"), class = "pull-below"))),
                                           value=FALSE),
                             uiOutput("Code_sql_select_tera"),
                             selectInput("fileType_tera", label=HTML(paste0("Format du fichier",div(helpText("Indiquer le format du fichier à télécharger"),class = "pull-below"))), choices = c("csv", "txt")),
                             style = "background-color: #e1f1f7;")),
                         tags$br(),
                         actionButton("Executer_tera", "Exécuter la requête",style = "background-color: green;color: white;"),
                         actionButton("reset_tera", "Réinitialiser les champs"),
                         style = "background-color: #e1f1f7;"),

                     ),
                     # Main Panel ####
                     mainPanel(
                       tabsetPanel(type="tabs",
                                   tabPanel("Résultats", style='width: 1000px; height: 1000px',
                                            h4("Résultats de la requête"),
                                            h5("Code sql:"),
                                            p("Afficher le script sql exécuté pour effectuer la requête demandée.", style = "font-family: 'times'; font-si16pt"),
                                            shinyAce ::aceEditor(outputId = "ace",
                                                                 selectionId = "selection",
                                                                 placeholder = "Afficher le script sql de la requête ...",
                                                                 mode="sql",
                                                                 theme="sqlserver"),
                                            h5("Message de progression et warnings:"),
                                            verbatimTextOutput("result"),
                                            uiOutput("DT_final_ui"),
                                            uiOutput("DT_final_ui1")),
                                   tabPanel("Teradata Studio", style='width: 1000px; height: 1000px',
                                            h4("Résultats de la requête"),
                                            h5("Code sql:"),
                                            p("Afficher le script sql exécuté pour effectuer la requête demandée.", style = "font-family: 'times'; font-si16pt"),
                                            shinyAce ::aceEditor(outputId = "ace_tera",
                                                                 selectionId = "selection_tera",
                                                                 placeholder = "Afficher le script sql de la requête ...",
                                                                 mode="sql",
                                                                 theme="sqlserver"),
                                            h5("Message de progression et warnings:"),
                                            verbatimTextOutput("result_tera"),
                                            uiOutput("DT_final_ui_tera")),
                                   tabPanel("Vignettes", style='width: 1000px; height: 1000px',
                                            h4("vignette de la fonction"),
                                            conditionalPanel(
                                              condition = "input.DT == 'SMOD'",
                                              includeHTML(system.file("vignettes/user_I_SMOD_SERV_MD_CM_AG.html", package = "RequeteGeneriqueBDCA"))),
                                            conditionalPanel(
                                              condition = "input.DT == 'BDCU'",
                                              includeHTML(system.file("vignettes/user_V_EPISO_SOIN_DURG_CM_AG.html", package = "RequeteGeneriqueBDCA"))),
                                            conditionalPanel(
                                              condition = "input.DT == 'MEDECHO'",
                                              includeHTML(system.file("vignettes/user_MEDECHO_DIAGN_SEJ_HOSP_CM.html", package = "RequeteGeneriqueBDCA")))
                                   )
                       )
                     )
                   )
          ),
          # Création d'une cohorte ####
          tabPanel("Création d'une cohorte",style='width: 1500px; height: 1000px',
                   sidebarLayout(
                     sidebarPanel(
                       wellPanel(
                       textInput("fct_tab2", label = h3("Fonction générique exécutée"),value = "SQL_reperage_cond_med"),
                       textInput("sql_user_tab2", label = HTML(paste0("Identifiant", div(helpText("Indiquer votre identifiant teradata (ms0xx)"), class = "pull-below"))),
                                 value = "ms069a"),
                       passwordInput("sql_pwd_tab2", label = HTML(paste0("Mot de passe", div(helpText("Indiquer votre mot de passe teradata"), class="pull-below"))),
                                     value = "Kong2051"),
                       textInput("Dx_table_tab2", label = HTML(paste0("Liste des diagnostics", div(helpText("Indiquer la liste des diagnostics de la condition médicale à utiliser pour la création de la cohorte.",tags$br(),
                                                                                                            "Par exemple : list(Coronaro = list(CIM9 = paste0(c(491, 492, 496), '%'), CIM10 = paste0('J', 41:44, '%')))"), class = "pull-below"))),
                                 value = "list(Coronaro = list(CIM9 = paste0(c(491, 492, 496), '%'), CIM10 = paste0('J', 41:44, '%')))"),
                       textInput("CIM_tab2", label = HTML(paste0("Classification des diagnostics", div(helpText("Indiquer la classification des diagnostics à utiliser (CIM9, CIM10 ou les deux)"), class = "pull-below"))),
                                 value = paste(c("CIM9","CIM10"),collapse = ",")),

                       textInput("debut_cohort_tab2", label = HTML(paste0("Date début de la cohorte", div(helpText("Indiquer le début de la période pour la cohorte (AAAA-MM-JJ)"), class = "pull-below"))),
                                 value="2020-01-01"),
                       textInput("fin_cohort_tab2", label = HTML(paste0("Date fin de la cohorte", div(helpText("Indiquer la finde la période pour la cohorte (AAAA-MM-JJ)"), class = "pull-below"))),
                                 value="2020-12-31"),
                       checkboxInput("by_Dx_tab2", label = HTML(paste0("Stratification de la cohorte", div(helpText("Indiquer si la cohorte doit être stratifiée ou non selon les conditions médicales définies dans la liste des diagnostics"), class = "pull-below"))),
                                     value=FALSE),
                       selectInput("date_dx_var_tab2", label = HTML(paste0("Date d'admission ou de départ", div(helpText("Indiquer si on doit utiliser la date d’admission ou de départ comme date d’incidence lors des hospitalisations ou des visites à l’urgence"), class = "pull-below"))),
                                   choices = c("admis","depar"),selected="admis"),
                       h4("PARAMÈTRES DE L’ALGORITHME"),
                       helpText("Indiquer les paramètres de l’algorithme soit : le nombre de jours (n1) entre 2 diagnostics, la période de temps (n2) à considérer pour confirmer le premier diagnostic et le nombre minimum d’occurrences du diagnostic pour confirmer la condition (nDx).",
                                "(Valeurs par défaut n1= 30 jours, n2= 730 jours et nDx=1)"),
                       numericInput("n1_tab2", label = HTML(paste0("n1 (nombre de jours)", div(helpText("Indiquer le nombre de jours entre lesquels deux diagnostics doit se situer pour que le deuxième Dx confirme le premier"), class = "pull-below"))),
                                    value = 30),
                       numericInput("n2_tab2", label = HTML(paste0("n2 (période de temps) ", div(helpText("Indiquer le nombre de jours entre lesquels deux diagnostics doit se situer pour que le deuxième Dx confirme le premier"), class = "pull-below"))),
                                    value = 730),
                       numericInput("nDx_tab2", label = HTML(paste0("nDx (nombre de diagnostics)", div(helpText("Choisir le nombre de diagnostic qui permet de confirmer le premier Dx"), class = "pull-below"))),
                                    value = 0),

                       selectInput("fileType_tab2", label=HTML(paste0("Format du fichier",div(helpText("Indiquer le format du fichier à télécharger"),class = "pull-below"))), choices = c("csv", "txt")),
                       actionButton("Executer_tab2", "Exécuter la requête",style = "background-color: green;color: white;"),
                       actionButton("reset_tab2", "Réinitialiser les champs"),
                       style = "background-color: #e1f1f7;")),
                     mainPanel(
                       tabsetPanel(type="tabs",
                                   tabPanel("Résultats", style='width: 1000px; height: 1000px',
                                            h4("Résultats de la requête"),
                                            h5("Message de progression et warnings:"),
                                            verbatimTextOutput("result_tab2"),
                                            uiOutput("DT_final_ui_tab2")),
                                   tabPanel("Vignettes", style='width: 1000px; height: 1000px',
                                            h4("vignette de la fonction"),
                                            includeHTML(system.file("vignettes/SQL_reperage_cond_med.html", package = "RequeteGeneriqueBDCA"))
                                   )
                       )
                     )
                   )
          ),
          # standardisation ####
          tabPanel("Standardisation",style='width: 1500px; height: 1000px',
                   sidebarLayout(
                     sidebarPanel(
                       wellPanel(
                       textInput("fct_tab3", label = h3("Fonction générique exécutée"),value = "standardisation"),
                       textInput("donnees", label = HTML(paste0("donnees", div(helpText(""), class = "pull-below"))),
                                 value = "donnees"),
                       fileInput("DT_tab3", "Choisir votre cohorte d'intérêt"),
                       textInput("regroupement", label = HTML(paste0("regroupement", div(helpText(""), class = "pull-below"))),
                                 value = ""),
                       textInput("regroupement_reference", label = HTML(paste0("regroupement_reference", div(helpText(""), class = "pull-below"))),
                                 value = "NULL"),
                       textInput("age", label = HTML(paste0("age", div(helpText(""), class = "pull-below"))),
                                 value = "age_patient"),
                       textInput("age_cat_mins", label = HTML(paste0("age_cat_mins", div(helpText(""), class = "pull-below"))),
                                 value ="0,50,70,80"),
                       textInput("sexe", label = HTML(paste0("sexe", div(helpText(""), class = "pull-below"))),
                                 value ="sex"),
                       textInput("autres_vars", label = HTML(paste0("autres_vars", div(helpText(""), class = "pull-below"))),
                                 value ="NULL"),
                       textInput("indicateur", label = HTML(paste0("indicateur", div(helpText(""), class = "pull-below"))),
                                 value ="deces"),
                       selectInput("ref_externe", label = HTML(paste0("ref_externe", div(helpText(""), class = "pull-below"))),
                                   choices = c(FALSE,TRUE)),
                       textInput("ref_externe_annee", label = HTML(paste0("ref_externe_annee", div(helpText(""), class = "pull-below"))),
                                 value =NULL),
                       textInput("ref_externe_age_cat_mins", label = HTML(paste0("ref_externe_age_cat_mins", div(helpText(""), class = "pull-below"))),
                                 value =NULL),
                       selectInput("methode", label = HTML(paste0("methode", div(helpText(""), class = "pull-below"))),
                                   choices = c("direct","indirect")),
                       numericInput("multiplicateur", label = HTML(paste0("multiplicateur", div(helpText(""), class = "pull-below"))),
                                    value =100),
                       selectInput("fileType_tab3", "Download: Type fichier:", choices = c("csv", "txt")),
                       actionButton("Executer_tab3", "Exécuter la requête",style = "background-color: green;color: white;"),
                       actionButton("reset_tab3", "Réinitialiser les champs"),
                       style = "background-color: #e1f1f7;")),
                     mainPanel(
                       tabsetPanel(type="tabs",
                                   tabPanel("Résultats", style='width: 1000px; height: 1000px',
                                            h4("Résultats de la requête"),
                                            h5("Message de progression et warnings:"),
                                            verbatimTextOutput("result_tab3"),
                                            uiOutput("DT_final_ui_tab3"),
                                            imageOutput("image")),
                                   tabPanel("Vignettes", style='width: 1000px; height: 1000px',
                                            h4("vignette de la fonction"))#,
                                   #includeHTML("vignettes/"))

                       )
                     )
                   )
          ),
          # combine périodes / épisodes de soins ####
          tabPanel("combine périodes / épisodes de soins",style='width: 1500px; height: 1000px',
                   sidebarLayout(
                     sidebarPanel(
                       wellPanel(
                       textInput("fct_tab4", label = h3("Fonction générique exécutée"),value = "combine_periodes"),
                       selectInput("dt", label = HTML(paste0("Table de données", div(helpText("Cohorte d'intérêt"), class = "pull-below"))),
                                   choices = "Data"),
                       fileInput("file_tab4", label = HTML(paste0("Choisir votre cohorte d'intérêt", div(helpText("Importer la cohorte d'intérêt"), class = "pull-below")))),
                       textInput("id", label = HTML(paste0("Identifiant du bénéficiaire", div(helpText("Indiquer le nom de la colonne de l'identifiant du bénéficiaire"), class = "pull-below"))),
                                 value = "ID"),
                       textInput("debut", label = HTML(paste0("Début de la période", div(helpText("Indiquer le nom de la colonne de la date de début de la période"), class = "pull-below"))),
                                 value = "DatAdm"),
                       textInput("fin", label = HTML(paste0("Fin de la période", div(helpText("Indiquer le nom de la colonne de la date de fin de la période"), class = "pull-below"))),
                                 value = "DatDep"),
                       numericInput("njours", label = HTML(paste0("Nombre de jours", div(helpText("Indiquer le nombre de jours max entre le début et la fin précédente pour effectuer une combinaison"), class = "pull-below"))),
                                    value = 1),
                       textInput("par_cols", label = HTML(paste0("Autres cols à considérer", div(helpText("Indiquer le nom des autres colonnes qui doivent être incluses dans l'analyse. Par défaut `NULL`.
                                                                                                        Exemple: DIN, DENOM"), class = "pull-below"))),
                                 value = "NULL"),
                       checkboxInput("Code_sql_tab4", label = HTML(paste0("Paramétres d'affichage du script", div(helpText("Selectionner pour changer les paramétres d'affichage de script"), class = "pull-below"))),
                                     value=FALSE),
                       uiOutput("Code_sql_select_tab4"),

                       selectInput("fileType_tab4", "Download: Type fichier:", choices = c("csv", "txt")),
                       actionButton("Executer_tab4", "Exécuter la requête",style = "background-color: green;color: white;"),
                       actionButton("reset_tab4", "Réinitialiser les champs"),
                       style = "background-color: #e1f1f7;")),
                     mainPanel(
                       tabsetPanel(type="tabs",
                                   tabPanel("Résultats", style='width: 1000px; height: 1000px',
                                            h4("Résultats de la requête"),
                                            h5("Script:"),
                                            p("Afficher le script exécuté pour effectuer la requête demandée.", style = "font-family: 'times'; font-si16pt"),
                                            shinyAce ::aceEditor(outputId = "ace_tab4",
                                                                 selectionId = "selection_tab4",
                                                                 placeholder = "Afficher le script de la requête ...",
                                                                 mode="r",
                                                                 theme="cobalt"),
                                            h5("Message de progression et warnings:"),
                                            verbatimTextOutput("result_tab4"),
                                            uiOutput("DT_final_ui_tab4")),
                                   tabPanel("Vignettes", style='width: 1000px; height: 1000px',
                                            h4("vignette de la fonction"),
                                            includeHTML(system.file("vignettes/combiner_periodes.html", package = "RequeteGeneriqueBDCA"))
                                   )
                       )
                     )
                   )
          ),
          # Indice de comorbidités ####
          tabPanel("Indice de comorbidités",style='width: 1500px; height: 1000px',
                   sidebarLayout(
                     sidebarPanel(
                       wellPanel(
                         textInput("fct_tab5", label = h3("Fonction générique exécutée"),value = "comorbidity_index"),

                         textInput("sql_user_tab5", label = HTML(paste0("Identifiant", div(helpText("Indiquer votre identifiant teradata (ms0xx)"), class = "pull-below"))), value = "ms069a"),
                         passwordInput("sql_pwd_tab5", label = HTML(paste0("Mot de passe", div(helpText("Indiquer votre mot de passe teradata"), class="pull-below"))), value = "Kong2051"),


                         selectInput("dt_tab5", label = HTML(paste0("Table de données", div(helpText("Indiquer votre cohorte. Une DT ayant au moins deux colonnes : ID et DATE_INDEX"), class = "pull-below"))),
                                     choices ="Data"),
                         fileInput("file_tab5", label = HTML(paste0("Choisir votre cohorte d'intérêt", div(helpText("Importer la cohorte d'intérêt"), class = "pull-below")))),
                         textInput("id_tab5", label = HTML(paste0("Identifiant du bénéficiaire", div(helpText("Indiquer le nom de la colonne contenant l'identifiant du bénéficiaire"), class = "pull-below"))),
                                   value="ID"),
                         textInput("DATE_INDEX_tab5", label = HTML(paste0("Date index", div(helpText("Indiquer le nom de la colonne contenant la date index de chaque bénéficiaire"), class = "pull-below"))),
                                   value="DATE_INDEX"),


                         selectInput("Dx_table_tab5", label = HTML(paste0("Liste des diagnostics", div(helpText("Indiquer la base de données contenant la liste des codes de diagnostics identifiés pour le calcul de l'indice de comorbidité",tags$br(),
                                                                                                                "Par exemple : Charlson_Dx_CCI_INSPQ18"), class = "pull-below"))),
                                     choices = c("Charlson_Dx_CCI_INSPQ18", "Charlson_Dx_UManitoba16", "Elixhauser_Dx_CCI_INSPQ18", "Combine_Dx_CCI_INSPQ18")),
                         textInput("CIM_tab5", label = HTML(paste0("Classification des diagnostics", div(helpText("Indiquer la classification des diagnostics à utiliser (CIM9, CIM10 ou les deux)"), class = "pull-below"))),
                                   value = paste(c("CIM9","CIM10"),collapse = ",")),
                         h4("PARAMÈTRES DE L’ALGORITHME"),
                         helpText("Indiquer les paramètres de l’algorithme soit : le nombre de jours (n1) entre 2 diagnostics, la période de temps (n2) à considérer pour confirmer le premier diagnostic et le nombre minimum d’occurrences du diagnostic pour confirmer la condition (nDx).",
                                  "(Valeurs par défaut n1= 30 jours, n2= 730 jours et nDx=1)"),
                         numericInput("n1_tab5", label = HTML(paste0("n1 (nombre de jours)", div(helpText("Indiquer le nombre de jours entre lesquels deux diagnostics doit se situer pour que le deuxième Dx confirme le premier"), class = "pull-below"))),
                                      value = 30),
                         numericInput("n2_tab5", label = HTML(paste0("n2 (période de temps) ", div(helpText("Indiquer le nombre de jours entre lesquels deux diagnostics doit se situer pour que le deuxième Dx confirme le premier"), class = "pull-below"))),
                                      value = 730),

                         selectInput("scores", label = HTML(paste0("Poids à donner pour chaque comorbidité", div(helpText("Indiquer le nom de la table contenant le poids de chaque comorbidité à utiliser pour le calcul des indices",tags$br(),
                                                                                                                          "Par exemple : CCI_INSPQ_2018_CIM10"), class = "pull-below"))),
                                     choices = c("CCI_INSPQ_2018_CIM9", "CCI_INSPQ_2018_CIM10", "UManitoba_2016")),
                         numericInput("lookup", label = HTML(paste0("Période de recule", div(helpText("Indiquer le nombre d'années à concidérer avant la date indexe de chaque bénéficiaire"), class = "pull-below"))),
                                      value = 2),
                         selectInput("date_dx_var_tab5", label = HTML(paste0("Date d'admission ou de départ", div(helpText("Indiquer si on doit utiliser la date d’admission ou de départ comme date d’incidence lors des hospitalisations ou des visites à l’urgence"), class = "pull-below"))),
                                     choices = c("admis","depar"),selected="admis"),
                         selectInput("obstetric_exclu", label = HTML(paste0("Exclusion des diagnostics obstetric", div(helpText("Indiquer Si on doit exclure (TRUE) les diabètes et les hypertensions de type gestationnel"), class = "pull-below"))),
                                     choices = c(FALSE,TRUE)),
                         textInput("exclu_diagn_tab5", label = HTML(paste0("Exclusion des diagnostics", div(helpText("Indiquer Si on doit exclure un ou plusieurs Dx de la liste des diagnostics",tags$br(),
                                                                                                                     "Par exemple: exclu_diagn = c('ld'), pour 'Liver disease'"), class = "pull-below"))),
                                   value = "NULL"),
                         checkboxInput("verbose_tab5", label = HTML(paste0("verbose", div(helpText("Par défault verbose=TRUE, un message de progression est affiché sous l'onglet résultats: Message de progression et warnings:"), class = "pull-below"))),
                                       value=TRUE),

                         selectInput("keep_confirm_data", label = HTML(paste0("Exclusion des diagnostics obstetric", div(helpText("Indiquer Si on doit exclure (TRUE) les diabètes et les hypertensions de type gestationnel"), class = "pull-below"))),
                                     choices = c(FALSE,TRUE)),

                         selectInput("fileType_tab5", label=HTML(paste0("Format du fichier",div(helpText("Indiquer le format du fichier à télécharger"),class = "pull-below"))), choices = c("csv", "txt")),
                         actionButton("Executer_tab5", "Exécuter la requête",style = "background-color: green;color: white;"),
                         actionButton("reset_tab5", "Réinitialiser les champs"),
                         style = "background-color: #e1f1f7;")),

                     mainPanel(
                       tabsetPanel(type="tabs",
                                   tabPanel("Résultats", style='width: 1000px; height: 1000px',
                                            h4("Résultats de la requête"),
                                            h5("Script:"),
                                            p("Afficher le script exécuté pour effectuer la requête demandée.", style = "font-family: 'times'; font-si16pt"),
                                            shinyAce ::aceEditor(outputId = "ace_tab5",
                                                                 selectionId = "selection_tab5",
                                                                 placeholder = "Afficher le script de la requête ...",
                                                                 mode="r",
                                                                 theme="cobalt"),
                                            h5("Message de progression et warnings:"),
                                            verbatimTextOutput("result_tab5"),
                                            uiOutput("DT_final_ui_tab5")),
                                   tabPanel("Vignettes", style='width: 1000px; height: 1000px',
                                            h4("vignette de la fonction")#,
                                            #includeHTML(system.file("vignettes/combiner_periodes.html", package = "RequeteGeneriqueBDCA"))
                                   )
                       )
                     )
                   )
          )
        )
    )
  )




  # server- Define the server logic for the Shiny app ####
  server<-shinyServer(function(input, output, session) {
    #bs_themer()

    # Extraction Donnees ####
    ## renderUI: ####
    ### Fonction exécutée####
    observeEvent(input$DT, {
      if (input$DT == "SMOD") {
        updateTextInput(session, "fct", value = "user_I_SMOD_SERV_MD_CM_AG")
      } else if (input$DT == "BDCU") {
        updateTextInput(session, "fct", value = "user_V_EPISO_SOIN_DURG_CM_AG")
      } else if (input$DT == "MEDECHO") {
        updateTextInput(session, "fct", value = "user_MEDECHO_DIAGN_SEJ_HOSP_CM")
      }
      else {
        updateTextInput(session, "fct", value = "")
      }
    })
    ### Param spécifique aux vued de données et Task ####
    output$DTInput <- renderUI({
      if (input$DT == "SMOD"){
        tagList(
          textInput(inputId = "CodeActe", label = HTML(paste0("Codes d'acte SMOD",
                                                              div(helpText("Indiquer les codes d'acte SMOD d'intérêt.",tags$br(),
                                                                           "Exemple: 07122,07237,07800,07089"),
                                                                  class = "pull-below"))),value = "07122,07237,07800,07089,07800"),
          selectInput(inputId = "omni_spec", label = HTML(paste0("Spécialité du médecin", div(helpText("Indiquer la spécialité du médecin (défaut=all)"), class = "pull-below"))),
                      choices = c("all","omni","spec")),
          selectInput(inputId = "catg_etab", label = HTML(paste0("Catégorie du lieu", div(helpText("Indiquer la catégorie de lieu ou l’acte a été réalisé (défaut=all)"), class = "pull-below"))),
                      choices = c("all","ambulatoire")),
          textInput(inputId = "code_stat_decis", label = HTML(paste0("Statut de paiement de l'acte", div(helpText("Indiquer le statut de paiement de l'acte (défaut=payé 'PAY', prépayé 'PPY')"), class = "pull-below"))),
                    value = paste(c("PAY","PPY"),collapse = ","))
        )
      }
      else if (input$DT == "MEDECHO") {
        if (input$task == "extraction_MEDECHO_DIAGN_SEJ"){
          tagList(
            selectInput("date_dx_var", label = HTML(paste0("Date d'admission ou de départ", div(helpText("Indiquer si on doit utiliser la date d’admission ou de départ comme date d’incidence lors des visites à l’urgence"), class = "pull-below"))),
                        choices = c("admis","depar"),selected="admis"),
            textInput("diagn", label = HTML(paste0("Liste des diagnostics", div(helpText("Indiquer la liste des diagnostics de la condition médicale à utiliser pour la création de la cohorte.",tags$br(),
                                                                                         "Par exemple : list(Coronaro = list(CIM9 = paste0(c(491, 492, 496), '%'), CIM10 = paste0('J', 41:44, '%')))"), class = "pull-below")))),
            textInput("typ_diagn", label = HTML(paste0("Type de diagnostic", div(helpText("Indiquer le type de diagnostic du séjour hospitalier (défaut=A,P,S,D)"), class = "pull-below"))),
                      value = paste(c("A","P","S","D"),collapse = ","))
          )
        }
        else if (input$task == "extraction_MEDECHO_SEJ_SERV") {
          tagList(
            selectInput("date_dx_var", label = HTML(paste0("Date d'admission ou de départ", div(helpText("Indiquer si on doit utiliser la date d’admission ou de départ comme date d’incidence lors des visites à l’urgence"), class = "pull-below"))),
                        choices = c("admis","depar"),selected="admis"),
            textInput("diagn", label = HTML(paste0("Liste des diagnostics", div(helpText("Indiquer la liste des diagnostics de la condition médicale à utiliser pour la création de la cohorte.",tags$br(),
                                                                                         "Par exemple : list(Coronaro = list(CIM9 = paste0(c(491, 492, 496), '%'), CIM10 = paste0('J', 41:44, '%')))"), class = "pull-below"))))
          )
        }
        else if (input$task == "extraction_MEDECHO_SEJ_HOSP"){
          tagList(
            selectInput("date_dx_var", label = HTML(paste0("Date d'admission ou de départ", div(helpText("Indiquer si on doit utiliser la date d’admission ou de départ comme date d’incidence lors des visites à l’urgence"), class = "pull-below"))),
                        choices = c("admis","depar"),selected="admis")
          )
        }
      }
      else if (input$DT=="BDCU"){
        if(input$task == "extraction_BDCU_EPISO_SOIN_DURG"){
          tagList(
            selectInput("date_dx_var", label = HTML(paste0("Date d'admission ou de départ", div(helpText("Indiquer si on doit utiliser la date d’admission ou de départ comme date d’incidence lors des visites à l’urgence"), class = "pull-below"))),
                        choices = c("admis","depar"),selected="admis"),
            textInput("diagn", label = HTML(paste0("Liste des diagnostics", div(helpText("Indiquer la liste des diagnostics de la condition médicale à utiliser pour la création de la cohorte.",tags$br(),
                                                                                         "Par exemple : list(Coronaro = list(CIM9 = paste0(c(491, 492, 496), '%'), CIM10 = paste0('J', 41:44, '%')))"), class = "pull-below"))))
          )
        }
      }
      else {
        tagList()
      }
    })

    observeEvent(input$DT, {
      if (input$DT == "SMOD") {
        updateSelectInput(session, "task", choices = c("extraction_SMOD", "projet_facturation_acte"))
      } else if (input$DT == "BDCU") {
        updateSelectInput(session, "task", choices = c("extraction_BDCU_EPISO_SOIN_DURG","extraction_BDCU_OCCU_CIVIE_DURG","extraction_BDCU_CNSUL_DURG"))
      } else if (input$DT == "MEDECHO") {
        updateSelectInput(session, "task", choices = c("extraction_MEDECHO_SEJ_HOSP","extraction_MEDECHO_DIAGN_SEJ","extraction_MEDECHO_SEJ_SERV"))
      }
    })
    ### Param creation de cohorte ####
    Dx_table<- reactiveVal("NULL")
    observeEvent(input$Dx_table, {
      Dx_table(input$Dx_table)
    })

    debut_cohort<- reactiveVal("")
    observeEvent(input$debut_cohort, {
      debut_cohort(input$debut_cohort)
    })

    fin_cohort<- reactiveVal("")
    observeEvent(input$fin_cohort, {
      fin_cohort(input$fin_cohort)
    })

    CIM<- reactiveVal(c("CIM9","CIM10"))
    observeEvent(input$CIM, {
      CIM(input$CIM)
    })

    by_Dx<- reactiveVal(FALSE)
    observeEvent(input$by_Dx, {
      by_Dx(input$by_Dx)
    })

    date_dx_var<- reactiveVal("admis")
    observeEvent(input$date_dx_var, {
      date_dx_var(input$date_dx_var)
    })

    n1<- reactiveVal(30)
    observeEvent(input$n1, {
      n1(input$n1)
    })

    n2<- reactiveVal(730)
    observeEvent(input$n2, {
      n2(input$n2)
    })

    nDx<- reactiveVal(0)
    observeEvent(input$nDx, {
      nDx(input$nDx)
    })

    output$creation_cohort_select <- renderUI({
      if (input$creation_cohort) {
        tagList(
          textInput("Dx_table", label = HTML(paste0("Liste des diagnostics", div(helpText("Indiquer la liste des diagnostics de la condition médicale à utiliser pour la création de la cohorte.",tags$br(),
                                                                                          "Par exemple : list(Coronaro = list(CIM9 = paste0(c(491, 492, 496), '%'), CIM10 = paste0('J', 41:44, '%')))"), class = "pull-below"))),
                    value = "NULL"),
          textInput("CIM", label = HTML(paste0("Classification des diagnostics", div(helpText("Indiquer la classification des diagnostics à utiliser (CIM9, CIM10 ou les deux)"), class = "pull-below"))),
                    value = paste(c("CIM9","CIM10"),collapse = ",")),

          textInput("debut_cohort", label = HTML(paste0("Date début de la cohorte", div(helpText("Indiquer le début de la période pour la cohorte (AAAA-MM-JJ)"), class = "pull-below"))),
                    value="2020-01-01"),
          textInput("fin_cohort", label = HTML(paste0("Date fin de la cohorte", div(helpText("Indiquer la finde la période pour la cohorte (AAAA-MM-JJ)"), class = "pull-below"))),
                    value="2020-12-31"),
          checkboxInput("by_Dx_tab2", label = HTML(paste0("Stratification de la cohorte", div(helpText("Indiquer si la cohorte doit être stratifiée ou non selon les conditions médicales définies dans la liste des diagnostics"), class = "pull-below"))),
                        value=FALSE),
          selectInput("date_dx_var", label = HTML(paste0("Date d'admission ou de départ", div(helpText("Indiquer si on doit utiliser la date d’admission ou de départ comme date d’incidence lors des hospitalisations ou des visites à l’urgence"), class = "pull-below"))),
                      choices = c("admis","depar"),selected="admis"),
          h4("PARAMÈTRES DE L’ALGORITHME"),
          helpText("Indiquer les paramètres de l’algorithme soit : le nombre de jours (n1) entre 2 diagnostics, la période de temps (n2) à considérer pour confirmer le premier diagnostic et le nombre minimum d’occurrences du diagnostic pour confirmer la condition (nDx).",
                   "(Valeurs par défaut n1= 30 jours, n2= 730 jours et nDx=1)"),
          numericInput("n1", label = HTML(paste0("n1 (nombre de jours)", div(helpText("Indiquer le nombre de jours entre lesquels deux diagnostics doit se situer pour que le deuxième Dx confirme le premier"), class = "pull-below"))),
                       value = 30),
          numericInput("n2", label = HTML(paste0("n2 (période de temps) ", div(helpText("Indiquer le nombre de jours entre lesquels deux diagnostics doit se situer pour que le deuxième Dx confirme le premier"), class = "pull-below"))),
                       value = 730),
          numericInput("nDx", label = HTML(paste0("nDx (nombre de diagnostics)", div(helpText("Choisir le nombre de diagnostic qui permet de confirmer le premier Dx"), class = "pull-below"))),
                       value = 0),

        )
      }
    })
    ### Import DT cohort ####
    output$fileInput <- renderUI({
      if (input$cohort == "cohort")
        return(fileInput("file", "Veuillez choisir votre cohorte d'intérêt"))})
    ### Affichage code sql ####
    mode<- reactiveVal("sql")
    observeEvent(input$mode, {
      mode(input$mode)
    })

    theme<- reactiveVal("sqlserver")
    observeEvent(input$theme, {
      theme(input$theme)
    })

    output$Code_sql_select <- renderUI({
      if (input$Code_sql) {
        tagList(selectInput("mode", label = HTML(paste0("Mode d'affichage du code générique: ", div(helpText("Par défaut, le code est affiché en mode sql sous l'onglet 'Code sql:'. Il est possible de choisir d'autres modes"), class = "pull-below"))),
                            choices = getAceModes(), selected = mode()),
                selectInput("theme", label = HTML(paste0("Thème d'affichage du code générique: ", div(helpText("Par défaut, le code est affiché avec le thème 'sqlserver'. Il est possible de choisir d'autres thèmes"), class = "pull-below"))),
                            choices = getAceThemes(), selected = theme())
        )
      }
    })
    ### Parm setwd ####
    textInput("setwd", label = "setwd", value = "NULL")
    setwd<- reactiveVal("NULL")
    observeEvent(input$setwd, {
      setwd(input$setwd)
    })


    # Résultats: Extraction Données ####
    observeEvent(input$Executer, {

      # Test des arguments ####
      if(input$keep_all==TRUE){
        if(input$date_age=="NULL"){
          showNotification("L'exécution de la requête est intérompue: L'argument 'date_age' est vide. Veuillez indiquer la date à laquelle l'âge des bénéficiaires n'ayant pas eu d'acte doit être calculé", duration = 0, type="warning")
          #stop("L'exécution de la requête a été intérompue: L'argument 'date_age' est vide. Veuillez indiquer la date à laquelle l'âge des bénéficiaires n'ayant pas eu d'acte doit être calculé")
        }
      }
      if(input$cohort=="cohort"){
        if(is.null(input$file)){
          showNotification("L'exécution de la requête est intérompue: L'argument 'file' est vide. Veuillez selectionner un fichier pour votre cohorte d'intérêt", duration = 0, type="warning")
          #stop("L'exécution de la requête a été intérompue: L'argument 'file' est vide. Veuillez selectionner un fichier pour votre cohorte d'intérêt")
        }
      }
      if(input$benef_adr=="date_fixe"){
        if(input$date_adr=="NULL"){
          showNotification(paste("L'exécution de la requête est intérompue: L'argument 'date_adr' est vide. Veuillez indiquer la date à laquelle l'adresse des bénéficiaires doit être ciblée"), duration = 0, type="warning")
          #stop("L'exécution de la requête a été intérompue: L'argument 'date_adr' est vide. Veuillez indiquer la date à laquelle l'adresse des bénéficiaires doit être ciblée")
        }
      }
      #####
      if(input$DT=="SMOD"){
        if (input$task == "extraction_SMOD") {
          warnings <- reactiveValues(data = character(0))
          result <- eventReactive(input$Executer, {
            conn <- RequeteGeneriqueBDCA::SQL_connexion(noquote(input$sql_user),noquote(input$sql_pwd))
            cohort <- if (input$cohort == "NULL") {
              NULL
            } else {
              tmp <- data.table::fread(input$file$datapath)
              tmp<-tmp[[1]]
              cohort<-tmp
              assign("cohort", cohort, envir = .GlobalEnv)
            }
            CodeActe <- unlist(str_split(input$CodeActe, ","))
            code_stat_decis <- unlist(str_split(input$code_stat_decis, ","))
            CIM <- unlist(str_split(CIM(), ","))
            Dx_table<-if (Dx_table() == "NULL") {
              NULL
            } else {
              eval(parse(text = Dx_table()))
            }
            setwd <- if (setwd() == "NULL") NULL else setwd()

            DT_final<-withCallingHandlers(
              RequeteGeneriqueBDCA::user_I_SMOD_SERV_MD_CM_AG(
                task = input$task,
                conn = conn,
                cohort = cohort,
                debut_periode = input$debut_periode,
                fin_periode = input$fin_periode,
                CodeActe = CodeActe,
                omni_spec = input$omni_spec,
                catg_etab = input$catg_etab,
                code_stat_decis = code_stat_decis,
                benef_adr=input$benef_adr,
                date_adr=input$date_adr,
                date_age = input$date_age,

                Dx_table = Dx_table,
                debut_cohort = debut_cohort(),
                fin_cohort = fin_cohort(),
                CIM = CIM,
                by_Dx = by_Dx(),
                date_dx_var = date_dx_var(),
                n1 = n1(),
                n2 = n2(),
                nDx = nDx(),
                keep_all = input$keep_all,
                verbose = input$verbose,
                setwd=setwd
              ),

              warning = function(w) {
                warnings$data <- c(warnings$data, w$message)})

          })
          # afficher le code sql ####
          init <- eventReactive(input$Executer,{
            if (input$task == "extraction_SMOD") {
              CodeActe <- unlist(str_split(input$CodeActe, ","))
              code_stat_decis <- unlist(str_split(input$code_stat_decis, ","))
              Dx_table<-if (Dx_table() == "NULL") {
                NULL
              } else {
                eval(parse(text = Dx_table()))
              }
              return(query_I_SMOD_SERV_MD_CM_AG(query=input$task,
                                                debut=input$debut_periode,
                                                fin=input$fin_periode,
                                                debut_cohort=input$debut_cohort,
                                                fin_cohort=input$fin_cohort,
                                                diagn=Dx_table,
                                                CodeActe=CodeActe,
                                                omni_spec=input$omni_spec,
                                                catg_etab=input$catg_etab,
                                                code_stat_decis=code_stat_decis,
                                                benef_adr=input$benef_adr,
                                                date_adr=input$date_adr))
            }

            else{
              return("Afficher le code sql de la requête ...")
            }
          })
          observe({ cat(input$ace, "\n")})
          observe({cat(input$ace_selection, "\n")})
          observe({
            updateAceEditor(
              session,
              "ace",
              value = init(),
              theme = theme(),
              mode = mode(),
            )
          })

          # afficher le message de progression ####
          output$result <- renderPrint({
            if ((input$keep_all == TRUE && input$date_age == "NULL") ||
                (input$cohort == "cohort" && is.null(input$file)) ||
                (input$benef_adr == "date_fixe" && input$date_adr == "NULL")) {
              result<-result()
              if (length(warnings$data) > 0) {
                print(paste("Warnings: ", warnings$data))
              }
            }else{
              withProgress(message = "La requête est en exécution. Veuillez-svp patienter....", value = 0, {
                n<-10
                for (i in 1:9) {
                  Sys.sleep(0.5)
                  incProgress(1/n, detail = paste("Doing part", i*10,"%"))
                }
                result<-result()
                if (length(warnings$data) > 0) {
                  print(paste("Warnings: ", warnings$data))
                }
              })
            }
          })

          # Définir les noms tableaux / figs à affichés ####
          output$DT_final_ui <- renderUI({
            navbarPage(
              "Cohorte et Tableaux descriptifs",
              theme = "bootstrap",
              header = tags$style(".navbar-default .navbar-brand {color: black;}"),# font-weight: bold;
              tabPanel("DT_final",
                       downloadButton("downloadData", "Download"),
                       dataTableOutput("DT_final")
              )
            )
          })



          # Afficher la cohorte ####
          output$DT_final <- renderDataTable(result()) #DT::renderDT()
          # Download BD ####
          observeEvent(input$Executer, {
            output$downloadData <- downloadHandler(
              filename = function() {
                paste("DT_final_", format(Sys.Date(), "%Y%m%d"), ".", input$fileType, sep = "")
              },
              content = function(file) {
                if (input$fileType == "csv") {
                  write.csv(result(), file, row.names = FALSE)
                } else {
                  write.table(result(), file, sep = ";", dec = ".",quote=FALSE,row.names = FALSE, col.names = TRUE)
                }
              }
            )
          })

        }
        else if (input$task == "projet_facturation_acte") {
          warnings <- reactiveValues(data = character(0))
          result <- eventReactive(input$Executer, {

            conn <- RequeteGeneriqueBDCA::SQL_connexion(noquote(input$sql_user),noquote(input$sql_pwd))
            cohort <- if (input$cohort == "NULL") {
              NULL
            } else {
              tmp <- data.table::fread(input$file$datapath)
              tmp<-tmp[[1]]
              cohort<-tmp
              assign("cohort", cohort, envir = .GlobalEnv)
            }
            CodeActe <- unlist(str_split(input$CodeActe, ","))
            code_stat_decis <- unlist(str_split(input$code_stat_decis, ","))
            CIM <- unlist(str_split(CIM(), ","))
            Dx_table<-if (Dx_table() == "NULL") {
              NULL
            } else {
              eval(parse(text = Dx_table()))
            }
            setwd <- if (setwd() == "NULL") NULL else setwd()

            DT_final<-withCallingHandlers(
              RequeteGeneriqueBDCA::user_I_SMOD_SERV_MD_CM_AG(
                task = input$task,
                conn = conn,
                cohort = cohort,
                debut_periode = input$debut_periode,
                fin_periode = input$fin_periode,
                CodeActe = CodeActe,
                omni_spec = input$omni_spec,
                catg_etab = input$catg_etab,
                code_stat_decis = code_stat_decis,
                benef_adr=input$benef_adr,
                date_adr=input$date_adr,
                date_age = input$date_age,

                Dx_table = Dx_table,
                debut_cohort = debut_cohort(),
                fin_cohort = fin_cohort(),
                CIM = CIM,
                by_Dx = by_Dx(),
                date_dx_var = date_dx_var(),
                n1 = n1(),
                n2 = n2(),
                nDx = nDx(),
                keep_all = input$keep_all,
                verbose = input$verbose,
                setwd=setwd
              ),

              warning = function(w) {
                warnings$data <- c(warnings$data, w$message)})

          })
          # afficher le code sql ####

          init<- eventReactive(input$Executer,{
            if (input$task =="projet_facturation_acte") {
              CodeActe <- unlist(str_split(input$CodeActe, ","))
              code_stat_decis <- unlist(str_split(input$code_stat_decis, ","))
              Dx_table<-if (Dx_table() == "NULL") {
                NULL
              } else {
                eval(parse(text = Dx_table()))
              }
              return(query_I_SMOD_SERV_MD_CM_AG(query="extraction_SMOD",
                                                debut=input$debut_periode,
                                                fin=input$fin_periode,
                                                debut_cohort=input$debut_cohort,
                                                fin_cohort=input$fin_cohort,
                                                diagn=Dx_table,
                                                CodeActe=CodeActe,
                                                omni_spec=input$omni_spec,
                                                catg_etab=input$catg_etab,
                                                code_stat_decis=code_stat_decis,
                                                benef_adr=input$benef_adr,
                                                date_adr=input$date_adr))
            }
            else{
              return("Afficher le code sql de la requête ...")
            }
          })
          observe({ cat(input$ace, "\n")})
          observe({cat(input$ace_selection, "\n")})
          observe({
            updateAceEditor(
              session,
              "ace",
              value = init(),
              theme = input$theme,
              mode = input$mode,
            )
          })

          # afficher le message de progression ####
          output$result <- renderPrint({
            if ((input$keep_all == TRUE && input$date_age == "NULL") ||
                (input$cohort == "cohort" && is.null(input$file)) ||
                (input$benef_adr == "date_fixe" && input$date_adr == "NULL")) {
              result<-result()
              if (length(warnings$data) > 0) {
                print(paste("Warnings: ", warnings$data))
              }
            }else{
              withProgress(message = "La requête est en exécution. Veuillez-svp patienter....", value = 0, {
                n<-10
                for (i in 1:9) {
                  Sys.sleep(0.5)
                  incProgress(1/n, detail = paste("Doing part", i*10,"%"))
                }
                result<-result()
                if (length(warnings$data) > 0) {
                  print(paste("Warnings: ", warnings$data))
                }
              })
            }
          })

          # Définir les noms tableaux / figs à affichés ####

          output$DT_final_ui1 <- renderUI({
            if (!is.null(result())) {
              do.call(navbarPage, c("Cohorte et Tableaux descriptifs",
                                    list(theme = "bootstrap",
                                         header = tags$style(".navbar-default .navbar-brand {color: black;}")),
                                    lapply(1:length(result()), function(i) {
                                      if (names(result())[i] == "Tab1_Nb_Acte_annee") {
                                        tabPanel(names(result())[i],
                                                 downloadButton(paste0("downloadData_", i), "Download"),
                                                 dataTableOutput(paste0("DT_final_", i)),
                                                 plotly::plotlyOutput(paste0("DT_final_plot_", i), height = 500, width = 1500)
                                        )
                                      } else if (is.data.frame(result()[[i]])) {
                                        tabPanel(names(result())[i],
                                                 downloadButton(paste0("downloadData_", i), "Download"),
                                                 dataTableOutput(paste0("DT_final_", i))
                                        )

                                      } else {
                                        tabPanel(names(result())[i],
                                                 downloadButton(paste0("downloadData_", i), "Download"),
                                                 plotOutput(paste0("DT_final_plot_", i), height = 800, width = 2000)
                                        )

                                      }
                                    })
              ))
            }
          })

          # Afficher les tables et figures ####
          observe({
            lapply(1:length(result()), function(i) {
              if (names(result())[i] == "Tab1_Nb_Acte_annee") {
                output[[paste0("DT_final_", i)]] <- renderDataTable({
                  as.data.frame(result()[[i]])
                })
                output[[paste0("DT_final_plot_", i)]] <- plotly::renderPlotly({
                  plotly::ggplotly(
                    ggplot(data=result()[[i]] %>%
                             data.frame() %>% slice(-nrow(.)) %>%
                             rename(Acte="Var.1") %>%
                             mutate(Acte=as.factor(Acte)) %>%
                             tidyr::pivot_longer(-Acte, names_to = "years", values_to = "Value") %>%
                             mutate(years=as.factor(years)) %>%
                             arrange(years), aes(x = years, y = Value,  group = Acte, color = Acte)) +
                      geom_line()
                  )
                })
              }
              else if (is.data.frame(result()[[i]])) {
                output[[paste0("DT_final_", i)]] <- renderDataTable({
                  as.data.frame(result()[[i]])
                })
              }
              else {
                output[[paste0("DT_final_plot_", i)]] <- renderPlot({
                  plot(result()[[i]])
                })
              }
            })
          })


          # download tables and graphics ####
          observeEvent(input$Executer, {
            lapply(1:length(result()), function(i) {
              output[[paste0("downloadData_", i)]] <- downloadHandler(
                filename = function() {
                  paste(names(result())[i], "_", format(Sys.Date(), "%Y%m%d"), ".", input$fileType, sep = "")
                },
                content = function(file) {
                  if (is.data.frame(result()[[i]])) {
                    switch(input$fileType,
                           "csv" = write.csv(result()[[i]], file = file, row.names = F),
                           "txt" = write.table(result()[[i]], file = file, sep = ";", dec = ".", quote = F, row.names = F, col.names = T),
                           stop("Invalid file type")
                    )
                  } else if (input$fileType=="png"){
                    ggplot2::ggsave(result()[[i]], file = file, width = 30, height = 20, dpi = 300, units = "in")
                  }
                }
              )
            })
          })

        }
      }
      else if(input$DT=="BDCU"){
        if (input$task == "extraction_BDCU_EPISO_SOIN_DURG") {
          warnings <- reactiveValues(data = character(0))
          result <- eventReactive(input$Executer, {
            conn <- RequeteGeneriqueBDCA::SQL_connexion(noquote(input$sql_user),noquote(input$sql_pwd))
            cohort <- if (input$cohort == "NULL") {
              NULL
            } else {
              tmp <- data.table::fread(input$file$datapath)
              tmp<-tmp[[1]]
              cohort<-tmp
              assign("cohort", cohort, envir = .GlobalEnv)
            }
            CIM <- unlist(str_split(CIM(), ","))
            Dx_table<-if (Dx_table() == "NULL") {
              NULL
            } else {
              eval(parse(text = Dx_table()))
            }
            diagn<-if (input$diagn == "NULL") {
              NULL
            } else {
              eval(parse(text = input$diagn))
            }
            setwd <- if (setwd() == "NULL") NULL else setwd()

            DT_final<-withCallingHandlers(
              RequeteGeneriqueBDCA::user_V_EPISO_SOIN_DURG_CM_AG(
                task = input$task,
                conn = conn,
                cohort = cohort,
                debut_periode = input$debut_periode,
                fin_periode = input$fin_periode,
                benef_adr=input$benef_adr,
                date_adr=input$date_adr,
                date_age = input$date_age,
                diagn=diagn,

                Dx_table = Dx_table,
                debut_cohort = debut_cohort(),
                fin_cohort = fin_cohort(),
                CIM = CIM,
                by_Dx = by_Dx(),
                date_dx_var = date_dx_var(),
                n1 = n1(),
                n2 = n2(),
                nDx = nDx(),
                keep_all = input$keep_all,
                verbose = input$verbose,
                setwd=setwd
              ),

              warning = function(w) {
                warnings$data <- c(warnings$data, w$message)})

          })
          # afficher le code sql ####
          init <- eventReactive(input$Executer,{
            if (input$task == "extraction_BDCU_EPISO_SOIN_DURG") {
              diagn<-if (input$diagn == "NULL") {
                NULL
              } else {
                eval(parse(text = input$diagn))
              }
              return(query_V_EPISO_SOIN_DURG_CM_AG (query=input$task,
                                                    debut=input$debut_periode,
                                                    fin=input$fin_periode,
                                                    debut_cohort=input$debut_cohort,
                                                    fin_cohort=input$fin_cohort,
                                                    diagn=diagn,
                                                    date_dx_var=input$date_dx_var,
                                                    benef_adr=input$benef_adr,
                                                    date_adr=input$date_adr))
            }
            else{
              return("Afficher le code sql de la requête ...")
            }
          })
          observe({ cat(input$ace, "\n")})
          observe({cat(input$ace_selection, "\n")})
          observe({
            updateAceEditor(
              session,
              "ace",
              value = init(),
              theme = theme(),
              mode = mode(),
            )
          })

          # afficher le message de progression ####
          output$result <- renderPrint({
            if ((input$keep_all == TRUE && input$date_age == "NULL") ||
                (input$cohort == "cohort" && is.null(input$file)) ||
                (input$benef_adr == "date_fixe" && input$date_adr == "NULL")) {
              result<-result()
              if (length(warnings$data) > 0) {
                print(paste("Warnings: ", warnings$data))
              }
            }else{
              withProgress(message = "La requête est en exécution. Veuillez-svp patienter....", value = 0, {
                n<-10
                for (i in 1:9) {
                  Sys.sleep(0.5)
                  incProgress(1/n, detail = paste("Doing part", i*10,"%"))
                }
                result<-result()
                if (length(warnings$data) > 0) {
                  print(paste("Warnings: ", warnings$data))
                }
              })
            }
          })
          # Définir les noms tableaux / figs à affichés ####
          output$DT_final_ui <- renderUI({navbarPage("Cohorte et Tableaux descriptifs",
                                                     theme = "bootstrap",
                                                     header = tags$style(".navbar-default .navbar-brand {color: black;}"),# font-weight: bold;
                                                     tabPanel("DT_final",
                                                              downloadButton("downloadData", "Download"),
                                                              dataTableOutput("DT_final")
                                                     ))})
          # Afficher la cohorte ####
          output$DT_final <- renderDataTable(result()) #DT::renderDT()
          # Download BD ####
          observeEvent(input$Executer, {
            output$downloadData <- downloadHandler(
              filename = function() {
                paste("DT_final_", format(Sys.Date(), "%Y%m%d"), ".", input$fileType, sep = "")
              },
              content = function(file) {
                if (input$fileType == "csv") {
                  write.csv(result(), file, row.names = FALSE)
                } else {
                  write.table(result(), file, sep = ";", dec = ".",quote=FALSE,row.names = FALSE, col.names = TRUE)
                }
              }
            )
          })

        }
        else if (input$task == "extraction_BDCU_OCCU_CIVIE_DURG") {
          warnings <- reactiveValues(data = character(0))
          result <- eventReactive(input$Executer, {
            conn <- RequeteGeneriqueBDCA::SQL_connexion(noquote(input$sql_user),noquote(input$sql_pwd))
            cohort <- if (input$cohort == "NULL") {
              NULL
            } else {
              tmp <- data.table::fread(input$file$datapath)
              tmp<-tmp[[1]]
              cohort<-tmp
              assign("cohort", cohort, envir = .GlobalEnv)
            }
            CIM <- unlist(str_split(CIM(), ","))
            Dx_table<-if (Dx_table() == "NULL") {
              NULL
            } else {
              eval(parse(text = Dx_table()))
            }


            DT_final<-withCallingHandlers(
              RequeteGeneriqueBDCA::user_V_EPISO_SOIN_DURG_CM_AG(
                task = input$task,
                conn = conn,
                cohort = cohort,
                debut_periode = input$debut_periode,
                fin_periode = input$fin_periode,
                benef_adr=input$benef_adr,
                date_adr=input$date_adr,
                date_age = input$date_age,
                diagn=NULL,

                Dx_table = Dx_table,
                debut_cohort = debut_cohort(),
                fin_cohort = fin_cohort(),
                CIM = CIM,
                by_Dx = by_Dx(),
                date_dx_var = date_dx_var(),
                n1 = n1(),
                n2 = n2(),
                nDx = nDx(),
                keep_all = input$keep_all,
                verbose = input$verbose,
                setwd=setwd
              ),

              warning = function(w) {
                warnings$data <- c(warnings$data, w$message)})

          })
          # afficher le code sql ####
          init <- eventReactive(input$Executer,{
            if (input$task == "extraction_BDCU_OCCU_CIVIE_DURG") {

              return(query_V_EPISO_SOIN_DURG_CM_AG (query=input$task,
                                                    debut=input$debut_periode,
                                                    fin=input$fin_periode,
                                                    debut_cohort=input$debut_cohort,
                                                    fin_cohort=input$fin_cohort,
                                                    diagn=NULL,
                                                    date_dx_var=input$date_dx_var,
                                                    benef_adr=input$benef_adr,
                                                    date_adr=input$date_adr))
            }
            else{
              return("Afficher le code sql de la requête ...")
            }
          })
          observe({ cat(input$ace, "\n")})
          observe({cat(input$ace_selection, "\n")})
          observe({
            updateAceEditor(
              session,
              "ace",
              value = init(),
              theme = theme(),
              mode = mode(),
            )
          })

          # afficher le message de progression ####
          output$result <- renderPrint({
            if ((input$keep_all == TRUE && input$date_age == "NULL") ||
                (input$cohort == "cohort" && is.null(input$file)) ||
                (input$benef_adr == "date_fixe" && input$date_adr == "NULL")) {
              result<-result()
              if (length(warnings$data) > 0) {
                print(paste("Warnings: ", warnings$data))
              }
            }else{
              withProgress(message = "La requête est en exécution. Veuillez-svp patienter....", value = 0, {
                n<-10
                for (i in 1:9) {
                  Sys.sleep(0.5)
                  incProgress(1/n, detail = paste("Doing part", i*10,"%"))
                }
                result<-result()
                if (length(warnings$data) > 0) {
                  print(paste("Warnings: ", warnings$data))
                }
              })
            }
          })
          # Définir les noms tableaux / figs à affichés ####
          output$DT_final_ui <- renderUI({navbarPage("Cohorte et Tableaux descriptifs",
                                                     theme = "bootstrap",
                                                     header = tags$style(".navbar-default .navbar-brand {color: black;}"),# font-weight: bold;
                                                     tabPanel("DT_final",
                                                              downloadButton("downloadData", "Download"),
                                                              dataTableOutput("DT_final")
                                                     ))})
          # Afficher la cohorte ####
          output$DT_final <- renderDataTable(result()) #DT::renderDT()
          # Download BD ####
          observeEvent(input$Executer, {
            output$downloadData <- downloadHandler(
              filename = function() {
                paste("DT_final_", format(Sys.Date(), "%Y%m%d"), ".", input$fileType, sep = "")
              },
              content = function(file) {
                if (input$fileType == "csv") {
                  write.csv(result(), file, row.names = FALSE)
                } else {
                  write.table(result(), file, sep = ";", dec = ".",quote=FALSE,row.names = FALSE, col.names = TRUE)
                }
              }
            )
          })

        }
        else if (input$task == "extraction_BDCU_CNSUL_DURG") {
          warnings <- reactiveValues(data = character(0))
          result <- eventReactive(input$Executer, {
            conn <- RequeteGeneriqueBDCA::SQL_connexion(noquote(input$sql_user),noquote(input$sql_pwd))
            cohort <- if (input$cohort == "NULL") {
              NULL
            } else {
              tmp <- data.table::fread(input$file$datapath)
              tmp<-tmp[[1]]
              cohort<-tmp
              assign("cohort", cohort, envir = .GlobalEnv)
            }
            CIM <- unlist(str_split(CIM(), ","))
            Dx_table<-if (Dx_table() == "NULL") {
              NULL
            } else {
              eval(parse(text = Dx_table()))
            }


            DT_final<-withCallingHandlers(
              RequeteGeneriqueBDCA::user_V_EPISO_SOIN_DURG_CM_AG(
                task = input$task,
                conn = conn,
                cohort = cohort,
                debut_periode = input$debut_periode,
                fin_periode = input$fin_periode,
                benef_adr=input$benef_adr,
                date_adr=input$date_adr,
                date_age = input$date_age,
                diagn=NULL,

                Dx_table = Dx_table,
                debut_cohort = debut_cohort(),
                fin_cohort = fin_cohort(),
                CIM = CIM,
                by_Dx = by_Dx(),
                date_dx_var = date_dx_var(),
                n1 = n1(),
                n2 = n2(),
                nDx = nDx(),
                keep_all = input$keep_all,
                verbose = input$verbose,
                setwd=setwd
              ),

              warning = function(w) {
                warnings$data <- c(warnings$data, w$message)})

          })
          # afficher le code sql ####
          init <- eventReactive(input$Executer,{
            if (input$task == "extraction_BDCU_CNSUL_DURG") {

              return(query_V_EPISO_SOIN_DURG_CM_AG (query=input$task,
                                                    debut=input$debut_periode,
                                                    fin=input$fin_periode,
                                                    debut_cohort=input$debut_cohort,
                                                    fin_cohort=input$fin_cohort,
                                                    diagn=NULL,
                                                    date_dx_var=input$date_dx_var,
                                                    benef_adr=input$benef_adr,
                                                    date_adr=input$date_adr))
            }
            else{
              return("Afficher le code sql de la requête ...")
            }
          })
          observe({ cat(input$ace, "\n")})
          observe({cat(input$ace_selection, "\n")})
          observe({
            updateAceEditor(
              session,
              "ace",
              value = init(),
              theme = theme(),
              mode = mode(),
            )
          })

          # afficher le message de progression ####
          output$result <- renderPrint({
            if ((input$keep_all == TRUE && input$date_age == "NULL") ||
                (input$cohort == "cohort" && is.null(input$file)) ||
                (input$benef_adr == "date_fixe" && input$date_adr == "NULL")) {
              result<-result()
              if (length(warnings$data) > 0) {
                print(paste("Warnings: ", warnings$data))
              }
            }else{
              withProgress(message = "La requête est en exécution. Veuillez-svp patienter....", value = 0, {
                n<-10
                for (i in 1:9) {
                  Sys.sleep(0.5)
                  incProgress(1/n, detail = paste("Doing part", i*10,"%"))
                }
                result<-result()
                if (length(warnings$data) > 0) {
                  print(paste("Warnings: ", warnings$data))
                }
              })
            }
          })
          # Définir les noms tableaux / figs à affichés ####
          output$DT_final_ui <- renderUI({navbarPage("Cohorte et Tableaux descriptifs",
                                                     theme = "bootstrap",
                                                     header = tags$style(".navbar-default .navbar-brand {color: black;}"),# font-weight: bold;
                                                     tabPanel("DT_final",
                                                              downloadButton("downloadData", "Download"),
                                                              dataTableOutput("DT_final")
                                                     ))})
          # Afficher la cohorte ####
          output$DT_final <- renderDataTable(result()) #DT::renderDT()
          # Download BD ####
          observeEvent(input$Executer, {
            output$downloadData <- downloadHandler(
              filename = function() {
                paste("DT_final_", format(Sys.Date(), "%Y%m%d"), ".", input$fileType, sep = "")
              },
              content = function(file) {
                if (input$fileType == "csv") {
                  write.csv(result(), file, row.names = FALSE)
                } else {
                  write.table(result(), file, sep = ";", dec = ".",quote=FALSE,row.names = FALSE, col.names = TRUE)
                }
              }
            )
          })

        }
      }
      else if(input$DT=="MEDECHO"){
        if(input$task=="extraction_MEDECHO_SEJ_HOSP"){
          warnings <- reactiveValues(data = character(0))
          result <- eventReactive(input$Executer, {
            conn <- RequeteGeneriqueBDCA::SQL_connexion(noquote(input$sql_user),noquote(input$sql_pwd))
            cohort <- if (input$cohort == "NULL") {
              NULL
            } else {
              tmp <- data.table::fread(input$file$datapath)
              tmp<-tmp[[1]]
              cohort<-tmp
              assign("cohort", cohort, envir = .GlobalEnv)
            }
            CIM <- unlist(str_split(CIM(), ","))
            Dx_table<-if (Dx_table() == "NULL") {
              NULL
            } else {
              eval(parse(text = Dx_table()))
            }
            setwd <- if (setwd() == "NULL") NULL else setwd()
            #typ_diagn <- unlist(str_split(typ_diagn(), ","))

            DT_final<-withCallingHandlers(
              RequeteGeneriqueBDCA::user_MEDECHO_DIAGN_SEJ_HOSP_CM(
                task = input$task,
                conn = conn,
                cohort = cohort,
                debut_periode = input$debut_periode,
                fin_periode = input$fin_periode,
                benef_adr=input$benef_adr,
                date_adr=input$date_adr,
                date_age = input$date_age,
                typ_diagn=NULL,

                Dx_table = Dx_table,
                debut_cohort = debut_cohort(),
                fin_cohort = fin_cohort(),
                CIM = CIM,
                by_Dx = by_Dx(),
                date_dx_var = date_dx_var(),
                n1 = n1(),
                n2 = n2(),
                nDx = nDx(),
                keep_all = input$keep_all,
                verbose = input$verbose,
                setwd=setwd
              ),

              warning = function(w) {
                warnings$data <- c(warnings$data, w$message)})

          })
          # afficher le code sql ####
          init <- eventReactive(input$Executer,{
            if (input$task == "extraction_MEDECHO_SEJ_HOSP") {
              Dx_table<-if (Dx_table() == "NULL") {
                NULL
              } else {
                eval(parse(text = Dx_table()))
              }

              return(query_V_SEJ_HOSP_CM(query=input$task,
                                         debut_periode=input$debut_periode,
                                         fin_periode=input$fin_periode,
                                         date_dx_var=input$date_dx_var,
                                         benef_adr=input$benef_adr,
                                         date_adr=input$date_adr))
            }
            else{
              return("Afficher le code sql de la requête ...")
            }
          })
          observe({ cat(input$ace, "\n")})
          observe({cat(input$ace_selection, "\n")})
          observe({
            updateAceEditor(
              session,
              "ace",
              value = init(),
              theme = theme(),
              mode = mode(),
            )
          })

          # afficher le message de progression ####
          output$result <- renderPrint({
            if ((input$keep_all == TRUE && input$date_age == "NULL") ||
                (input$cohort == "cohort" && is.null(input$file)) ||
                (input$benef_adr == "date_fixe" && input$date_adr == "NULL")) {
              result<-result()
              if (length(warnings$data) > 0) {
                print(paste("Warnings: ", warnings$data))
              }
            }else{
              withProgress(message = "La requête est en exécution. Veuillez-svp patienter....", value = 0, {
                n<-10
                for (i in 1:9) {
                  Sys.sleep(0.5)
                  incProgress(1/n, detail = paste("Doing part", i*10,"%"))
                }
                result<-result()
                if (length(warnings$data) > 0) {
                  print(paste("Warnings: ", warnings$data))
                }
              })
            }
          })
          # Définir les noms tableaux / figs à affichés ####
          output$DT_final_ui <- renderUI({navbarPage("Cohorte et Tableaux descriptifs",
                                                     theme = "bootstrap",
                                                     header = tags$style(".navbar-default .navbar-brand {color: black;}"),# font-weight: bold;
                                                     tabPanel("DT_final",
                                                              downloadButton("downloadData", "Download"),
                                                              dataTableOutput("DT_final")
                                                     ))})
          # Afficher la cohorte ####
          output$DT_final <- renderDataTable(result()) #DT::renderDT()
          # Download BD ####
          observeEvent(input$Executer, {
            output$downloadData <- downloadHandler(
              filename = function() {
                paste("DT_final_", format(Sys.Date(), "%Y%m%d"), ".", input$fileType, sep = "")
              },
              content = function(file) {
                if (input$fileType == "csv") {
                  write.csv(result(), file, row.names = FALSE)
                } else {
                  write.table(result(), file, sep = ";", dec = ".",quote=FALSE,row.names = FALSE, col.names = TRUE)
                }
              }
            )
          })


        }
        else if (input$task=="extraction_MEDECHO_DIAGN_SEJ"){
          warnings <- reactiveValues(data = character(0))
          result <- eventReactive(input$Executer, {
            conn <- RequeteGeneriqueBDCA::SQL_connexion(noquote(input$sql_user),noquote(input$sql_pwd))
            cohort <- if (input$cohort == "NULL") {
              NULL
            } else {
              tmp <- data.table::fread(input$file$datapath)
              tmp<-tmp[[1]]
              cohort<-tmp
              assign("cohort", cohort, envir = .GlobalEnv)
            }
            CIM <- unlist(str_split(CIM(), ","))
            diagn<-if (input$diagn == "NULL") {
              NULL
            } else {
              eval(parse(text = input$diagn))
            }
            Dx_table<-if (Dx_table() == "NULL") {
              NULL
            } else {
              eval(parse(text = Dx_table()))
            }

            setwd <- if (setwd() == "NULL") NULL else setwd()
            typ_diagn <- unlist(str_split(input$typ_diagn, ","))

            DT_final<-withCallingHandlers(
              RequeteGeneriqueBDCA::user_MEDECHO_DIAGN_SEJ_HOSP_CM(
                task = input$task,
                conn = conn,
                cohort = cohort,
                debut_periode = input$debut_periode,
                fin_periode = input$fin_periode,
                benef_adr=input$benef_adr,
                date_adr=input$date_adr,
                date_age = input$date_age,
                diagn=diagn,
                typ_diagn=typ_diagn,

                Dx_table = Dx_table,
                debut_cohort = debut_cohort(),
                fin_cohort = fin_cohort(),
                CIM = CIM,
                by_Dx = by_Dx(),
                date_dx_var = date_dx_var(),
                n1 = n1(),
                n2 = n2(),
                nDx = nDx(),
                keep_all = input$keep_all,
                verbose = input$verbose,
                setwd=setwd
              ),

              warning = function(w) {
                warnings$data <- c(warnings$data, w$message)})

          })
          # afficher le code sql ####
          init <- eventReactive(input$Executer,{
            if (input$task == "extraction_MEDECHO_DIAGN_SEJ") {
              diagn<-if (input$diagn == "NULL") {
                NULL
              } else {
                eval(parse(text = input$diagn))
              }
              typ_diagn <- unlist(str_split(input$typ_diagn, ","))

              return(query_V_DIAGN_SEJ_HOSP_CM_AG(query=input$task,
                                                  debut_periode=input$debut_periode,
                                                  fin_periode=input$fin_periode,
                                                  diagn =diagn,
                                                  date_dx_var=input$date_dx_var,
                                                  typ_diagn=typ_diagn))
            }
            else{
              return("Afficher le code sql de la requête ...")
            }
          })
          observe({ cat(input$ace, "\n")})
          observe({cat(input$ace_selection, "\n")})
          observe({
            updateAceEditor(
              session,
              "ace",
              value = init(),
              theme = theme(),
              mode = mode(),
            )
          })

          # afficher le message de progression ####
          output$result <- renderPrint({
            if ((input$keep_all == TRUE && input$date_age == "NULL") ||
                (input$cohort == "cohort" && is.null(input$file)) ||
                (input$benef_adr == "date_fixe" && input$date_adr == "NULL")) {
              result<-result()
              if (length(warnings$data) > 0) {
                print(paste("Warnings: ", warnings$data))
              }
            }else{
              withProgress(message = "La requête est en exécution. Veuillez-svp patienter....", value = 0, {
                n<-10
                for (i in 1:9) {
                  Sys.sleep(0.5)
                  incProgress(1/n, detail = paste("Doing part", i*10,"%"))
                }
                result<-result()
                if (length(warnings$data) > 0) {
                  print(paste("Warnings: ", warnings$data))
                }
              })
            }
          })
          # Définir les noms tableaux / figs à affichés ####
          output$DT_final_ui <- renderUI({navbarPage("Cohorte et Tableaux descriptifs",
                                                     theme = "bootstrap",
                                                     header = tags$style(".navbar-default .navbar-brand {color: black;}"),# font-weight: bold;
                                                     tabPanel("DT_final",
                                                              downloadButton("downloadData", "Download"),
                                                              dataTableOutput("DT_final")
                                                     ))})
          # Afficher la cohorte ####
          output$DT_final <- renderDataTable(result()) #DT::renderDT()
          # Download BD ####
          observeEvent(input$Executer, {
            output$downloadData <- downloadHandler(
              filename = function() {
                paste("DT_final_", format(Sys.Date(), "%Y%m%d"), ".", input$fileType, sep = "")
              },
              content = function(file) {
                if (input$fileType == "csv") {
                  write.csv(result(), file, row.names = FALSE)
                } else {
                  write.table(result(), file, sep = ";", dec = ".",quote=FALSE,row.names = FALSE, col.names = TRUE)
                }
              }
            )
          })



        }
        else if (input$task=="extraction_MEDECHO_SEJ_SERV"){
          warnings <- reactiveValues(data = character(0))
          result <- eventReactive(input$Executer, {

            conn <- RequeteGeneriqueBDCA::SQL_connexion(noquote(input$sql_user),noquote(input$sql_pwd))
            cohort <- if (input$cohort == "NULL") {
              NULL
            } else {
              tmp <- data.table::fread(input$file$datapath)
              tmp<-tmp[[1]]
              cohort<-tmp
              assign("cohort", cohort, envir = .GlobalEnv)
            }
            CIM <- unlist(str_split(CIM(), ","))
            Dx_table<-if (Dx_table() == "NULL") {
              NULL
            } else {
              eval(parse(text = Dx_table()))
            }
            diagn<-if (input$diagn == "NULL") {
              NULL
            } else {
              eval(parse(text = input$diagn))
            }
            setwd <- if (setwd() == "NULL") NULL else setwd()


            DT_final<-withCallingHandlers(
              RequeteGeneriqueBDCA::user_MEDECHO_DIAGN_SEJ_HOSP_CM(
                task = input$task,
                conn = conn,
                cohort = cohort,
                debut_periode = input$debut_periode,
                fin_periode = input$fin_periode,
                benef_adr=input$benef_adr,
                date_adr=input$date_adr,
                date_age = input$date_age,
                diagn=diagn,
                typ_diagn=NULL,

                Dx_table = Dx_table,
                debut_cohort = debut_cohort(),
                fin_cohort = fin_cohort(),
                CIM = CIM,
                by_Dx = by_Dx(),
                date_dx_var = date_dx_var(),
                n1 = n1(),
                n2 = n2(),
                nDx = nDx(),
                keep_all = input$keep_all,
                verbose = input$verbose,
                setwd=setwd
              ),

              warning = function(w) {
                warnings$data <- c(warnings$data, w$message)})

          })
          # afficher le code sql ####
          init <- eventReactive(input$Executer,{
            if (input$task == "extraction_MEDECHO_SEJ_SERV") {
              diagn<-if (input$diagn == "NULL") {
                NULL
              } else {
                eval(parse(text = input$diagn))
              }


              return(query_V_SEJ_SERV_HOSP_CM_AG(query=input$task,
                                                 debut_periode=input$debut_periode,
                                                 fin_periode=input$fin_periode,
                                                 diagn =diagn,
                                                 date_dx_var=input$date_dx_var))
            }
            else{
              return("Afficher le code sql de la requête ...")
            }
          })
          observe({ cat(input$ace, "\n")})
          observe({cat(input$ace_selection, "\n")})
          observe({
            updateAceEditor(
              session,
              "ace",
              value = init(),
              theme = theme(),
              mode = mode(),
            )
          })

          # afficher le message de progression ####
          output$result <- renderPrint({
            if ((input$keep_all == TRUE && input$date_age == "NULL") ||
                (input$cohort == "cohort" && is.null(input$file)) ||
                (input$benef_adr == "date_fixe" && input$date_adr == "NULL")) {
              result<-result()
              if (length(warnings$data) > 0) {
                print(paste("Warnings: ", warnings$data))
              }
            }else{
              withProgress(message = "La requête est en exécution. Veuillez-svp patienter....", value = 0, {
                n<-10
                for (i in 1:9) {
                  Sys.sleep(0.5)
                  incProgress(1/n, detail = paste("Doing part", i*10,"%"))
                }
                result<-result()
                if (length(warnings$data) > 0) {
                  print(paste("Warnings: ", warnings$data))
                }
              })
            }
          })
          # Définir les noms tableaux / figs à affichés ####
          output$DT_final_ui <- renderUI({navbarPage("Cohorte et Tableaux descriptifs",
                                                     theme = "bootstrap",
                                                     header = tags$style(".navbar-default .navbar-brand {color: black;}"),# font-weight: bold;
                                                     tabPanel("DT_final",
                                                              downloadButton("downloadData", "Download"),
                                                              dataTableOutput("DT_final")
                                                     ))})
          # Afficher la cohorte ####
          output$DT_final <- renderDataTable(result()) #DT::renderDT()
          # Download BD ####
          observeEvent(input$Executer, {
            output$downloadData <- downloadHandler(
              filename = function() {
                paste("DT_final_", format(Sys.Date(), "%Y%m%d"), ".", input$fileType, sep = "")
              },
              content = function(file) {
                if (input$fileType == "csv") {
                  write.csv(result(), file, row.names = FALSE)
                } else {
                  write.table(result(), file, sep = ";", dec = ".",quote=FALSE,row.names = FALSE, col.names = TRUE)
                }
              }
            )
          })




        }
      }
    })
    # Résultats: Requete Tearadata ####
    mode_tera <- reactiveVal("sql")
    observeEvent(input$mode_tera, {
      mode_tera(input$mode_tera)
      updateAceEditor(session, "ace_tera", mode = mode_tera())
    })

    theme_tera <- reactiveVal("sqlserver")
    observeEvent(input$theme_tera, {
      theme_tera(input$theme_tera)
      updateAceEditor(session, "ace_tera", theme = theme_tera())
    })

    output$Code_sql_select_tera <- renderUI({
      if (input$Code_sql_tera) {
        tagList(
          selectInput("mode_tera", label = HTML(paste0("Mode d'affichage du code générique: ", div(helpText("Par défaut, le code est affiché en mode sql sous l'onglet 'Code sql:'. Il est possible de choisir d'autres modes"), class = "pull-below"))),
                      choices = getAceModes(), selected = mode_tera()),
          selectInput("theme_tera", label = HTML(paste0("Thème d'affichage du code générique: ", div(helpText("Par défaut, le code est affiché avec le thème 'sqlserver'. Il est possible de choisir d'autres thèmes"), class = "pull-below"))),
                      choices = getAceThemes(), selected = theme_tera())
        )
      }
    })

    observeEvent(input$Executer_tera, {

      observe({cat(input$ace_tera, "\n")})
      observe({cat(input$selection_tera, "\n")})
      updateAceEditor(
        session,
        "ace_tera",
        value = input$ace_tera,
        theme = theme_tera(),
        mode = mode_tera()
      )

      warnings <- reactiveValues(data = character(0))
      result <- eventReactive(input$Executer_tera, {
        conn <- RequeteGeneriqueBDCA::SQL_connexion(noquote(input$sql_user_tera),noquote(input$sql_pwd_tera))
        withCallingHandlers(
          {
            DT_final <- dbGetQuery(conn, input$ace_tera)
            DT_final <- data.frame(DT_final)
          },
          warning = function(w) {
            warnings$data <- c(warnings$data, w$message)
          }
        )
      })

      # afficher le message de progression ####
      output$result_tera <- renderPrint({
        withProgress(message = "La requête est en exécution. Veuillez-svp patienter....", value = 0, {
          n<-10
          for (i in 1:9) {
            Sys.sleep(0.5)
            incProgress(1/n, detail = paste("Doing part", i*10,"%"))
          }
          result<-result()
          if (length(warnings$data) > 0) {
            print(paste("Warnings: ", warnings$data))
          }
        })
      })

      # Définir les noms tableaux / figs à affichés ####
      output$DT_final_ui_tera <- renderUI({navbarPage("Cohorte et Tableaux descriptifs",
                                                      theme = "bootstrap",
                                                      header = tags$style(".navbar-default .navbar-brand {color: black;}"),# font-weight: bold;
                                                      tabPanel("DT_final_tera",
                                                               downloadButton("downloadData_tera", "Download"),
                                                               dataTableOutput("DT_final_tera")
                                                      ))})
      # Afficher la cohorte ####
      output$DT_final_tera <- renderDataTable(result()) #DT::renderDT()
      # Download BD ####
      observeEvent(input$Executer_tera, {
        output$downloadData_tera <- downloadHandler(
          filename = function() {
            paste("DT_final_", format(Sys.Date(), "%Y%m%d"), ".", input$fileType_tera, sep = "")
          },
          content = function(file) {
            if (input$fileType_tera == "csv") {
              write.csv(result(), file, row.names = FALSE)
            } else {
              write.table(result(), file, sep = ";", dec = ".",quote=FALSE,row.names = FALSE, col.names = TRUE)
            }
          }
        )
      })

    })
    #####
    '#####
     #####'
    # Creation cohorte ####
    # renderUI: ####
    textInput("code_stat_decis_tab2", label = HTML(paste0("code_stat_decis", div(helpText("Indiquer si vous voulez des actes payés et/ou prépayés"), class = "pull-below"))),
              value = paste(c("PAY","PPY"),collapse = ","))
    # Résultat: Création cohort ####
    observeEvent(input$Executer_tab2, {
      warnings <- reactiveValues(data = character(0))
      result_tab2 <- eventReactive(input$Executer_tab2, {
        conn <- RequeteGeneriqueBDCA::SQL_connexion(noquote(input$sql_user_tab2),noquote(input$sql_pwd_tab2))
        code_stat_decis <- unlist(str_split(input$code_stat_decis_tab2, ","))
        CIM <- unlist(str_split(input$CIM_tab2, ","))
        Dx_table<-if (input$Dx_table_tab2 == "NULL") {
          NULL
        } else {
          eval(parse(text = input$Dx_table_tab2))
        }

        DT_final<-withCallingHandlers(
          RequeteGeneriqueBDCA::SQL_reperage_cond_med(
            conn = conn,
            Dx_table = Dx_table,
            debut_cohort = input$debut_cohort_tab2,
            fin_cohort = input$fin_cohort_tab2,
            CIM = CIM,
            by_Dx = input$by_Dx_tab2,
            date_dx_var = input$date_dx_var_tab2,
            n1 = input$n1_tab2,
            n2 = input$n2_tab2,
            nDx = input$nDx_tab2,
            code_stat_decis = code_stat_decis
          ),

          warning = function(w) {
            warnings$data <- c(warnings$data, w$message)})

      })

      # afficher le message de progression ####
      output$result_tab2 <- renderPrint({
        withProgress(message = "La requête est en exécution. Veuillez-svp patienter...", value = 0, {
          #incProgress(0.1)
          n<-10
          for (i in 1:9) {
            Sys.sleep(0.5)
            incProgress(1/n, detail = paste("Doing part", i*10,"%"))
          }
          result_tab2<-result_tab2()
          if (length(warnings$data) > 0) {
            print(paste("Warnings: ", warnings$data))
          }
        })
      })

      # Définir les noms tableaux / figs à affichés ####
      output$DT_final_ui_tab2 <- renderUI({navbarPage("Cohorte et Tableaux descriptifs",
                                                      theme = "bootstrap",
                                                      header = tags$style(".navbar-default .navbar-brand {color: black;}"),# font-weight: bold;
                                                      tabPanel("cohort_final",
                                                               downloadButton("downloadData_tab2", "Download"),
                                                               dataTableOutput("cohort_final")
                                                      ))})
      # Afficher la cohorte ####
      output$cohort_final <- renderDataTable(result_tab2()) #DT::renderDT()
      # Download BD ####
      observeEvent(input$Executer_tab2, {
        output$downloadData_tab2 <- downloadHandler(
          filename = function() {
            paste("cohort_final-", Sys.Date(), ".", input$fileType_tab2, sep = "")
          },
          content = function(file) {
            if (input$fileType_tab2 == "csv") {
              write.csv(result_tab2(), file, row.names = FALSE)
            } else {
              write.table(result_tab2(), file, sep = ";", dec = ".",quote=FALSE,row.names = FALSE, col.names = TRUE)
            }
          }
        )
      })

    })
    #####
    '#####
     #####'
    # Standardisation ####
    observeEvent(input$Executer_tab3, {
      output$image <- renderImage({
        list(
          src = system.file("image/Trav_cours.jpg", package = "RequeteGeneriqueBDCA"),
          contentType = "image/jpeg"
        )
      }, deleteFile = FALSE)
    })
    #####
    '#####
     #####'
    # combine périodes / episodes soins ####
    # renderUI: ####
    # Affichage code sql ####
    mode_r<- reactiveVal("r")
    observeEvent(input$mode_tab4, {
      mode_r(input$mode_tab4)
    })

    theme_r<- reactiveVal("cobalt")
    observeEvent(input$theme_tab4, {
      theme_r(input$theme_tab4)
    })

    output$Code_sql_select_tab4 <- renderUI({
      if (input$Code_sql_tab4) {
        tagList(selectInput("mode_tab4", label = HTML(paste0("Mode d'affichage du code générique: ", div(helpText("Par défaut, le code est affiché en mode r sous l'onglet 'script:'. Il est possible de choisir d'autres modes"), class = "pull-below"))),
                            choices = getAceModes(), selected = mode_r()),
                selectInput("theme_tab4", label = HTML(paste0("Thème d'affichage du code générique: ", div(helpText("Par défaut, le code est affiché avec le thème 'cobalt'. Il est possible de choisir d'autres thèmes"), class = "pull-below"))),
                            choices = getAceThemes(), selected = theme_r())
        )
      }
    })
    # Résultats: combine périodes / epi soins ####
    observeEvent(input$Executer_tab4, {
      # Test des arguments ####
      if(is.null(input$file_tab4)){
        showNotification("L'exécution de la requête est intérompue: L'argument 'file' est vide. Veuillez-svp selectionner un fichier pour votre cohorte d'intérêt", duration = 0, type="warning")
      }
      #####
      warnings <- reactiveValues(data = character(0))
      result_tab4 <- eventReactive(input$Executer_tab4, {

        DT <- if (input$dt == "NULL") {
          NULL
        } else {
          tmp <- data.table::fread(input$file_tab4$datapath)
          DT<-tmp
          assign("DT", DT, envir = .GlobalEnv)
        }

        par_cols<-if (input$par_cols == "NULL") {
          NULL
        } else {
          unlist(str_split(input$par_cols, ","))
        }


        DT_final<-withCallingHandlers(
          RequeteGeneriqueBDCA::combiner_periodes(
            dt=DT,
            id=input$id,
            debut=input$debut,
            fin=input$fin,
            njours=input$njours,
            par_cols=par_cols
          ),

          warning = function(w) {
            warnings$data <- c(warnings$data, w$message)})

      })
      # afficher le script R ####
      init <- eventReactive(input$Executer_tab4,{
        if (input$dt == "Data") {
          return('
    # Convertir data.table au besoin
    if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
    }

    # Sélectionner et renommer les colonnes nécessaires
    if (is.null(par_cols)) {
    dt <- dt[, c(id, debut, fin), with = FALSE]
    } else {
    dt <- dt[, c(id, par_cols, debut, fin), with = FALSE]
    }
    setnames(dt, c(id, debut, fin), c("id", "debut", "fin"))

    # Trier en ordre croissant les périodes
    if (is.null(par_cols)) {
    setorder(dt, id, debut, fin)
    } else {
    setorderv(dt, c("id", par_cols, "debut", "fin"))
    }

    # Combiner les périodes qui se chevauchent
    if (is.null(par_cols)) {
    dt[, diff := as.integer(debut - shift(fin)), .(id)]  # nombre de jours entre début{i} et fin{i-1}
    idx <- rmNA(dt[, .I[diff <= njours]])  # vérifier si une combinaison des périodes doit être faite
    while (length(idx)) {
    dt[is.na(diff), diff := 0L]
    dt[, per := 0L][diff > 1, per := 1L]  # 0=même période, 1=changement de période
    dt[, per := cumsum(per) + 1L, .(id)]  # numéroter les périodes de 1 à N
    dt <- dt[  # combiner les mêmes périodes en conservant le min et le max
    , .(debut = min(debut),
    fin = max(fin)),
    .(id, per)
    ][, per := NULL]  # supprimer la colonne

    # Vérifier si on doit refaire lalgorithme
    dt[, diff := as.integer(debut - shift(fin)), .(id)]  # nombre de jours entre début{i} et fin{i-1}
    idx <- rmNA(dt[, .I[diff <= njours]])  # vérifier si une combinaison des périodes doit être faite
    }
    } else {
    dt[, diff := as.integer(debut - shift(fin)), by = c("id", par_cols)]  # nombre de jours entre début{i} et fin{i-1}
    idx <- rmNA(dt[, .I[diff <= njours]])  # vérifier si une combinaison des périodes doit être faite
    while (length(idx)) {  # appliquer lalgorithme à chaque fois quil y a au moins un diff <= njours
    dt[is.na(diff), diff := 0L]
    dt[, per := 0L][diff > 1, per := 1L]  # 0=même période, 1=changement de période
    dt[, per := cumsum(per) + 1L, by = c("id", par_cols)]  # numéroter les périodes de 1 à N
    dt <- dt[  # combiner les mêmes périodes en conservant le min et le max
    , .(debut = min(debut),
    fin = max(fin)),
    by = c("id", par_cols, "per")
    ][, per := NULL]  # supprimer la colonne

    # Vérifier si on doit refaire lalgorithme
    dt[, diff := as.integer(debut - shift(fin)), by = c("id", par_cols)]  # nombre de jours entre début{i} et fin{i-1}
    idx <- rmNA(dt[, .I[diff <= njours]])  # vérifier si une combinaison des périodes doit être faite
    }
    }

    # Arranger la table finale
    dt[, diff := NULL]
    setnames(dt, c("id", "debut", "fin"), c(id, debut, fin))

    return(dt)')}
        else{
          return ("Afficher le script de la requête ...")
        }
      })
      observe({ cat(input$ace_tab4, "\n")})
      observe({cat(input$ace_selection_tab4, "\n")})
      observe({
        updateAceEditor(
          session,
          "ace_tab4",
          value = init(),
          theme = theme_r(),
          mode = mode_r(),
        )
      })

      # afficher le message de progression ####
      output$result_tab4 <- renderPrint({
        if(is.null(input$file_tab4)){
          result_tab4<-result_tab4()
          if (length(warnings$data) > 0) {
            print(paste("Warnings: ", warnings$data))
          }
        } else {
          withProgress(
            message = "La requête est en exécution. Veuillez-svp patienter...",
            value = 0,
            {
              n <- 10
              for (i in 1:9) {
                Sys.sleep(0.5)
                incProgress(1/n, detail = paste("Doing part", i*10,"%"))
              }
              result_tab4<-result_tab4()
              if (length(warnings$data) > 0) {
                print(paste("Warnings: ", warnings$data))
              }
            }
          )
        }
      }
      )

      # Définir les noms tableaux / figs à affichés ####
      output$DT_final_ui_tab4 <- renderUI({navbarPage("Cohorte et Tableaux descriptifs",
                                                      theme = "bootstrap",
                                                      header = tags$style(".navbar-default .navbar-brand {color: black;}"),# font-weight: bold;
                                                      tabPanel("DT_final_combin",
                                                               downloadButton("downloadData_tab4", "Download"),
                                                               dataTableOutput("DT_final_combin")
                                                      ))})
      # Afficher la cohorte ####
      output$DT_final_combin <- renderDataTable(result_tab4()) #DT::renderDT()
      # Download BD ####
      observeEvent(input$Executer_tab4, {
        output$downloadData_tab4 <- downloadHandler(
          filename = function() {
            paste("DT_final_combin-", Sys.Date(), ".", input$fileType_tab4, sep = "")
          },
          content = function(file) {
            if (input$fileType_tab4 == "csv") {
              write.csv(result_tab4(), file, row.names = FALSE)
            } else {
              write.table(result_tab4(), file, sep = ";", dec = ".",quote=FALSE,row.names = FALSE, col.names = TRUE)
            }
          }
        )
      })

    })
    #####
    '#####
     #####'
    # rénésialisation des champs ####
    # Extraction données ####
    observeEvent(input$reset, {
      updateAceEditor(session, "ace", value = "")
      output$result <- renderPrint({NULL})
      output$DT_final_ui <- renderUI({NULL})
      output$DT_final_ui1 <- renderUI({NULL})
      output$DT_final <- renderDataTable({NULL})
    })
    # Teradata Studio ####
    observeEvent(input$reset_tera, {
      updateAceEditor(session, "ace_tera", value = "")
      output$result_tera <- renderPrint({NULL})
      output$DT_final_ui_tera <- renderUI({NULL})
      output$DT_final_tera <- renderDataTable({NULL})
    })
    # Creation cohorte ####
    observeEvent(input$reset_tab2, {
      output$result_tab2 <- renderPrint({NULL})
      output$DT_final_ui_tab2 <- renderUI({NULL})
      output$cohort_final <- renderDataTable({NULL})
    })
    # Standardisation ####
    # Combine période ####
    observeEvent(input$reset_tab4, {
      updateAceEditor(session, "ace_tab4", value = "")
      output$result_tab4 <- renderPrint({NULL})
      output$DT_final_ui_tab4 <- renderUI({NULL})
      output$DT_final_combin <- renderDataTable({NULL})
    })

  })


  # Run App ####
  shinyApp(ui = ui, server = server)


}





