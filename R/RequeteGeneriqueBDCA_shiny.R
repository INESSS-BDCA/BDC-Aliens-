#' @title user_I_SMOD_SERV_MD_CM_shiny
#'
#' @description L'application `user_I_SMOD_SERV_MD_CM_shiny` est destinée aux utilisateurs. Elle permet d'exécuter la rêquete SQL retournée par la fonction `query_I_SMOD_SERV_MD_CM_AG`.\cr
#' Pour plus d'information, veuillez consulter la vignette de la fonction `user_I_SMOD_SERV_MD_CM_AG`.\cr
#'
#' #' Elle permet: \cr
#' 1) d'extraire les variables pertinantes de la base de données SMOD, jumlées avec d'autres bases de données pour rajouter les caractéristiques de dispensateur, du bénéficiaire et de l'établissement de soins.\cr
#' 2) d'extraire les variables pertinantes de la base de données SMOD et d'effectuer des analyses discriptives afin d'orienter la prise de décision.\cr
#'
#' @inheritParams user_I_SMOD_SERV_MD_CM_AG
#'
#' @encoding UTF-8
#' @import shiny shinyAce bslib shinycssloaders shinyjs plotly ggplot2 dplyr stringr
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
                       textInput("fct", label = h3("Fonction générique exécutée"),value = ""),
                       selectInput("DT", label = "Vue de données",choices = c("SMOD","BDCU","MEDECHO")),
                       helpText("Veuillez choisir la vue de données"),
                       selectInput("task", "task", choices = NULL),
                       helpText("Veuillez indiquer la requête que vous voulez exécuter"),
                       textInput("sql_user", "Identifiant", value = "ms069a"),
                       helpText("Veuillez indiquer votre ms0xx"),
                       passwordInput("sql_pwd", "Mot de passe", value = "Kong2050"),
                       helpText("Veuillez rentrer votre mot de passe teradata"),
                       selectInput("cohort", "cohort",choices = c("NULL", "cohort")),
                       helpText("Veuillez indiquer si vous avez une cohorte prédéfinie"),
                       uiOutput("fileInput"),
                       textInput("debut_periode", label = "debut_periode","2020-01-01"),
                       helpText("Veuillez indiquer le début de la période de l'étude"),
                       textInput("fin_periode", label ="fin_periode","2020-12-31"),
                       helpText("Veuillez indiquer la fin de la période de l'étude"),
                       uiOutput("DTInput"),
                       selectInput("benef_adr", "benef_adr",choices = c("date_fixe","dernière_adresse","première_adresse","long_adresse"),selected = "dernière_adresse"),
                       helpText("Veuillez indiquer la méthode d'identification de l'adresse du bénéficiaire"),
                       textInput("date_adr", label = "date_adr",value="NULL"),
                       helpText("Si benef_adr=date_fixe,veuillez indiquer une date pour identifier l'adresse du bénéficiaire"),
                       textInput("date_age", label = "date_age",value="NULL"),
                       helpText("Si keep_all=TRUE, veuillez indiquer la date à laquelle l’âge des bénéficaires qui n’ont pas eu d’acte est calculé.
                               L’âge de bénéficiaires qui ont eu un acte à l’intérieur de la période de l’étude est calculé à la date de l’acte."),
                       checkboxInput("keep_all", "keep_all", value = FALSE),
                       helpText("Par défault keep_all=FALSE, soit garder seulement les ids ayant eu un Dx et un acte à l'interieur de la période de l'étude.
                               Si keep_all=TRUE, garder tous les IDs"),
                       checkboxInput("verbose", "verbose", value = TRUE),
                       helpText("Par défault verbose=TRUE, un message de progression est affiché dans les résultats sous l'onglet 'Message de progression et warnings:'"),
                       checkboxInput("creation_cohort", "Paramétres pour la création d'une cohorte", value = FALSE),
                       uiOutput("creation_cohort_select"),
                       checkboxInput("Code_sql", "Paramétres d'affichage du code sql", value = FALSE),
                       uiOutput("Code_sql_select"),
                       selectInput("fileType", "Download: Type fichier:", choices = c("csv", "txt","png")),
                       actionButton("Executer", "Exécuter la requête",style = "background-color: green;color: white;"),
                       actionButton("reset", "Réinitialiser les champs"),
                     ),
                     mainPanel(
                       tabsetPanel(type="tabs",
                                   tabPanel("Résultats", style='width: 1000px; height: 1000px',
                                            h4("Résultats de la requête"),
                                            h5("Code sql:"),
                                            p("Afficher le code sql exécuté pour effectuer la requête demandée.", style = "font-family: 'times'; font-si16pt"),
                                            aceEditor(outputId = "ace",
                                                      selectionId = "selection",
                                                      placeholder = "Afficher le code sql de la requête ..."),
                                            h5("Message de progression et warnings:"),
                                            verbatimTextOutput("result"),
                                            uiOutput("DT_final_ui"),
                                            uiOutput("DT_final_ui1")),
                                   tabPanel("Vignettes", style='width: 1000px; height: 1000px',
                                            h4("vignette de la fonction"),
                                            conditionalPanel(
                                              condition = "input.DT == 'SMOD'",
                                              includeHTML("vignettes/user_I_SMOD_SERV_MD_CM_AG.html")),
                                            conditionalPanel(
                                              condition = "input.DT == 'BDCU'",
                                              includeHTML("vignettes/user_V_EPISO_SOIN_DURG_CM_AG.html"))
                                   )
                       )
                     )
                   )
          ),
          # Création d'une cohorte ####
          tabPanel("Création d'une cohorte",style='width: 1500px; height: 1000px',
                   sidebarLayout(
                     sidebarPanel(
                       textInput("fct_tab2", label = h3("Fonction générique exécutée"),value = "SQL_reperage_cond_med"),
                       textInput("sql_user_tab2", "Identifiant", value = "ms069a"),
                       helpText("Veuillez indiquer votre ms0xx"),
                       passwordInput("sql_pwd_tab2", "Mot de passe", value = "Kong2050"),
                       helpText("Veuillez rentrer votre mot de passe teradata"),
                       textInput("Dx_table_tab2", label = "Dx_table",value="list(Coronaro = list(CIM9 = paste0(c(491, 492, 496), '%'), CIM10 = paste0('J', 41:44, '%')))"),
                       helpText("Si vous voulez créer une cohorte,veuillez indiquer les Dx cim9 et/ou cim10\n
                           Voici un exemple: list(Coronaro = list(CIM9 = paste0(c(491, 492, 496), '%'), CIM10 = paste0('J', 41:44, '%')))"),
                       textInput("debut_cohort_tab2", label = "debut_cohort","2020-01-01"),
                       helpText("Si vous voulez créer une cohorte,veuillez indiquer la date de début de la période"),
                       textInput("fin_cohort_tab2", label = "fin_cohort","2021-01-01"),
                       helpText("Si vous voulez créer une cohorte,veuillez indiquer la date de fin de la période"),
                       textInput("CIM_tab2", label ="CIM",value = paste(c("CIM9","CIM10"),collapse = ",")),
                       helpText("Si vous voulez créer une cohorte,veuillez indiquer le type de Dx cim9, cim10 ou les deux"),
                       checkboxInput("by_Dx_tab2", "by_Dx", value = FALSE),
                       helpText("Si vous voulez créer une cohorte,veuillez indiquer si vous voulez différencier entre les conditions médicales définies dans Dx_table"),
                       selectInput("date_dx_var_tab2", "date_dx_var",choices = c("admis","depar"),selected="admis"),
                       helpText("Si vous voulez créer une cohorte,veuillez choisir entre la date d’admission ou la date de départ comme date d’incidence dans les vues suivantes :
                           V_DIAGN_SEJ_HOSP_CM, V_SEJ_SERV_HOSP_CM, V_EPISO_SOIN_DURG_CM"),
                       numericInput("n1_tab2", label = "n1", value = 30),
                       numericInput("n2_tab2", label = "n2", value = 730),
                       helpText("Si vous voulez créer une cohorte,veuillez indiquer le nombre de jours entre lesquels deux diagnostics doit se situer pour que le deuxième Dx confirme le premier."),
                       numericInput("nDx_tab2", label ="nDx", value = 0),
                       helpText("Si vous voulez créer une cohorte,veuillez choisir le nombre de diagnostic qui permet de confirmer le premier Dx"),
                       textInput("code_stat_decis_tab2", label = "code_stat_decis",value = paste(c("PAY","PPY"),collapse = ",")),
                       helpText("Veuillez indiquer si vous voulez les actes payéa et/ou prépayés"),
                       selectInput("fileType_tab2", "Download: Type fichier:", choices = c("csv", "txt")),
                       actionButton("Executer_tab2", "Exécuter la requête",style = "background-color: green;color: white;"),
                       actionButton("reset_tab2", "Réinitialiser les champs")),
                     mainPanel(
                       tabsetPanel(type="tabs",
                                   tabPanel("Résultats", style='width: 1000px; height: 1000px',
                                            h4("Résultats de la requête"),
                                            h5("Message de progression et warnings:"),
                                            verbatimTextOutput("result_tab2"),
                                            uiOutput("DT_final_ui_tab2")),
                                   tabPanel("Vignettes", style='width: 1000px; height: 1000px',
                                            h4("vignette de la fonction"),
                                            includeHTML("vignettes/SQL_reperage_cond_med.html"))

                       )
                     )
                   )
          ),
          # standardisation ####
          tabPanel("Standardisation",style='width: 1500px; height: 1000px',
                   sidebarLayout(
                     sidebarPanel(
                       textInput("donnees","donnees",value = "donnees"),
                       fileInput("DT_tab3", "Veuillez choisir votre cohorte d'intérêt"),
                       textInput("regroupement","regroupement"),
                       textInput("regroupement_reference","regroupement_reference",value="NULL"),
                       textInput("age","age",value="age_patient"),
                       textInput("age_cat_mins","age_cat_mins","0,50,70,80"),
                       textInput("sexe","sexe","sex"),
                       textInput("autres_vars","autres_vars","NULL"),
                       textInput("indicateur","indicateur","deces"),
                       textInput("valeur","valeur",value = "1_Oui"),
                       textInput("ref_externe","ref_externe",value = c(FALSE,TRUE)),
                       textInput("ref_externe_annee","ref_externe_annee",value = NULL),
                       textInput("ref_externe_age_cat_mins","ref_externe_age_cat_mins",value = NULL),
                       selectInput("methode","methode",choices = c("direct","indirect")),
                       textInput("multiplicateur","multiplicateur",value=100),
                       selectInput("fileType_tab3", "Download: Type fichier:", choices = c("csv", "txt")),
                       actionButton("Executer_tab3", "Exécuter la requête",style = "background-color: green;color: white;"),
                       actionButton("reset_tab3", "Réinitialiser les champs")),
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
          )
          #####
        )
    )
  )


  # server- Define the server logic for the Shiny app ####
  server<-shinyServer(function(input, output, session) {
    #bs_themer()

    # Extraction Donnees ####
    ## renderUI: extraction donnees ####
    ### Fonction exécutée####
    observeEvent(input$DT, {
      if (input$DT == "SMOD") {
        updateTextInput(session, "fct", value = "user_I_SMOD_SERV_MD_CM_AG")
      } else if (input$DT == "BDCU") {
        updateTextInput(session, "fct", value = "user_V_EPISO_SOIN_DURG_CM_AG")
      } else {
        updateTextInput(session, "fct", value = "")
      }
    })
    ### Choix de vue de données et Task ####
    output$DTInput <- renderUI({
      if (input$DT == "SMOD"){
        tagList(
          textInput("CodeActe", label = "CodeActe","07122,07237,07800,07089,0780"),
          helpText("Veuillez indiquer les codes d'acte d'intérêt"),
          selectInput("omni_spec", "omni_spec",choices = c("all","omni","spec")),
          helpText("Veuillez indiquer si vous voulez les actes facturés par les omni et/ou les spec"),
          selectInput("catg_etab", "catg_etab",choices = c("all","ambulatoire")),
          helpText("Veuillez indiquer si vous voulez les actes facturés selon l'établissement de soins"),
          textInput("code_stat_decis", label = "code_stat_decis",value = paste(c("PAY","PPY"),collapse = ",")),
          helpText("Veuillez indiquer si vous voulez les actes payéa et/ou prépayés")
        )
      } else if (input$DT=="BDCU"){
        tagList(
          selectInput("date_dx_var", "date_dx_var",choices = c("admis","depar"),selected="admis"),
          helpText("Si vous voulez créer une cohorte,veuillez choisir entre la date d’admission ou la date de départ comme date d’incidence dans les vues suivantes :
                V_DIAGN_SEJ_HOSP_CM, V_SEJ_SERV_HOSP_CM, V_EPISO_SOIN_DURG_CM")
        )
      }
    })


    observeEvent(input$DT, {
      if (input$DT == "SMOD") {
        updateSelectInput(session, "task", choices = c("extraction_SMOD", "projet_facturation_acte"))
      } else if (input$DT == "BDCU") {
        updateSelectInput(session, "task", choices = c("extraction_BDCU"))
      } else {
        updateSelectInput(session, "task", choices = c("MEDECHO"))
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
          textInput("Dx_table", label = "Dx_table",value="NULL"),
          helpText("Si vous voulez créer une cohorte,veuillez indiquer les Dx cim9 et/ou cim10\n
                   Voici un exemple: list(Coronaro = list(CIM9 = paste0(c(491, 492, 496), '%'), CIM10 = paste0('J', 41:44, '%')))"),
          textInput("debut_cohort", label = "debut_cohort"),
          helpText("Si vous voulez créer une cohorte,veuillez indiquer la date de début de la période"),
          textInput("fin_cohort", label = "fin_cohort"),
          helpText("Si vous voulez créer une cohorte,veuillez indiquer la date de fin de la période"),
          textInput("CIM", label ="CIM",value = paste(c("CIM9","CIM10"),collapse = ",")),
          helpText("Si vous voulez créer une cohorte,veuillez indiquer le type de Dx cim9, cim10 ou les deux"),
          checkboxInput("by_Dx", "by_Dx", value = FALSE),
          helpText("Si vous voulez créer une cohorte,veuillez indiquer si vous voulez différencier entre les conditions médicales définies dans Dx_table"),
          selectInput("date_dx_var", "date_dx_var",choices = c("admis","depar"),selected="admis"),
          helpText("Si vous voulez créer une cohorte,veuillez choisir entre la date d’admission ou la date de départ comme date d’incidence dans les vues suivantes :
                V_DIAGN_SEJ_HOSP_CM, V_SEJ_SERV_HOSP_CM, V_EPISO_SOIN_DURG_CM"),
          numericInput("n1", label = "n1", value = 30),
          numericInput("n2", label = "n2", value = 730),
          helpText("Si vous voulez créer une cohorte,veuillez indiquer le nombre de jours entre lesquels deux diagnostics doit se situer pour que le deuxième Dx confirme le premier."),
          numericInput("nDx", label ="nDx", value = 0),
          helpText("Si vous voulez créer une cohorte,veuillez choisir le nombre de diagnostic qui permet de confirmer le premier Dx")

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
        tagList(selectInput("mode", "Mode d'affichage du code générique: ", choices = getAceModes(), selected = mode()),
                helpText("Par défaut, le code est affiché en mode sql sous l'onglet 'Code sql:'. Il est possible de choisir d'autres modes"),
                selectInput("theme", "Thème d'affichage du code générique: ", choices = getAceThemes(), selected = theme()),
                helpText("Par défaut, le code est affiché avec le thème 'sqlserver'. Il est possible de choisir d'autres thèmes")

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
          showNotification("L'exécution de la requête a été intérompue: L'argument 'date_age' est vide. Veuillez indiquer la date à laquelle l'âge des bénéficiaires n'ayant pas eu d'acte doit être calculé", duration = 0, type="warning")
          #stop("L'exécution de la requête a été intérompue: L'argument 'date_age' est vide. Veuillez indiquer la date à laquelle l'âge des bénéficiaires n'ayant pas eu d'acte doit être calculé")
        }
      }
      if(input$cohort=="cohort"){
        if(is.null(input$file)){
          showNotification("L'exécution de la requête a été intérompue: L'argument 'file' est vide. Veuillez selectionner un fichier pour votre cohorte d'intérêt", duration = 0, type="warning")
          #stop("L'exécution de la requête a été intérompue: L'argument 'file' est vide. Veuillez selectionner un fichier pour votre cohorte d'intérêt")
        }
      }
      if(input$benef_adr=="date_fixe"){
        if(input$date_adr=="NULL"){
          showNotification(paste("L'exécution de la requête a été intérompue: L'argument 'date_adr' est vide. Veuillez indiquer la date à laquelle l'adresse des bénéficiaires doit être ciblée"), duration = 0, type="warning")
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
            withProgress(message = "La requête est en exécution. Veuillez-svp patienter...", value = 0, {
              #incProgress(0.1)
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
          output$DT_final_ui <- renderUI({navbarPage("Cohorte et Tableaux descriptifs", # we can use a tabsetPanel instide navbarPage
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
                paste("DT_final-", Sys.Date(), ".", input$fileType, sep = "")
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
          init <- eventReactive(input$Executer,{
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
            withProgress(message = "La requête est en exécution. Veuillez-svp patienter...", value = 0, {
              #incProgress(0.1)
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

          output$DT_final_ui1 <- renderUI({
            if (!is.null(result())) {
              do.call(navbarPage, c("Cohorte et Tableaux discriptifs",
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
        if (input$task == "extraction_BDCU") {
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
            if (input$task == "extraction_BDCU") {
              Dx_table<-if (Dx_table() == "NULL") {
                NULL
              } else {
                eval(parse(text = Dx_table()))
              }
              return(query_V_EPISO_SOIN_DURG_CM_AG (query=input$task,
                                                    debut=input$debut_periode,
                                                    fin=input$fin_periode,
                                                    debut_cohort=input$debut_cohort,
                                                    fin_cohort=input$fin_cohort,
                                                    diagn=Dx_table,
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
            withProgress(message = "La requête est en exécution. Veuillez-svp patienter...", value = 0, {
              #incProgress(0.1)
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
          output$DT_final_ui <- renderUI({navbarPage("Cohorte et Tableaux descriptifs", # we can use a tabsetPanel instide navbarPage
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
                paste("DT_final-", Sys.Date(), ".", input$fileType, sep = "")
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




    # Creation cohorte ####
    ## renderUI Creation cohort ####
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
      output$DT_final_ui_tab2 <- renderUI({navbarPage("Cohorte et Tableaux descriptifs", # we can use a tabsetPanel instide navbarPage
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






    # Standardisation ####
    observeEvent(input$Executer_tab3, {
      output$image <- renderImage({
        list(
          src = "image/Trav_cours.jpg",
          contentType = "image/jpeg"
        )
      }, deleteFile = FALSE)
    })
    # rénésialisation des champs ####
    observeEvent(input$reset, {
      updateAceEditor(session, "ace", value = "")
      output$result <- renderPrint({NULL})
      output$DT_final_ui <- renderUI({NULL})
      output$DT_final_ui1 <- renderUI({NULL})
      output$DT_final <- renderDataTable({NULL})
    })

  })

  # Run App ####
  shinyApp(ui = ui, server = server)


}



