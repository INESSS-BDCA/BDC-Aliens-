#' @title user_I_SMOD_SERV_MD_CM_shiny
#'
#' @description La fonction `user_I_SMOD_SERV_MD_CM_shiny` est destinée aux utilisateurs. Elle permet d'exécuter la rêquette SQL retournée par la fonction `query_I_SMOD_SERV_MD_CM_AG`.\cr
#'
#' #' Elle permet: \cr
#' 1) d'extraire les variables pertinantes de la base de données SMOD, jumlées avec d'autres bases de données pour rajouter les caractéristiques de dispensateur, du bénéficiaire et de l'établissement de soins.\cr
#' 2) d'extraire les variables pertinantes de la base de données SMOD et d'effectuer des analyses discriptives afin d'orienter la prise de décision.\cr
#'
#' La fonction `user_I_SMOD_SERV_MD_CM_shiny` utilise plusieurs autres fonctions génériques telles que:\cr
#'
#' `SQL_reperage_cond_med` pour la création de la cohorte. Voir \code{\link{SQL_reperage_cond_med}}\cr
#' `query_V_FICH_ID_BEN_CM` pour l'extraction des caractéristiques de bénéficiaire. Voir \code{\link{query_V_FICH_ID_BEN_CM}}
#'
#' @encoding UTF-8
#' @import shiny shinyAce plotly ggplot2 dplyr stringr
#' @export

user_I_SMOD_SERV_MD_CM_shiny<-function(){
  # Ui- Define the user interface for the Shiny app ####
  ui <- fluidPage(
    titlePanel("user_I_SMOD_SERV_MD_CM_AG Shiny-App"),
    shinythemes::shinytheme("cerulean"),
    sidebarLayout(
      sidebarPanel(
        selectInput("task", "task",choices = c("extraction_SMOD","projet_facturation_acte")),
        helpText("Veuillez indiquer la requête que vous voulez exécuter", style = "font-size: 10px;"),
        textInput("sql_user", "Identifiant", value = "ms069a"),
        helpText("Veuillez indiquer votre ms0xx", style = "font-size: 10px;"),
        passwordInput("sql_pwd", "Mot de passe", value = "Kong2050"),
        helpText("Veuillez rentrer votre mot de passe teradata", style = "font-size: 10px;"),
        selectInput("cohort", "cohort",choices = c("NULL", "cohort")),
        helpText("Veuillez indiquer si vous avez une cohorte prédéfinie", style = "font-size: 10px;"),
        fileInput("file", "Veuillez choisir votre cohorte d'intérêt"),

        textInput("debut_periode", label = "debut_periode","2020-01-01"),
        helpText("Veuillez indiquer le début de la période de l'étude", style = "font-size: 10px;"),
        textInput("fin_periode", label ="fin_periode","2020-12-31"),
        helpText("Veuillez indiquer la fin de la période de l'étude", style = "font-size: 10px;"),
        textInput("CodeActe", label = "CodeActe","07122,07237,07800,07089,0780"),
        helpText("Veuillez indiquer les codes d'acte d'intérêt", style = "font-size: 10px;"),
        selectInput("omni_spec", "omni_spec",choices = c("all","omni","spec")),
        helpText("Veuillez indiquer si vous voulez les actes facturés par les omni et/ou les spec", style = "font-size: 10px;"),
        selectInput("catg_etab", "catg_etab",choices = c("all","ambulatoire")),
        helpText("Veuillez indiquer si vous voulez les actes facturés selon l'établissement de soins", style = "font-size: 10px;"),
        textInput("code_stat_decis", label = "code_stat_decis",value = paste(c("PAY","PPY"),collapse = ",")),
        helpText("Veuillez indiquer si vous voulez les actes payéa et/ou prépayés", style = "font-size: 10px;"),
        selectInput("benef_adr", "benef_adr",choices = c("date_fixe","dernière_adresse","première_adresse","long_adresse"),selected = "dernière_adresse"),
        helpText("Veuillez indiquer la méthode d'identification de l'adresse du bénéficiaire", style = "font-size: 10px;"),
        textInput("date_adr", label = "date_adr",value="NULL"),
        helpText("Si benef_adr=date_fixe,veuillez indiquer une date pour identifier l'adresse du bénéficiaire", style = "font-size: 10px;"),
        textInput("date_age", label = "date_age",value="NULL"),
        helpText("Si keep_all=TRUE, veuillez indiquer la date à laquelle l’âge des bénéficaires qui n’ont pas eu d’acte est calculé.
               L’âge de bénéficiaires qui ont eu un acte à l’intérieur de la période de l’étude est calculé à la date de l’acte.", style = "font-size: 10px;"),

        checkboxInput("keep_all", "keep_all", value = FALSE),
        helpText("Si keep_all=FALSE, garder seulement les ids ayant eu un DX et un acete à l'interieur de la période de l'étude.
               Si keep_all=TRUE, garder tous les IDs", style = "font-size: 10px;"),
        checkboxInput("verbose", "verbose", value = TRUE),
        helpText("Si verbose=TRUE, un message de progression est affiché dans les résultats sous l'onglet 'Message de progression et warnings:'", style = "font-size: 10px;"),

        checkboxInput("creation_cohort", "Paramétres pour la création d'une cohorte", value = FALSE),
        uiOutput("creation_cohort_select"),

        checkboxInput("Code_sql", "Paramétres d'affichage du code sql", value = FALSE),
        uiOutput("Code_sql_select"),

        selectInput("fileType", "Download: Type fichier:", choices = c("csv", "txt","png")),
        actionButton("Executer", "Exécuter la requête",style = "background-color: green;color: white;"),
        actionButton("reset", "Réinitialiser les champs"),
      ),

      mainPanel(
        h4("Résultats de la requête"),
        h5("Code sql:"),
        p("Afficher le code sql exécuté pour effectuer la requête demandée.", style = "font-family: 'times'; font-si16pt"),
        aceEditor(outputId = "ace",
                  selectionId = "selection",
                  placeholder = "Afficher le code sql de la requête ..."),
        h5("Message de progression et warnings:"),
        verbatimTextOutput("result"),
        uiOutput("DT_final_ui"),
        uiOutput("DT_final_ui1")
      )
    )
  )
  # server- Define the server logic for the Shiny app ####
server<-shinyServer(function(input, output, session) {

    # Hiden parameters: Cohort creation ####
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

    date_dx_var<- reactiveVal(c("admis","depar"))
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
                   Voici un exemple: list(Coronaro = list(CIM9 = paste0(c(491, 492, 496), '%'), CIM10 = paste0('J', 41:44, '%')))", style = "font-size: 10px;"),
          textInput("debut_cohort", label = "debut_cohort"),
          helpText("Si vous voulez créer une cohorte,veuillez indiquer la date de début de la période", style = "font-size: 10px;"),
          textInput("fin_cohort", label = "fin_cohort"),
          helpText("Si vous voulez créer une cohorte,veuillez indiquer la date de fin de la période", style = "font-size: 10px;"),
          textInput("CIM", label ="CIM",value = paste(c("CIM9","CIM10"),collapse = ",")),
          helpText("Si vous voulez créer une cohorte,veuillez indiquer le type de Dx cim9, cim10 ou les deux", style = "font-size: 10px;"),
          checkboxInput("by_Dx", "by_Dx", value = FALSE),
          helpText("Si vous voulez créer une cohorte,veuillez indiquer si vous voulez différencier entre les conditions médicales définies dans Dx_table", style = "font-size: 10px;"),
          selectInput("date_dx_var", "date_dx_var",choices = c("admis","depar")),
          helpText("Si vous voulez créer une cohorte,veuillez choisir entre la date d’admission ou la date de départ comme date d’incidence dans les vues suivantes :
                V_DIAGN_SEJ_HOSP_CM, V_SEJ_SERV_HOSP_CM, V_EPISO_SOIN_DURG_CM", style = "font-size: 10px;"),
          numericInput("n1", label = "n1", value = 30),
          numericInput("n2", label = "n2", value = 730),
          helpText("Si vous voulez créer une cohorte,veuillez indiquer le nombre de jours entre lesquels deux diagnostics doit se situer pour que le deuxième Dx confirme le premier.", style = "font-size: 10px;"),
          numericInput("nDx", label ="nDx", value = 0),
          helpText("Si vous voulez créer une cohorte,veuillez choisir le nombre de diagnostic qui permet de confirmer le premier Dx", style = "font-size: 10px;")

        )
      }
    })

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
                helpText("Par défaut, le code est affiché en mode sql sous l'onglet 'Code sql:'. Il est possible de choisir d'autres modes", style = "font-size: 10px;"),
                selectInput("theme", "Thème d'affichage du code générique: ", choices = getAceThemes(), selected = theme()),
                helpText("Par défaut, le code est affiché avec le thème 'sqlserver'. Il est possible de choisir d'autres thèmes", style = "font-size: 10px;")

        )
      }
    })

    textInput("setwd", label = "setwd", value = "NULL")
    setwd<- reactiveVal("NULL")
    observeEvent(input$setwd, {
      setwd(input$setwd)
    })

    #####

    observeEvent(input$Executer, {
      # Test des arguments ####
      if(input$keep_all==TRUE){
        if(input$date_age=="NULL"){
          stop("L'exécution de la requête a été intérompue: L'argument 'date_age' est vide. Veuillez indiquer la date à laquelle l'âge des bénéficiaires n'ayant pas eu d'acte doit être calculé")
        }
      }
      if(input$cohort=="cohort"){
        if(is.null(input$file)){
          stop("L'exécution de la requête a été intérompue: L'argument 'file' est vide. Veuillez selectionner un fichier pour votre cohorte d'intérêt")
        }
      }
      if(input$benef_adr=="date_fixe"){
        if(input$date_adr=="NULL"){
          stop("L'exécution de la requête a été intérompue: L'argument 'date_adr' est vide. Veuillez indiquer la date à laquelle l'adresse des bénéficiaires doit être ciblée")
        }
      }

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
            return("
SELECT

-- Sources des variables :
-- Nom de vue : Prod.I_SMOD_SERV_MD_CM (SMOD)
BD_SMOD.SMOD_NO_INDIV_BEN_BANLS AS ID, --Identifiant du bénéficiaire banalisé
case when EXTRACT(MONTH FROM SMOD_DAT_SERV) between 4 and 12 THEN EXTRACT(YEAR FROM SMOD_DAT_SERV)
ELSE EXTRACT(YEAR FROM SMOD_DAT_SERV)-1 end AS AnFinan, --Année financière
Extract (YEAR From BD_SMOD.SMOD_DAT_SERV) AS AnCivil,  --Année civile
BD_SMOD.SMOD_DAT_SERV AS DateActe,  --Date Acte
BD_SMOD.SMOD_COD_ACTE AS CodeActe,  --Code Acte
BD_SMOD.SMOD_MNT_PAIMT AS CoutActe,  --Coût Acte
BD_SMOD.SMOD_COD_DIAGN_PRIMR AS DxActe,  --Premier diagnostic porté par le professionnel
BD_SMOD.SMOD_NO_ETAB_USUEL AS Num_ETAB_USUEL,  --Ancien numéro de létablissement
BD_SMOD.SMOD_COD_ROLE, --Code des rôles selon lentente
BD_SMOD.SMOD_COD_ENTEN AS CodEntente,  --Code de lentente
BD_SMOD.ETAB_COD_SECT_ACTIV_ETAB AS SecActiv,  --secteur dactivité

-- Nom de vue : PROD.V_FICH_ID_BEN_CM (FIPA)

BD_Caract.BENF_COD_SEXE AS Sexe,  --Sexe du bénéficiaire
BD_Caract.BENF_DAT_NAISS AS DatNais,  --Date de naissance
BD_Caract.BENF_DAT_DECES AS DatDeces,  --Date de décès
Cast(((BD_SMOD.SMOD_DAT_SERV) - BD_Caract.BENF_DAT_NAISS) / 365.25 AS BIGINT) AS Age,  --Age calculé à la date de service

-- Nom de vue : PROD.D_DISP_SPEC_CM (FIP)

BD_SMOD.DISP_NO_SEQ_DISP_BANLS AS NumDispSp,  --Numéro du dispensateur
BD_SMOD.SMOD_COD_SPEC AS SPDisp_smod, --Spécialité du dispensateur (SMOD)
BD_Disp.DISP_COD_SPEC AS SPDisp_fip,  --Spécialité du dispensateur (FIP)
BD_SMOD.DISP_NO_SEQ_DISP_REFNT_BANLS AS NumDispRef,  --Numéro du médecin référent
BD_DispR.DISP_COD_SPEC AS SPDispRefs,  --Spécialité du médecin référent

-- Nom de vue : prod.V_ETAB_USUEL, V_ETAB_USUEL_DERN, V_COD_CATG_ETAB_EI

BD_Etab.ETAB_NO_ETAB AS NO_ETAB,
BD_Etab.ETAB_COD_CATG_ETAB_EI AS Cat_Etab, --(Urgence, Clinique externe, ...)

-- Nom de vue : RES_SSS.V_NOM_ETAB_DERN_TYP_NOM

BD_NomEtab.ETAB_NOM_ETAB AS NOM_ETAB,

-- Nom de vue (Info BEN) : PROD.I_BENF_ADR_CM (FIPA), tGI_RLS_RTS

BD_Adr.LGEO_COD_RSS AS RSS_Benf, --Région socio-sanitaire Bénéf
CASE
WHEN BD_Adr.LGEO_COD_RSS=10 THEN '1001'
WHEN BD_Adr.LGEO_COD_RSS=17 THEN '1701'
WHEN BD_Adr.LGEO_COD_RSS=18 THEN '1801' ELSE BD_Adr.LGEO_COD_TERRI_RLS END AS RLS_Benef, --Code RLS Bénéf
BD_NomReg_Ben.NomRSS_Benef,
BD_NomReg_Ben.NomRLS_Benef,

-- Nom de vue (Info ETAB) : V_DECOU_GEO_POSTL_DERN_CP, V_ETAB_USUEL, V_ETAB_USUEL_DERN, tGI_RLS_RTS

BD_Etab.LGEO_COD_RSS AS RSS_Etab,
BD_Etab.LGEO_COD_TERRI_RLS AS RLS_Etab,
BD_NomReg_Etab.NomRSS_Etab,
BD_NomReg_Etab.NomRLS_Etab

FROM Prod.I_SMOD_SERV_MD_CM AS BD_SMOD
LEFT JOIN Prod.V_FICH_ID_BEN_CM AS BD_Caract
ON BD_SMOD.SMOD_NO_INDIV_BEN_BANLS = BD_Caract.BENF_NO_INDIV_BEN_BANLS

/* cette requête cible la spécialité principale du disp dans fip au moment du service */
LEFT JOIN Prod.D_DISP_SPEC_CM AS BD_Disp
ON BD_SMOD.DISP_NO_SEQ_DISP_BANLS = BD_Disp.DISP_NO_SEQ_DISP_BANLS
AND BD_Disp.DISP_COD_NIV_SPEC = 1
AND BD_SMOD.SMOD_DAT_SERV BETWEEN BD_Disp.DISP_DD_SPEC_DISP AND BD_Disp.DISP_DF_SPEC_DISP

/* Cette requête est incluse dans la version actuelle, mais elle pourrait être optionnelle, car la spécialité du médecin référent est rarement demandée */
LEFT JOIN Prod.D_DISP_SPEC_CM AS BD_DispR
ON BD_SMOD.DISP_NO_SEQ_DISP_REFNT_BANLS = BD_DispR.DISP_NO_SEQ_DISP_BANLS
AND BD_DispR.DISP_COD_NIV_SPEC = 1
AND BD_SMOD.SMOD_DAT_SERV BETWEEN BD_DispR.DISP_DD_SPEC_DISP AND BD_DispR.DISP_DF_SPEC_DISP

/* Découpage géographique du bénéficiaire : codes RSS et RLS:
Plusieurs méthode pour sélectionner l'adresse du bénéficiaire durant la période sélectionnée:
1) cibler la première adresse de la période
2) cibler la dernière adresse de la période
2) cibler l'adresse dans laquelle le BEN a résidé le plus longtemps durant la période
3) cibler l'adresse à une date fixe (p. ex. 31 mars, 1 juillet, mi-année) */

LEFT JOIN (
SELECT
BENF_NO_INDIV_BEN_BANLS,
BENF_DD_ADR_BEN,
BENF_DF_ADR_BEN,
LGEO_COD_RSS,
LGEO_COD_TERRI_RLS

FROM(
SELECT
BENF_NO_INDIV_BEN_BANLS,
BENF_DD_ADR_BEN,
BENF_DF_ADR_BEN,
LGEO_COD_RSS,
LGEO_COD_TERRI_RLS,

CASE
WHEN (BENF_DD_ADR_BEN <= 'debut' AND BENF_DF_ADR_BEN >= 'fin') THEN DATE 'fin' - DATE 'debut' + 1
WHEN (BENF_DD_ADR_BEN >= 'debut' AND BENF_DF_ADR_BEN >= 'fin') THEN DATE 'fin' - BENF_DD_ADR_BEN
WHEN (BENF_DD_ADR_BEN <= 'debut' AND BENF_DF_ADR_BEN <= 'fin') THEN BENF_DF_ADR_BEN - DATE 'debut'
WHEN (BENF_DD_ADR_BEN >= 'debut' AND BENF_DF_ADR_BEN <= 'fin') THEN BENF_DF_ADR_BEN - BENF_DD_ADR_BEN
ELSE 0
END AS days_count,
ROW_NUMBER() OVER (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY days_count DESC) AS row_num /* cibler l'adresse la plus longue */

FROM Prod.I_BENF_ADR_CM
WHERE BENF_IND_ADR_HQ IN ('N') AND BENF_COD_TYP_ADR IN ('R')
AND (BENF_DD_ADR_BEN<='fin' AND BENF_DF_ADR_BEN>='debut')

AND 'date_adr' between BENF_DD_ADR_BEN AND BENF_DF_ADR_BEN  /* cibler une date fixe */
QUALIFY Row_Number()Over (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY BENF_DD_ADR_BEN DESC)=1 /* cibler la dernière adresse */
QUALIFY Row_Number()Over (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY BENF_DD_ADR_BEN ASC)=1 /* cibler la première adresse */
)AS A
WHERE A.row_num=1

) AS BD_Adr
ON BD_SMOD.SMOD_NO_INDIV_BEN_BANLS=BD_Adr.BENF_NO_INDIV_BEN_BANLS

/* On ajoute ici les noms des codes RSS et RLS du bénéficiaire */

LEFT JOIN (
SELECT
CodRLS,
NomRLS AS NomRLS_Benef,
NomRSS AS NomRSS_Benef
FROM DONNE_INESSS.tGI_RLS_RTS) AS BD_NomReg_Ben
ON BD_Adr.LGEO_COD_TERRI_RLS=BD_NomReg_Ben.CodRLS

/* Découpage géographique de létablissement : numéro unique, catégorie, nom de létab, code et nom RSS, code et nom RLS */

/* On ajoute ici le numéro détablissement (unique : ETAB_NO_ETAB), le code postal de létablissement et la catégorie de létablissement */

LEFT JOIN (
SELECT BD_Etab.ETAB_NO_ETAB_USUEL, BD_Etab.ETAB_NO_ETAB, BD_Etab.ETAB_COD_POSTL,
BD_Decoup_Etab.LGEO_COD_RSS, BD_Decoup_Etab.LGEO_COD_TERRI_RLS,
BD_Catg_etab.ETAB_COD_CATG_ETAB_EI, BD_NomCatg_etab.ETAB_NOM_CATG_ETAB_EI
FROM V_ETAB_USUEL_DERN AS BD_Etab
LEFT JOIN Prod.V_DECOU_GEO_POSTL_DERN_CP AS BD_Decoup_Etab ON BD_Etab.ETAB_COD_POSTL=BD_Decoup_Etab.LGEO_COD_POSTL
LEFT JOIN (SELECT DISTINCT ETAB_NO_ETAB, ETAB_COD_CATG_ETAB_EI FROM V_ETAB_USUEL
QUALIFY Row_Number()Over (PARTITION BY ETAB_NO_ETAB ORDER BY ETAB_DD_ETAB_USUEL DESC)=1) AS BD_Catg_etab ON BD_Etab.ETAB_NO_ETAB=BD_Catg_etab.ETAB_NO_ETAB
LEFT JOIN Prod.V_COD_CATG_ETAB_EI AS BD_NomCatg_etab ON BD_Catg_etab.ETAB_COD_CATG_ETAB_EI=BD_NomCatg_etab.ETAB_COD_CATG_ETAB_EI
) AS BD_Etab
ON BD_SMOD.SMOD_NO_ETAB_USUEL=BD_Etab.ETAB_NO_ETAB_USUEL

/* On ajoute le NOM de létablissement */

LEFT JOIN (SELECT ETAB_NO_ETAB, ETAB_NOM_ETAB FROM RES_SSS.V_NOM_ETAB_DERN_TYP_NOM) AS BD_NomEtab
ON BD_Etab.ETAB_NO_ETAB = BD_NomEtab.ETAB_NO_ETAB

/* On ajoute ici les noms des codes RSS et RLS de létablissement : ceci serait optionnel si la requête est trop lourde */

LEFT JOIN (SELECT CodRLS, NomRLS AS NomRLS_Etab, NomRSS AS NomRSS_Etab FROM DONNE_INESSS.tGI_RLS_RTS) AS BD_NomReg_Etab
ON BD_Etab.LGEO_COD_TERRI_RLS=BD_NomReg_Etab.CodRLS

WHERE BD_SMOD.SMOD_DAT_SERV BETWEEN 'debut' AND 'fin'

/* query_SQL_CodeActe.where_SMOD_COD_STA_DECIS (code_stat_decis)
      si code_stat_decis='PAY': AND BD_SMOD.SMOD_COD_STA_DECIS IN ('PAY')
      si code_stat_decis='PPY': AND BD_SMOD.SMOD_COD_STA_DECIS IN ('PPY')
      si les deux: AND BD_SMOD.SMOD_COD_STA_DECIS IN ('PAY','PPY') */

/* query_SQL_CodeActe.where_SMOD_COD_ACTE (CodeActe)
      si CodeActe=NULL: extraction de tous les actes dans la période détude, sinon extraction des actes indiqués dans le vecteur CodeActe */

/* query_I_SMOD_SERV_MD_CM.diagn(diagn)
      à préciser si on veut extraire des actes pour des bénéficiaires ayant un ou plusieurs Dx spécifiques */

/* query_SQL_CodeActe.where_SMOD_COD_SPEC(omni_spec):
        si omni_spec='all': extraction des actes facturés par les omni et les spécialistes
      si omni_spec='omni': AND SMOD_COD_SPEC IS NULL
      si omni_spec='spec': AND SMOD_COD_SPEC IS NOT NULL */

/* query_SQL_CodeActe.where_ETAB_COD_CATG_ETAB_EI(catg_etab),
      si catg_etab='all': extraction des visites effectuées dans toutes les catégories détablissement confondues
      si catg_etab='ambulatoire': AND (ETAB_COD_CATG_ETAB_EI IN ('54X', '55X', '57X', '9X2', '8X5', '4X1','0X1') OR BD_SMOD.SMOD_NO_ETAB_USUEL IS NULL) */

/* Cette exclusion est toujours importante à considérer, car les actes qui représentent des forfaits ne correspondent pas à des consultations, mais, plutôt à des suppléments monétaires associés à une demande de facturation */

AND BD_SMOD.SMOD_COD_ACTE NOT IN (
SELECT DISTINCT NMOD_COD_ACTE
FROM Prod.V_RPERT_COD_ACTE
WHERE NMOD_COD_GRP_ACTE IN (170, 180, 181, 182, 183, 187, 195, 196, 197, 198, 199, 854))
ORDER BY 1,2,3,4")
          }
          else if (input$task == "projet_facturation_acte"){
            return("SELECT

-- Sources des variables :
-- Nom de vue : Prod.I_SMOD_SERV_MD_CM (SMOD)
BD_SMOD.SMOD_NO_INDIV_BEN_BANLS AS ID, --Identifiant du bénéficiaire banalisé
case when EXTRACT(MONTH FROM SMOD_DAT_SERV) between 4 and 12 THEN EXTRACT(YEAR FROM SMOD_DAT_SERV)
ELSE EXTRACT(YEAR FROM SMOD_DAT_SERV)-1 end AS AnFinan, --Année financière
Extract (YEAR From BD_SMOD.SMOD_DAT_SERV) AS AnCivil,  --Année civile
BD_SMOD.SMOD_DAT_SERV AS DateActe,  --Date Acte
BD_SMOD.SMOD_COD_ACTE AS CodeActe,  --Code Acte
BD_SMOD.SMOD_MNT_PAIMT AS CoutActe,  --Coût Acte
BD_SMOD.SMOD_COD_DIAGN_PRIMR AS DxActe,  --Premier diagnostic porté par le professionnel
BD_SMOD.SMOD_NO_ETAB_USUEL AS Num_ETAB_USUEL,  --Ancien numéro de létablissement
BD_SMOD.SMOD_COD_ROLE, --Code des rôles selon lentente
BD_SMOD.SMOD_COD_ENTEN AS CodEntente,  --Code de lentente
BD_SMOD.ETAB_COD_SECT_ACTIV_ETAB AS SecActiv,  --secteur dactivité

-- Nom de vue : PROD.V_FICH_ID_BEN_CM (FIPA)

BD_Caract.BENF_COD_SEXE AS Sexe,  --Sexe du bénéficiaire
BD_Caract.BENF_DAT_NAISS AS DatNais,  --Date de naissance
BD_Caract.BENF_DAT_DECES AS DatDeces,  --Date de décès
Cast(((BD_SMOD.SMOD_DAT_SERV) - BD_Caract.BENF_DAT_NAISS) / 365.25 AS BIGINT) AS Age,  --Age calculé à la date de service

-- Nom de vue : PROD.D_DISP_SPEC_CM (FIP)

BD_SMOD.DISP_NO_SEQ_DISP_BANLS AS NumDispSp,  --Numéro du dispensateur
BD_SMOD.SMOD_COD_SPEC AS SPDisp_smod, --Spécialité du dispensateur (SMOD)
BD_Disp.DISP_COD_SPEC AS SPDisp_fip,  --Spécialité du dispensateur (FIP)
BD_SMOD.DISP_NO_SEQ_DISP_REFNT_BANLS AS NumDispRef,  --Numéro du médecin référent
BD_DispR.DISP_COD_SPEC AS SPDispRefs,  --Spécialité du médecin référent

-- Nom de vue : prod.V_ETAB_USUEL, V_ETAB_USUEL_DERN, V_COD_CATG_ETAB_EI

BD_Etab.ETAB_NO_ETAB AS NO_ETAB,
BD_Etab.ETAB_COD_CATG_ETAB_EI AS Cat_Etab, --(Urgence, Clinique externe, ...)

-- Nom de vue : RES_SSS.V_NOM_ETAB_DERN_TYP_NOM

BD_NomEtab.ETAB_NOM_ETAB AS NOM_ETAB,

-- Nom de vue (Info BEN) : PROD.I_BENF_ADR_CM (FIPA), tGI_RLS_RTS

BD_Adr.LGEO_COD_RSS AS RSS_Benf, --Région socio-sanitaire Bénéf
CASE
WHEN BD_Adr.LGEO_COD_RSS=10 THEN '1001'
WHEN BD_Adr.LGEO_COD_RSS=17 THEN '1701'
WHEN BD_Adr.LGEO_COD_RSS=18 THEN '1801' ELSE BD_Adr.LGEO_COD_TERRI_RLS END AS RLS_Benef, --Code RLS Bénéf
BD_NomReg_Ben.NomRSS_Benef,
BD_NomReg_Ben.NomRLS_Benef,

-- Nom de vue (Info ETAB) : V_DECOU_GEO_POSTL_DERN_CP, V_ETAB_USUEL, V_ETAB_USUEL_DERN, tGI_RLS_RTS

BD_Etab.LGEO_COD_RSS AS RSS_Etab,
BD_Etab.LGEO_COD_TERRI_RLS AS RLS_Etab,
BD_NomReg_Etab.NomRSS_Etab,
BD_NomReg_Etab.NomRLS_Etab

FROM Prod.I_SMOD_SERV_MD_CM AS BD_SMOD
LEFT JOIN Prod.V_FICH_ID_BEN_CM AS BD_Caract
ON BD_SMOD.SMOD_NO_INDIV_BEN_BANLS = BD_Caract.BENF_NO_INDIV_BEN_BANLS

/* cette requête cible la spécialité principale du disp dans fip au moment du service */
LEFT JOIN Prod.D_DISP_SPEC_CM AS BD_Disp
ON BD_SMOD.DISP_NO_SEQ_DISP_BANLS = BD_Disp.DISP_NO_SEQ_DISP_BANLS
AND BD_Disp.DISP_COD_NIV_SPEC = 1
AND BD_SMOD.SMOD_DAT_SERV BETWEEN BD_Disp.DISP_DD_SPEC_DISP AND BD_Disp.DISP_DF_SPEC_DISP

/* Cette requête est incluse dans la version actuelle, mais elle pourrait être optionnelle, car la spécialité du médecin référent est rarement demandée */
LEFT JOIN Prod.D_DISP_SPEC_CM AS BD_DispR
ON BD_SMOD.DISP_NO_SEQ_DISP_REFNT_BANLS = BD_DispR.DISP_NO_SEQ_DISP_BANLS
AND BD_DispR.DISP_COD_NIV_SPEC = 1
AND BD_SMOD.SMOD_DAT_SERV BETWEEN BD_DispR.DISP_DD_SPEC_DISP AND BD_DispR.DISP_DF_SPEC_DISP

/* Découpage géographique du bénéficiaire : codes RSS et RLS:
Plusieurs méthode pour sélectionner l'adresse du bénéficiaire durant la période sélectionnée:
1) cibler la première adresse de la période
2) cibler la dernière adresse de la période
2) cibler l'adresse dans laquelle le BEN a résidé le plus longtemps durant la période
3) cibler l'adresse à une date fixe (p. ex. 31 mars, 1 juillet, mi-année) */

LEFT JOIN (
SELECT
BENF_NO_INDIV_BEN_BANLS,
BENF_DD_ADR_BEN,
BENF_DF_ADR_BEN,
LGEO_COD_RSS,
LGEO_COD_TERRI_RLS

FROM(
SELECT
BENF_NO_INDIV_BEN_BANLS,
BENF_DD_ADR_BEN,
BENF_DF_ADR_BEN,
LGEO_COD_RSS,
LGEO_COD_TERRI_RLS,

CASE
WHEN (BENF_DD_ADR_BEN <= 'debut' AND BENF_DF_ADR_BEN >= 'fin') THEN DATE 'fin' - DATE 'debut' + 1
WHEN (BENF_DD_ADR_BEN >= 'debut' AND BENF_DF_ADR_BEN >= 'fin') THEN DATE 'fin' - BENF_DD_ADR_BEN
WHEN (BENF_DD_ADR_BEN <= 'debut' AND BENF_DF_ADR_BEN <= 'fin') THEN BENF_DF_ADR_BEN - DATE 'debut'
WHEN (BENF_DD_ADR_BEN >= 'debut' AND BENF_DF_ADR_BEN <= 'fin') THEN BENF_DF_ADR_BEN - BENF_DD_ADR_BEN
ELSE 0
END AS days_count,
ROW_NUMBER() OVER (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY days_count DESC) AS row_num /* cibler l'adresse la plus longue */

FROM Prod.I_BENF_ADR_CM
WHERE BENF_IND_ADR_HQ IN ('N') AND BENF_COD_TYP_ADR IN ('R')
AND (BENF_DD_ADR_BEN<='fin' AND BENF_DF_ADR_BEN>='debut')

AND 'date_adr' between BENF_DD_ADR_BEN AND BENF_DF_ADR_BEN  /* cibler une date fixe */
QUALIFY Row_Number()Over (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY BENF_DD_ADR_BEN DESC)=1 /* cibler la dernière adresse */
QUALIFY Row_Number()Over (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY BENF_DD_ADR_BEN ASC)=1 /* cibler la première adresse */
)AS A
WHERE A.row_num=1

) AS BD_Adr
ON BD_SMOD.SMOD_NO_INDIV_BEN_BANLS=BD_Adr.BENF_NO_INDIV_BEN_BANLS

/* On ajoute ici les noms des codes RSS et RLS du bénéficiaire */

LEFT JOIN (
SELECT
CodRLS,
NomRLS AS NomRLS_Benef,
NomRSS AS NomRSS_Benef
FROM DONNE_INESSS.tGI_RLS_RTS) AS BD_NomReg_Ben
ON BD_Adr.LGEO_COD_TERRI_RLS=BD_NomReg_Ben.CodRLS

/* Découpage géographique de létablissement : numéro unique, catégorie, nom de létab, code et nom RSS, code et nom RLS */

/* On ajoute ici le numéro détablissement (unique : ETAB_NO_ETAB), le code postal de létablissement et la catégorie de létablissement */

LEFT JOIN (
SELECT BD_Etab.ETAB_NO_ETAB_USUEL, BD_Etab.ETAB_NO_ETAB, BD_Etab.ETAB_COD_POSTL,
BD_Decoup_Etab.LGEO_COD_RSS, BD_Decoup_Etab.LGEO_COD_TERRI_RLS,
BD_Catg_etab.ETAB_COD_CATG_ETAB_EI, BD_NomCatg_etab.ETAB_NOM_CATG_ETAB_EI
FROM V_ETAB_USUEL_DERN AS BD_Etab
LEFT JOIN Prod.V_DECOU_GEO_POSTL_DERN_CP AS BD_Decoup_Etab ON BD_Etab.ETAB_COD_POSTL=BD_Decoup_Etab.LGEO_COD_POSTL
LEFT JOIN (SELECT DISTINCT ETAB_NO_ETAB, ETAB_COD_CATG_ETAB_EI FROM V_ETAB_USUEL
QUALIFY Row_Number()Over (PARTITION BY ETAB_NO_ETAB ORDER BY ETAB_DD_ETAB_USUEL DESC)=1) AS BD_Catg_etab ON BD_Etab.ETAB_NO_ETAB=BD_Catg_etab.ETAB_NO_ETAB
LEFT JOIN Prod.V_COD_CATG_ETAB_EI AS BD_NomCatg_etab ON BD_Catg_etab.ETAB_COD_CATG_ETAB_EI=BD_NomCatg_etab.ETAB_COD_CATG_ETAB_EI
) AS BD_Etab
ON BD_SMOD.SMOD_NO_ETAB_USUEL=BD_Etab.ETAB_NO_ETAB_USUEL

/* On ajoute le NOM de létablissement */

LEFT JOIN (SELECT ETAB_NO_ETAB, ETAB_NOM_ETAB FROM RES_SSS.V_NOM_ETAB_DERN_TYP_NOM) AS BD_NomEtab
ON BD_Etab.ETAB_NO_ETAB = BD_NomEtab.ETAB_NO_ETAB

/* On ajoute ici les noms des codes RSS et RLS de létablissement : ceci serait optionnel si la requête est trop lourde */

LEFT JOIN (SELECT CodRLS, NomRLS AS NomRLS_Etab, NomRSS AS NomRSS_Etab FROM DONNE_INESSS.tGI_RLS_RTS) AS BD_NomReg_Etab
ON BD_Etab.LGEO_COD_TERRI_RLS=BD_NomReg_Etab.CodRLS

WHERE BD_SMOD.SMOD_DAT_SERV BETWEEN 'debut' AND 'fin'

/* query_SQL_CodeActe.where_SMOD_COD_STA_DECIS (code_stat_decis)
      si code_stat_decis='PAY': AND BD_SMOD.SMOD_COD_STA_DECIS IN ('PAY')
      si code_stat_decis='PPY': AND BD_SMOD.SMOD_COD_STA_DECIS IN ('PPY')
      si les deux: AND BD_SMOD.SMOD_COD_STA_DECIS IN ('PAY','PPY') */

/* query_SQL_CodeActe.where_SMOD_COD_ACTE (CodeActe)
      si CodeActe=NULL: extraction de tous les actes dans la période détude, sinon extraction des actes indiqués dans le vecteur CodeActe */

/* query_I_SMOD_SERV_MD_CM.diagn(diagn)
      à préciser si on veut extraire des actes pour des bénéficiaires ayant un ou plusieurs Dx spécifiques */

/* query_SQL_CodeActe.where_SMOD_COD_SPEC(omni_spec):
        si omni_spec='all': extraction des actes facturés par les omni et les spécialistes
      si omni_spec='omni': AND SMOD_COD_SPEC IS NULL
      si omni_spec='spec': AND SMOD_COD_SPEC IS NOT NULL */

/* query_SQL_CodeActe.where_ETAB_COD_CATG_ETAB_EI(catg_etab),
      si catg_etab='all': extraction des visites effectuées dans toutes les catégories détablissement confondues
      si catg_etab='ambulatoire': AND (ETAB_COD_CATG_ETAB_EI IN ('54X', '55X', '57X', '9X2', '8X5', '4X1','0X1') OR BD_SMOD.SMOD_NO_ETAB_USUEL IS NULL) */

/* Cette exclusion est toujours importante à considérer, car les actes qui représentent des forfaits ne correspondent pas à des consultations, mais, plutôt à des suppléments monétaires associés à une demande de facturation */

AND BD_SMOD.SMOD_COD_ACTE NOT IN (
SELECT DISTINCT NMOD_COD_ACTE
FROM Prod.V_RPERT_COD_ACTE
WHERE NMOD_COD_GRP_ACTE IN (170, 180, 181, 182, 183, 187, 195, 196, 197, 198, 199, 854))
ORDER BY 1,2,3,4;")
          }
          else{
            return("la valeur de l'argument 'Task' est non valide")
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
          result<-result()
          if (length(warnings$data) > 0) {
            print(paste("Warnings: ", warnings$data))
          }
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
          if (input$task == "extraction_SMOD") {
            return("
SELECT

-- Sources des variables :
-- Nom de vue : Prod.I_SMOD_SERV_MD_CM (SMOD)
BD_SMOD.SMOD_NO_INDIV_BEN_BANLS AS ID, --Identifiant du bénéficiaire banalisé
case when EXTRACT(MONTH FROM SMOD_DAT_SERV) between 4 and 12 THEN EXTRACT(YEAR FROM SMOD_DAT_SERV)
ELSE EXTRACT(YEAR FROM SMOD_DAT_SERV)-1 end AS AnFinan, --Année financière
Extract (YEAR From BD_SMOD.SMOD_DAT_SERV) AS AnCivil,  --Année civile
BD_SMOD.SMOD_DAT_SERV AS DateActe,  --Date Acte
BD_SMOD.SMOD_COD_ACTE AS CodeActe,  --Code Acte
BD_SMOD.SMOD_MNT_PAIMT AS CoutActe,  --Coût Acte
BD_SMOD.SMOD_COD_DIAGN_PRIMR AS DxActe,  --Premier diagnostic porté par le professionnel
BD_SMOD.SMOD_NO_ETAB_USUEL AS Num_ETAB_USUEL,  --Ancien numéro de létablissement
BD_SMOD.SMOD_COD_ROLE, --Code des rôles selon lentente
BD_SMOD.SMOD_COD_ENTEN AS CodEntente,  --Code de lentente
BD_SMOD.ETAB_COD_SECT_ACTIV_ETAB AS SecActiv,  --secteur dactivité

-- Nom de vue : PROD.V_FICH_ID_BEN_CM (FIPA)

BD_Caract.BENF_COD_SEXE AS Sexe,  --Sexe du bénéficiaire
BD_Caract.BENF_DAT_NAISS AS DatNais,  --Date de naissance
BD_Caract.BENF_DAT_DECES AS DatDeces,  --Date de décès
Cast(((BD_SMOD.SMOD_DAT_SERV) - BD_Caract.BENF_DAT_NAISS) / 365.25 AS BIGINT) AS Age,  --Age calculé à la date de service

-- Nom de vue : PROD.D_DISP_SPEC_CM (FIP)

BD_SMOD.DISP_NO_SEQ_DISP_BANLS AS NumDispSp,  --Numéro du dispensateur
BD_SMOD.SMOD_COD_SPEC AS SPDisp_smod, --Spécialité du dispensateur (SMOD)
BD_Disp.DISP_COD_SPEC AS SPDisp_fip,  --Spécialité du dispensateur (FIP)
BD_SMOD.DISP_NO_SEQ_DISP_REFNT_BANLS AS NumDispRef,  --Numéro du médecin référent
BD_DispR.DISP_COD_SPEC AS SPDispRefs,  --Spécialité du médecin référent

-- Nom de vue : prod.V_ETAB_USUEL, V_ETAB_USUEL_DERN, V_COD_CATG_ETAB_EI

BD_Etab.ETAB_NO_ETAB AS NO_ETAB,
BD_Etab.ETAB_COD_CATG_ETAB_EI AS Cat_Etab, --(Urgence, Clinique externe, ...)

-- Nom de vue : RES_SSS.V_NOM_ETAB_DERN_TYP_NOM

BD_NomEtab.ETAB_NOM_ETAB AS NOM_ETAB,

-- Nom de vue (Info BEN) : PROD.I_BENF_ADR_CM (FIPA), tGI_RLS_RTS

BD_Adr.LGEO_COD_RSS AS RSS_Benf, --Région socio-sanitaire Bénéf
CASE
WHEN BD_Adr.LGEO_COD_RSS=10 THEN '1001'
WHEN BD_Adr.LGEO_COD_RSS=17 THEN '1701'
WHEN BD_Adr.LGEO_COD_RSS=18 THEN '1801' ELSE BD_Adr.LGEO_COD_TERRI_RLS END AS RLS_Benef, --Code RLS Bénéf
BD_NomReg_Ben.NomRSS_Benef,
BD_NomReg_Ben.NomRLS_Benef,

-- Nom de vue (Info ETAB) : V_DECOU_GEO_POSTL_DERN_CP, V_ETAB_USUEL, V_ETAB_USUEL_DERN, tGI_RLS_RTS

BD_Etab.LGEO_COD_RSS AS RSS_Etab,
BD_Etab.LGEO_COD_TERRI_RLS AS RLS_Etab,
BD_NomReg_Etab.NomRSS_Etab,
BD_NomReg_Etab.NomRLS_Etab

FROM Prod.I_SMOD_SERV_MD_CM AS BD_SMOD
LEFT JOIN Prod.V_FICH_ID_BEN_CM AS BD_Caract
ON BD_SMOD.SMOD_NO_INDIV_BEN_BANLS = BD_Caract.BENF_NO_INDIV_BEN_BANLS

/* cette requête cible la spécialité principale du disp dans fip au moment du service */
LEFT JOIN Prod.D_DISP_SPEC_CM AS BD_Disp
ON BD_SMOD.DISP_NO_SEQ_DISP_BANLS = BD_Disp.DISP_NO_SEQ_DISP_BANLS
AND BD_Disp.DISP_COD_NIV_SPEC = 1
AND BD_SMOD.SMOD_DAT_SERV BETWEEN BD_Disp.DISP_DD_SPEC_DISP AND BD_Disp.DISP_DF_SPEC_DISP

/* Cette requête est incluse dans la version actuelle, mais elle pourrait être optionnelle, car la spécialité du médecin référent est rarement demandée */
LEFT JOIN Prod.D_DISP_SPEC_CM AS BD_DispR
ON BD_SMOD.DISP_NO_SEQ_DISP_REFNT_BANLS = BD_DispR.DISP_NO_SEQ_DISP_BANLS
AND BD_DispR.DISP_COD_NIV_SPEC = 1
AND BD_SMOD.SMOD_DAT_SERV BETWEEN BD_DispR.DISP_DD_SPEC_DISP AND BD_DispR.DISP_DF_SPEC_DISP

/* Découpage géographique du bénéficiaire : codes RSS et RLS:
Plusieurs méthode pour sélectionner l'adresse du bénéficiaire durant la période sélectionnée:
1) cibler la première adresse de la période
2) cibler la dernière adresse de la période
2) cibler l'adresse dans laquelle le BEN a résidé le plus longtemps durant la période
3) cibler l'adresse à une date fixe (p. ex. 31 mars, 1 juillet, mi-année) */

LEFT JOIN (
SELECT
BENF_NO_INDIV_BEN_BANLS,
BENF_DD_ADR_BEN,
BENF_DF_ADR_BEN,
LGEO_COD_RSS,
LGEO_COD_TERRI_RLS

FROM(
SELECT
BENF_NO_INDIV_BEN_BANLS,
BENF_DD_ADR_BEN,
BENF_DF_ADR_BEN,
LGEO_COD_RSS,
LGEO_COD_TERRI_RLS,

CASE
WHEN (BENF_DD_ADR_BEN <= 'debut' AND BENF_DF_ADR_BEN >= 'fin') THEN DATE 'fin' - DATE 'debut' + 1
WHEN (BENF_DD_ADR_BEN >= 'debut' AND BENF_DF_ADR_BEN >= 'fin') THEN DATE 'fin' - BENF_DD_ADR_BEN
WHEN (BENF_DD_ADR_BEN <= 'debut' AND BENF_DF_ADR_BEN <= 'fin') THEN BENF_DF_ADR_BEN - DATE 'debut'
WHEN (BENF_DD_ADR_BEN >= 'debut' AND BENF_DF_ADR_BEN <= 'fin') THEN BENF_DF_ADR_BEN - BENF_DD_ADR_BEN
ELSE 0
END AS days_count,
ROW_NUMBER() OVER (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY days_count DESC) AS row_num /* cibler l'adresse la plus longue */

FROM Prod.I_BENF_ADR_CM
WHERE BENF_IND_ADR_HQ IN ('N') AND BENF_COD_TYP_ADR IN ('R')
AND (BENF_DD_ADR_BEN<='fin' AND BENF_DF_ADR_BEN>='debut')

AND 'date_adr' between BENF_DD_ADR_BEN AND BENF_DF_ADR_BEN  /* cibler une date fixe */
QUALIFY Row_Number()Over (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY BENF_DD_ADR_BEN DESC)=1 /* cibler la dernière adresse */
QUALIFY Row_Number()Over (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY BENF_DD_ADR_BEN ASC)=1 /* cibler la première adresse */
)AS A
WHERE A.row_num=1

) AS BD_Adr
ON BD_SMOD.SMOD_NO_INDIV_BEN_BANLS=BD_Adr.BENF_NO_INDIV_BEN_BANLS

/* On ajoute ici les noms des codes RSS et RLS du bénéficiaire */

LEFT JOIN (
SELECT
CodRLS,
NomRLS AS NomRLS_Benef,
NomRSS AS NomRSS_Benef
FROM DONNE_INESSS.tGI_RLS_RTS) AS BD_NomReg_Ben
ON BD_Adr.LGEO_COD_TERRI_RLS=BD_NomReg_Ben.CodRLS

/* Découpage géographique de létablissement : numéro unique, catégorie, nom de létab, code et nom RSS, code et nom RLS */

/* On ajoute ici le numéro détablissement (unique : ETAB_NO_ETAB), le code postal de létablissement et la catégorie de létablissement */

LEFT JOIN (
SELECT BD_Etab.ETAB_NO_ETAB_USUEL, BD_Etab.ETAB_NO_ETAB, BD_Etab.ETAB_COD_POSTL,
BD_Decoup_Etab.LGEO_COD_RSS, BD_Decoup_Etab.LGEO_COD_TERRI_RLS,
BD_Catg_etab.ETAB_COD_CATG_ETAB_EI, BD_NomCatg_etab.ETAB_NOM_CATG_ETAB_EI
FROM V_ETAB_USUEL_DERN AS BD_Etab
LEFT JOIN Prod.V_DECOU_GEO_POSTL_DERN_CP AS BD_Decoup_Etab ON BD_Etab.ETAB_COD_POSTL=BD_Decoup_Etab.LGEO_COD_POSTL
LEFT JOIN (SELECT DISTINCT ETAB_NO_ETAB, ETAB_COD_CATG_ETAB_EI FROM V_ETAB_USUEL
QUALIFY Row_Number()Over (PARTITION BY ETAB_NO_ETAB ORDER BY ETAB_DD_ETAB_USUEL DESC)=1) AS BD_Catg_etab ON BD_Etab.ETAB_NO_ETAB=BD_Catg_etab.ETAB_NO_ETAB
LEFT JOIN Prod.V_COD_CATG_ETAB_EI AS BD_NomCatg_etab ON BD_Catg_etab.ETAB_COD_CATG_ETAB_EI=BD_NomCatg_etab.ETAB_COD_CATG_ETAB_EI
) AS BD_Etab
ON BD_SMOD.SMOD_NO_ETAB_USUEL=BD_Etab.ETAB_NO_ETAB_USUEL

/* On ajoute le NOM de létablissement */

LEFT JOIN (SELECT ETAB_NO_ETAB, ETAB_NOM_ETAB FROM RES_SSS.V_NOM_ETAB_DERN_TYP_NOM) AS BD_NomEtab
ON BD_Etab.ETAB_NO_ETAB = BD_NomEtab.ETAB_NO_ETAB

/* On ajoute ici les noms des codes RSS et RLS de létablissement : ceci serait optionnel si la requête est trop lourde */

LEFT JOIN (SELECT CodRLS, NomRLS AS NomRLS_Etab, NomRSS AS NomRSS_Etab FROM DONNE_INESSS.tGI_RLS_RTS) AS BD_NomReg_Etab
ON BD_Etab.LGEO_COD_TERRI_RLS=BD_NomReg_Etab.CodRLS

WHERE BD_SMOD.SMOD_DAT_SERV BETWEEN 'debut' AND 'fin'

/* query_SQL_CodeActe.where_SMOD_COD_STA_DECIS (code_stat_decis)
      si code_stat_decis='PAY': AND BD_SMOD.SMOD_COD_STA_DECIS IN ('PAY')
      si code_stat_decis='PPY': AND BD_SMOD.SMOD_COD_STA_DECIS IN ('PPY')
      si les deux: AND BD_SMOD.SMOD_COD_STA_DECIS IN ('PAY','PPY') */

/* query_SQL_CodeActe.where_SMOD_COD_ACTE (CodeActe)
      si CodeActe=NULL: extraction de tous les actes dans la période détude, sinon extraction des actes indiqués dans le vecteur CodeActe */

/* query_I_SMOD_SERV_MD_CM.diagn(diagn)
      à préciser si on veut extraire des actes pour des bénéficiaires ayant un ou plusieurs Dx spécifiques */

/* query_SQL_CodeActe.where_SMOD_COD_SPEC(omni_spec):
        si omni_spec='all': extraction des actes facturés par les omni et les spécialistes
      si omni_spec='omni': AND SMOD_COD_SPEC IS NULL
      si omni_spec='spec': AND SMOD_COD_SPEC IS NOT NULL */

/* query_SQL_CodeActe.where_ETAB_COD_CATG_ETAB_EI(catg_etab),
      si catg_etab='all': extraction des visites effectuées dans toutes les catégories détablissement confondues
      si catg_etab='ambulatoire': AND (ETAB_COD_CATG_ETAB_EI IN ('54X', '55X', '57X', '9X2', '8X5', '4X1','0X1') OR BD_SMOD.SMOD_NO_ETAB_USUEL IS NULL) */

/* Cette exclusion est toujours importante à considérer, car les actes qui représentent des forfaits ne correspondent pas à des consultations, mais, plutôt à des suppléments monétaires associés à une demande de facturation */

AND BD_SMOD.SMOD_COD_ACTE NOT IN (
SELECT DISTINCT NMOD_COD_ACTE
FROM Prod.V_RPERT_COD_ACTE
WHERE NMOD_COD_GRP_ACTE IN (170, 180, 181, 182, 183, 187, 195, 196, 197, 198, 199, 854))
ORDER BY 1,2,3,4")
          }
          else if (input$task == "projet_facturation_acte"){
            return("
SELECT

-- Sources des variables :
-- Nom de vue : Prod.I_SMOD_SERV_MD_CM (SMOD)
BD_SMOD.SMOD_NO_INDIV_BEN_BANLS AS ID, --Identifiant du bénéficiaire banalisé
case when EXTRACT(MONTH FROM SMOD_DAT_SERV) between 4 and 12 THEN EXTRACT(YEAR FROM SMOD_DAT_SERV)
ELSE EXTRACT(YEAR FROM SMOD_DAT_SERV)-1 end AS AnFinan, --Année financière
Extract (YEAR From BD_SMOD.SMOD_DAT_SERV) AS AnCivil,  --Année civile
BD_SMOD.SMOD_DAT_SERV AS DateActe,  --Date Acte
BD_SMOD.SMOD_COD_ACTE AS CodeActe,  --Code Acte
BD_SMOD.SMOD_MNT_PAIMT AS CoutActe,  --Coût Acte
BD_SMOD.SMOD_COD_DIAGN_PRIMR AS DxActe,  --Premier diagnostic porté par le professionnel
BD_SMOD.SMOD_NO_ETAB_USUEL AS Num_ETAB_USUEL,  --Ancien numéro de létablissement
BD_SMOD.SMOD_COD_ROLE, --Code des rôles selon lentente
BD_SMOD.SMOD_COD_ENTEN AS CodEntente,  --Code de lentente
BD_SMOD.ETAB_COD_SECT_ACTIV_ETAB AS SecActiv,  --secteur dactivité

-- Nom de vue : PROD.V_FICH_ID_BEN_CM (FIPA)

BD_Caract.BENF_COD_SEXE AS Sexe,  --Sexe du bénéficiaire
BD_Caract.BENF_DAT_NAISS AS DatNais,  --Date de naissance
BD_Caract.BENF_DAT_DECES AS DatDeces,  --Date de décès
Cast(((BD_SMOD.SMOD_DAT_SERV) - BD_Caract.BENF_DAT_NAISS) / 365.25 AS BIGINT) AS Age,  --Age calculé à la date de service

-- Nom de vue : PROD.D_DISP_SPEC_CM (FIP)

BD_SMOD.DISP_NO_SEQ_DISP_BANLS AS NumDispSp,  --Numéro du dispensateur
BD_SMOD.SMOD_COD_SPEC AS SPDisp_smod, --Spécialité du dispensateur (SMOD)
BD_Disp.DISP_COD_SPEC AS SPDisp_fip,  --Spécialité du dispensateur (FIP)
BD_SMOD.DISP_NO_SEQ_DISP_REFNT_BANLS AS NumDispRef,  --Numéro du médecin référent
BD_DispR.DISP_COD_SPEC AS SPDispRefs,  --Spécialité du médecin référent

-- Nom de vue : prod.V_ETAB_USUEL, V_ETAB_USUEL_DERN, V_COD_CATG_ETAB_EI

BD_Etab.ETAB_NO_ETAB AS NO_ETAB,
BD_Etab.ETAB_COD_CATG_ETAB_EI AS Cat_Etab, --(Urgence, Clinique externe, ...)

-- Nom de vue : RES_SSS.V_NOM_ETAB_DERN_TYP_NOM

BD_NomEtab.ETAB_NOM_ETAB AS NOM_ETAB,

-- Nom de vue (Info BEN) : PROD.I_BENF_ADR_CM (FIPA), tGI_RLS_RTS

BD_Adr.LGEO_COD_RSS AS RSS_Benf, --Région socio-sanitaire Bénéf
CASE
WHEN BD_Adr.LGEO_COD_RSS=10 THEN '1001'
WHEN BD_Adr.LGEO_COD_RSS=17 THEN '1701'
WHEN BD_Adr.LGEO_COD_RSS=18 THEN '1801' ELSE BD_Adr.LGEO_COD_TERRI_RLS END AS RLS_Benef, --Code RLS Bénéf
BD_NomReg_Ben.NomRSS_Benef,
BD_NomReg_Ben.NomRLS_Benef,

-- Nom de vue (Info ETAB) : V_DECOU_GEO_POSTL_DERN_CP, V_ETAB_USUEL, V_ETAB_USUEL_DERN, tGI_RLS_RTS

BD_Etab.LGEO_COD_RSS AS RSS_Etab,
BD_Etab.LGEO_COD_TERRI_RLS AS RLS_Etab,
BD_NomReg_Etab.NomRSS_Etab,
BD_NomReg_Etab.NomRLS_Etab

FROM Prod.I_SMOD_SERV_MD_CM AS BD_SMOD
LEFT JOIN Prod.V_FICH_ID_BEN_CM AS BD_Caract
ON BD_SMOD.SMOD_NO_INDIV_BEN_BANLS = BD_Caract.BENF_NO_INDIV_BEN_BANLS

/* cette requête cible la spécialité principale du disp dans fip au moment du service */
LEFT JOIN Prod.D_DISP_SPEC_CM AS BD_Disp
ON BD_SMOD.DISP_NO_SEQ_DISP_BANLS = BD_Disp.DISP_NO_SEQ_DISP_BANLS
AND BD_Disp.DISP_COD_NIV_SPEC = 1
AND BD_SMOD.SMOD_DAT_SERV BETWEEN BD_Disp.DISP_DD_SPEC_DISP AND BD_Disp.DISP_DF_SPEC_DISP

/* Cette requête est incluse dans la version actuelle, mais elle pourrait être optionnelle, car la spécialité du médecin référent est rarement demandée */
LEFT JOIN Prod.D_DISP_SPEC_CM AS BD_DispR
ON BD_SMOD.DISP_NO_SEQ_DISP_REFNT_BANLS = BD_DispR.DISP_NO_SEQ_DISP_BANLS
AND BD_DispR.DISP_COD_NIV_SPEC = 1
AND BD_SMOD.SMOD_DAT_SERV BETWEEN BD_DispR.DISP_DD_SPEC_DISP AND BD_DispR.DISP_DF_SPEC_DISP

/* Découpage géographique du bénéficiaire : codes RSS et RLS:
Plusieurs méthode pour sélectionner l'adresse du bénéficiaire durant la période sélectionnée:
1) cibler la première adresse de la période
2) cibler la dernière adresse de la période
2) cibler l'adresse dans laquelle le BEN a résidé le plus longtemps durant la période
3) cibler l'adresse à une date fixe (p. ex. 31 mars, 1 juillet, mi-année) */

LEFT JOIN (
SELECT
BENF_NO_INDIV_BEN_BANLS,
BENF_DD_ADR_BEN,
BENF_DF_ADR_BEN,
LGEO_COD_RSS,
LGEO_COD_TERRI_RLS

FROM(
SELECT
BENF_NO_INDIV_BEN_BANLS,
BENF_DD_ADR_BEN,
BENF_DF_ADR_BEN,
LGEO_COD_RSS,
LGEO_COD_TERRI_RLS,

CASE
WHEN (BENF_DD_ADR_BEN <= 'debut' AND BENF_DF_ADR_BEN >= 'fin') THEN DATE 'fin' - DATE 'debut' + 1
WHEN (BENF_DD_ADR_BEN >= 'debut' AND BENF_DF_ADR_BEN >= 'fin') THEN DATE 'fin' - BENF_DD_ADR_BEN
WHEN (BENF_DD_ADR_BEN <= 'debut' AND BENF_DF_ADR_BEN <= 'fin') THEN BENF_DF_ADR_BEN - DATE 'debut'
WHEN (BENF_DD_ADR_BEN >= 'debut' AND BENF_DF_ADR_BEN <= 'fin') THEN BENF_DF_ADR_BEN - BENF_DD_ADR_BEN
ELSE 0
END AS days_count,
ROW_NUMBER() OVER (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY days_count DESC) AS row_num /* cibler l'adresse la plus longue */

FROM Prod.I_BENF_ADR_CM
WHERE BENF_IND_ADR_HQ IN ('N') AND BENF_COD_TYP_ADR IN ('R')
AND (BENF_DD_ADR_BEN<='fin' AND BENF_DF_ADR_BEN>='debut')

AND 'date_adr' between BENF_DD_ADR_BEN AND BENF_DF_ADR_BEN  /* cibler une date fixe */
QUALIFY Row_Number()Over (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY BENF_DD_ADR_BEN DESC)=1 /* cibler la dernière adresse */
QUALIFY Row_Number()Over (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY BENF_DD_ADR_BEN ASC)=1 /* cibler la première adresse */
)AS A
WHERE A.row_num=1

) AS BD_Adr
ON BD_SMOD.SMOD_NO_INDIV_BEN_BANLS=BD_Adr.BENF_NO_INDIV_BEN_BANLS

/* On ajoute ici les noms des codes RSS et RLS du bénéficiaire */

LEFT JOIN (
SELECT
CodRLS,
NomRLS AS NomRLS_Benef,
NomRSS AS NomRSS_Benef
FROM DONNE_INESSS.tGI_RLS_RTS) AS BD_NomReg_Ben
ON BD_Adr.LGEO_COD_TERRI_RLS=BD_NomReg_Ben.CodRLS

/* Découpage géographique de létablissement : numéro unique, catégorie, nom de létab, code et nom RSS, code et nom RLS */

/* On ajoute ici le numéro détablissement (unique : ETAB_NO_ETAB), le code postal de létablissement et la catégorie de létablissement */

LEFT JOIN (
SELECT BD_Etab.ETAB_NO_ETAB_USUEL, BD_Etab.ETAB_NO_ETAB, BD_Etab.ETAB_COD_POSTL,
BD_Decoup_Etab.LGEO_COD_RSS, BD_Decoup_Etab.LGEO_COD_TERRI_RLS,
BD_Catg_etab.ETAB_COD_CATG_ETAB_EI, BD_NomCatg_etab.ETAB_NOM_CATG_ETAB_EI
FROM V_ETAB_USUEL_DERN AS BD_Etab
LEFT JOIN Prod.V_DECOU_GEO_POSTL_DERN_CP AS BD_Decoup_Etab ON BD_Etab.ETAB_COD_POSTL=BD_Decoup_Etab.LGEO_COD_POSTL
LEFT JOIN (SELECT DISTINCT ETAB_NO_ETAB, ETAB_COD_CATG_ETAB_EI FROM V_ETAB_USUEL
QUALIFY Row_Number()Over (PARTITION BY ETAB_NO_ETAB ORDER BY ETAB_DD_ETAB_USUEL DESC)=1) AS BD_Catg_etab ON BD_Etab.ETAB_NO_ETAB=BD_Catg_etab.ETAB_NO_ETAB
LEFT JOIN Prod.V_COD_CATG_ETAB_EI AS BD_NomCatg_etab ON BD_Catg_etab.ETAB_COD_CATG_ETAB_EI=BD_NomCatg_etab.ETAB_COD_CATG_ETAB_EI
) AS BD_Etab
ON BD_SMOD.SMOD_NO_ETAB_USUEL=BD_Etab.ETAB_NO_ETAB_USUEL

/* On ajoute le NOM de létablissement */

LEFT JOIN (SELECT ETAB_NO_ETAB, ETAB_NOM_ETAB FROM RES_SSS.V_NOM_ETAB_DERN_TYP_NOM) AS BD_NomEtab
ON BD_Etab.ETAB_NO_ETAB = BD_NomEtab.ETAB_NO_ETAB

/* On ajoute ici les noms des codes RSS et RLS de létablissement : ceci serait optionnel si la requête est trop lourde */

LEFT JOIN (SELECT CodRLS, NomRLS AS NomRLS_Etab, NomRSS AS NomRSS_Etab FROM DONNE_INESSS.tGI_RLS_RTS) AS BD_NomReg_Etab
ON BD_Etab.LGEO_COD_TERRI_RLS=BD_NomReg_Etab.CodRLS

WHERE BD_SMOD.SMOD_DAT_SERV BETWEEN 'debut' AND 'fin'

/* query_SQL_CodeActe.where_SMOD_COD_STA_DECIS (code_stat_decis)
      si code_stat_decis='PAY': AND BD_SMOD.SMOD_COD_STA_DECIS IN ('PAY')
      si code_stat_decis='PPY': AND BD_SMOD.SMOD_COD_STA_DECIS IN ('PPY')
      si les deux: AND BD_SMOD.SMOD_COD_STA_DECIS IN ('PAY','PPY') */

/* query_SQL_CodeActe.where_SMOD_COD_ACTE (CodeActe)
      si CodeActe=NULL: extraction de tous les actes dans la période détude, sinon extraction des actes indiqués dans le vecteur CodeActe */

/* query_I_SMOD_SERV_MD_CM.diagn(diagn)
      à préciser si on veut extraire des actes pour des bénéficiaires ayant un ou plusieurs Dx spécifiques */

/* query_SQL_CodeActe.where_SMOD_COD_SPEC(omni_spec):
        si omni_spec='all': extraction des actes facturés par les omni et les spécialistes
      si omni_spec='omni': AND SMOD_COD_SPEC IS NULL
      si omni_spec='spec': AND SMOD_COD_SPEC IS NOT NULL */

/* query_SQL_CodeActe.where_ETAB_COD_CATG_ETAB_EI(catg_etab),
      si catg_etab='all': extraction des visites effectuées dans toutes les catégories détablissement confondues
      si catg_etab='ambulatoire': AND (ETAB_COD_CATG_ETAB_EI IN ('54X', '55X', '57X', '9X2', '8X5', '4X1','0X1') OR BD_SMOD.SMOD_NO_ETAB_USUEL IS NULL) */

/* Cette exclusion est toujours importante à considérer, car les actes qui représentent des forfaits ne correspondent pas à des consultations, mais, plutôt à des suppléments monétaires associés à une demande de facturation */

AND BD_SMOD.SMOD_COD_ACTE NOT IN (
SELECT DISTINCT NMOD_COD_ACTE
FROM Prod.V_RPERT_COD_ACTE
WHERE NMOD_COD_GRP_ACTE IN (170, 180, 181, 182, 183, 187, 195, 196, 197, 198, 199, 854))
ORDER BY 1,2,3,4")
          }
          else{
            return("la valeur de l'argument 'Task' est non valide")
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
          result()
          if (length(warnings$data) > 0) {
            print(paste("Warnings: ", warnings$data))
          }
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



