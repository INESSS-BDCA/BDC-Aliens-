#' @title Code SQL: query_I_SMOD_SERV_MD_CM_AG `(SMOD)`
#'
#' @description  `query_I_SMOD_SERV_MD_CM_AG` est une fonction multi-tâches. Elle utilise principalement la vue `I_SMOD_SERV_MD_CM` jumlée avec d'autres vues pour générer un code SQL qui permet :\cr
#' 1) d'extraire les variables pertinantes de la base de données SMOD, jumlées avec les caractéristiques de dispensateur, du bénéficiaire et de l'établissement de soins ainsi que le découpage géographique.\cr
#' 2) d'extraire les diagnostics pour créer par exemple une cohorte.\cr
#'
#' @param query indique si on veut effectuer 1) une extraction brute des vaiables pertinantes dans SMOD jumlées avec d'autres bases de données "extraction_SMOD",
#' 2) une extraction des diagnostics "extraction_Dx".
#' @param debut_periode Date de début de la période d'étude au format `"AAAA-MM-JJ"`.
#' @param fin_periode Date de fin de la période d'étude au format `"AAAA-MM-JJ"`.
#' @param debut_cohort Date de début de la période pour la création d'une cohorte au format `"AAAA-MM-JJ"`.
#' @param fin_cohort Date de fin de la période pour la création d'une cohorte au format `"AAAA-MM-JJ"`.
#' @param diagn `vecteur` indique les codes de diagnostics *CIM9* et/ou *CIM10* à extraire.
#' @param CodeActe `vecteur` indique les codes d'acte d'interêt.
#' @param omni_spec peut prendre la valeur `omni`(extraction des actes facturés par les omni: SMOD_COD_SPEC IS NULL),
#' `spec` (extraction des actes facturés par les spec: SMOD_COD_SPEC IS NOT NULL) ou `all` (pas de spécification).
#' @param catg_etab peut prendre la valeur `ambulatoire` (pour les actes facturés en ambulatoire) ou `all` (pas de spécification).
#' @param code_stat_decis peut prendre la valeur `c('PAY','PPY')`, `PAY`, ou `PPY`
#' @param benef_adr permet de définir la méthode d’identification de l’adresse de bénéficiaire. Peut prendre la valeur `c("date_fixe","dernière_adresse","première_adresse","long_adresse","date_acte")`
#' @param date_adr si `benef_adr=“date_fixe”`, date_adr permet de choisir une date fixe pour identifier l’adresse de bénéficiaire.

#' @return chaîne de caractères, code SQL.
#' @keywords internal
#' @encoding UTF-8
#' @export
#'
#' @examples
#' \dontrun{
#' DT<-data.table::as.data.table(odbc::dbGetQuery(conn=SQL_connexion("ms069a"),
#' statement=query_I_SMOD_SERV_MD_CM_AG (
#' query= c("extraction_SMOD","extraction_SMOD_combin","extraction_Dx"),
#' debut_periode="2021-04-01",
#' fin_periode="2022-03-31",
#' debut_cohort=NULL,
#' fin_cohort=NULL,
#' CodeActe=c('07122', '07237', '07800', '07089', '0780'),
#' code_stat_decis=c("PAY","PPY"),
#' benef_adr=c("date_fixe","dernière_adresse","première_adresse","long_adresse"),
#' date_adr="2021-07-01" # à préciser lorsque l'argument benef_adr="date_fixe")))
#'}

query_I_SMOD_SERV_MD_CM_AG <- function(query,debut_periode, fin_periode, debut_cohort,fin_cohort,diagn,CodeActe,omni_spec,catg_etab,code_stat_decis,benef_adr,date_adr) {
  switch (query,
          "extraction_SMOD"={
            return(paste0(
              "
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
BD_SMOD.SMOD_NO_ETAB_USUEL AS Num_ETAB_USUEL,  --Ancien numéro de l'établissement
BD_SMOD.SMOD_COD_ROLE, --Code des rôles selon l'entente
BD_SMOD.SMOD_COD_ENTEN AS SMOD_COD_ENTEN,  --Code de l'entente
BD_SMOD.ETAB_COD_SECT_ACTIV_ETAB AS ETAB_COD_SECT_ACTIV_ETAB,  --secteur d'activité

-- Nom de vue : PROD.V_FICH_ID_BEN_CM (FIPA)

BD_Caract.BENF_COD_SEXE AS Sexe,  --Sexe du bénéficiaire
BD_Caract.BENF_DAT_NAISS AS DatNais,  --Date de naissance
BD_Caract.BENF_DAT_DECES AS DatDeces,  --Date de décès
Cast(((BD_SMOD.SMOD_DAT_SERV) - BD_Caract.BENF_DAT_NAISS) / 365.25 AS BIGINT) AS Age,  --Age calculé à la date de service

-- Nom de vue : PROD.D_DISP_SPEC_CM (FIP)

BD_SMOD.DISP_NO_SEQ_DISP_BANLS AS NumDispSp,  --Numéro du dispensateur
BD_SMOD.SMOD_COD_SPEC AS SMOD_COD_SPEC, --Spécialité du dispensateur (SMOD)
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

/* cette requête cible la spécialité principale du disp dans fip au moment du service*/
LEFT JOIN Prod.D_DISP_SPEC_CM AS BD_Disp
ON BD_SMOD.DISP_NO_SEQ_DISP_BANLS = BD_Disp.DISP_NO_SEQ_DISP_BANLS
AND BD_Disp.DISP_COD_NIV_SPEC = 1
AND BD_SMOD.SMOD_DAT_SERV BETWEEN BD_Disp.DISP_DD_SPEC_DISP AND BD_Disp.DISP_DF_SPEC_DISP

/* Cette requête est incluse dans la version actuelle, mais elle pourrait être optionnelle, car la spécialité du médecin référent est rarement demandée */
LEFT JOIN Prod.D_DISP_SPEC_CM AS BD_DispR
ON BD_SMOD.DISP_NO_SEQ_DISP_REFNT_BANLS = BD_DispR.DISP_NO_SEQ_DISP_BANLS
AND BD_DispR.DISP_COD_NIV_SPEC = 1
AND BD_SMOD.SMOD_DAT_SERV BETWEEN BD_DispR.DISP_DD_SPEC_DISP AND BD_DispR.DISP_DF_SPEC_DISP

/**********************************************************
Découpage géographique du bénéficiaire : codes RSS et RLS
***********************************************************/
/*Plusieurs méthode pour sélectionner l'adresse du bénéficiaire durant la période sélectionnée:
1) cibler la première adresse de la période
2) cibler l'adresse dans laquelle le BEN a résidé le plus longtemps durant la période
3) cibler l'adresse à une date fixe (p. ex. 31 mars, 1 juillet, mi-année)*/\n",
query_I_SMOD_SERV_MD_CM.where_I_BENF_ADR_CM(benef_adr,debut_periode,fin_periode,date_adr),

"/* On ajoute ici les NOMS des codes RSS et RLS du bénéficiaire : ceci serait optionnel si la requête est trop lourde */

LEFT JOIN (
SELECT
CodRLS,
NomRLS AS NomRLS_Benef,
NomRSS AS NomRSS_Benef
FROM DONNE_INESSS.tGI_RLS_RTS) AS BD_NomReg_Ben
ON BD_Adr.LGEO_COD_TERRI_RLS=BD_NomReg_Ben.CodRLS

/*********************************************************************************************************************
Découpage géographique de l'établissement : numéro unique, catégorie, nom de l'étab, code et nom RSS, code et nom RLS
**********************************************************************************************************************/
/* On ajoute ici le numéro d'établissement (unique : ETAB_NO_ETAB), le code postal de l'établissement et la catégorie de l'établissement */

LEFT JOIN (
SELECT
BD_Etab.ETAB_NO_ETAB_USUEL,
BD_Etab.ETAB_NO_ETAB,
BD_Etab.ETAB_COD_POSTL,
BD_Decoup_Etab.LGEO_COD_RSS,
BD_Decoup_Etab.LGEO_COD_TERRI_RLS,
BD_Catg_etab.ETAB_COD_CATG_ETAB_EI,
BD_NomCatg_etab.ETAB_NOM_CATG_ETAB_EI
FROM V_ETAB_USUEL_DERN AS BD_Etab
LEFT JOIN Prod.V_DECOU_GEO_POSTL_DERN_CP AS BD_Decoup_Etab ON BD_Etab.ETAB_COD_POSTL=BD_Decoup_Etab.LGEO_COD_POSTL
LEFT JOIN (SELECT DISTINCT
ETAB_NO_ETAB,
ETAB_COD_CATG_ETAB_EI FROM V_ETAB_USUEL
QUALIFY Row_Number()Over (PARTITION BY ETAB_NO_ETAB ORDER BY ETAB_DD_ETAB_USUEL DESC)=1) AS BD_Catg_etab ON BD_Etab.ETAB_NO_ETAB=BD_Catg_etab.ETAB_NO_ETAB
LEFT JOIN Prod.V_COD_CATG_ETAB_EI AS BD_NomCatg_etab ON BD_Catg_etab.ETAB_COD_CATG_ETAB_EI=BD_NomCatg_etab.ETAB_COD_CATG_ETAB_EI
) AS BD_Etab
ON BD_SMOD.SMOD_NO_ETAB_USUEL=BD_Etab.ETAB_NO_ETAB_USUEL

/* On ajoute le NOM de l'établissement */

LEFT JOIN (SELECT ETAB_NO_ETAB, ETAB_NOM_ETAB FROM RES_SSS.V_NOM_ETAB_DERN_TYP_NOM) AS BD_NomEtab
ON BD_Etab.ETAB_NO_ETAB = BD_NomEtab.ETAB_NO_ETAB

/* On ajoute ici les NOMS des codes RSS et RLS de l'établissement : ceci serait optionnel si la requête est trop lourde */

LEFT JOIN (SELECT CodRLS, NomRLS AS NomRLS_Etab, NomRSS AS NomRSS_Etab FROM DONNE_INESSS.tGI_RLS_RTS) AS BD_NomReg_Etab
ON BD_Etab.LGEO_COD_TERRI_RLS=BD_NomReg_Etab.CodRLS

WHERE BD_SMOD.SMOD_DAT_SERV BETWEEN '",debut_periode,"' AND '",fin_periode,"'\n",
              query_I_SMOD_SERV_MD_CM.where_SMOD_COD_STA_DECIS (code_stat_decis),
              query_I_SMOD_SERV_MD_CM.where_SMOD_COD_ACTE (CodeActe),
              query_I_SMOD_SERV_MD_CM.diagn(diagn),
              query_I_SMOD_SERV_MD_CM.where_SMOD_COD_SPEC(omni_spec),
              query_I_SMOD_SERV_MD_CM.where_ETAB_COD_CATG_ETAB_EI(catg_etab),
              "/*Cette exclusion est toujours importante à considérer, car les actes qui représentent des forfaits ne correspondent pas à des consultations,
mais, plutôt à des suppléments monétaires associés à une demande de facturation */
AND BD_SMOD.SMOD_COD_ACTE NOT IN (
SELECT DISTINCT NMOD_COD_ACTE
FROM Prod.V_RPERT_COD_ACTE
WHERE NMOD_COD_GRP_ACTE IN (170, 180, 181, 182, 183, 187, 195, 196, 197, 198, 199, 854))

ORDER BY 1,2,3,4;"
            ))},
          "extraction_Dx"={
            return(paste0(
              "SELECT
              ID,
              DATE_DX
              FROM (
              SELECT
              DISTINCT\n",
              "BD_SMOD.SMOD_NO_INDIV_BEN_BANLS as ID,\n",
              "BD_SMOD.SMOD_DAT_SERV as DATE_DX,\n",
              "BD_SMOD.SMOD_COD_DIAGN_PRIMR AS DX\n",
              "FROM PROD.I_SMOD_SERV_MD_CM AS BD_SMOD\n",
              "where BD_SMOD.SMOD_DAT_SERV between '",debut_cohort,"' and '",fin_cohort,"'\n",
              query_I_SMOD_SERV_MD_CM.where_SMOD_COD_STA_DECIS (code_stat_decis),
              query_I_SMOD_SERV_MD_CM.diagn(diagn),
              ") AS A order by 1,2;"
            )) }
          )
}

#' @title query_I_SMOD_SERV_MD_CM.where_SMOD_COD_SPEC
#' @description Section `where` du code SQL où on demande les actes facturés par les omipraticiens et/ou spécialistes.
#' @encoding UTF-8
#' @keywords internal
query_I_SMOD_SERV_MD_CM.where_SMOD_COD_SPEC <- function(omni_spec) {
  switch(omni_spec,
         "all" = {
           return(paste0(
             indent(),
             "/*AND BD_SMOD.SMOD_COD_SPEC IS NULL*/\n"
           ))
         },
         "omni" = {
           return(paste0(
             indent(),
             "AND BD_SMOD.SMOD_COD_SPEC IS NULL\n"
           ))
         },
         "spec" = {
           return(paste0(
             indent(),
             "AND BD_SMOD.SMOD_COD_SPEC IS NOT NULL\n"
           ))
         })
}


#' @title query_I_SMOD_SERV_MD_CM.where_SMOD_COD_ACTE
#' @description Section `where` du code SQL où on demande des actes spécifiques et tous actes confondus.
#' @encoding UTF-8
#' @keywords internal
query_I_SMOD_SERV_MD_CM.where_SMOD_COD_ACTE <- function(CodeActe) {
  if (is.null(CodeActe)){
    return(paste0(
      indent(),
      "/*AND BD_SMOD.SMOD_COD_ACTE IN (",qu(CodeActe),")*/\n"
    ))
  }
  else{
    return(paste0(
      indent(),
      "AND BD_SMOD.SMOD_COD_ACTE IN (",qu(CodeActe),")\n"
    ))
  }
}

#' @title query_I_SMOD_SERV_MD_CM.where_SMOD_COD_STA_DECIS
#' @description Section `where` du code SQL où on demande le Code de statu de décision.
#' @encoding UTF-8
#' @keywords internal
query_I_SMOD_SERV_MD_CM.where_SMOD_COD_STA_DECIS <- function(code_stat_decis) {
  return(paste0(
    indent(),
    "AND BD_SMOD.SMOD_COD_STA_DECIS IN (", qu(code_stat_decis), ")\n"
  ))
}

#' @title query_I_SMOD_SERV_MD_CM.where_ETAB_COD_CATG_ETAB_EI
#' @description Section `where` du code SQL où on demande les visites en ambulatoire ou dans d'autres établissements.
#' @encoding UTF-8
#' @keywords internal
query_I_SMOD_SERV_MD_CM.where_ETAB_COD_CATG_ETAB_EI <- function(catg_etab) {
  switch(catg_etab,
         "all" = {
           return(paste0(
             indent(),
             "/*AND (ETAB_COD_CATG_ETAB_EI IN ('54X', '55X', '57X', '9X2', '8X5', '4X1','0X1') OR BD_SMOD.SMOD_NO_ETAB_USUEL IS NULL)*/\n"
           ))
         },
         "ambulatoire" = {
           return(paste0(
             indent(),
             "AND (BD_Etab.ETAB_COD_CATG_ETAB_EI IN ('54X', '55X', '57X', '9X2', '8X5', '4X1','0X1') OR BD_SMOD.SMOD_NO_ETAB_USUEL IS NULL)\n"
           ))
         })
}

#' @title query_V_EPISO_SOIN_DURG_CM.diagn
#' @description Section `where` du code SQL où on demande des DX spécifiques et tous DX confondus.
#' @encoding UTF-8
#' @keywords internal
query_I_SMOD_SERV_MD_CM.diagn <- function(diagn) {
  if (is.null(diagn)){
    return(paste0(
      indent(),
      "/*AND BD_SMOD.SMOD_COD_DIAGN_PRIMR like any (",qu(diagn),")*/\n"
    ))
  }
  else{
    return(paste0(
      indent(),
      "AND BD_SMOD.SMOD_COD_DIAGN_PRIMR like any (",qu(diagn),")\n"
    ))
  }
}

#' @title query_I_SMOD_SERV_MD_CM.where_I_BENF_ADR_CM
#' @description Section `where` du code SQL où on demande les adresses de Benef.
#' @encoding UTF-8
#' @keywords internal
query_I_SMOD_SERV_MD_CM.where_I_BENF_ADR_CM<- function(benef_adr,debut_periode,fin_periode,date_adr) {
  switch(benef_adr,
         "date_fixe" = {
           return(paste0(
             indent(),
             "LEFT JOIN (
              SELECT
              BENF_NO_INDIV_BEN_BANLS,
              BENF_DD_ADR_BEN,
              BENF_DF_ADR_BEN,
              LGEO_COD_RSS,
              LGEO_COD_TERRI_RLS

              FROM Prod.I_BENF_ADR_CM
              WHERE BENF_IND_ADR_HQ IN ('N') AND BENF_COD_TYP_ADR IN ('R')
              AND (BENF_DD_ADR_BEN<='",fin_periode,"' AND BENF_DF_ADR_BEN>='",debut_periode,"')
              AND '",date_adr,"' between BENF_DD_ADR_BEN AND BENF_DF_ADR_BEN)
              AS BD_Adr
             ON BD_SMOD.SMOD_NO_INDIV_BEN_BANLS=BD_Adr.BENF_NO_INDIV_BEN_BANLS"
           ))
         },
         "dernière_adresse" = {
           return(paste0(
             indent(),
             "LEFT JOIN (
              SELECT
              BENF_NO_INDIV_BEN_BANLS,
              BENF_DD_ADR_BEN,
              BENF_DF_ADR_BEN,
              LGEO_COD_RSS,
              LGEO_COD_TERRI_RLS

              FROM Prod.I_BENF_ADR_CM
              WHERE BENF_IND_ADR_HQ IN ('N') AND BENF_COD_TYP_ADR IN ('R')
              AND (BENF_DD_ADR_BEN<='",fin_periode,"' AND BENF_DF_ADR_BEN>='",debut_periode,"')
              QUALIFY Row_Number()Over (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY BENF_DD_ADR_BEN DESC)=1 --dernière adresse
               \n)
              AS BD_Adr
             ON BD_SMOD.SMOD_NO_INDIV_BEN_BANLS=BD_Adr.BENF_NO_INDIV_BEN_BANLS"
           ))
         },
         "première_adresse" = {
           return(paste0(
             indent(),
             "LEFT JOIN (
              SELECT
              BENF_NO_INDIV_BEN_BANLS,
              BENF_DD_ADR_BEN,
              BENF_DF_ADR_BEN,
              LGEO_COD_RSS,
              LGEO_COD_TERRI_RLS

              /*FROM(
              SELECT
              BENF_NO_INDIV_BEN_BANLS,
              BENF_DD_ADR_BEN,
              BENF_DF_ADR_BEN,
              LGEO_COD_RSS,
              LGEO_COD_TERRI_RLS,
              CASE
              WHEN (BENF_DD_ADR_BEN <= '",debut_periode,"' AND BENF_DF_ADR_BEN >= '",fin_periode,"') THEN DATE '",fin_periode,"' - DATE '",debut_periode,"' + 1
              WHEN (BENF_DD_ADR_BEN >= '",debut_periode,"' AND BENF_DF_ADR_BEN >= '",fin_periode,"') THEN DATE '",fin_periode,"' - BENF_DD_ADR_BEN
              WHEN (BENF_DD_ADR_BEN <= '",debut_periode,"' AND BENF_DF_ADR_BEN <= '",fin_periode,"') THEN BENF_DF_ADR_BEN - DATE '",debut_periode,"'
              WHEN (BENF_DD_ADR_BEN >= '",debut_periode,"' AND BENF_DF_ADR_BEN <= '",fin_periode,"') THEN BENF_DF_ADR_BEN - BENF_DD_ADR_BEN
              ELSE 0
              END AS days_count,
              ROW_NUMBER() OVER (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY days_count DESC) AS row_num */
              FROM Prod.I_BENF_ADR_CM
              WHERE BENF_IND_ADR_HQ IN ('N') AND BENF_COD_TYP_ADR IN ('R')
              AND (BENF_DD_ADR_BEN<='",fin_periode,"' AND BENF_DF_ADR_BEN>='",debut_periode,"')
              /*AND '",date_adr,"' between BENF_DD_ADR_BEN AND BENF_DF_ADR_BEN  --option 1 date fixe
              QUALIFY Row_Number()Over (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY BENF_DD_ADR_BEN DESC)=1*/ --option 2 dernière adresse
              QUALIFY Row_Number()Over (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY BENF_DD_ADR_BEN ASC)=1 --option 3 première adresse
              /*)AS A
              WHERE A.row_num=1 */
             )
              AS BD_Adr
             ON BD_SMOD.SMOD_NO_INDIV_BEN_BANLS=BD_Adr.BENF_NO_INDIV_BEN_BANLS \n"
           ))
         },
         "long_adresse" = {
           return(paste0(
             indent(),
             "LEFT JOIN (
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
                WHEN (BENF_DD_ADR_BEN <= '",debut_periode,"' AND BENF_DF_ADR_BEN >= '",fin_periode,"') THEN DATE '",fin_periode,"' - DATE '",debut_periode,"' + 1
                WHEN (BENF_DD_ADR_BEN >= '",debut_periode,"' AND BENF_DF_ADR_BEN >= '",fin_periode,"') THEN DATE '",fin_periode,"' - BENF_DD_ADR_BEN
                WHEN (BENF_DD_ADR_BEN <= '",debut_periode,"' AND BENF_DF_ADR_BEN <= '",fin_periode,"') THEN BENF_DF_ADR_BEN - DATE '",debut_periode,"'
                WHEN (BENF_DD_ADR_BEN >= '",debut_periode,"' AND BENF_DF_ADR_BEN <= '",fin_periode,"') THEN BENF_DF_ADR_BEN - BENF_DD_ADR_BEN
                ELSE 0
              END AS days_count,
              ROW_NUMBER() OVER (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY days_count DESC) AS row_num
              FROM Prod.I_BENF_ADR_CM
              WHERE BENF_IND_ADR_HQ IN ('N') AND BENF_COD_TYP_ADR IN ('R')
              AND (BENF_DD_ADR_BEN<='",fin_periode,"' AND BENF_DF_ADR_BEN>='",debut_periode,"')
              )AS A
               WHERE A.row_num=1
             )
              AS BD_Adr
             ON BD_SMOD.SMOD_NO_INDIV_BEN_BANLS=BD_Adr.BENF_NO_INDIV_BEN_BANLS \n"
           ))
         },
         "date_acte" = {
           return(paste0(
             indent(),
             "LEFT JOIN (
              SELECT
              BENF_NO_INDIV_BEN_BANLS,
              BENF_DD_ADR_BEN,
              BENF_DF_ADR_BEN,
              LGEO_COD_RSS,
              LGEO_COD_TERRI_RLS

              FROM Prod.I_BENF_ADR_CM
              WHERE BENF_IND_ADR_HQ IN ('N') AND BENF_COD_TYP_ADR IN ('R')
              AND (BENF_DD_ADR_BEN<='",fin_periode,"' AND BENF_DF_ADR_BEN>='",debut_periode,"')
              )
              AS BD_Adr
             ON BD_SMOD.SMOD_NO_INDIV_BEN_BANLS=BD_Adr.BENF_NO_INDIV_BEN_BANLS
             AND BD_SMOD.SMOD_DAT_SERV between BD_Adr.BENF_DD_ADR_BEN AND BD_Adr.BENF_DF_ADR_BEN \n"
           ))
         }
  )
}






