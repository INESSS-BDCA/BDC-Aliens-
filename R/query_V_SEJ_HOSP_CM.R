#' @title Code SQL: Extraction de diagnostics dans BDCU
#'
#' @description Générateur de code SQL pour l'extraction de diagnostics de la vue `V_SEJ_HOSP_CM`.
#'
#' @param debut_cohort Date de début de la période d'étude.
#' @param fin_cohort Date de fin de la période d'étude.
#' @param date_dx_var `'admis'` ou `'depar'`. Indique si on utilise la date d'admission ou la date de départ comme date indexe pour l'étude.
#'
#' @return chaîne de caractères, code SQL.
#' @keywords internal
#' @encoding UTF-8
#' @export
#'

query_V_SEJ_HOSP_CM<- function(query,debut_periode, fin_periode,date_dx_var,benef_adr,date_adr) {
  switch (query,
          "extraction_MEDECHO_SEJ_HOSP"={
    return(paste0(
"select
BD_sej_hosp.SHOP_NO_INDIV_BEN_BANLS AS ID,
BD_sej_hosp.SHOP_NO_SEQ_SEJ_HOSP,
BD_sej_hosp.SHOP_AN_PER_TEMPS,
BD_sej_hosp.SHOP_DAT_ARRIV_URG_SEJ_HOSP,
BD_sej_hosp.SHOP_DAT_ADMIS_SEJ_HOSP,
BD_sej_hosp.SHOP_HRE_ADMIS_SEJ_HOSP,
BD_sej_hosp.SHOP_DAT_DEPAR_SEJ_HOSP,
BD_sej_hosp.SHOP_HRE_DEPAR_SEJ_HOSP,
BD_sej_hosp.SHOP_NBR_JR_ABSNT_SEJ_HOSP,
BD_sej_hosp.SHOP_TYP_LIEU_SEJ_HOSP_PROVE,
BD_sej_hosp.SHOP_NO_ETAB_MSSS_PROVE,
BD_sej_hosp.SHOP_TYP_LIEU_SEJ_HOSP_DESTI,
BD_sej_hosp.SHOP_NO_ETAB_MSSS_DESTI,
BD_sej_hosp.SHOP_TYP_SOIN_SEJ_HOSP,
BD_sej_hosp.SHOP_TYP_ADMIS_SEJ_HOSP,
BD_sej_hosp.SHOP_IND_NOUV_NE_SEJ_HOSP,
BD_sej_hosp.SHOP_TYP_DECES_SEJ_HOSP,
BD_sej_hosp.SHOP_NBR_CNSUL_SEJ_HOSP,
BD_sej_hosp.SHOP_DAT_ACCID_SEJ_HOSP,
BD_sej_hosp.SHOP_COD_DIAGN_LIEU_ACCID,
BD_sej_hosp.SHOP_COD_DIAGN_CAUS_ACCID,
BD_sej_hosp.SHOP_NO_SEQ_SYS_CLA,
BD_sej_hosp.SHOP_NO_ETAB_MSSS,

-- vue: PROD.V_FICH_ID_BEN_CM (FIPA); Prod.I_BENF_ADR_CM; DONNE_INESSS.tGI_RLS_RTS
BD_Caract_benef.Sexe,
BD_Caract_benef.DatNais,
BD_Caract_benef.DatDeces,
BD_Caract_benef.RSS_Benf,
BD_Caract_benef.RLS_Benef,
BD_Caract_benef.NomRSS_Benef,
BD_Caract_benef.NomRLS_Benef,

-- vue: RES_SSS.V_ETAB_CTRE_HOSP_MSSS_DERN; PROD.V_DECOU_GEO_POSTL_DERN_CP; DONNE_INESSS.tGI_NOM_ETAB_HOSP; DONNE_INESSS.tGI_RLS_RTS
W.LGEO_COD_TERRI_RLS AS RLS_ETAB,
W.NomRLS_Etab,
W.CODE_RSS AS RSS_ETAB,
W.NOM_RSS AS NOM_RSS_Etab,
W.NOM_ETAB

FROM RES_SSS.V_SEJ_HOSP_CM AS BD_sej_hosp

---Requête caractéristiques, rss, rls bénéficiaire
LEFT JOIN (
  SELECT
  ID,
  Sexe,
  DatNais,
  DatDeces,
  --Age,
  RSS_Benf,
  RLS_Benef,
  NomRSS_Benef,
  NomRLS_Benef

  FROM (
    SELECT
    B.BENF_NO_INDIV_BEN_BANLS AS ID,
    B.BENF_COD_SEXE AS Sexe,  --Sexe du bénéficiaire
    B.BENF_DAT_NAISS AS DatNais,  --Date de naissance
    B.BENF_DAT_DECES AS DatDeces, --Date de décès

    /* On fait le recodage du code RSS pour le code RLS 1610, les variables avec les NOMS (RSS) sont correctes  */
      CASE WHEN BD_Adr.LGEO_COD_RSS=16 AND BD_Adr.LGEO_COD_TERRI_RLS IN ('1610') THEN 15
    ELSE BD_Adr.LGEO_COD_RSS END AS RSS_Benf,
    BD_NomReg_Ben.NomRSS_Benef,

    /* On ajoute un codes RLS fonctionnel pour les régions sans RLS on fait le recodgae de code RLS 1610, les variables avec les NOMS (RLS) sont correctes */
      CASE
    WHEN BD_Adr.LGEO_COD_RSS=10 THEN '1001'
    WHEN BD_Adr.LGEO_COD_RSS=17 THEN '1701'
    WHEN BD_Adr.LGEO_COD_RSS=18 THEN '1801'
    WHEN BD_Adr.LGEO_COD_TERRI_RLS IN ('1610') THEN '0511' ELSE BD_Adr.LGEO_COD_TERRI_RLS END AS RLS_Benef,
    BD_NomReg_Ben.NomRLS_Benef

    FROM PROD.V_FICH_ID_BEN_CM AS B

    /**********************************************************
      Découpage géographique du bénéficiaire : codes RSS et RLS
    ***********************************************************/
      /* On sélectionne la dernière adresse du bénéficiaire durant la période sélectionnée.
    Autres méthodes :
      1- cibler la première (la plus récente) adresse de la période
      2- cibler l'adresse dans laquelle le BEN a résidé le plus longtemps durant la période
      3- cibler l'adresse à une date fixe (p. ex. 31 mars, 1 juillet, mi-année) */

      LEFT JOIN (\n",
query_V_SEJ_HOSP_CM.where_I_BENF_ADR_CM(benef_adr,debut_periode,fin_periode,date_adr),
") AS BD_Adr
    ON B.BENF_NO_INDIV_BEN_BANLS=BD_Adr.BENF_NO_INDIV_BEN_BANLS

    /* On ajoute ici les NOMS des codes RSS et RLS du bénéficiaire : ceci serait optionnel si la requête est trop lourde */
      LEFT JOIN (
        SELECT
        CodRLS,
        NomRLS AS NomRLS_Benef,
        NomRSS AS NomRSS_Benef
        FROM DONNE_INESSS.tGI_RLS_RTS) AS BD_NomReg_Ben
    ON BD_Adr.LGEO_COD_TERRI_RLS=BD_NomReg_Ben.CodRLS
  ) AS BD_Caract
) AS BD_Caract_benef
ON BD_Caract_benef.ID=BD_sej_hosp.SHOP_NO_INDIV_BEN_BANLS \n",



 "---Requête Étabilissement de soins
LEFT JOIN (
  SELECT
  Z.ETAB_NO_ETAB_MSSS,
  Z.LGEO_COD_TERRI_RLS,
  Z.NomRLS_Etab,
  Z.CODE_RSS,
  Z.NOM_RSS,
  NOM_ETAB

  FROM (
    SELECT *
      FROM (
        SELECT distinct
        ETAB_NO_ETAB,
        ETAB_NO_ETAB_MSSS,
        ETAB_COD_POSTL
        FROM RES_SSS.V_ETAB_CTRE_HOSP_MSSS_DERN
      ) AS A

    LEFT JOIN (
      SELECT
      LGEO_COD_POSTL,
      LGEO_COD_TERRI_RLS
      FROM PROD.V_DECOU_GEO_POSTL_DERN_CP
      QUALIFY ROW_NUMBER() OVER (PARTITION BY LGEO_COD_POSTL ORDER BY LGEO_DD_PER DESC)=1
    ) AS B
    ON A.ETAB_COD_POSTL=B.LGEO_COD_POSTL

    LEFT JOIN (
      select
      NO_ETAB_MSSS,
      NOM_ETAB,
      CODE_RSS,
      NOM_RSS
      FROM DONNE_INESSS.tGI_NOM_ETAB_HOSP
      where ROW_NO_ETAB_MSSS=1
    ) AS F
    ON A.ETAB_NO_ETAB_MSSS=F.NO_ETAB_MSSS

    LEFT JOIN (
      SELECT
      CodRLS,
      NomRLS AS NomRLS_Etab
      FROM DONNE_INESSS.tGI_RLS_RTS
    ) AS Y
    ON B.LGEO_COD_TERRI_RLS=Y.CodRLS
  ) AS Z
) AS W
ON BD_sej_hosp.SHOP_NO_ETAB_MSSS=W.ETAB_NO_ETAB_MSSS \n",

"WHERE ",query_V_SEJ_HOSP_CM.date_dx_var(date_dx_var)," between to_date('",debut_periode,"') and to_date('",fin_periode,"')\n"))
          }
)
}



#' @title query_V_SEJ_HOSP_CM
#' @description Retourne le nom de la variable à utiliser selon la valeur de la variable `date_dx_var`.
#' @encoding UTF-8
#' @keywords internal
query_V_SEJ_HOSP_CM.date_dx_var <- function(date_dx_var) {
  if (date_dx_var == "admis") {
    return("SHOP_DAT_ADMIS_SEJ_HOSP")
  } else if (date_dx_var == "depar") {
    return("SHOP_DAT_DEPAR_SEJ_HOSP")
  } else {
    stop("query_V_DIAGN_SEJ_HOSP_CM.date_dx_var : date_dx_var ne contient pas une valeur permise.")
  }
}

#' @title query_I_SMOD_SERV_MD_CM.where_I_BENF_ADR_CM
#' @description Section `where` du code SQL où on demande les adresses de Benef.
#' @encoding UTF-8
#' @keywords internal
query_V_SEJ_HOSP_CM.where_I_BENF_ADR_CM<- function(benef_adr,debut_periode,fin_periode,date_adr) {
  switch(benef_adr,
         "date_fixe" = {
           return(paste0(
             indent(),
             "SELECT
              BENF_NO_INDIV_BEN_BANLS,
              BENF_DD_ADR_BEN,
              BENF_DF_ADR_BEN,
              LGEO_COD_RSS,
              LGEO_COD_TERRI_RLS

              FROM Prod.I_BENF_ADR_CM
              WHERE BENF_IND_ADR_HQ IN ('N') AND BENF_COD_TYP_ADR IN ('R')
              AND (BENF_DD_ADR_BEN<='",fin_periode,"' AND BENF_DF_ADR_BEN>='",debut_periode,"')
              AND '",date_adr,"' between BENF_DD_ADR_BEN AND BENF_DF_ADR_BEN"
           ))
         },
         "dernière_adresse" = {
           return(paste0(
             indent(),
             "SELECT
              BENF_NO_INDIV_BEN_BANLS,
              BENF_DD_ADR_BEN,
              BENF_DF_ADR_BEN,
              LGEO_COD_RSS,
              LGEO_COD_TERRI_RLS

              FROM Prod.I_BENF_ADR_CM
              WHERE BENF_IND_ADR_HQ IN ('N') AND BENF_COD_TYP_ADR IN ('R')
              AND (BENF_DD_ADR_BEN<='",fin_periode,"' AND BENF_DF_ADR_BEN>='",debut_periode,"')
              QUALIFY Row_Number()Over (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY BENF_DD_ADR_BEN DESC)=1 --dernière adresse
               \n"
           ))
         },
         "première_adresse" = {
           return(paste0(
             indent(),
             "SELECT
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
              WHERE A.row_num=1 */ \n"
           ))
         },
         "long_adresse" = {
           return(paste0(
             indent(),
             "SELECT
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
               WHERE A.row_num=1\n"
           ))
         }
  )
}

