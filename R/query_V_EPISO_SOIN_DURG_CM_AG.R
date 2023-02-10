#' @title Code SQL: Extraction de diagnostics dans BDCU
#'
#' @description Générateur de code SQL pour l'extraction de diagnostics de la vue `V_EPISO_SOIN_DURG_CM`.
#'
#' @param debut_cohort Date de début de la période d'étude.
#' @param fin_cohort Date de fin de la période d'étude.
#' @param diagn `vector` indiquant les codes de diagnostics *CIM9* et/ou *CIM10* à extraire.
#' @param date_dx_var `'admis'` ou `'depar'`. Indique si on utilise la date d'admission ou la date de départ comme date de diagnostic pour l'étude.
#'
#' @return chaîne de caractères, code SQL.
#' @keywords internal
#' @encoding UTF-8
#' @export
#'
query_V_EPISO_SOIN_DURG_CM_AG <- function(query,debut_cohort,fin_cohort, diagn, date_dx_var) {
  switch (query,
          "extraction_BDCU"={
            return(paste0(
            "SELECT
              BD_BDCU.SURG_NO_INDIV_BEN_BANLS AS ID,
              BD_BDCU.SURG_NO_EPISO_SOIN_DURG,
              BD_BDCU.SURG_AN_PER_TEMPS, --Année période temps
              BD_BDCU.SURG_DHD_EPISO_SOIN_DURG,
              Cast(BD_BDCU.SURG_DHD_EPISO_SOIN_DURG AS DATE) AS DD_SOIN_DURG, --Date et heure de début d'un épisode de soins
              BD_BDCU.SURG_DH_DEPAR_USAG_DURG,
              CAST(BD_BDCU.SURG_DH_DEPAR_USAG_DURG AS DATE) AS DF_SOIN_DURG, --DateHeure de départ d'un usager
              BD_BDCU.SURG_DH_PREM_TRIAG_DURG, --Date heure du premier triage d'urgence
              BD_BDCU.SURG_DH_PRIS_CHARG_DURG, --Date et heure de prise en charge
              BD_BDCU.SURG_DH_DEM_ADMIS_DURG, --Date et heure de la demande d'admission en CH
              BD_BDCU.SURG_DH_DECES_DURG, --Date et heure du décès
              BD_BDCU.SURG_COD_PRIOR_TRIAG_DURG, --Code de priorité du premier triage
              BD_BDCU.SURG_COD_AUTNM_PREM_TRIAG_DURG, --Code d'autonomie après le premier triage
              BD_BDCU.SURG_COD_RAIS_VISIT_DURG, --Code de raison de visite en urgence
              BD_BDCU.SURG_CATG_MAJR_DIAGN_DURG, --Catégorie majeure de diagnostic
              BD_BDCU.SURG_COD_DIAGN_MDCAL_CLINQ, --Code de diagnostic médical clinique\n",
              query_V_EPISO_SOIN_DURG_CM.date_dx_var(date_dx_var)," as DATE_DX,\n",
              "BD_BDCU.SURG_NBR_CNSUL_DURG, --Nombre de consultations en département d'urgence
              BD_BDCU.SURG_COD_MODE_ARRIV_DURG,--Code du mode d'arrivée de l'usager en urgence
              BD_BDCU.SURG_TYP_ORITN_USAG_DEPAR_DURG,--Type d'orientation de l'usager à son départ
              BD_BDCU.SURG_NO_ETAB_MSSS--Numero de l'etablissement attribue par MSSS \n",
              "FROM RES_SSS.V_EPISO_SOIN_DURG_CM AS BD_BDCU\n",
              "WHERE ",query_V_EPISO_SOIN_DURG_CM.date_dx_var(date_dx_var)," between to_date('",debut_cohort,"') and to_date('",fin_cohort,"')\n",
              query_V_EPISO_SOIN_DURG_CM.diagn(diagn),
              "AND SURG_COD_PRIOR_TRIAG_DURG IN ('1','2','3','4','5');"))},
          "extraction_BDCU_combin"={
            return(paste0(
              "SELECT
              -- vue: RES_SSS.V_EPISO_SOIN_DURG_CM
              BD_BDCU.SURG_NO_INDIV_BEN_BANLS AS ID,
              CAST(BD_BDCU.SURG_NO_EPISO_SOIN_DURG AS CHAR(100)),
              --BD_BDCU.SURG_NO_EPISO_SOIN_DURG,-- Numéro d'épisode de soins - Départements d'urgence
              BD_BDCU.SURG_AN_PER_TEMPS, --Année période temps
              BD_BDCU.SURG_DHD_EPISO_SOIN_DURG,
              Cast(BD_BDCU.SURG_DHD_EPISO_SOIN_DURG AS DATE) AS DD_SOIN_DURG, --Date et heure de début d'un épisode de soins
              BD_BDCU.SURG_DH_DEPAR_USAG_DURG,
              CAST(BD_BDCU.SURG_DH_DEPAR_USAG_DURG AS DATE) AS DF_SOIN_DURG, --DateHeure de départ d'un usager
              BD_BDCU.SURG_DH_PREM_TRIAG_DURG, --Date heure du premier triage d'urgence
              BD_BDCU.SURG_DH_PRIS_CHARG_DURG, --Date et heure de prise en charge
              BD_BDCU.SURG_DH_DEM_ADMIS_DURG, --Date et heure de la demande d'admission en CH
              BD_BDCU.SURG_DH_DECES_DURG, --Date et heure du décès
              BD_BDCU.SURG_COD_PRIOR_TRIAG_DURG, --Code de priorité du premier triage
              BD_BDCU.SURG_COD_AUTNM_PREM_TRIAG_DURG, --Code d'autonomie après le premier triage
              BD_BDCU.SURG_COD_RAIS_VISIT_DURG, --Code de raison de visite en urgence
              BD_BDCU.SURG_CATG_MAJR_DIAGN_DURG, --Catégorie majeure de diagnostic
              BD_BDCU.SURG_COD_DIAGN_MDCAL_CLINQ, --Code de diagnostic médical clinique\n",
              query_V_EPISO_SOIN_DURG_CM.date_dx_var(date_dx_var)," as DATE_DX,\n",
              "BD_BDCU.SURG_NBR_CNSUL_DURG, --Nombre de consultations en département d'urgence
              BD_BDCU.SURG_COD_MODE_ARRIV_DURG,--Code du mode d'arrivée de l'usager en urgence
              BD_BDCU.SURG_TYP_ORITN_USAG_DEPAR_DURG,--Type d'orientation de l'usager à son départ
              BD_BDCU.SURG_NO_ETAB_MSSS,--Numero de l'etablissement attribue par MSSS

              -- vue: RES_SSS.V_OCCU_CIVIE_DURG_CM
              BD_OCCUCIVIE.SURG_NO_SEQ_OCCU_CIVIE_DURG,--	Numéro de séquence de l'occupation de civière
              BD_OCCUCIVIE.SURG_NO_INDIV_BEN_BANLS,--	Numéro d'individu de la personne assurée
              BD_OCCUCIVIE.SURG_DHD_OCCU_CIVIE_DURG,--	Date et heure de début d'occupation de civière
              BD_OCCUCIVIE.SURG_DHF_OCCU_CIVIE_DURG,--	Date et heure de fin_cohort d'occupation de civière
              BD_OCCUCIVIE.SURG_CATG_CIVIE_DURG,--	Catégorie de civière en département d'urgence

              -- vue: RES_SSS.V_CNSUL_DURG_CM
              BD_CNSUL.SURG_NO_SEQ_CNSUL_DURG,--	Numéro de séquence de consultation
              BD_CNSUL.SURG_NO_INDIV_BEN_BANLS,--	Numéro d'individu de la personne assurée
              BD_CNSUL.SURG_COD_SPEC_CNSUL_DURG,--	Code de spécialité de consultation d'urgence
              BD_CNSUL.SURG_COD_ETA_CNSUL_DURG,--	Code spécifiant l'état de la consultation
              BD_CNSUL.SURG_DH_DEM_CNSUL_DURG,--	Date et heure de la demande de consultation
              BD_CNSUL.SURG_DH_REAL_CNSUL_DURG,--	Date et heure de réalisation de consultation

              -- vue: PROD.V_FICH_ID_BEN_CM (FIPA); Prod.I_BENF_ADR_CM; DONNE_INESSS.tGI_RLS_RTS
              BD_Caract_benef.Sexe,
              BD_Caract_benef.DatNais,
              BD_Caract_benef.DatDeces,
              --BD_Caract_benef.Age,
              BD_Caract_benef.RSS_Benf,
              BD_Caract_benef.RLS_Benef,
              BD_Caract_benef.NomRSS_Benef,
              BD_Caract_benef.NomRLS_Benef,

              -- vue: RES_SSS.V_ETAB_CTRE_HOSP_MSSS_DERN; PROD.V_DECOU_GEO_POSTL_DERN_CP; DONNE_INESSS.tGI_NOM_ETAB_HOSP; DONNE_INESSS.tGI_RLS_RTS
              W.LGEO_COD_TERRI_RLS AS RLS_ETAB,
              W.NomRLS_Etab,
              W.CODE_RSS AS RSS_ETAB,
              W.NOM_RSS AS NOM_RSS_Etab,
              W.NOM_ETAB \n",

              "FROM RES_SSS.V_EPISO_SOIN_DURG_CM AS BD_BDCU \n",
              "LEFT JOIN RES_SSS.V_OCCU_CIVIE_DURG_CM AS BD_OCCUCIVIE \n
              ON BD_BDCU.SURG_NO_EPISO_SOIN_DURG = BD_OCCUCIVIE.SURG_NO_EPISO_SOIN_DURG \n",
              "LEFT JOIN RES_SSS.V_CNSUL_DURG_CM AS BD_CNSUL \n
              ON BD_BDCU.SURG_NO_EPISO_SOIN_DURG=BD_CNSUL.SURG_NO_EPISO_SOIN_DURG \n",


              "LEFT JOIN (
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
              FROM(
              SELECT
              B.BENF_NO_INDIV_BEN_BANLS AS ID,
              B.BENF_COD_SEXE AS Sexe,  --Sexe du bénéficiaire
              B.BENF_DAT_NAISS AS DatNais,  --Date de naissance
              B.BENF_DAT_DECES AS DatDeces, --Date de décès
              --CAST(((CAST('2020-07-01' AS DATE FORMAT 'yyyy-mm-dd')) - B.BENF_DAT_NAISS) / 365.25 AS BIGINT) AS Age,

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
              /*
              On sélectionne la dernière adresse du bénéficiaire durant la période sélectionnée.
              Autres méthodes :
              1) cibler la première (la plus récente) adresse de la période
              2) cibler l'adresse dans laquelle le BEN a résidé le plus longtemps durant la période
              3) cibler l'adresse à une date fixe (p. ex. 31 mars, 1 juillet, mi-année)
              */

                LEFT JOIN (
                  SELECT
                  BENF_NO_INDIV_BEN_BANLS,
                  BENF_DD_ADR_BEN,
                  BENF_DF_ADR_BEN,
                  LGEO_COD_RSS,
                  LGEO_COD_TERRI_RLS
                  FROM Prod.I_BENF_ADR_CM
                  WHERE BENF_IND_ADR_HQ IN ('N') AND BENF_COD_TYP_ADR IN ('R')
                  AND (BENF_DD_ADR_BEN<='",fin_cohort,"' AND BENF_DF_ADR_BEN>='",debut_cohort,"')
                  QUALIFY Row_Number()Over (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY BENF_DD_ADR_BEN DESC)=1
                ) AS BD_Adr
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
              ON  BD_Caract_benef.ID=BD_BDCU.SURG_NO_INDIV_BEN_BANLS \n",

              "LEFT JOIN (
                SELECT
                ETAB_NO_ETAB_MSSS,
                LGEO_COD_TERRI_RLS,
                NomRLS_Etab,
                CODE_RSS,
                NOM_RSS,
                NOM_ETAB
                FROM(
                SELECT *
                FROM(
                SELECT
                ETAB_COD_POSTL,
                ETAB_NO_ETAB,
                ETAB_NO_ETAB_MSSS
                FROM RES_SSS.V_ETAB_CTRE_HOSP_MSSS_DERN
                QUALIFY ROW_NUMBER() OVER (PARTITION BY ETAB_NO_ETAB ORDER BY ETAB_DD_ETAB DESC)=1
                ) AS A
                LEFT JOIN(
                    SELECT
                        LGEO_COD_POSTL,
                        LGEO_COD_TERRI_RLS
                    FROM PROD.V_DECOU_GEO_POSTL_DERN_CP
                    QUALIFY ROW_NUMBER() OVER (PARTITION BY LGEO_COD_POSTL ORDER BY LGEO_DD_PER DESC)=1
                ) AS B
                ON A.ETAB_COD_POSTL=B.LGEO_COD_POSTL

                LEFT JOIN(
                    SELECT
                        NO_ETAB_MSSS,
                        NOM_ETAB,
                        CODE_RSS,
                        NOM_RSS
                    FROM DONNE_INESSS.tGI_NOM_ETAB_HOSP
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
              ON BD_BDCU.SURG_NO_ETAB_MSSS=W.ETAB_NO_ETAB_MSSS\n",

              "WHERE ",query_V_EPISO_SOIN_DURG_CM.date_dx_var(date_dx_var)," between to_date('",debut_cohort,"') and to_date('",fin_cohort,"')\n",
              query_V_EPISO_SOIN_DURG_CM.diagn(diagn),
              "AND SURG_COD_PRIOR_TRIAG_DURG IN ('1','2','3','4','5')
              ORDER BY 1;"))},
          "extraction_Dx"={
            return(paste0(
            "SELECT
            ID,
            DATE_DX
            FROM (
            SELECT
            DISTINCT
            SURG_NO_INDIV_BEN_BANLS as ID,\n",
            indent("select"),query_V_EPISO_SOIN_DURG_CM.date_dx_var(date_dx_var)," as DATE_DX,\n",
            "SURG_COD_DIAGN_MDCAL_CLINQ AS DX\n",
            "FROM RES_SSS.V_EPISO_SOIN_DURG_CM\n",
            "WHERE ",query_V_EPISO_SOIN_DURG_CM.date_dx_var(date_dx_var)," between to_date('",debut_cohort,"') and to_date('",fin_cohort,"')\n",
            indent(),"and SURG_COD_DIAGN_MDCAL_CLINQ like any (",qu(diagn),")",
            ") AS A;"
          ))}
  )

}

#' @title query_V_EPISO_SOIN_DURG_CM
#' @description Retourne le nom de la variable à utiliser selon la valeur de la variable `date_dx_var`.
#' @encoding UTF-8
#' @keywords internal
query_V_EPISO_SOIN_DURG_CM.date_dx_var <- function(date_dx_var) {
  if (date_dx_var == "admis") {
    return("SURG_DHD_EPISO_SOIN_DURG")
  } else if (date_dx_var == "depar") {
    return("SURG_DH_DEPAR_USAG_DURG")
  } else {
    stop("query_V_EPISO_SOIN_DURG_CM.date_dx_var : date_dx_var ne contient pas une valeur permise.")
  }
}

#' @title query_V_EPISO_SOIN_DURG_CM.diagn
#' @description Section `where` du code SQL où on demande des DX spécifiques et tous DX confondus.
#' @encoding UTF-8
#' @keywords internal
query_V_EPISO_SOIN_DURG_CM.diagn <- function(diagn) {
  if (is.null(diagn)){
    return(paste0(
      indent(),
      "/*AND BD_BDCU.SURG_COD_DIAGN_MDCAL_CLINQ like any (",qu(diagn),")*/\n"
    ))
  }
  else{
    return(paste0(
      indent(),
      "AND BD_BDCU.SURG_COD_DIAGN_MDCAL_CLINQ like any (",qu(diagn),")\n"
    ))
  }
}




