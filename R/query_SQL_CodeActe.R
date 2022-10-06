#' @title query_SQL_CodeActe
#'
#' @description La fonction `query_SQL_CodeActe` est une fonction intermédiaire qui est utilisée par la fonction \code{\link{SQL_CodeActe}} afin
#' d'extraire les actes entre une date de début et de fin d'étude. Pour plus de détails, veuillez-vous référer à la vignette `query_SQL_CodeActe.html`.
#'
#'
#' @param DateDebut Date de début de la période d'étude au format `AAAA-MM-JJ`.
#' @param DateFin Date de fin de la période d'étude au format `AAAA-MM-JJ`.
#' @param CodeActe Vecteur comprenant les codes d'acte d'interêt.
#'
#' @return Chaîne de caractères, code SQL.
#'
#' @encoding UTF-8
#' @keywords internal
#' @export
#'
#' @examples
#'  \dontrun{
#' query_SQL_CodeActe (DateDeb="2019-03-01",
#'                     DateFin="2022-05-31",
#'                     CodeActe=c(07122, 07237, 07800, 07089, 0780))
#' }
#'


query_SQL_CodeActe<-function(DateDebut, DateFin,CodeActe){

  DT<-paste0("
SELECT
--Nom de Vue:PROD.I_SMOD_SERV_MD_CM (SMOD)

                             BD_SMOD.SMOD_NO_INDIV_BEN_BANLS AS BenBanls, --Identifiant banalisé
                             EXTRACT (YEAR FROM BD_SMOD.SMOD_DAT_SERV) AS AnCivil, --Année civile
                             BD_SMOD.SMOD_DAT_SERV AS DateActe,--Date Acte
                             BD_SMOD.SMOD_COD_ACTE AS CodeActe, --Code Acte
                             BD_SMOD.SMOD_MNT_PAIMT AS CoutActe, --Coût Acte
                             BD_SMOD.SMOD_COD_DIAGN_PRIMR AS DxActe,--Premier diagnostic porté par le professionnel
                             BD_SMOD.SMOD_COD_LIEU_DISP AS LieuDisp,--Code du lieu de dispensation dun service (C:Service rendu en cabinet ou à domicile; E:Service rendu en établissement; X:Indéterminé)
                             BD_SMOD.ETAB_COD_SECT_ACTIV_ETAB AS SecActiv,--secteur d'activité
                             BD_SMOD.SMOD_NO_ETAB_USUEL AS Num_ETAB_USUEL,--Ancien numéro de l'établissement
                             BD_SMOD.SMOD_COD_ENTEN AS CodEntente,--Code de l'entente

--Nom de vue:PROD.V_FICH_ID_BEN_CM (FIPA)

                             BD_Caract.BENF_COD_SEXE AS Sexe, -- Sexe
                             BD_Caract.BENF_DAT_NAISS AS DatNais, --Date de naissance
                             BD_Caract.BENF_DAT_DECES AS DatDeces, --Date de décès
                             CAST(((BD_SMOD.SMOD_DAT_SERV) - BD_Caract.BENF_DAT_NAISS) / 365.25 AS BIGINT) AS Age,


--Nom de vue:PROD.D_DISP_SPEC_CM (FIP)

                             BD_SMOD.DISP_NO_SEQ_DISP_BANLS AS NumDispSp, -- numéro du dispensateur
                             BD_Disp.DISP_COD_SPEC AS SPDisps,--spécialité du dispensateur
                             BD_SMOD.DISP_NO_SEQ_DISP_REFNT_BANLS AS NumDispRef, -- numéro du médecin référent
                             BD_DispR.DISP_COD_SPEC AS SPDispRefs,-- spécialité du médecin référent

--Nom de vue:prod.V_ETAB_USUEL
                             BD_Etab.ETAB_NO_ETAB AS NO_ETAB,
                             BD_Etab.ETAB_COD_CATG_ETAB_EI AS Grp_Cat_Etab, -- (Urgence, Clinique externe, ...)
                             BD_NomCat_Etab.ETAB_COD_REGR_CATG_ETAB_EI AS Cat_Etab,
                             BD_NomCat_Etab.ETAB_NOM_CATG_ETAB_EI AS Nom_Grp_Cat_Etab,

--Nom de vue:RES_SSS.V_NOM_ETAB_DERN_TYP_NOM
                             BD_NomEtab.ETAB_NOM_ETAB AS NOM_ETAB,

--Nom de vue:PROD.I_BENF_ADR_CM (FIPA)
                             BD_RG_Benef.LGEO_COD_RSS AS RSS_Benf, --Région socio-sanitaire Bénéf
                             CASE
                             WHEN BD_RG_Benef.LGEO_COD_RSS=10 THEN '1001'
                             WHEN BD_RG_Benef.LGEO_COD_RSS=17 THEN '1701'
                             WHEN BD_RG_Benef.LGEO_COD_RSS=18 THEN '1801' ELSE BD_RG_Benef.LGEO_COD_TERRI_RLS END AS RLS_Benef,
                             k.NomRLS_Benef,
                             k.NomRSS_Benef,
                             -- Nom de vue: prod.V_DECOU_GEO_POSTL_DERN_CP
                             BD_RG_Etab.LGEO_COD_RSS AS RSS_Etab,
                             BD_RG_Etab.LGEO_COD_TERRI_RLS AS RLS_Etab,
                             x.NomRLS_Etab,
                             x.NomRSS_Etab

                             FROM PROD.I_SMOD_SERV_MD_CM  AS  BD_SMOD

                             LEFT JOIN PROD.V_FICH_ID_BEN_CM AS BD_Caract
                             ON BD_SMOD.SMOD_NO_INDIV_BEN_BANLS = BD_Caract.BENF_NO_INDIV_BEN_BANLS

                             LEFT JOIN prod.D_DISP_SPEC_CM AS BD_Disp
                             ON BD_SMOD.DISP_NO_SEQ_DISP_BANLS = BD_Disp.DISP_NO_SEQ_DISP_BANLS
                             AND BD_Disp.DISP_COD_NIV_SPEC = 1
                             AND BD_SMOD.SMOD_DAT_SERV BETWEEN BD_Disp.DISP_DD_SPEC_DISP AND BD_Disp.DISP_DF_SPEC_DISP

                             LEFT JOIN prod.D_DISP_SPEC_CM AS BD_DispR
                             ON BD_SMOD.DISP_NO_SEQ_DISP_REFNT_BANLS = BD_DispR.DISP_NO_SEQ_DISP_BANLS
                             AND BD_DispR.DISP_COD_NIV_SPEC = 1
                             AND BD_SMOD.SMOD_DAT_SERV BETWEEN BD_DispR.DISP_DD_SPEC_DISP AND BD_DispR.DISP_DF_SPEC_DISP


--Benef RSS, RLS
                             LEFT JOIN (
                             SELECT jj.*
                             FROM (
                             SELECT BENF_NO_INDIV_BEN_BANLS, BENF_DD_ADR_BEN, BENF_DF_ADR_BEN, BENF_IND_ADR_HQ, LGEO_COD_RSS, LGEO_COD_TERRI_RLS,
                             (Row_Number() Over (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY BENF_DD_ADR_BEN DESC) AS RW)
                             FROM PROD.I_BENF_ADR_CM
                             WHERE BENF_IND_ADR_HQ IN ('N')
                             ) jj
                             WHERE RW=1
                             ) AS BD_RG_Benef
                             ON BD_SMOD.SMOD_NO_INDIV_BEN_BANLS=BD_RG_Benef.BENF_NO_INDIV_BEN_BANLS

                             LEFT JOIN (
                             SELECT kk.*
                             FROM (
                             SELECT	CodRLS,
                             NomRLS AS NomRLS_Benef,
                             NomRSS AS NomRSS_Benef
                             FROM DONNE_INESSS.tGI_RLS_RTS
                             ) KK ) AS k
                             ON BD_RG_Benef.LGEO_COD_TERRI_RLS=k.CodRLS

--Etablissement:Num, Catég, Nom, Num_RLS, Num_RSS, Nom_RLS, Nom_RSS
                             LEFT JOIN prod.V_ETAB_USUEL AS BD_Etab
                             ON BD_SMOD.SMOD_NO_ETAB_USUEL = BD_Etab.ETAB_NO_ETAB_USUEL
                             AND BD_SMOD.SMOD_DAT_SERV BETWEEN BD_Etab.ETAB_DD_ETAB_USUEL AND BD_Etab.ETAB_DF_ETAB_USUEL

                             LEFT JOIN (SELECT ETAB_NO_ETAB, ETAB_COD_POSTL
                             FROM (SELECT ETAB_NO_ETAB, ETAB_COD_POSTL,
                             (ROW_NUMBER() OVER (PARTITION  BY ETAB_NO_ETAB ORDER BY ETAB_DD_ETAB DESC) AS RW)
                             FROM  Prod.V_ETAB
                             WHERE ETAB_NO_ETAB IS NOT NULL
                             ) AS xEtab
                             WHERE RW=1) AS E
                             ON E.ETAB_NO_ETAB  = BD_Etab.ETAB_NO_ETAB

                             LEFT JOIN  prod.V_DECOU_GEO_POSTL_DERN_CP AS  BD_RG_Etab
                             ON E.ETAB_COD_POSTL=BD_RG_Etab.LGEO_COD_POSTL

                             LEFT JOIN (
                             SELECT XX.*
                             FROM (
                             SELECT	CodRLS,
                             NomRLS AS NomRLS_Etab,
                             NomRSS AS NomRSS_Etab
                             FROM DONNE_INESSS.tGI_RLS_RTS
                             ) XX ) AS x
                             ON BD_RG_Etab.LGEO_COD_TERRI_RLS=X.CodRLS


                             LEFT JOIN PROD.V_COD_CATG_ETAB_EI AS BD_NomCat_Etab
                             ON BD_NomCat_Etab.ETAB_COD_CATG_ETAB_EI = BD_Etab.ETAB_COD_CATG_ETAB_EI

                             LEFT JOIN RES_SSS.V_NOM_ETAB_DERN_TYP_NOM AS BD_NomEtab
                             ON BD_Etab.ETAB_NO_ETAB = BD_NomEtab.ETAB_NO_ETAB

                             WHERE BD_SMOD.SMOD_COD_ACTE IN (",qu(CodeActe),")
                             AND BD_SMOD.SMOD_DAT_SERV BETWEEN '",DateDebut,"' AND '",DateFin,"'
                             AND BD_SMOD.SMOD_COD_STA_DECIS = 'PAY'

                             ORDER BY 1,2,3,4"

  )

  return(DT)

  if (nrow(DT)) {
    return(DT)
  } else {
    return(NULL)
  }

}

