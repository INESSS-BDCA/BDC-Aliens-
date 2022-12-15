#' @title Requête SQL: query_SQL_CodeActe
#'
#' @description La fonction `query_SQL_CodeActe` est une fonction intermédiaire qui est utilisée par la fonction \code{\link{SQL_CodeActe}} afin
#' d'extraire les actes entre une date de début et de fin d'étude avec des données sur le bénéficiaire ainsi que l'établissement de soins. Pour plus de détails, veuillez-vous référer à la vignette `query_SQL_CodeActe.html`.
#'
#'
#' @param debut Date de début de la période d'étude au format `AAAA-MM-JJ`.
#' @param fin Date de fin de la période d'étude au format `AAAA-MM-JJ`.
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
#'DT<-as.data.table(odbc::dbGetQuery(conn=SQL_connexion(),
#'statement=query_SQL_CodeActe (debut="2019-04-01",fin="2022-03-31",
#'CodeActe=c(07122, 07237, 07800, 07089, 0780))
#' ))
#' }
#'


query_SQL_CodeActe<-function(debut, fin, CodeActe){

return (paste0("
SELECT

-- Sources des variables :
-- Nom de vue : Prod.I_SMOD_SERV_MD_CM (SMOD)

BD_SMOD.SMOD_NO_INDIV_BEN_BANLS AS ID, --Identifiant du bénéficiaire banalisé

/* Il est possible de classer les actes par année financière.
Pour ce faire, on propose d'appliquer la ligne de programmation suivante : */
case when EXTRACT(MONTH FROM SMOD_DAT_SERV) between 4 and 12 THEN EXTRACT(YEAR FROM SMOD_DAT_SERV)
ELSE EXTRACT(YEAR FROM SMOD_DAT_SERV)-1 end AS AnFinan, --Année financière
Extract (YEAR From BD_SMOD.SMOD_DAT_SERV) AS AnCivil,  --Année civile

BD_SMOD.SMOD_DAT_SERV AS DateActe,  --Date Acte
BD_SMOD.SMOD_COD_ACTE AS CodeActe,  --Code Acte
BD_SMOD.SMOD_MNT_PAIMT AS CoutActe,  --Coût Acte
BD_SMOD.SMOD_COD_DIAGN_PRIMR AS DxActe,  --Premier diagnostic porté par le professionnel
BD_SMOD.SMOD_NO_ETAB_USUEL AS Num_ETAB_USUEL,  --Ancien numéro de l'établissement

/* Ces variables sont souvent optionnelles, la variable la plus intéressante serait la catégorie d'établissement en format : 0X0 (voir plus bas) */
BD_SMOD.SMOD_COD_LIEU_DISP AS LieuDisp,  --Code du lieu de dispensation du service
BD_SMOD.SMOD_COD_ENTEN AS CodEntente,  --Code de l'entente
BD_SMOD.ETAB_COD_SECT_ACTIV_ETAB AS SecActiv,  --secteur d'activité

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
/* Cette variable pourrait être optionnelle, car le format caractère prend beaucoup de place dans le fichier sortant */
BD_Etab.ETAB_NOM_CATG_ETAB_EI AS Nom_Cat_Etab,

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

/* Une approche alternative (rapide) serait de sélectionner la spécialité déclarée au moment de la demande de facturation */
LEFT JOIN Prod.D_DISP_SPEC_CM AS BD_Disp
ON BD_SMOD.DISP_NO_SEQ_DISP_BANLS = BD_Disp.DISP_NO_SEQ_DISP_BANLS
AND BD_Disp.DISP_COD_NIV_SPEC = 1 AND BD_SMOD.SMOD_DAT_SERV BETWEEN BD_Disp.DISP_DD_SPEC_DISP AND BD_Disp.DISP_DF_SPEC_DISP

/* Cette requête pourrait optionnelle, car la spécialité du médecin référent est rarement demandée */
LEFT JOIN Prod.D_DISP_SPEC_CM AS BD_DispR
ON BD_SMOD.DISP_NO_SEQ_DISP_REFNT_BANLS = BD_DispR.DISP_NO_SEQ_DISP_BANLS
AND BD_DispR.DISP_COD_NIV_SPEC = 1
AND BD_SMOD.SMOD_DAT_SERV BETWEEN BD_DispR.DISP_DD_SPEC_DISP AND BD_DispR.DISP_DF_SPEC_DISP

/**********************************************************
Découpage géographique du bénéficiaire : codes RSS et RLS
***********************************************************/
/*
On sélectionne la dernière adresse du bénéficiaire durant la période sélectionnée.
Autres méthodes :
1) cibler la première adresse de la période
2) cibler l'adresse dans laquelle le BEN a résidé le plus longtemps durant la période
3) cibler l'adresse à une date fixe (p. ex. 31 mars, 1 juillet, mi-année)
*/
LEFT JOIN (
			SELECT
			BENF_NO_INDIV_BEN_BANLS,
			BENF_DD_ADR_BEN,
			BENF_DF_ADR_BEN,
			LGEO_COD_RSS, LGEO_COD_TERRI_RLS
			FROM Prod.I_BENF_ADR_CM
			WHERE BENF_IND_ADR_HQ IN ('N') AND BENF_COD_TYP_ADR IN ('R')
			AND (BENF_DD_ADR_BEN<='",fin,"' AND BENF_DF_ADR_BEN>='",debut,"')
			QUALIFY Row_Number()Over (PARTITION BY BENF_NO_INDIV_BEN_BANLS ORDER BY BENF_DD_ADR_BEN DESC)=1
			) AS BD_Adr
			ON BD_SMOD.SMOD_NO_INDIV_BEN_BANLS=BD_Adr.BENF_NO_INDIV_BEN_BANLS

/* On ajoute ici les NOMS des codes RSS et RLS du bénéficiaire : ceci serait optionnel si la requête est trop lourde */
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

/* On ajoute le NOM de l'établissement */
LEFT JOIN (SELECT ETAB_NO_ETAB, ETAB_NOM_ETAB FROM RES_SSS.V_NOM_ETAB_DERN_TYP_NOM) AS BD_NomEtab
ON BD_Etab.ETAB_NO_ETAB = BD_NomEtab.ETAB_NO_ETAB

/* On ajoute ici les NOMS des codes RSS et RLS de l'établissement : ceci serait optionnel si la requête est trop lourde */
LEFT JOIN (SELECT CodRLS, NomRLS AS NomRLS_Etab, NomRSS AS NomRSS_Etab FROM DONNE_INESSS.tGI_RLS_RTS) AS BD_NomReg_Etab
ON BD_Etab.LGEO_COD_TERRI_RLS=BD_NomReg_Etab.CodRLS

WHERE BD_SMOD.SMOD_COD_STA_DECIS IN ('PAY')
/*
1) On pourrait sélectionner 1 année ou plusieurs années en format année financière (01 avril au 31 mars) ou année civile (01 janvier au 31 décembre)
2) Puis, à noter que plus d'années sont ciblées, plus le nombre d'observations augmente et ceci pourrait bloquer la requête
3) Toutefois, la sélection des observations sous la base de codes de diagnostic ou d'actes pourrait aussi un impact sur le nombre de lignes à retenir
*/
AND BD_SMOD.SMOD_DAT_SERV BETWEEN '",debut,"' AND '",fin,"'

/*
On utilise plus fréquemment le code de diagnostic comme critère de sélection,
donc, il faudra ajouter la condition qui soit la plus pertinente pour exécuter cette requête
Options :
a) 1 code précis, fonction : IN
b) 1 code dont la racine du code est connue, fonction : LIKE XX%
c) plusieurs codes connus : IN
d) plusieurs codes dont la racine du code est connue, fonction : LIKE ANY XX%
*/
/* Exemples : */
AND BD_SMOD.SMOD_COD_ACTE IN (",qu(CodeActe),")

/*
Cette exclusion est toujours importante à considérer, car les actes qui représentent des forfaits ne correspondent pas à de consultations
sinon à des suppléments monétaires associés à une demande de facturation
*/
AND BD_SMOD.SMOD_COD_ACTE NOT IN (
									SELECT DISTINCT NMOD_COD_ACTE
									FROM Prod.V_RPERT_COD_ACTE
									WHERE NMOD_COD_GRP_ACTE IN (170, 180, 181, 182, 183, 187, 195, 196, 197, 198, 199, 854)
									)

ORDER BY 1,2,3,4

"

))

}

