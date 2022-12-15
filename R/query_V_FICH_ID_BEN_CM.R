#' @title Code SQL: Extraction des caractéristiques du bénéficiaire
#'
#' @description Générateur de code SQL pour l'extraction des variables suivante: BENF_NO_INDIV_BEN_BANLS, BENF_COD_SEXE, BENF_DAT_NAISS, BENF_DAT_DECES de la vue `V_FICH_ID_BEN_CM`.
#' L'âge est calculé à une date fixe spécifiée par l'argement `date_age`.
#'
#' @param debut Date de début de la période d'étude.
#' @param fin Date de fin de la période d'étude.
#' @param date_age `"AAAA-MM-JJ"` indiquant la date à laquelle l'âge des bénéficaires est calculé.
#'
#' @return chaîne de caractères, code SQL.
#' @keywords internal
#' @encoding UTF-8
#' @export
#'
#'@examples
#'\dontrun{
#'DT1<-as.data.table(odbc::dbGetQuery(conn=SQL_connexion(),
#'statement=query_V_FICH_ID_BEN_CM(debut="2022-02-01", fin="2022-03-31", date_age="2022-03-31")
#'}
#'
#'
query_V_FICH_ID_BEN_CM<-function(debut, fin, date_age){

return(paste0("
SELECT
B.BENF_NO_INDIV_BEN_BANLS AS ID,
B.BENF_COD_SEXE AS Sexe,  --Sexe du bénéficiaire
B.BENF_DAT_NAISS AS DatNais,  --Date de naissance
B.BENF_DAT_DECES AS DatDeces, --Date de décès
CAST(((CAST('",date_age,"' AS DATE FORMAT 'yyyy-mm-dd')) - B.BENF_DAT_NAISS) / 365.25 AS BIGINT) AS Age,

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
1) cibler la première adresse de la période
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
    AND (BENF_DD_ADR_BEN<='",fin,"' AND BENF_DF_ADR_BEN>='",debut,"')
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
ORDER BY 1
"
))

}
