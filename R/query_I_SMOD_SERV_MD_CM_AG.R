#' @title Code SQL: query_I_SMOD_SERV_MD_CM_AG `(SMOD)`
#'
#' @description  `query_I_SMOD_SERV_MD_CM_AG` est une fonction multi-tâches. Elle utilise principalement la vue `I_SMOD_SERV_MD_CM` jumlée avec d'autres vues pour générer un code SQL qui permet :\cr
#' 1) d'extraire les variables pertinantes de la base de données SMOD.\cr
#' 2) d'extraire les variables pertinantes de la base de données SMOD, jumlées avec d'autres bases de données pour rajouter les caractéristiques de dispensateur, du bénéficiaire et de l'établissement de soins.\cr
#' 3) d'extraire les diagnostics pour créer par exemple une cohorte.\cr
#' 4) d'extraire les dates de visites (services).\cr
#' 5) de calculer le nombre d'acte par bénéficiaire entre la date de début et de fin d'une étude.\cr
#'
#' @param query indiquant si on veut effectuer 1) une extraction brute des vaiables pertinantes dans SMOD "extraction_SMOD", 2) une extraction brute des vaiables pertinantes dans SMOD jumlées avec d'autres bases de données "extraction_SMOD_combin",
#' 3) une extraction des diagnostics "extraction_Dx", 4) une extraction des dates de visites, ou 5) calculer le nombre d'acte "calcul_Nb_acte".
#' @param debut Date de début de la période d'étude au format `"AAAA-MM-JJ"`.
#' @param fin Date de fin de la période d'étude au format `"AAAA-MM-JJ"`.
#' @param diagn `vecteur` indiquant les codes de diagnostics *CIM9* et/ou *CIM10* à extraire.
#' @param CodeActe `vecteur` indiquant les codes d'acte d'interêt.
#' @param omni_spec peut prendre la valeur `omni`(extraction des actes facturés par les omni: SMOD_COD_SPEC IS NULL),
#' `spec` (extraction des actes facturés par les spec: SMOD_COD_SPEC IS NOT NULL) ou `all` (pas de spécification).
#' @param catg_etab peut prendre la valeur `ambulatoire` (pour les actes facturés en ambulatoire) ou `all` (pas de spécification).
#' @param code_stat_decis peut prendre la valeur `PAY-PPY`, `PAY`, ou `PPY`
#'
#' @return chaîne de caractères, code SQL.
#' @keywords internal
#' @encoding UTF-8
#' @export
#'
#' @examples
#' \dontrun{
#' **Extraction brute des variables pertinantes dans SMOD:**
#'DT<-data.table::as.data.table(odbc::dbGetQuery(conn=SQL_connexion("ms069a"),
#'statement=query_I_SMOD_SERV_MD_CM_AG (
#'query="extraction_SMOD",
#'debut="2021-04-01",
#'fin="2022-03-31",
#'CodeActe=c('07122', '07237', '07800', '07089', '0780'),
#'code_stat_decis="PAY-PPY")))
#'
#' **Extraction brute des variables pertinantes dans SMOD combiné avec d'autres bases de données:**
#'DT<-data.table::as.data.table(odbc::dbGetQuery(conn=SQL_connexion("ms069a"),
#'statement=query_I_SMOD_SERV_MD_CM_AG (
#'query="extraction_SMOD_combin",
#'debut="2021-04-01",
#'fin="2022-03-31",
#'CodeActe=c('07122', '07237', '07800', '07089', '0780'),
#'omni_spec="all",
#'catg_etab="all",
#'code_stat_decis="PAY-PPY")))
#'
#'  **Extraction des diagnostics:**
#'DT<-data.table::as.data.table(odbc::dbGetQuery(conn=SQL_connexion("ms069a"),
#'statement=query_I_SMOD_SERV_MD_CM_AG (
#'query="extraction_Dx",
#'debut="2021-04-01",
#'fin="2022-03-31",
#'diagn=c('5995%','6180%','6181%','6183%','6184%','N810%','N811%','N812%'),
#'code_stat_decis="PAY-PPY")))
#'
#' **Extraction des dates de visites:**
#'DT<-data.table::as.data.table(odbc::dbGetQuery(conn=SQL_connexion("ms069a"),
#'statement=query_I_SMOD_SERV_MD_CM_AG (
#'query="date_visite",
#'debut="2021-04-01",
#'fin="2022-03-31",
#'CodeActe=c('07122', '07237', '07800', '07089', '0780'), #Si CodeActe=NULL, l'extrcation inclut tout les actes facturés à l'intérieur de la période d'étude.
#'omni_spec="all",
#'catg_etab="all",
#'code_stat_decis="PAY-PPY")))
#'
#'  **Calcul du nombre d'acte par bénéficiaire:**
#'DT<-data.table::as.data.table(odbc::dbGetQuery(conn=SQL_connexion("ms069a"),
#'statement=query_I_SMOD_SERV_MD_CM_AG (
#'query="calcul_Nb_acte",
#'debut="2021-04-01",
#'fin="2022-03-31",
#'CodeActe=c('07122', '07237', '07800', '07089', '0780'), #Si CodeActe=NULL, le calcul se fait sur tout les actes facturés à l'intérieur de la période d'étude.
#'omni_spec="all",
#'catg_etab="all",
#'code_stat_decis="PAY-PPY")))
#'}

query_I_SMOD_SERV_MD_CM_AG <- function(query,debut, fin, diagn,CodeActe,omni_spec,catg_etab,code_stat_decis) {
  switch (query,
          "extraction_SMOD"={
            return(paste0(
              "SELECT

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

            BD_SMOD.DISP_NO_SEQ_DISP_BANLS AS NumDispSp,  --Numéro du dispensateur
            BD_SMOD.SMOD_COD_ROLE, --Code des rôles selon l'entente
            BD_SMOD.SMOD_COD_SPEC AS SMOD_COD_SPEC, --Spécialité du dispensateur (SMOD)
            BD_SMOD.DISP_NO_SEQ_DISP_REFNT_BANLS AS NumDispRef,  --Numéro du médecin référent



            BD_SMOD.SMOD_COD_ENTEN AS SMOD_COD_ENTEN,  --Code de l'entente
            BD_SMOD.SMOD_NO_ETAB_USUEL AS Num_ETAB_USUEL,  --Ancien numéro de l'établissement
            BD_SMOD.ETAB_COD_SECT_ACTIV_ETAB AS ETAB_COD_SECT_ACTIV_ETAB  --secteur d'activité

            FROM Prod.I_SMOD_SERV_MD_CM AS BD_SMOD
            WHERE BD_SMOD.SMOD_DAT_SERV BETWEEN '",debut,"' AND '",fin,"'\n",
              query_SQL_CodeActe.where_SMOD_COD_STA_DECIS (code_stat_decis),
              query_SQL_CodeActe.where_SMOD_COD_ACTE (CodeActe),
              "/*Cette exclusion est toujours importante à considérer, car les actes qui représentent des forfaits ne correspondent pas à des consultations,
            mais, plutôt à des suppléments monétaires associés à une demande de facturation */
            AND BD_SMOD.SMOD_COD_ACTE NOT IN (
									SELECT DISTINCT NMOD_COD_ACTE
									FROM Prod.V_RPERT_COD_ACTE
									WHERE NMOD_COD_GRP_ACTE IN (170, 180, 181, 182, 183, 187, 195, 196, 197, 198, 199, 854))

            ORDER BY 1,2,3,4"))
          },
          "extraction_SMOD_combin"={
            return(paste0(
              "SELECT

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
            /*On sélectionne la dernière adresse du bénéficiaire durant la période sélectionnée.
            Autres méthodes :
            1) cibler la première adresse de la période
            2) cibler l'adresse dans laquelle le BEN a résidé le plus longtemps durant la période
            3) cibler l'adresse à une date fixe (p. ex. 31 mars, 1 juillet, mi-année)*/

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

            WHERE BD_SMOD.SMOD_DAT_SERV BETWEEN '",debut,"' AND '",fin,"'\n",
              query_SQL_CodeActe.where_SMOD_COD_STA_DECIS (code_stat_decis),
              query_SQL_CodeActe.where_SMOD_COD_ACTE (CodeActe),
              query_SQL_CodeActe.where_SMOD_COD_SPEC(omni_spec),
              query_SQL_CodeActe.where_ETAB_COD_CATG_ETAB_EI(catg_etab),
              "/*Cette exclusion est toujours importante à considérer, car les actes qui représentent des forfaits ne correspondent pas à des consultations,
            mais, plutôt à des suppléments monétaires associés à une demande de facturation */
            AND BD_SMOD.SMOD_COD_ACTE NOT IN (
									SELECT DISTINCT NMOD_COD_ACTE
									FROM Prod.V_RPERT_COD_ACTE
									WHERE NMOD_COD_GRP_ACTE IN (170, 180, 181, 182, 183, 187, 195, 196, 197, 198, 199, 854))

            ORDER BY 1,2,3,4"
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
              "where BD_SMOD.SMOD_DAT_SERV between '",debut,"' and '",fin,"'\n",
              query_SQL_CodeActe.where_SMOD_COD_STA_DECIS (code_stat_decis),
              "AND BD_SMOD.SMOD_COD_DIAGN_PRIMR like any (",qu(diagn),")\n",
              ") AS A order by 1,2;"
            )) },
          "date_visite"={
            return(paste0(
              "SELECT
                ID,
                Date_visite
                FROM(
                  SELECT DISTINCT
                    BD_SMOD.SMOD_NO_INDIV_BEN_BANLS AS ID,
                    BD_SMOD.SMOD_DAT_SERV AS Date_visite,
                    BD_SMOD.DISP_NO_SEQ_DISP_BANLS,
                    CASE WHEN BD_SMOD.SMOD_NO_ETAB_USUEL IS NULL THEN 99999 ELSE BD_SMOD.SMOD_NO_ETAB_USUEL END AS NO_ETAB_USUEL

                    FROM Prod.I_SMOD_SERV_MD_CM AS BD_SMOD

                    LEFT JOIN (
                      SELECT
                      BD_Etab.ETAB_NO_ETAB_USUEL,
                      BD_Etab.ETAB_NO_ETAB,
                      BD_Etab.ETAB_COD_POSTL,
                      BD_Decoup_Etab.LGEO_COD_RSS,
                      BD_Decoup_Etab.LGEO_COD_TERRI_RLS,
                      BD_Catg_etab.ETAB_COD_CATG_ETAB_EI,
                      BD_NomCatg_etab.ETAB_NOM_CATG_ETAB_EI

                      FROM Prod.V_ETAB_USUEL_DERN AS BD_Etab
                      LEFT JOIN Prod.V_DECOU_GEO_POSTL_DERN_CP AS BD_Decoup_Etab
                      ON BD_Etab.ETAB_COD_POSTL=BD_Decoup_Etab.LGEO_COD_POSTL

                      LEFT JOIN (
                        SELECT DISTINCT
                        ETAB_NO_ETAB,
                        ETAB_COD_CATG_ETAB_EI

                        FROM V_ETAB_USUEL
                        QUALIFY Row_Number()Over (PARTITION BY ETAB_NO_ETAB ORDER BY ETAB_DD_ETAB_USUEL DESC)=1) AS BD_Catg_etab
                      ON BD_Etab.ETAB_NO_ETAB=BD_Catg_etab.ETAB_NO_ETAB
                      LEFT JOIN Prod.V_COD_CATG_ETAB_EI AS BD_NomCatg_etab
                      ON BD_Catg_etab.ETAB_COD_CATG_ETAB_EI=BD_NomCatg_etab.ETAB_COD_CATG_ETAB_EI
                    ) AS BD_Etab
                    ON BD_SMOD.SMOD_NO_ETAB_USUEL=BD_Etab.ETAB_NO_ETAB_USUEL
                    WHERE BD_SMOD.SMOD_DAT_SERV BETWEEN '",debut,"' AND '",fin,"'\n",
              query_SQL_CodeActe.where_SMOD_COD_ACTE (CodeActe),
              query_SQL_CodeActe.where_SMOD_COD_STA_DECIS (code_stat_decis),
              query_SQL_CodeActe.where_SMOD_COD_SPEC(omni_spec),
              query_SQL_CodeActe.where_ETAB_COD_CATG_ETAB_EI(catg_etab),
              "AND BD_SMOD.SMOD_COD_ACTE NOT IN (
                      SELECT DISTINCT
                      NMOD_COD_ACTE
                      FROM Prod.V_RPERT_COD_ACTE
                      WHERE NMOD_COD_GRP_ACTE IN (170, 180, 181, 182, 183, 187, 195, 196, 197, 198, 199, 854))

                  ) AS A

                ORDER BY 1,2"))
          },
          "calcul_Nb_acte"={
            return(paste0(
              "SELECT
            ID,
            Sum(acte) AS nb_actes
            FROM (
            SELECT DISTINCT
            BD_SMOD.SMOD_NO_INDIV_BEN_BANLS AS ID,
            BD_SMOD.SMOD_DAT_SERV AS Date_visite,
            BD_SMOD.DISP_NO_SEQ_DISP_BANLS,
            CASE WHEN BD_SMOD.SMOD_NO_ETAB_USUEL IS NULL THEN 99999 ELSE BD_SMOD.SMOD_NO_ETAB_USUEL END AS NO_ETAB_USUEL,
            1 AS acte
            FROM Prod.I_SMOD_SERV_MD_CM AS BD_SMOD

             LEFT JOIN (
                      SELECT
                      BD_Etab.ETAB_NO_ETAB_USUEL,
                      BD_Etab.ETAB_NO_ETAB,
                      BD_Etab.ETAB_COD_POSTL,
                      BD_Decoup_Etab.LGEO_COD_RSS,
                      BD_Decoup_Etab.LGEO_COD_TERRI_RLS,
                      BD_Catg_etab.ETAB_COD_CATG_ETAB_EI,
                      BD_NomCatg_etab.ETAB_NOM_CATG_ETAB_EI

                      FROM Prod.V_ETAB_USUEL_DERN AS BD_Etab
                      LEFT JOIN Prod.V_DECOU_GEO_POSTL_DERN_CP AS BD_Decoup_Etab
                      ON BD_Etab.ETAB_COD_POSTL=BD_Decoup_Etab.LGEO_COD_POSTL

                      LEFT JOIN (
                        SELECT DISTINCT
                        ETAB_NO_ETAB,
                        ETAB_COD_CATG_ETAB_EI

                        FROM V_ETAB_USUEL
                        QUALIFY Row_Number()Over (PARTITION BY ETAB_NO_ETAB ORDER BY ETAB_DD_ETAB_USUEL DESC)=1) AS BD_Catg_etab
                      ON BD_Etab.ETAB_NO_ETAB=BD_Catg_etab.ETAB_NO_ETAB
                      LEFT JOIN Prod.V_COD_CATG_ETAB_EI AS BD_NomCatg_etab
                      ON BD_Catg_etab.ETAB_COD_CATG_ETAB_EI=BD_NomCatg_etab.ETAB_COD_CATG_ETAB_EI
                    ) AS BD_Etab
                    ON BD_SMOD.SMOD_NO_ETAB_USUEL=BD_Etab.ETAB_NO_ETAB_USUEL

            WHERE BD_SMOD.SMOD_DAT_SERV BETWEEN '",debut,"' AND '",fin,"'\n",
              query_SQL_CodeActe.where_SMOD_COD_STA_DECIS (code_stat_decis),
              query_SQL_CodeActe.where_SMOD_COD_ACTE (CodeActe),
              query_SQL_CodeActe.where_SMOD_COD_SPEC(omni_spec),
              query_SQL_CodeActe.where_ETAB_COD_CATG_ETAB_EI(catg_etab),
              "AND SMOD_COD_ACTE NOT IN (
            SELECT DISTINCT
            NMOD_COD_ACTE
            FROM Prod.V_RPERT_COD_ACTE
            WHERE NMOD_COD_GRP_ACTE IN (170, 180, 181, 182, 183, 187, 195, 196, 197, 198, 199, 854))
            ) AS A
            GROUP BY ID
            ORDER BY ID"
            ))}
          )
}

#' @title query_SQL_CodeActe.where_SMOD_COD_SPEC
#' @description Section `where` du code SQL où on demande les actes facturés par les omipraticiens et/ou spécialistes.
#' @encoding UTF-8
#' @keywords internal
query_SQL_CodeActe.where_SMOD_COD_SPEC <- function(omni_spec) {
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


#' @title query_SQL_CodeActe.where_SMOD_COD_ACTE
#' @description Section `where` du code SQL où on demande des actes spécifiques et tous actes confondus.
#' @encoding UTF-8
#' @keywords internal
query_SQL_CodeActe.where_SMOD_COD_ACTE <- function(CodeActe) {
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

#' @title query_SQL_CodeActe.where_SMOD_COD_STA_DECIS
#' @description Section `where` du code SQL où on demande le Code de statu de décision.
#' @encoding UTF-8
#' @keywords internal
query_SQL_CodeActe.where_SMOD_COD_STA_DECIS <- function(code_stat_decis) {
  switch(code_stat_decis,
         "PAY" = {
           return(paste0(
             indent(),
             "AND BD_SMOD.SMOD_COD_STA_DECIS IN ('PAY')\n"
           ))
         },
         "PPY" = {
           return(paste0(
             indent(),
             "AND BD_SMOD.SMOD_COD_STA_DECIS IN ('PPY')\n"
           ))
         },
         "PAY-PPY" = {
           return(paste0(
             indent(),
             "AND BD_SMOD.SMOD_COD_STA_DECIS IN ('PAY','PPY')\n"
           ))
         })
}

#' @title query_SQL_CodeActe.where_ETAB_COD_CATG_ETAB_EI
#' @description Section `where` du code SQL où on demande les visites en ambulatoire ou dans d'autres établissements.
#' @encoding UTF-8
#' @keywords internal
query_SQL_CodeActe.where_ETAB_COD_CATG_ETAB_EI <- function(catg_etab) {
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

