#' @title qu
#'
#' @description La fonction `qu` permet de mettre de formater les codes d'acte et les diagnostics pour qu'ils soient laisible dans les rêquetes SQL.
#'
#' @param Dx Un vecteur incluant les codes d'acte ou les diagnostics.
#'
#' @return Chaîne de caractères sans les quotes ("").
#' @import data.table
#' @import odbc
#' @encoding UTF-8
#' @keywords internal
#' @export
#'
#' @examples
#'  \dontrun{
#' Dx<-c(07122, 07237, 07800, 07089, 0780)
#' qu(Dx)
#' }


qu<-function(Dx){
  Dx<-paste(shQuote(Dx, type="sh"), collapse=", ")
  Dx<-noquote(Dx)
  Dx<-gsub("'","",Dx)
  return(Dx)
}

