

#' xpose4 validation figures using xpose4
#'
#' @param run
#' @param stratify
#'
#' @return
#' @export
#' @import xpose4
#'
#' @examples
#'


xp_figures <- function(run, stratify='', log = '', idv ='', col = 'black', pch = 16, cex = 1){
  newdb <- xpose.data(run)
  if(!missing(idv)){change.xvardef(newdb,var='idv') <- idv}
  if(!missing(col)){
    col = col
  }


  # Pred versus DV
  gof1 <- dv.vs.pred(newdb, col=col, smooth = T, pch=pch, type ="p",
             cex=cex, xlb=list("Population predictions"), ylb=list("Observations"), main = NULL)

  if(tolower(log) == 'y' | tolower(log) == 'yes' | tolower(log) == 'true' |
     tolower(log) == 't' ){
    gof1 <- dv.vs.pred(newdb, col=col, smooth = T, pch=pch, type ="p", cex=cex,
                       logx = T, logy =T,
                       xlb=list("Population predictions"), ylb=list("Observations"), main = NULL)}

  if(!missing(stratify)){
    gof1 <- dv.vs.pred(newdb, col=col,by=stratify, smooth = T, pch=pch, type ="p",
                       cex=cex, xlb=list("Population predictions"), ylb=list("Observations"), main = NULL)

    if(tolower(log) == 'y' | tolower(log) == 'yes' | tolower(log) == 'true' |
       tolower(log) == 't' ){
      gof1 <- dv.vs.pred(newdb, col=col, by=stratify, smooth = T, pch=pch, type ="p", cex=cex,
                         logx = T, logy =T,
            xlb=list("Population predictions"), ylb=list("Observations"), main = NULL)
    }
  }

  # Pred versus DV
  gof2 <- dv.vs.ipred(newdb, col=col, smooth = T, pch=pch, type ="p",
                     cex=cex, xlb=list("Individual predictions"), ylb=list("Observations"), main = NULL)

  if(tolower(log) == 'y' | tolower(log) == 'yes' | tolower(log) == 'true' |
     tolower(log) == 't' ){
    gof2 <- dv.vs.ipred(newdb, col=col, smooth = T, pch=pch, type ="p", cex=cex,
                       logx = T, logy =T,
                       xlb=list("Individual predictions"), ylb=list("Observations"), main = NULL)}

  if(!missing(stratify)){
    gof2 <- dv.vs.ipred(newdb, col=col,by=stratify, smooth = T, pch=pch, type ="p",
                       cex=cex, xlb=list("Individual predictions"), ylb=list("Observations"), main = NULL)

    if(tolower(log) == 'y' | tolower(log) == 'yes' | tolower(log) == 'true' |
       tolower(log) == 't' ){
      gof2 <- dv.vs.ipred(newdb, col=col, by=stratify, smooth = T, pch=pch, type ="p", cex=cex,
                         logx = T, logy =T,
                         xlb=list("Individual predictions"), ylb=list("Observations"), main = NULL)
    }
  }

  gof_figures <- list(gof1,gof2)

  return(gof_figures)
}
