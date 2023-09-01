

#' xpose4 validation figures using xpose4
#'
#' @param run The run number /(e.g. like '001/)
#' @param stratify Stratify on item
#' @param log Log transforms data
#' @param idv Independent variable which can be changed
#' @param col Color
#' @param pch pch shape of markers lattice
#' @param cex cex size of markers lattice
#'
#' @return
#' @export
#' @import xpose4 gridExtra
#'


xp_figures <- function(run, stratify='', log = '', idv ='', col = 'black', pch = 16, cex = 1, sep_figs =''){
  newdb <- xpose4::xpose.data(run)
  if(!missing(idv)){change.xvardef(newdb,var='idv') <- idv}
  if(!missing(col)){
    col = col
  }


  # Pred versus DV
  gof1 <- xpose4::dv.vs.pred(newdb, col=col, smooth = T, pch=pch, type ="p",
             cex=cex, xlb=list("Population predictions"), ylb=list("Observations"), main = NULL)

  if(tolower(log) == 'y' | tolower(log) == 'yes' | tolower(log) == 'true' |
     tolower(log) == 't' ){
    gof1 <- xpose4::dv.vs.pred(newdb, col=col, smooth = T, pch=pch, type ="p", cex=cex,
                       logx = T, logy =T,
                       xlb=list("Population predictions"), ylb=list("Observations"), main = NULL)}

  if(!missing(stratify)){
    gof1 <- dv.vs.pred(newdb, col=col,by=stratify, smooth = T, pch=pch, type ="p",
                       cex=cex, xlb=list("Population predictions"), ylb=list("Observations"), main = NULL)

    if(tolower(log) == 'y' | tolower(log) == 'yes' | tolower(log) == 'true' |
       tolower(log) == 't' ){
      gof1 <- xpose4::dv.vs.pred(newdb, col=col, by=stratify, smooth = T, pch=pch, type ="p", cex=cex,
                         logx = T, logy =T,
            xlb=list("Population predictions"), ylb=list("Observations"), main = NULL)
    }
  }

  # Pred versus DV
  gof2 <- xpose4::dv.vs.ipred(newdb, col=col, smooth = T, pch=pch, type ="p",
                     cex=cex, xlb=list("Individual predictions"), ylb=list("Observations"), main = NULL)

  if(tolower(log) == 'y' | tolower(log) == 'yes' | tolower(log) == 'true' |
     tolower(log) == 't' ){
    gof2 <- xpose4::dv.vs.ipred(newdb, col=col, smooth = T, pch=pch, type ="p", cex=cex,
                       logx = T, logy =T,
                       xlb=list("Individual predictions"), ylb=list("Observations"), main = NULL)}

  if(!missing(stratify)){
    gof2 <- xpose4::dv.vs.ipred(newdb, col=col,by=stratify, smooth = T, pch=pch, type ="p",
                       cex=cex, xlb=list("Individual predictions"), ylb=list("Observations"), main = NULL)

    if(tolower(log) == 'y' | tolower(log) == 'yes' | tolower(log) == 'true' |
       tolower(log) == 't' ){
      gof2 <- xpose4::dv.vs.ipred(newdb, col=col, by=stratify, smooth = T, pch=pch, type ="p", cex=cex,
                         logx = T, logy =T,
                         xlb=list("Individual predictions"), ylb=list("Observations"), main = NULL)
    }
  }


  # CWRES versus idv
  gof3 <- xpose4::cwres.vs.idv(newdb, col=col, smooth = T, pch=pch, type ="p",
               cex=cex, xlb=list("Independent variable"), ylb=list("Conditional weighted residuals"), main = NULL)

  if(!missing(stratify)){
    gof3 <- xpose4::cwres.vs.idv(newdb, col=col, smooth = T, pch=pch, type ="p", by= stratify,
               cex=cex, xlb=list("Independent variable"), ylb=list("Conditional weighted residuals"), main = NULL)
    }

  # CWRES versus pred
  gof4 <- xpose4::cwres.vs.pred(newdb, col=col, smooth = T, pch=pch, type ="p",
                               cex=cex, xlb=list("Predictions"), ylb=list("Conditional weighted residuals"), main = NULL)

  if(tolower(log) == 'y' | tolower(log) == 'yes' | tolower(log) == 'true' | tolower(log) == 't' ){
    gof4 <- xpose4::cwres.vs.pred(newdb, col=col, smooth = T, pch=pch, type ="p",
               cex=cex, xlb=list("Predictions"), ylb=list("Conditional weighted residuals"), logx=T, main = NULL)
    }

  if(!missing(stratify)){
    gof4 <- xpose4::cwres.vs.pred(newdb, col=col, smooth = T, pch=pch, type ="p", by= stratify,
                                 cex=cex, xlb=list("Predictions"), ylb=list("Conditional weighted residuals"), main = NULL)

        if(tolower(log) == 'y' | tolower(log) == 'yes' | tolower(log) == 'true' | tolower(log) == 't' ){
          gof4 <- xpose4::cwres.vs.pred(newdb, col=col, smooth = T, pch=pch, type ="p",by= stratify,
                                    cex=cex, xlb=list("Predictions"), ylb=list("Conditional weighted residuals"), logx=T, main = NULL)
      }
    }


  gof_figures <- gridExtra::grid.arrange(gof1,gof2,gof3,gof4)

  if(tolower(sep_figs) == 'y' | tolower(sep_figs) == 'yes' | tolower(sep_figs) == 'true' | tolower(sep_figs) == 't' ){
    gof_figures <- list(dv_vs_pred=gof1,dv_vs_ipred=gof2, cwres_vs_idv=gof3, cwres_vs_pred = gof4)
  }

  return(gof_figures)
}
