
plot.sPLANN <- function(x, n.groups=5, pro.time=NULL, newdata=NULL, ...){
  
  if(is.null(pro.time)){ pro.time <- median(x$y[,1]) }
  
  if(is.null(newdata))
  {
    cova <-data.frame(x$x)
    colnames(cova) <- x$coefnames
    time <- x$y[,1];  event <- x$y[,2]
    }
  
  if(!is.null(newdata))
  {
    if(!is.data.frame(newdata)) stop("Argument 'newdata' must be a data frame")
    
    indic <- gsub("\\+", "", attr(terms(x$formula), "term.labels") ) %in% names(newdata) 
    if( sum(!indic) > 0 ) stop("Missing predictor in the data frame")
    
    cova <- data.frame(newdata[,gsub("\\+", "", attr(terms(x$formula), "term.labels"))])
    time <- newdata[,as.character(x$formula[[2]][2])]
    event <- newdata[,as.character(x$formula[[2]][3])]
  }
  
  .pred <- predict(x, newdata=cova, newtimes=pro.time)$predictions
  .pred <- .pred[,-1]
  
  .grps <- as.numeric(cut(.pred,
                         breaks = c(-Inf, quantile(.pred, seq(1/n.groups, 1, 1/n.groups))),
                         labels = 1:n.groups))
  
  .est <- sapply(1:n.groups, FUN = function(x) { mean(.pred[.grps==x]) } )
  
  .data <- data.frame(time = time, event = event, grps = .grps)
  
  .survfit <- summary(survfit(Surv(time, event) ~ grps, data=.data))
  
  .obs <- sapply(1:n.groups, FUN = function(x) {
    .indic <- sum(as.numeric(.survfit$strata)==x & .survfit$time<=pro.time)
    .survfit$surv[ .indic ] } )
  
  .lower <- sapply(1:n.groups, FUN = function(x) {
    .indic <- sum(as.numeric(.survfit$strata)==x & .survfit$time<=pro.time)
    .survfit$lower[ .indic ] } )
  
  .upper <- sapply(1:n.groups, FUN = function(x) {
    .indic <- sum(as.numeric(.survfit$strata)==x & .survfit$time<=pro.time)
    .survfit$upper[ .indic ] } )
  
  if(hasArg(cex)==FALSE) {cex <-1} else {cex <- list(...)$cex}
  if(hasArg(cex.lab)==FALSE) {cex.lab <- 1} else {cex.lab <- list(...)$cex.lab}
  if(hasArg(cex.axis)==FALSE) {cex.axis <- 1} else {cex.axis <- list(...)$cex.axis}
  if(hasArg(cex.main)==FALSE) {cex.main <- 1} else {cex.main <- list(...)$cex.main}
  if(hasArg(type)==FALSE) {type <- "b"} else {type <- list(...)$type}
  if(hasArg(col)==FALSE) {col <- 1} else {col <- list(...)$col}
  if(hasArg(lty)==FALSE) {lty <- 1} else {lty <- list(...)$lty}
  if(hasArg(lwd)==FALSE) {lwd <- 1} else {lwd <- list(...)$lwd}
  if(hasArg(pch)==FALSE) {pch <- 16} else {pch <- list(...)$pch}
  
  if(hasArg(ylim)==FALSE) {ylim <- c(0,1)} else {ylim <- list(...)$ylim}
  if(hasArg(xlim)==FALSE) {xlim  <- c(0,1)} else {xlim <- list(...)$xlim}
  
  if(hasArg(ylab)==FALSE) {ylab <- "Kaplan-Meier estimations"} else {ylab <- list(...)$ylab}
  if(hasArg(xlab)==FALSE) {xlab <- "sPLANN estimations"} else {xlab <- list(...)$xlab}
  if(hasArg(main)==FALSE) {main <- ""} else {main <- list(...)$main}
  
  plot(.est, .obs, cex = cex, cex.lab = cex.lab, cex.axis = cex.axis, cex.main = cex.main,
       type = type, col = col, lty = lty, lwd = lwd, main=main,
       pch = pch, ylim = ylim, xlim = xlim, ylab=ylab, xlab=xlab)
  
  abline(c(0,1), lty=2)
  
  segments(x0 = .est, y0 = .lower, x1 = .est, y1 = .upper, col = col, lwd = lwd)
}