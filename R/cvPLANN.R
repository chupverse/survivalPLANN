
cvPLANN <- function(formula, pro.time=NULL, eval.time = NULL, data, cv=10, inter=365.241/12, size = 32, decay = 0.01,
                    maxit = 100, MaxNWts=10000, metric = "ibs", stand = NULL)
{
  
  ####### check errors
  if (missing(formula)) stop("a formula argument is required")
  
  times <- as.character(formula[[2]][2])
  failures <- as.character(formula[[2]][3])
  
  all_terms <- attr(terms(formula), "term.labels")
  group_term <- grep("group\\(", all_terms, value = TRUE)
  CV <- setdiff(all_terms, c(group_term))

  cova <- as.matrix(data[, CV, drop = FALSE])
  
  for(i in colnames(cova)){ if(!is.numeric(cova[,i])) stop("All covariates must be numeric")  }
  
  extract_vars <- function(term) {
    var_string <- sub("^[^\\(]+\\((.*)\\)$", "\\1", term)
    vars <- trimws(unlist(strsplit(var_string, ",")))
    return(vars)
  }

  warn <- 0 
  col_warn <- c()
  
  for (col in CV) {
    
    unique_values <- unique(as.data.frame(cova)[[col]])
    
    if (length(unique_values) == 2 && !all(unique_values %in% c(0, 1))) {
      warn <- warn + 1  
      col_warn <- c(col_warn, col)
    }
    
  }
  if (warn > 0) {
    warning(paste(warn, "columns have exactly 2 modalities but are not 0 and 1. (",col_warn,"). Those
                  columns have been considered as quantitative variables."))
  }
  
    
  if(length(group_term) == 0){
    group = NULL
  }else{
    group <- unlist(lapply(group_term, extract_vars))
    if(!all(unique(data[,group]) %in% c(0, 1))) stop("The ", group," covariate can only take the values 0 or 1.") 
    }

  ####
  
  data.plann <- data[,c(times, failures, group, CV)]
  
  if (is.null(pro.time)) {pro.time <- max(data[data[, failures] == 1, times])}
  t.max <- min(pro.time, max(data[data[, failures] == 1, times]))
  if(is.null(eval.time)) {eval.time <- median(data[,times])}
  if (metric %in% c("bs", "bll", "ci", "auc", "ribs", "ribll") && eval.time > t.max)
    stop("eval.time must be <= min(pro.time, last event time)")  
  
  sample_id <- sample(nrow(data.plann))
  folds <- cut(seq(1,nrow(data.plann)), breaks=cv, labels=FALSE)
  folds_id <- folds[sample_id]
  data.plann$folds <- folds_id
  
  if(!is.null(group)){
     .outcome <- paste("Surv(", times, ",", failures, ")")
     .f <- as.formula( paste(.outcome, "~", paste( CV,  collapse = " + "), "+", group) )
  }else{
    .f <- formula
  }
  
  .time <- sort(unique(data.plann[,times]))
  .time <- .time[.time <= t.max]
  
  .grid <-  expand.grid(inter=inter, size=size, decay=decay, maxit=maxit, MaxNWts=MaxNWts)
  
  .CVtune<-vector("list",cv*dim(.grid)[1])
  
  if (!is.null(stand)) {
        if (!is.character(stand))
      stop("'standardize' must be a character vector of variable names.")
    missing_vars <- setdiff(stand, names(data.plann))
    if (length(missing_vars) > 0)
      stop("The following variables are not in 'data': ",
        paste(missing_vars, collapse = ", "))
  }
  
  l<-1
  for (k in 1:cv){
    trainSt <- data.plann[data.plann$folds != k, ]
    validSt <- data.plann[data.plann$folds == k, ]
    if (!is.null(stand)) {
      muSt <- sapply(trainSt[, stand, drop = FALSE], mean)
      sigmaSt <- sapply(trainSt[, stand, drop = FALSE], sd)
      trainSt[, stand] <- scale(trainSt[, stand, drop = FALSE], center = muSt, scale = sigmaSt)
      validSt[, stand] <- scale(validSt[, stand, drop = FALSE],center = muSt,scale = sigmaSt)
    }
    for (j in 1:dim(.grid)[1]){
      .CVtune[[l]]<-list(train=trainSt, valid=validSt, grid=.grid[j,])
      l=l+1
    }
  }
  
  plann.time.par<-function(xx, times, failures, group, CV, newtimes){
    
    inter=xx$grid$inter
    size=xx$grid$size
    decay=xx$grid$decay
    maxit=xx$grid$maxit
    MaxNWts=xx$grid$MaxNWts
    
    data=xx$train
    newdata=xx$valid
    
    if(!(is.null(group))){
      .data <- data[,c(times, failures, group, CV)]}   else{
        .data <- data[,c(times, failures, CV)] }
    
    if(!is.null(group)){
      .outcome <- paste("Surv(", times, ",", failures, ")")
      .f <- as.formula( paste(.outcome, "~", paste( CV,  collapse = " + "), "+", group) )
    }else{
      .f <- formula
    }
    
    .plann <- sPLANN(.f, data = .data, pro.time= pro.time,
                     inter = inter, size = size, decay = decay,  maxit = maxit, MaxNWts = MaxNWts)
    
    .pred.temp <- predict(.plann, newdata=newdata)
    .pred <- .pred.temp$predictions
    .time.plann <- .pred.temp$times

    
    if(!is.null(newtimes)) {
      .pred.plann <- .pred
      .time.plann <- .time.plann
      
      idx <- findInterval(newtimes, .time.plann, left.open = TRUE)
      .pred <- .pred.plann[, pmin(idx + 1, ncol(.pred.plann))]
      
      .time <- newtimes
    }
    
    return(as.matrix(.pred))
  }
  
  .preFIT<-list()
  .preFIT<-lapply(.CVtune, plann.time.par, times=times, failures=failures,
                  group=group, CV = CV, newtimes=.time)
  
  .FitCV <- replicate(dim(.grid)[1], matrix(NA, nrow = length(data[,times]),
                                            ncol = length(.time)), simplify=F)
  l<-1
  for (k in 1:cv){
    for (j in 1:dim(.grid)[1]){
      .FitCV[[j]][data.plann$folds==k,] <- .preFIT[[l]]
      l<-l+1
    }
  }
  
  plann.best.measure <- function(prediction.matrix, formula, data, prediction.times){
    .times <- as.character(formula[[2]][2])
    .failures <- as.character(formula[[2]][3])
    .outcome <- paste("Surv(", .times, ",", .failures, ")")
    .predformula <- as.formula(paste(.outcome, "~ 1")) 
    return(metrics(formula = .predformula, prediction.matrix = 
                    as.matrix(as.data.frame(prediction.matrix)),
                   data=data, prediction.times=prediction.times,
                   pro.time=eval.time, metric= metric))
  }
  
  .measure<-sapply(.FitCV, plann.best.measure, formula = formula , data=data.plann, prediction.times=.time)
  
  .res <- data.frame(inter = .grid[,1], size = .grid[,2], decay=.grid[,3],
                     maxit = .grid[,4], MaxNWts = .grid[,5], measure = .measure)
  if(metric %in% c("ci", "auc")){
    .maxi<-.res[which(.res$measure==max(.res$measure, na.rm=TRUE) & is.na(.res$measure)==FALSE),]
    .maxi<-.maxi[1,]
  }
  if(metric %in% c("ibs", "bs", "ribs", "ibll", "bll","ribll")){
    .maxi<-.res[which(.res$measure==min(.res$measure, na.rm=TRUE) & is.na(.res$measure)==FALSE),]
    .maxi<-.maxi[1,]
    }
  return( list(optimal=list(inter=.maxi$inter,
                            size=.maxi$size,
                            decay=.maxi$decay,
                            maxit=.maxi$maxit,
                            MaxNWts=.maxi$MaxNWts),
               results=.res ))
}