
predict.sPLANN <- function(object, newdata = NULL, newtimes = NULL, ...)
{ 
  
  intervals = object$intervals
  formula = object$formula
  time = object$y[,1]
  pro.time = object$pro.time
  if(!is.null(newdata))
  {
    if(!is.data.frame(newdata)) stop("Argument 'newdata' must be a data frame")
    indic <- gsub("\\+", "", attr(terms(formula), "term.labels") ) %in% names(newdata) 
    if( sum(!indic) > 0 ) stop("Missing predictor in the data frame")
  }
  
  if(is.null(newdata))  { newdata <- object$data[,-dim(object$data)[2]] }
  
  data_dupli <- newdata[, attr(terms(formula), "term.labels"), drop = FALSE ]
  data_dupli$id <- seq_len(nrow(data_dupli))
  data_dupli <- as.data.frame(data_dupli[rep(seq_len(nrow(data_dupli)), each = length(intervals)-1),])
  if(length(attr(terms(formula), "term.labels") ) == 1 && attr(terms(formula), "term.labels") != "1" ){
    names(data_dupli)[1] <- attr(terms(formula), "term.labels")    
    
    orig_row_ids <- seq_len(nrow(newdata))  
    replicated_row_ids <- rep(orig_row_ids, each = length(intervals) - 1) 
    
    suffix <- unlist(lapply(table(replicated_row_ids), function(n) c("", paste0(".", seq_len(n-1)))))
    
    rownames(data_dupli) <- paste0(replicated_row_ids, suffix)
  }
  
  data_dupli$Intervals <- ave(1:nrow(data_dupli),
                              data_dupli$id,
                              FUN = seq_along)
  
  
  predictions <- predict(object$fitsurvivalnet, newdata = data_dupli, type ="raw", ...)
  
  grouped_df <- split(cbind(data_dupli,predictions),
                      rep(1:ceiling(nrow(data_dupli)/(length(intervals)-1)),
                          each=length(intervals)-1, length.out=nrow(data_dupli)))
  
  
  result_group <- lapply(grouped_df, function(group) {
    group$Survie <- cumprod(1-group$predictions)
    return(group)
  })
  result_df <- do.call(rbind, result_group)$Survie
  
  predictions <- as.data.frame(matrix(result_df, ncol = length(intervals)-1, byrow = TRUE))
  # predictions <- cbind(rep(1,dim(predictions)[1]), predictions)
  
  ints_names <- c()
  
  for( i in 1:(length(object$intervals)-1) ){
    ints_names <- c(ints_names, paste0("(",round(object$intervals[i], digits = 2), ";",
                                       round(object$intervals[i+1], digits = 2),"]"))
  }
  
  if(is.null(newtimes))  { 
    predictions <- unname(cbind(rep(1, dim(predictions)[1]), predictions))
    colnames(predictions) = c("0",ints_names)
    newtimes <- intervals
    
  }else{ 
    if(!is.vector(newtimes))stop("newtimes must be a vector")
    if(any(max(time, pro.time)<newtimes))warning("One or more values of 'newtimes' are  greater than the prognostic time . All predictions for those times are supposed to be NA.") #modif
    newtimes <- sort(newtimes[newtimes != 0])
    usable_times <- newtimes[newtimes <= pro.time]
    out_time <- newtimes[newtimes > pro.time]
    idx <- findInterval(usable_times, intervals, left.open = TRUE)
    predictions <- as.data.frame(predictions[,pmin(idx,length(intervals)-1)])
    col_names <- paste0(usable_times," in ",ints_names[idx])
    
    if(length(out_time)>0){
      more_times <- data.frame(matrix(NA, nrow = dim(predictions)[1], ncol = length(out_time) ))
      predictions <- unname(cbind(rep(1, dim(predictions)[1]), predictions, more_times))
      more_names <- c()
      for (i in 1:length(out_time)){
          
          more_names <- c(more_names, paste0(as.character(out_time[i]), " > ", round(pro.time, digits = 2) ))
      }
      
      colnames(predictions) = c("0",col_names, more_names)
    }else{
      
      predictions <- cbind(rep(1, dim(predictions)[1]), predictions)
      colnames(predictions) = c("0",col_names)
      
    }
    
    
    newtimes <- c(0, newtimes)
  }
  
  
  
  res <- list(times = newtimes, predictions = predictions)
  
  return(res)
}