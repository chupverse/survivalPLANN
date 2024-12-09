rsPLANN <- function(formula, data, pro.time=NULL, inter, size= 32, decay=0.01,
                    maxit=100, MaxNWts=10000, trace=FALSE,
                    ratetable) 
{
  
  ####### check errors
  if (missing(formula)) stop("a formula argument is required")
  if (missing(data)) stop("a data argument is required")
  if (missing(inter)) stop("an inter argument is required")
  if (as.character(class(formula)) != "formula") stop("The formula argument must be a formula")
  if (as.character(class(data)) != "data.frame") stop("The second argument must be a data frame")
  if (as.character(class(inter)) != "numeric") stop("The inter argument must be numeric")
  
  if (missing(ratetable)) stop("a ratetable argument is required")
  
  
  if (length(dim(ratetable))!=3) stop("The life table must have 3 dimensions: age, year, sex")
  if (dim(ratetable)[3]!=2) stop("The life table must have 3 dimensions: age, year, sex")
  
  ####### data management
  
  all_terms <- attr(terms(formula), "term.labels")
  ratetable_terms <- grep("^ratetable\\(", all_terms, value = TRUE)
  if(length(ratetable_terms) == 0) stop("Error: The formula must contain a ratetable() term.")
  if(length(ratetable_terms)>1) stop("More than one 'ratetable' term found in  the formula.")
  
  formula_string <- as.character(formula)
  rhs <- gsub("\\+ ratetable\\(.*?\\)", "", formula_string[3]) 
  formula_string <- paste(formula_string[2], "~", rhs)
  formula_w_ratetable <- formula
  formula <- as.formula(formula_string)
  extract_vars <- function(term) {
    var_string <- sub("^[^\\(]+\\((.*)\\)$", "\\1", term)
    vars <- trimws(unlist(strsplit(var_string, ",")))
    return(vars)
  }
  assign_ratetable_vars <- function(vars) {
    age <- year <- sex <- NULL
    for (var in vars) {
      if (grepl("age = ", var)) {
        age <- sub("age = ", "", var)
      } else if (grepl("year = ", var)) {
        year <- sub("year = ", "", var)
      } else if (grepl("sex = ", var)) {
        sex <- sub("sex = ", "", var)
      }
    }
    unnamed_vars <- setdiff(vars, c(age, sex, year))
    if (length(unnamed_vars) > 0) {
      if (is.null(age) && length(unnamed_vars) >= 1) age <- unnamed_vars[1]
      if (is.null(year) && length(unnamed_vars) >= 2) year <- unnamed_vars[2]
      if (is.null(sex) && length(unnamed_vars) >= 3) sex <- unnamed_vars[3]
    }
    return(list(age = age, year = year, sex = sex))
  }
  ratetable_vars <- assign_ratetable_vars(unlist(lapply(ratetable_terms, extract_vars)))
  age <- data[,ratetable_vars$age]  
  year <- data[,ratetable_vars$year]
  sex <- data[,ratetable_vars$sex] 
  
  ####### data management
  
  splann <- sPLANN(formula, data=data, pro.time=pro.time, inter=inter, 
                   size=size, decay=decay, maxit=maxit, MaxNWts=MaxNWts)
  
  predO <- predict(splann, newtimes=splann$intervals)
  
  times <- predO$times
  
  exphaz <- function(x,age,sex,year) { survivalNET::expectedhaz(ratetable, age=age, sex=sex, year=year, time=x)}
  
  survO <- as.matrix(predO$predictions)
  dimnames(survO) <- NULL
  
  N <- dim(survO)[1]
  P <- dim(survO)[2]
  
  hP <- matrix(-99, ncol=length(times), nrow=N)
  
  for (i in 1:N) # @Thomas : merci de voir si tu augmenter la vitesse du calcul de hP
  {
    hP[i,] <- sapply(times, FUN="exphaz", age=age[i],
                     sex=sex[i], year=year[i]) * splann$inter
  }
  
  hcumO <- -1*log(survO)
  hinstO <- hcumO[,2:length(times)] - hcumO[,1:(length(times)-1)]
  hinstO[hinstO==Inf] <- NA
  
  for (i in 1:N)
  {
    if(sum(is.na(survO[i,]))>0)
    {
      hinstO[i,is.na(hinstO[i,])] <- hinstO[i,!is.na(hinstO[i,])][sum(!is.na(hinstO[i,]))]
    }
  }
  
  distOa <- t(as.matrix(cumsum(data.frame(t(survO[,-P] * hinstO )))))
  distOb <- t(as.matrix(cumsum(data.frame(t(survO[,-1] * hinstO )))))
  distO <- cbind(rep(0, N), (distOa + distOb)/2)
  
  hinstP <- hP[,1:(length(times)-1)]
  distPa <- t(as.matrix(cumsum(data.frame(t(survO[,-P] * hinstP )))))
  distPb <- t(as.matrix(cumsum(data.frame(t(survO[,-1] * hinstP )))))
  distP <- cbind(rep(0, N), (distPa + distPb)/2)
  
  hinstE <- hinstO - hinstP
  distEa <- t(as.matrix(cumsum(data.frame(t(survO[,-P] * hinstE )))))
  distEb <- t(as.matrix(cumsum(data.frame(t(survO[,-1] * hinstE )))))
  distE <- cbind(rep(0, N), (distEa + distEb)/2)
  
  distP <- distP * (1-survO)/distO
  distE <- distE * (1-survO)/distO
  
  distP[survO==1] <- 0
  distE[survO==1] <- 0
  
  distO <- distP + distE
  
  distPinf <- distP[,P]
  distEinf <- distE[,P]
  
  estimPcure <- (round(distPinf + distEinf, 10) == 1)
  
  survP <- cbind(rep(1, N), exp(-t(as.matrix(cumsum(data.frame(t(hinstP)))))))
  survU <- cbind(rep(1, N), exp(-t(as.matrix(cumsum(data.frame(t(hinstE)))))))
  
  Pcure <- distPinf / (distPinf + (1-distPinf) * survU)
  
  sumS <- apply((1-distO), FUN="sum", MARGIN=2)
  
  SlE <- (1-distO) * cbind(rep(0, N), hinstE)
  sumSlE <- apply(SlE, FUN="sum", MARGIN=2)
  lambda_C <- sumSlE/sumS
  #Lambda_C <- cumsum(lambda_C)
  
  SlP <- (1-distO) * cbind(rep(0, N), hinstP)
  sumSlP <- apply(SlP, FUN="sum", MARGIN=2)
  lambda_P <- sumSlP/sumS
  #Lambda_P <- cumsum(lambda_P)
  
  # warning -> NA pour tCure ...
  
  ## on récpère intervalles où tombent les temps d'evt et de censure
  event_time <- findInterval(splann$y[,1], splann$interval,left.open = TRUE)
  
  #on récupère le risque instantané indivudel observé au temps d'evt/censure
  ind_hinstO <- sapply(1:(dim(splann$x)[1]), function(i) {
    hinstO[i, event_time[i]]
  })
  #et la survie observée individuelle au temps d'evt/censure 
  #on enlève la première colonne de 1-distO car c'est une ligne de 1 rajoutée et elle correspond 
  #a la survie en t=0, mais dans le premier intervalle, la valeur de survie est déjà descendue comme 
  #c'est entre (0:t_1] 
  ind_survO <-  sapply(1:(dim(splann$x)[1]), function(i) {
    (1-distO)[,-1][i, event_time[i]]
  })

    loglik <- sum(splann$y[,2]*log(ind_hinstO)+log(ind_survO))
  
  res <- list(formula = formula_w_ratetable,
              data = data,
              ratetable = ratetable,
              ays = data.frame(age = age, year = year, sex = sex),
              pro.time = pro.time,
              fitsurvivalnet = splann,
              times = times,
              loglik = loglik,
              ipredictions = list(survival_O=1-distO,
                                  survival_P=survP,
                                  survival_R=(1-distO)/survP,
                                  survival_E=survU, # remarque : S(1-distO)/survP = survU
                                  CIF_C = distE, 
                                  CIF_P = distP, 
                                  maxCIF_P = distPinf,
                                  cure = Pcure),
              mpredictions = list(survival_O = apply((1-distO), FUN="mean", MARGIN=2),
                                  survival_P = apply(survP, FUN="mean", MARGIN=2),
                                  survival_R = apply((1-distO), FUN="mean", MARGIN=2)/apply(survP, FUN="mean", MARGIN=2),
                                  survival_E = apply((1-distO)/survP, FUN="mean", MARGIN=2),
                                  CIF_C =  apply(distE, FUN="mean", MARGIN=2),
                                  CIF_P =  apply(distP, FUN="mean", MARGIN=2),   
                                  cure = apply(Pcure, FUN="mean", MARGIN=2),
                                  hazard_P = lambda_P,
                                  hazard_C = lambda_C
              )
  )
  
  class(res) <- "rsPLANN"
  return(res)
}