################################################################################
################################################################################
################################################################################

# simulate synthetic dataset according to 
# Random Utility Maximisation
# in simple unified matrix format

pls_simulate_mlogit_x <- function(n_prods, F, N, sigma2, seed) {
  
  set.seed(seed)
  n_periods <- length(n_prods)
  
  # preferences
  beta <- rnorm(F,0,sigma2)  
  X <- matrix(rnorm(sum(n_prods)*F), ncol = F)  
  
  # index
  period_start <- cumsum(c(1,n_prods))[- (n_periods + 1)]
  period_stop <- cumsum(c(n_prods))
  
  # calculate choice probabilities
  Pni <- lapply(1:n_periods, function(xx) {
    
    start <- period_start[xx]
    stop <- period_stop[xx]
    
    pls_pni_mlogit_rum(X[start:stop,, drop = FALSE], beta)
  })
  
  y <- lapply(1:n_periods, function(xx) {
    
    y <- sample(as.factor(1:n_prods[xx]), prob = Pni[[xx]], size = N + as.integer(rexp(1,.001)), replace = TRUE)
    y <- as.data.frame(table(y))
    y <- y$Freq
    
    return(y)  
    
  })
  
  Pni <- do.call(c, Pni)
  y <- do.call(c, y)
  
  return(list(n_periods = n_periods,
              n_prods = n_prods, 
              period_start = period_start, 
              period_stop = period_stop, 
              F = F, 
              y = y, 
              X = X,
              beta = beta))
}


pls_data_unif_to_list <- function(dta.unif) {
  
  y <- dta.unif[,"sales"]
  period <- dta.unif[,"period"]
  X <- dta.unif[,-c(1,2)]
  
  tmp <- split(dta.unif[,"period"], f = period)
  n_periods <- length(tmp)
  n_prods <- sapply(tmp, length)
  
  # index
  period_start <- as.integer(cumsum(c(1,n_prods))[- (n_periods + 1)])
  period_stop <- as.integer(cumsum(c(n_prods)))
  
  return(list(n_periods = n_periods,
              n_prods = n_prods, 
              period_start = period_start, 
              period_stop = period_stop, 
              F = ncol(X), 
              y = y, 
              X = X))
  
}




################################################################################
################################################################################
################################################################################

# simulate synthetic dataset according to 
# Random Utility Maximisation

pls_pni_mlogit_rum <- function(X,beta) {
  
  F <- ncol(X)
  J <- nrow(X)
  
  # calculate product utilities
  V <- X%*%beta
  
  # calculate pni
  Pni <- exp(V)
  Pni <- Pni/sum(Pni)
  
  return(Pni)
}

pls_simulate_mlogit_rum <- function(J,F,N,sigma2,seed) {
  
  set.seed(seed)
  
  # preferences
  beta <- rnorm(F,0,sigma2)  
  
  # product features
  X <- matrix(rnorm(J*F),ncol=F,nrow=J)  
  
  # calculate choice probabilities
  Pni <- pls_pni_mlogit_rum(X,beta) 
  
  # sample sales
  y <- base::sample(1:J,N,TRUE,Pni)
  
  # expand to full shelf:
  tab.obs <- as.data.frame(table(y))
  names(tab.obs) <- c("product","n")
  tab.prod <- data.frame(product = 1:J)
  tab.obs <- merge(tab.prod,tab.obs,all.x=TRUE,all.y=TRUE)
  tab.obs[is.na(tab.obs$n),"n"] <- 0
  y <- tab.obs$n
  
  return(list(J=J,F=F,N=N,beta=beta,X=X,y=y))
  
}

# pls_simulate_mlogit_rum(1,1,1,1,1)

################################################################################
################################################################################
################################################################################

# simulate synthetic dataset according to 
# Random Utility Maximisation

pls_pni_mlogit_rum_mpp <- function(Xd, Xs, beta) {
  
  W <- length(Xd)
  res <- lapply(1:W, function(xx) pls_pni_mlogit_rum(cbind(Xd[[xx]],Xs), beta))
  
  return(res)
}

pls_simulate_mlogit_rum_mpp <- function(J, S, D, W, N, sigma2, seed) {
  
  set.seed(seed)
  
  # preferences
  beta <- rnorm(D + S, 0, sigma2)  
  
  # product features
  Xd <- lapply(1:W, function(xx) matrix(rnorm(J*D), ncol = D, nrow = J))
  Xs <- matrix(rnorm(J*S), ncol = S, nrow = J)  
  
  # calculate choice probabilities
  Pni <- pls_pni_mlogit_rum_mpp(Xd,Xs,beta) 
  
  # sample sales
  y <- lapply(1:W, function(xx) base::sample(1:J, N, TRUE,prob =  Pni[[xx]]))
  
  # expand to full shelf:
  
  y <- lapply(1:W, function(xx){
    
    tab.obs <- as.data.frame(table(y[[xx]]))
    names(tab.obs) <- c("product","n")
    tab.prod <- data.frame(product = 1:J)
    tab.obs <- merge(tab.prod,tab.obs,all.x=TRUE,all.y=TRUE)
    tab.obs[is.na(tab.obs$n),"n"] <- 0
    
    return(tab.obs$n)
    
  })
  
  return(list(J = J, S = S, D = D, W = W, N = N, beta = beta, Xd = Xd, Xs = Xs, y = y))
  
}

pls_simulate_mlogit_rum_mpp(1,1,1,1,1,1,1)

################################################################################
################################################################################
################################################################################

# summary function for dcm model
# we assume that model is parametrized by beta

pls_dcm_summary <- function(stanfit,digits = 5) {
  
  pars <- rstan::extract(stanfit,pars = c("beta","lp__"))
  nb <- ncol(pars[["beta"]])
  res <- data.frame(
    "Estimate" = apply(pars[["beta"]],2,mean),
    "Std.Error" = apply(pars[["beta"]],2,sd)
  )
  res[,"t-value"] <- res[,"Estimate"]/res[,"Std.Error"]
  res <- round(res,digits)
  
  tmp <- round(2*pnorm(abs(res[,"t-value"]),lower.tail = FALSE),5)
  
  res[,"Pr(>|t|)"] <- as.character(tmp)
  res[,"Pr(>|t|)"][tmp < 2.2e-26] <- "< 2.2e-16"
  
  # significance codes
  res[," "] <- "   "
  res[tmp <= 0.001 ," "] <- "***"
  res[tmp > 0.001 & tmp <= 0.01 ," "] <- "**"
  res[tmp > 0.01 & tmp <= 0.05 ," "] <- "*"
  res[tmp > 0.05 & tmp <= 0.1 ," "] <- "."
  
  rownames(res) <- paste0("beta[",1:nb,"]")
  
  # McFadden R2
  # more data needed :/
  # ll <- mean(pars[["lp__"]])
  # l0 <- 
  
  # print
  cat("Coefficients: \n")
  print(res)
  cat("---\n")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
}



################################################################################
################################################################################
################################################################################

# summary function for dcm model (mle)


summary.mlogit <- function(object ,digits = 5) {
  
  est <- object$par[grep("beta", names(object$par))]
  nb <- length(est)
  
  
  vars <- apply(do.call(rbind, lapply(object$dta$Xd, function(xx) apply(xx, 2, var))), 2, max)
  vars <- c(vars, apply(object$dta$Xs, 2, var))
  
  is_empty <- vars < 1e-6
  
  sds <- diag(solve(-object$hessian[!is_empty, !is_empty]))
  
  sdev <- vars
  sdev[] <- 0.0
  sdev[!is_empty] <- sds
  
  is_empty[sdev < 0.0] <- TRUE
  sdev[sdev < 0.0] <- 0.0
  sdev <- sqrt(sdev)
  
  res <- data.frame(
    "Estimate" = est,
    "Std.Error" = sdev
  )
  res[,"t-value"] <- NA
  res[!is_empty, "t-value"] <- res[!is_empty, "Estimate"]/res[!is_empty, "Std.Error"]
  res <- round(res, digits)
  
  tmp <- round(2*pnorm(abs(res[,"t-value"]),lower.tail = FALSE),5)
  
  res[,"Pr(>|t|)"] <- as.character(tmp)
  res[,"Pr(>|t|)"][tmp < 2.2e-16] <- "< 2.2e-16"
  
  # significance codes
  res[," "] <- "   "
  res[tmp <= 0.001 & !is_empty ," "] <- "***"
  res[tmp > 0.001 & tmp <= 0.01 & !is_empty ," "] <- "**"
  res[tmp > 0.01 & tmp <= 0.05 & !is_empty ," "] <- "*"
  res[tmp > 0.05 & tmp <= 0.1 & !is_empty," "] <- "."
  
  rownames(res) <- object$names
  
  # McFadden R2
  # more data needed :/
  # ll <- mean(pars[["lp__"]])
  # l0 <- 
  
  # print
  cat("Coefficients: \n")
  print(res)
  cat("---\n")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
  
}




summary.pls.mlogit <- function(object ,digits = 5) {
  
  est <- object$par[grep("beta", names(object$par))]
  nb <- length(est)
  
  
  vars <- apply(object$dta$X, 2, var)
  
  is_empty <- vars < 1e-6
  
  sds <- diag(solve(-object$hessian[!is_empty, !is_empty]))
  
  sdev <- vars
  sdev[] <- 0.0
  sdev[!is_empty] <- sds
  
  is_empty[sdev < 0.0] <- TRUE
  sdev[sdev < 0.0] <- 0.0
  sdev <- sqrt(sdev)
  
  res <- data.frame(
    "Estimate" = est,
    "Std.Error" = sdev
  )
  res[,"t-value"] <- NA
  res[!is_empty, "t-value"] <- res[!is_empty, "Estimate"]/res[!is_empty, "Std.Error"]
  res <- round(res, digits)
  
  tmp <- round(2*pnorm(abs(res[,"t-value"]),lower.tail = FALSE),5)
  
  res[,"Pr(>|t|)"] <- as.character(tmp)
  res[,"Pr(>|t|)"][tmp < 2.2e-16] <- "< 2.2e-16"
  
  # significance codes
  res[," "] <- "   "
  res[tmp <= 0.001 & !is_empty ," "] <- "***"
  res[tmp > 0.001 & tmp <= 0.01 & !is_empty ," "] <- "**"
  res[tmp > 0.01 & tmp <= 0.05 & !is_empty ," "] <- "*"
  res[tmp > 0.05 & tmp <= 0.1 & !is_empty," "] <- "."
  
  rownames(res) <- object$names
  
  # McFadden R2
  # more data needed :/
  # ll <- mean(pars[["lp__"]])
  # l0 <- 
  
  # print
  cat("Coefficients: \n")
  print(res)
  cat("---\n")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
  
}

################################################################################
################################################################################
################################################################################

# predict function for mlogit model

predict.mlogit <- function(object, dta.model) {
  
  beta.est <- object$par[grep("beta", names(object$par))]
  pred <- pls_pni_mlogit_rum_mpp(dta.model$Xd, dta.model$Xs, beta.est)
  pred <- lapply(pred, function(xx) xx[,1])
  
  return(pred)
}

















