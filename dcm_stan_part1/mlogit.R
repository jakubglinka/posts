################################################################################
################################################################################
################################################################################

# additional functions
# source("./models/mlogit/src/mlogit_utils.R")

################################################################################
################################################################################
################################################################################

# fit mlogit function

# compile stan model
model.stan <- stan_model(
  file="dcm_stan_part1/mlogit.stan", 
  model_name = "mlogit",
  verbose=FALSE
)

mlogit <- function(data, iter = 1e5, verbose = TRUE, refresh = 500) {
  
  # reformat data
  dta <- pls_data_unif_to_list(data)
  
  # run model
  fit <- vb(
    model.stan,
    data = list(n_periods = dta$n_periods,
                period_start = dta$period_start,
                period_stop = dta$period_stop,
                F = dta$F,
                nr = nrow(dta$X),
                X = dta$X,
                y = dta$y
    ),
    pars = "beta",
    init = list(beta=c(rep(0,dta$F))),
    iter = iter
  )
  
  model.mlogit <- list(fit = fit)
  
  label <- as.numeric(do.call(c, lapply(split(dta$y, f = data[, "period"]), function(xx) xx/sum(xx))))
  model.mlogit$label <- label
  
  model.mlogit$names <- colnames(dta$X)
  model.mlogit$dta <- dta
  model.mlogit$coefficients <- fit@sim$samples[[1]] %>% bind_rows
  
  attr(model.mlogit, "class") <- "pls.mlogit"
  
  return(model.mlogit)
}


################################################################################
################################################################################
################################################################################

# importance method

importance <- function(object, ...)
{
  UseMethod("importance", object)
}

importance.pls.mlogit <- function(object, ...) {
  
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
  
  res <- res$`t-value`
  names(res) <- colnames(object$dta$X)
  res <- res[order(abs(res), decreasing = TRUE)]
  
  return(names(res))
  
}


################################################################################
################################################################################
################################################################################

# model prediction function
predict.pls.mlogit <- function(object, new.data = NULL) {
  
  if (!is.null(new.data)) {
    dta <- pls_data_unif_to_list(new.data)
  } else {
    dta <- object$dta
  }
  
  b <- as.numeric(object$coef)
  
  res <- lapply(1:dta$n_periods, function(xx) {
    
    start <- dta$period_start[xx]
    stop <- dta$period_stop[xx]
    
    return(pls_pni_mlogit_rum(dta$X[start:stop,], b))
    
  })  
  
  res <- do.call(rbind, res)
  return(res)
}

################################################################################
################################################################################
################################################################################

# model summary

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



