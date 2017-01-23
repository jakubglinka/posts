##################################################################
##################################################################
##################################################################

pgsg <- function(t, b, alpha, beta) (1 - exp(- b * t)) / ((1 + 1 / beta * exp(- b * t))^alpha)

simulate_dta <- function(b = .04, alpha = 1 , beta = .01, t_max = 130.0, sigma = 1/sqrt(1e6)) {
  t_max <- as.integer(round(t_max))
  f_t <- diff(pgsg(0:t_max, b, alpha, beta)) / pgsg(t_max, b, alpha, beta)
  S_t <- f_t  + rnorm(t_max, mean = 0, sd = sigma)
  S_t[S_t < 0] <- 0.0
  
  date <- lubridate::ymd(seq(ymd("2013-01-01"), by = "week", length.out = t_max))
  return(data.frame(date = date, S_t))
}


# compile stan model
stan_diff_sg <- stan_model(
  file="diffusion_SG.stan",
  model_name = "stan_diff_sg",
  verbose=FALSE
)

# wrapper
sg_diffusion <- function(date, x, iter = 1000, chains = 1) {
  
  S_t <- x
  fit <- sampling(
    stan_diff_sg,
    data = list(T = length(S_t),
                S = S_t
    ),
    pars = c("m", "p", "q", "sigma", "pred_s"),
    chains = chains,iter = 1000,
    init=lapply(1:1,FUN=function(i){return(list(b = .1, beta = 1, m = sum(S_t)))}),
    control=list(max_treedepth=10)
  )
  
  res <- list(date = date, x = x, fit = fit)
  class(res) <- "sg_diffusion"
  
  res
}

summary.sg_diffusion <- function(object, digits = 6) {
  
  res <- summary(object$fit, pars = c("m", "p", "q", "sigma"))  
  res <- res$summary
  res <- res[, c("mean", "sd", "n_eff")]
  res <- as.data.frame(res)
  res[,"t-value"] <- res[,"mean"]/res[,"sd"]
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
  
  # print
  cat("Coefficients: \n")
  print(res)
  cat("---\n")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
}

plot.sg_diffusion <- function(object, title = "Search term popularity") {
  
  model <- object$fit
  pred_s <- rstan::extract(object$fit, pars = c("pred_s"))[[1]] %>% 
    apply(2, mean)
  
  sigma <- as.numeric(rstan::extract(object$fit, pars = c("sigma"))[[1]])
  
  pred_s_lower <- rstan::extract(object$fit, pars = c("pred_s"))[[1]] - 1.96 * sigma
  pred_s_lower <- apply(pred_s_lower, 2, mean)
  pred_s_lower[pred_s_lower < 0] <- 0.0 
  
  pred_s_upper <- rstan::extract(object$fit, pars = c("pred_s"))[[1]] + 1.96 * sigma
  pred_s_upper <- apply(pred_s_upper, 2, mean)
  
  pars <- rstan::extract(object$fit, pars = c("m", "p", "q")) %>% map(mean) %>% 
    flatten_dbl
  pars[1] <- round(pars[1])
  pars[2] <- round(pars[2] * 1e3, 2)
  pars[3] <- round(pars[3] * 1e3, 2)
  
  dta.plot <- data.frame(date = object$date, s = object$x)
  p <- ggplot(aes(x = date, y = s), data = dta.plot) + 
    geom_line() +
    geom_ribbon(aes(ymin = pred_s_lower, ymax = pred_s_upper), fill = "#FFCCFF", alpha = .5) + 
    geom_line(aes(y = pred_s), linetype = 1, colour = "red", lwd = .5) + 
    theme_classic() + 
    ggtitle(title, subtitle = paste0("m = ", pars[1], ", p = ", pars[2], "e-3, q = ", pars[3],"e-3")) + 
    ylim(c(0, NA))
  
  print(p)
  
}



##################################################################
##################################################################
##################################################################
