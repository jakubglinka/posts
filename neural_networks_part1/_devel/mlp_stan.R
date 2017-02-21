##################################################################
##################################################################
##################################################################

library(magrittr)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

##################################################################
##################################################################
##################################################################

# compile stan model
stan_mlp <- stan_model(
  file="neural_networks_part1/mlp.stan",
  model_name = "stan_mlp",
  verbose=FALSE
)

##################################################################
##################################################################
##################################################################

library(mlbench)
dta <- mlbench::mlbench.spirals(1000, cycles = 1, sd = .02)
plot(dta)

dta <- iris

# wrapper
stan_mlp <- function(
              X = t(cbind(1, dta$x)), 
              label = as.integer(dta$classes), 
              iter = 1000, 
              chains = 1) {
  
  data <- list(K = max(label),
              N = ncol(X),
              D = nrow(X),
              J = 10,
              y = label,
              X = as.matrix(X)
              )
  str(data)
      

  
  fit <- vb(
    stan_mlp,
    seed = 2014,
    data = data,
    pars = c("h"),
    eval_elbo = 100,
    iter = 1000,
    algorithm = "meanfield"
  )
  
  pred_y <- rstan::extract(fit, "h")[[1]] %>% apply(c(2, 3), mean) %>% apply(2, which.max)
  e1071::classAgreement(table(pred_y, data$y))
  plot(dta$x, col = pred_y)
  
  
  w <- rstan::extract(fit, "w")[[1]] %>% apply(c(2, 3), mean)
  
  set.seed(2014)
  fit <- sampling(
    stan_mlp,
    data = data,
    pars = c("w", "h"),
    chains = chains,iter = 100,
    init=lapply(1:1,FUN=function(i){
      return(list(w = w))
    }),
    control=list(max_treedepth=10)
  )  
  
  pred_y_nn <- predict(nnet::multinom(Species~., data = iris))
  
  traceplot(fit, pars = "w")
  
  pred_y <- rstan::extract(fit, "h")[[1]] %>% apply(c(2, 3), mean) %>% apply(2, which.max)
  table(pred_y)
  
  table(data$y, pred_y)
  table(pred_y_nn, pred_y)
  table(data$y, pred_y_nn)
  
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


animation::saveGIF({
  for (i in 1:10) plot(runif(10), ylim = 0:1)
})








