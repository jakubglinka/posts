################################################################################
################################################################################
################################################################################

# load packages
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(tidyverse)
library(replyr)

# additional functions
source("./dcm_stan_part1/mlogit.R")
source("./dcm_stan_part1/mlogit_utils.R")

################################################################################
################################################################################
################################################################################

# TODO: plot with confidence intervals using bayesplot?

# set simulation parameters

data.settings <- list(
  n.periods = 52,
  n.prods = 100,
  avg.shelf = 50,
  avg.sales = 1e3,
  trend = FALSE,
  n.dynamic = 10,
  n.static = 20,
  n.categorical = 10,
  n.levels = 10,
  sigma2 = 1,
  seed = 2014
)
str(data.settings)
opts <- data.settings

################################################################################
################################################################################
################################################################################

# tidyrp (tidy approach to revealed preference data)

# generate tibble with all the necessary data
set.seed(opts$seed)

################################################################################
################################################################################
################################################################################

# tidy feature tibble

# tibble with one product in one row
# must have either single entry for feature
# or vector of length equal to the number of periods
td_ft <- data_frame(item_id = 1:n.prods, periods = opts$n.periods)

pls_gen_f_cat <- function()

for (i in 1:opts$n.categorical)
  let(list(feature = paste0("f", i)), {
    td_ft %>% mutate(feature = sample(as.character)) -> td_ft
    })
  
td_ft


################################################################################
################################################################################
################################################################################

# tidy rp table

# tibble with period, items [list], features [list of vectors], sales [list]
td_rp <- data_frame(period = 1:n.periods)
dta <- dta %>%
  mutate(items = )


1:opts$n.periods %>% map()

dta <- data_frame()
# generate choice sets
prods <- 1:opts$n.periods %>%
  lapply(function(xx) 
    sort(unique(
      sample(1:opts$avg.prods, opts$avg.prods, replace = TRUE)
    )))



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




################################################################################
################################################################################
################################################################################

# unified format
# https://confluence.gfk.com/display/DDA/Models+Evaluation

inc <- 0
period <- lapply(n_prods, function(xx) {
  inc <<- inc + 1
  return(rep(inc, xx))
})
period <- do.call(c, period)
dta.unif <- cbind(dta$y, period, dta$X)
colnames(dta.unif) <- c("sales", "period", paste0("f",1:ncol(dta$X)))
class(dta.unif)

head(round(dta.unif[, 1:10],2))

################################################################################
################################################################################
################################################################################

# train the model
model <- mlogit(dta.unif)

b.est <- model$coefficients %>% summarise_all(mean)
plot(as.numeric(b.est)[1:F], dta$beta)
cor(as.numeric(b.est)[1:F], dta$beta)


################################################################################
################################################################################
################################################################################

# class(model)
# summary(model)
# importance(model)
# 
# table(predict(model) == predict(model, dta.unif))
# 
# ################################################################################
# ################################################################################
# ################################################################################
# ## comparison with original values:
# 
# plot(jitter(model$label - predict(model)))
# plot(model$label, predict(model))
# cor(model$label, predict(model))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
