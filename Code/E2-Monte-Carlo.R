##############################################################################
##   
## Katana example 2
## Simple Monte-carlo Simulation
## Date: 30 June 2020
##
##############################################################################
# 1. Setup Environment
#-----------------------------------------------------------------------------

# 1.1 Specify paths
.libPaths("/home/z3312911/RPackages") # Change to folder where R packages can be saved
workdir <- "/home/z3312911/examples/" # Change to folder where job files are saved

# 1.2 Check libraries, install missing packages, update old packages, and then load required packages
libs <- c("dqrng","lme4","dplyr","tidyverse")
install <- !libs %in% installed.packages() | libs %in% old.packages()
if (any(install)) {
  install.packages(libs[install])
}
if (!"faux" %in% installed.packages()) {
  devtools::install_github("debruine/faux")
}
lapply(libs, library, character.only = TRUE)
library("faux")

# 1.3 Define arguments passed through to R from Katana
stream <- commandArgs(trailingOnly = TRUE) # This is passed to R by the PBS script

# 1.4 Set seed and stream
dqset.seed(56418,stream)

##############################################################################
# 2. Simulate data
#-----------------------------------------------------------------------------

# 2.1 Define simulation function
# set up the custom data simulation function
sim_data <- function(
  nsubj  = 100, # number of subjects
  nitem  = c(ingroup = 25, outgroup = 25),  # number of items
  b0     = 800, # grand mean
  b1     =  50, # effect of category
  I0i_sd =  80, # by-item random intercept sd
  S0s_sd = 100, # by-subject random intercept sd
  S1s_sd =  40, # by-subject random slope sd
  scor   = 0.2, # correlation between intercept and slope
  err_sd = 200){  # residual (standard deviation)
  
  # simulate items
  items <- faux::sim_design(
    between = list(category = c("ingroup", "outgroup")),
    n = nitem,
    sd = I0i_sd,
    dv = "I0i",
    id = "item_id",
    plot = FALSE
  )
  
  # effect code category
  items$cat <- recode(items$category, "ingroup" = -0.5, "outgroup" = 0.5)
  
  # simulate subjects
  subjects <- faux::sim_design(
    within = list(effect = c(S0s = "By-subject random intercepts", 
                             S1s = "By-subject random slopes")), 
    n = nsubj,
    sd = c(S0s_sd, S1s_sd), 
    r = scor,
    id = "subj_id",
    plot = FALSE
  )
  
  # simulate trials
  dat_sim <- crossing(subj_id = subjects$subj_id,
                      item_id = items$item_id) %>%
    inner_join(subjects, "subj_id") %>%
    inner_join(items, "item_id") %>%
    mutate(err = rnorm(nrow(.), mean = 0, sd = err_sd)) %>%
    mutate(RT = b0 + I0i + S0s + (b1 + S1s) * cat + err) %>%
    select(subj_id, item_id, category, cat, RT)
  
  dat_sim
}

sim_res <- matrix(unlist(lapply(rep(1, 1000), function (x) {
  dat_sim <- sim_data()
  mod_sim <- lmer(RT ~ 1 + cat + (1 | item_id) + (1 + cat | subj_id),
                  data = dat_sim,
                  REML = FALSE)
  results <- coef(summary(mod_sim))["cat",]
  results[1:2]
})),ncol=2,byrow=TRUE)

##############################################################################
# 3. Save results to be combined later
#-----------------------------------------------------------------------------

save(sim_res,file=paste0(workdir,"Sim-res-",stream,".RData",collapse=""))
