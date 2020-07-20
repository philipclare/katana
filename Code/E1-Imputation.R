##############################################################################
##   
## Katana example 1
## Multiple imputation using 'parlmice'
## Date: 30 June 2020
##
##############################################################################
# 1. Setup Environment
#-----------------------------------------------------------------------------

# 1.1 Specify paths
.libPaths("/home/z3312911/RPackages") # Change to folder where R packages can be saved
workdir <- "/home/z3312911/examples/" # Change to folder where job files are saved

# 1.2 Check libraries, install missing packages, update old packages, and then load packages
libs <- c("mice","miceadds","parallel")
install <- !libs %in% installed.packages() | libs %in% old.packages()
if (any(install)) {
  install.packages(libs[install])
}
lapply(libs, library, character.only = TRUE)

# 1.3 Set seed
set.seed(56188)

##############################################################################
# 2.Load data
#-----------------------------------------------------------------------------

data(nhanes)

##############################################################################
# 3. Define Imputation Paramaters
#-----------------------------------------------------------------------------

m <- 40 # Number of imputations
maxit <- 100; # Number of mice iterations
cluster.seed <- 23771 # Needs to be set within function for parallel computing
numcores <- as.numeric(Sys.getenv('NCPUS')) # Defined from cluster environment variable
n.imp.core = m/numcores # number of imputations per core
cl.type = "FORK" # Could be PSOCK for Windows based systems

##############################################################################
# 4. Imputation
#-----------------------------------------------------------------------------

# 3.3 Parallel imputation using parlmice and randomforests
imp_mice <- parlmice(data=nhanes,
                     cluster.seed=cluster.seed,
                     n.core=numcores,
                     n.imp.core=n.imp.core,
                     maxit=maxit,
                     method="rf",
                     cl.type=cl.type)
imp_mice <- mids2datlist(imp_mice)

##############################################################################
# 5. Save
# 5.1 Save imputation
save(imp_mice, file=paste0(workdir,"imputed-mice.RData"))
