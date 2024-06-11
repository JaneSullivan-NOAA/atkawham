# modified WHAM intro vignette
# 1) deterministic SCAA
# 2) simple state-space SCCA: recruitment as random effects
# 3) full state-space SCAA: NAA as random effects

# install growth branch
remotes::install_github(repo = 'GiancarloMCorrea/wham', ref='growth', INSTALL_opts = c("--no-docs", "--no-multiarch", "--no-demo"))

# load wham
library(wham)

if(!exists("write.dir")) write.dir = getwd()
if(!dir.exists(write.dir)) dir.create(write.dir)
setwd(write.dir)

# copy asap3 data file to working directory
wham.dir <- find.package("wham")
file.copy(from=file.path(wham.dir,"extdata","ex1_SNEMAYT.dat"), to=write.dir, overwrite=TRUE)

# confirm you are in the working directory and it has the ASAP_SNEMAYT.dat file
list.files()

# read asap3 data file and convert to input list for wham
asap3 <- read_asap3_dat("ex1_SNEMAYT.dat")
names(asap3$dat)

# Model 0: deterministic SCAA ----
# - recruitment expectation (recruit_model): random about mean (no S-R function)
# - recruitment deviations (NAA_re): NULL, independent fixed effects
# - selectivity: age-specific (fix sel=1 for ages 4-5 in fishery, age 4 in index1, and ages 2-4 in index2)
# - age compositions: multinomial
asap3$dat$n_ages # 6 ages
asap3$dat$n_fleets # 1 fishery
asap3$dat$n_indices # 2 survey indices

# ? prepare_wham_input
input0 <- prepare_wham_input(asap3, # most info comes from this asap3 object
                             recruit_model=2, # (default) Random about mean, i.e. steepness = 1
                             model_name="Ex 1: SNEMA Yellowtail Flounder",
                             # Each entry must be one of: "age-specific",
                             # "logistic", "double-logistic",
                             # "decreasing-logistic", "double-normal",
                             # "len-logistic", "len-decreasing-logistic" or
                             # "len-double-normal"
                             selectivity=list(model=rep("age-specific",3),
                                              re=rep("none",3),
                                              initial_pars=list(c(0.5,0.5,0.5,1,1,0.5), # fishery slx by age
                                                                c(0.5,0.5,0.5,1,0.5,0.5), # srv 1
                                                                c(0.5,1,1,1,0.5,0.5)),  # srv 2
                                              fix_pars=list(4:5,4,2:4)), # slx parameters to map off (where slx=1)
                             NAA_re = NULL # deterministic SCAA option in WHAM
)
# data list: we don't have the asap3 object, so we'll format our data list
# manually
names(input0$data)
input0$data$agg_catch
input0$data$catch_aging_error
input0$data$catch_paa
input0$data$index_paa

# parameters and map lists
sort(names(input0$par))
sort(names(input0$dat))

input0$dat$logR_sd # for projections only?
# input0$data$M_est # no longer used in wham

input0$par$log_NAA
input0$map$log_NAA
input0$par$log_NAA_sigma
input0$map$log_NAA_sigma
input0$par$log_N1_pars
input0$map$log_N1_pars
input0$par$logit_selpars
input0$map$logit_selpars

# random effects vector (none)
input0$random

m0 <- fit_wham(input0, do.osa = FALSE) # turn off OSA residuals to save time
names(m0)
# m0$simulate()
# m0$years_full

# Check that m0 converged (m0$opt$convergence should be 0, and the maximum gradient should be < 1e-06)
check_convergence(m0)
# plot_wham_output(m0)

# Model 1: simple state-space SCAA ----
# - Recruitment as random effects
# - otherwise same as Model 0
input1 <- prepare_wham_input(asap3,
                             recruit_model=2,
                             model_name="Ex 1: SNEMA Yellowtail Flounder",
                             selectivity=list(model=rep("age-specific",3),
                                              re=rep("none",3),
                                              initial_pars=list(c(0.5,0.5,0.5,1,1,0.5),c(0.5,0.5,0.5,1,0.5,0.5),
                                                                c(0.5,1,1,1,0.5,0.5)),
                                              fix_pars=list(4:5,4,2:4)),
                             NAA_re = list(sigma="rec", cor="iid")
)
all(input1$par$log_NAA == input0$par$log_NAA) # should be TRUE
input1$random # this is the difference
which(!is.na(input0$map$log_NAA == input1$map$log_NAA))

# sigmaR
input0$map$log_NAA_sigma # not estimated
input1$map$log_NAA_sigma # estimated
input0$par$log_NAA_sigma # same starting value
input1$par$log_NAA_sigma

m1 <- fit_wham(input1, do.osa = FALSE) # turn off OSA residuals to save time
names(m1)
# m1$simulate()
# m1$years_full

# Check that m1 converged (m1$opt$convergence should be 0, and the maximum gradient should be < 1e-06)
check_convergence(m1)
# plot_wham_output(m1)

# Model 2: full state-space SCAA ----
# - numbers at all ages are random effects (NAA_re$sigma = "rec+1")
# - otherwise same as Model 0
input2 <- prepare_wham_input(asap3,
                             recruit_model=2,
                             model_name="Ex 1: SNEMA Yellowtail Flounder",
                             selectivity=list(model=rep("age-specific",3),
                                              re=rep("none",3),
                                              initial_pars=list(c(0.5,0.5,0.5,1,1,0.5),c(0.5,0.5,0.5,1,0.5,0.5),
                                                                c(0.5,1,1,1,0.5,0.5)),
                                              fix_pars=list(4:5,4,2:4)),
                             NAA_re = list(sigma="rec+1", cor="iid")
)
all(input2$par$log_NAA == input0$par$log_NAA) # should be TRUE
input2$random
input0$map$log_NAA
input1$map$log_NAA
input2$map$log_NAA

# sigmas
input0$map$log_NAA_sigma # not estimated
input1$map$log_NAA_sigma # estimated
input2$map$log_NAA_sigma # estimated
input0$par$log_NAA_sigma # same starting value
input1$par$log_NAA_sigma
input2$par$log_NAA_sigma # two sigmas; 1 for rec, 1 for all other ages

m2 <- fit_wham(input2, do.osa = FALSE) # turn off OSA residuals to save time
names(m2)
# m2$simulate()
# m2$years_full

# Check that m2 converged (m2$opt$convergence should be 0, and the maximum gradient should be < 1e-06)
check_convergence(m2)
# plot_wham_output(m2)

# Proj and model sel ----

# # Save list of all fit models
# mods <- list(m0=m0, m1=m1, m2=m2)
# save("mods", file="ex1_models.RData")
#
# # Compare models by AIC and Mohn's rho...
# res <- compare_wham_models(mods, table.opts=list(fname="ex1_table", sort=TRUE))
# res$best
#
# # Project best model... (need to fill in appropriate model object below)
# # Use default values: 3-year projection, use average selectivity, M, etc. from last 5 years
# mbest_proj <- project_wham(model=mods$m...)
#
# # WHAM output plots for best model with projections
# plot_wham_output(mod=mbest_proj, out.type='html')
