# early attempts at bridging atka to wham

# notes:
# - still not sure if we want to use the asap3 approach or the manual approach

library(dplyr)
library(tidyr)
library(ggplot2)
library(PBSmodelling) # used for readList but I think that's it?
# remotes::install_github(repo = 'GiancarloMCorrea/wham', ref='growth', INSTALL_opts = c("--no-docs", "--no-multiarch", "--no-demo"))
library(wham)
source('R/helper.R')
theme_set(theme_bw())

# admb i/o
arep <- PBSmodelling::readList("mod22/For_R.rep")
# adat <- PBSmodelling::readList("mod22/am2022_forR.dat") # delete this once you've changed the code
adat <- PBSmodelling::readList("mod22/am2022.dat")
# ctl file will also be helpful for slx and recruitment controls
sort(names(arep))
sort(names(adat))

# Make input data for basic_info:
input_data = list()
input_data$ages = adat$rec_age:adat$n_ages
input_data$years = adat$styr:adat$endyr
input_data$n_fleets = adat$nfsh
input_data$n_indices = adat$nind
n_years = length(input_data$years)
n_ages = length(input_data$ages)

# Agg catch:
input_data$agg_catch = matrix(adat$catch, ncol = input_data$n_fleets, nrow = n_years) # Obs
input_data$catch_cv = matrix(adat$catch_cv, ncol = input_data$n_fleets, nrow = n_years) # Obs error

# Age comps (fishery):
names(adat)[grepl('fsh', names(adat))]
tmp <- as.data.frame(cbind(adat$yrs_ages_fsh, # yrs with age comps
                           matrix(1, ncol = input_data$n_fleets, nrow = length(adat$yrs_ages_fsh)), # matrix of 1s for use_catch_paa input (1=fit, 0=don't)
                           adat$sample_ages_fsh, # Neff
                           adat$page_fsh)) # marginal ages
names(tmp) <- c('year', 'use', 'Neff', adat$rec_age:adat$n_ages)
tmp <- tidyr::expand_grid(year = input_data$years) %>% dplyr::full_join(tmp)
tmp[is.na(tmp)] <- 0
input_data$catch_paa = array(as.matrix(tmp[,c(paste(1:n_ages))]), dim = c(input_data$n_fleets, n_years, n_ages)) # Obs
input_data$catch_Neff = matrix(as.matrix(tmp$Neff), ncol = input_data$n_fleets, nrow = n_years) # Obs error
input_data$use_catch_paa = matrix(as.matrix(tmp$use), ncol = input_data$n_fleets, nrow = n_years) # fit to data? 1/0

# Agg index: ------
names(adat)[grepl('ind|biom', names(adat))]
tmp <- as.data.frame(cbind(adat$yrs_ind, # yrs with biomass
                           matrix(1, ncol = input_data$n_indices, nrow = length(adat$yrs_ind)), # use_ind input (1=fit, 0=don't)
                           adat$biom_std/adat$biom_ind, # convert to cv, log-se
                           adat$biom_ind)) # biomass index
names(tmp) <- c('year', 'use', 'cv', 'ind')
tmp <- tidyr::expand_grid(year = input_data$years) %>% dplyr::full_join(tmp)
tmp[is.na(tmp)] <- 0

input_data$agg_indices = matrix(tmp$ind, ncol = input_data$n_indices, nrow = n_years) # Obs
input_data$index_cv = matrix(tmp$cv, ncol = input_data$n_indices, nrow = n_years) # Obs error
input_data$use_indices = matrix(as.matrix(tmp$use), ncol = input_data$n_indices, nrow = n_years) # fit to data? 1/0
# Additional info for BTS:
input_data$units_indices = matrix(1L, nrow = n_years, ncol = input_data$n_indices) # 0 = numbers, 1 = biomass
input_data$fracyr_indices = matrix(adat$month_ind/12, ncol = input_data$n_indices, nrow = n_years) # fraction of the year when survey occurs

# Age comps (index):
names(adat)[grepl('ind', names(adat))]
tmp <- as.data.frame(cbind(adat$yrs_ages_ind, # yrs with biomass
                           matrix(1, ncol = input_data$n_indices, nrow = length(adat$yrs_ages_ind)), # use_ind_paa input (1=fit, 0=don't)
                           adat$sample_ages_ind, # Neff
                           adat$page_ind)) # marginal ages
names(tmp) <- c('year', 'use', 'Neff', adat$rec_age:adat$n_ages)
tmp <- tidyr::expand_grid(year = input_data$years) %>% dplyr::full_join(tmp)
tmp[is.na(tmp)] <- 0
input_data$index_paa = array(as.matrix(tmp[,c(paste(1:n_ages))]), dim = c(input_data$n_indices, n_years, n_ages)) # Obs
input_data$index_Neff = matrix(as.matrix(tmp$Neff), ncol = input_data$n_indices, nrow = n_years) # Obs error
input_data$use_index_paa = matrix(as.matrix(tmp$use), ncol = input_data$n_indices, nrow = n_years) # fit to data? 1/0

# Selex pointers:
# FLAG: Jim ctl pls - implement simple non-tv parametric slx for bridging purposes?
input_data$selblock_pointer_fleets = matrix(1L, ncol = input_data$n_fleets, nrow = n_years)
input_data$selblock_pointer_indices = matrix(2L, ncol = input_data$n_indices, nrow = n_years)

# weight-at-age information:
names(adat)[grepl('wt', names(adat))]
input_data$waa = array(0, dim = c(3, n_years, n_ages))
input_data$waa[1,,] = matrix(adat$wt_age_pop, ncol = n_ages, nrow = n_years, byrow = TRUE) # population
input_data$waa[2,,] = adat$wt_age_ind
input_data$waa[3,,] = adat$wt_age_fsh
input_data$waa_pointer_fleets = 3
input_data$waa_pointer_indices = 2
input_data$waa_pointer_totcatch = 3
input_data$waa_pointer_ssb = 1
input_data$waa_pointer_jan1 = 1

# More information:
input_data$maturity = matrix(adat$maturity, ncol = n_ages, nrow = n_years, byrow = TRUE) # maturity
input_data$fracyr_SSB = matrix(8/12, ncol = 1, nrow = n_years) # spawning fraction
input_data$Fbar_ages = 1:10 # ages to include in mean F calculation # *FLAG* check on this
input_data$bias_correct_process = 1 # do process bias correction, 0 = no, 1 = yes
input_data$bias_correct_observation = 1 # do obs bias correction, 0 = no, 1 = yes


# prepare wham input -----

input <- prepare_wham_input(model_name = 'Case1_empiricalWAA',
                                basic_info = input_data,
                                # N1_model=1: 2 fixed effects parameters: an
                                # initial recruitment and an instantaneous
                                # fishing mortality rate to generate an
                                # equilibrium abundance at age.
                                NAA_re = list(N1_model = 1,
                                              N1_pars = c(1e+05, 0), # initial numbers in the first age class, and equilib F rate generating the rest of the NAA in the first year
                                              recruit_model = 2, # # estimating a mean recruitment with yearly recruitment as random effects
                                              recruit_pars = 1e+05, # mean rec
                                              sigma = 'rec', # rand eff on rec devs, all other ages deterministic
                                              cor = 'iid'),
                            # FLAG! Jim, what are the M assumptions in amak? arep$Mest what are these values?
                                M = list(model = 'constant', initial_means = 0.3),
                                selectivity = list(model = c('double-normal', 'double-normal'),
                                                   initial_pars = list(c(6,-0.5,0,0,-5,-3), c(5,-2,0,0,-5,-3)),
                                                   n_selblocks = 2),
                                catchability = list(initial_q = 1))

show_selex(model = "double-normal", initial_pars = c(6,-0.5,0,0,-5,-3), ages = input_data$ages)

# Fix some parameters:
input$map$logit_q = factor(NA)
input$map$log_N1_pars = factor(c(1,NA))

my_model = wham::fit_wham(MakeADFun.silent = TRUE, input = input, do.retro = FALSE, do.osa = FALSE)





# OLD ----

waa_fsh <- as.data.frame(adat$wt_age_fsh)
names(waa_fsh) <- 1:11
waa_fsh <- waa_fsh %>%
  mutate(year = adat$styr:adat$endyr) %>%
  pivot_longer(-year, names_to = 'age', values_to = 'waa') %>%
  mutate(age = as.numeric(age))

ggplot(waa_fsh, aes(x = year, y = waa)) +
  geom_point() +
  geom_line() +
  facet_wrap(~age, scales = 'free_y') +
  labs(title = 'fishery weight-at-age')#+
  # geom_smooth()

waa_ind <- as.data.frame(adat$wt_age_ind)
names(waa_ind) <- 1:11
waa_ind <- waa_ind %>%
  mutate(year = adat$styr:adat$endyr) %>%
  pivot_longer(-year, names_to = 'age', values_to = 'waa') %>%
  mutate(age = as.numeric(age))

ggplot(waa_ind, aes(x = year, y = waa)) +
  geom_point() +
  geom_line() +
  facet_wrap(~age, scales = 'free_y') +
  labs(title = 'survey weight-at-age')#+
  # geom_smooth()

adat$styr:adat$endyr
adat$wt_age_ind
adat$wt_age_pop

# asap3 file made manually following goa pk example
asap3 <- read_asap3_dat("amwham/am_asap3.txt")
input1 <- prepare_wham_input(asap3)
names(input1$par)

names(adat)

# match_input <- function(arep, asap3, NAA_re = list(sigma = 'rec', cor = 'iid')) {

arep = arep; asap3 = asap3; NAA_re = list(sigma = 'rec', cor = 'iid')

a <- 0; b <- 1
invsel <- function(y) -log((b-a)/(y-a)-1)
sel <- function(x) a+(b-a)/(1+exp(-x))
# logit <- function

tmp <- scales::rescale(arep$sel_fsh_1[,-c(1,2)])

tmp <- arep$sel_fsh_1[,-c(1,2)]
tmp <- t(apply(tmp, 1, scales::rescale))
tmp[tmp >= 1] <- 1-1e-15; tmp[tmp == 0] <- 1e-15 # prevents Inf in invlog transformation
tmp <- invsel(tmp)
tmp
y <- sel(colMeans(tmp))
(exp(y)/(1+exp(y)))
y <- scales::rescale(colMeans(arep$sel_fsh_1[,-c(1,2)]))
y[y >= 1] <- 1-1e-15; y[y == 0] <- 1e-15
tst <- (exp(y)/(1+exp(y)))
# }


names(input1$data)
input1$data$index_paa
input <- match_input(arep, asap3)
## map process variances off to start to match ADMB (penalized
## likelihood)
mapoff('sel_repars')                    # selex variance
mapoff('log_NAA_sigma')                 # recruit variance
mapoff('q_repars')                      # q variance

## Compare initial values to ADMB
fit0 <- fit_wham(input1, do.osa=FALSE, do.fit=TRUE, do.retro=FALSE,
                 do.sdrep=TRUE, MakeADFun.silent=TRUE)

fit0


plot_wham_output(fit0)
fit0$rep[grep('nll',names(fit0$rep))] %>% lapply(sum) %>% unlist
plot_checks(arep, fit0$rep) # special plot function to explore inits
plot_ssb(fit0, asdrep) ## matches closely except for initial NAA

## Compare platforms after optimizing.
fit1 <- fit_wham(input, do.osa=FALSE, do.fit=TRUE, do.retro=FALSE,
                 do.sdrep=TRUE, MakeADFun.silent=TRUE)
fit1$rep[grep('nll',names(fit1$rep))] %>% lapply(sum) %>% unlist
plot_ssb(fit1, asdrep) # mean and uncertainty match well

