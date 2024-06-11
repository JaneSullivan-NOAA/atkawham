# early attempts at bridging atka to wham

# notes:
# - still not sure if we want to use the asap3 approach or the manual approach

library(dplyr)
library(tidyr)
library(ggplot2)
library(PBSmodelling) # used for readList but I think that's it?
# remotes::install_github(repo = 'GiancarloMCorrea/wham', ref='growth', INSTALL_opts = c("--no-docs", "--no-multiarch", "--no-demo"))
library(wham)

theme_set(theme_bw())

# admb i/o
arep <- PBSmodelling::readList("mod22/For_R.rep")
adat <- PBSmodelling::readList("mod22/am2022_forR.dat")
sort(names(arep))
sort(names(adat))

# Make input data:
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

# Agg index: ------ STOP
names(adat)[grepl('ind', names(adat))]
tmp <- as.data.frame(cbind(adat$yrs_ages_ind, # yrs with biomass
                           matrix(1, ncol = input_data$n_indices, nrow = length(adat$yrs_ages_ind)), # use_ind_paa input (1=fit, 0=don't)
                           adat$sample_ages_ind, # Neff
                           adat$page_ind)) # marginal ages
names(tmp) <- c('year', 'use', 'Neff', adat$rec_age:adat$n_ages)
tmp <- tidyr::expand_grid(year = input_data$years) %>% dplyr::full_join(tmp)
tmp[is.na(tmp)] <- 0
input_data$agg_indices = matrix(adat$biom_ind, ncol = input_data$n_indices, nrow = n_years) # Obs
input_data$index_cv = matrix(index_df$cv, ncol = input_data$n_indices, nrow = n_years) # Obs error
# Additional info:
input_data$units_indices = matrix(1L, nrow = n_years, ncol = input_data$n_indices) # 0 = numbers, 1 = biomass
input_data$fracyr_indices = matrix(index_df$fr_yr, ncol = input_data$n_indices, nrow = n_years) # fraction of the year when survey occurs

# Age comps (index):
names(adat)[grepl('ind', names(adat))]
tmp <- as.data.frame(cbind(adat$yrs_ages_ind, # yrs with age comps
                           matrix(1, ncol = input_data$n_indices, nrow = length(adat$yrs_ages_ind)), # use_ind_paa input (1=fit, 0=don't)
                           adat$sample_ages_ind, # Neff
                           adat$page_ind)) # marginal ages
names(tmp) <- c('year', 'use', 'Neff', adat$rec_age:adat$n_ages)
tmp <- tidyr::expand_grid(year = input_data$years) %>% dplyr::full_join(tmp)
tmp[is.na(tmp)] <- 0

input_data$index_paa = array(as.matrix(index_comp_df[,3:12]), dim = c(input_data$n_indices, n_years, n_ages)) # Obs
input_data$index_Neff = matrix(index_comp_df$Nsamp, ncol = input_data$n_indices, nrow = n_years) # Obs error
# Selex pointers:
input_data$selblock_pointer_fleets = matrix(1L, ncol = input_data$n_fleets, nrow = n_years)
input_data$selblock_pointer_indices = matrix(2L, ncol = input_data$n_indices, nrow = n_years)
# weight-at-age information:
input_data$waa = array(0, dim = c(2, n_years, n_ages))
input_data$waa[1,,] = as.matrix(waa_jan1_df[, 2:11]) # jan1 (survey, spawning)
input_data$waa[2,,] = as.matrix(waa_jul1_df[, 2:11]) # jul1 (fishery)
input_data$waa_pointer_fleets = 2
input_data$waa_pointer_indices = 1
input_data$waa_pointer_totcatch = 2
input_data$waa_pointer_ssb = 1
input_data$waa_pointer_jan1 = 1
# More information:
input_data$maturity = as.matrix(maturity_df[,2:11]) # maturity
input_data$fracyr_SSB = matrix(0, ncol = 1, nrow = n_years) # spawning fraction (0 = spawn at beginning of year)
input_data$Fbar_ages = 1:10 # ages to include in mean F calculation
input_data$bias_correct_process = 1 # do process bias correction, 0 = no, 1 = yes
input_data$bias_correct_observation = 1 # do obs bias correction, 0 = no, 1 = yes



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

