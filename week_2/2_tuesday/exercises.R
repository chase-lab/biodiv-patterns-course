##  How does the spatial distribution or species and regional patterns of relative abundance 
##  influence diversity?

#--------clean the workspace and install and load packages-----------
rm(list=ls())

# install (if required, first time only) and load required packages
# install.packages(c("mobr", "mobsim", "tidyverse"))
library(mobr)
library(mobsim)
library(tidyverse)




#--------set parameters for regional communities-----
# total (regional) species richness
S_pool = 100
# set total number of individuals in region
N = 1000

#  set evenness of the regional SADs
#  cv_abund = 1 is 'even' community; 
# increasing cv_abund > 1 makes regional species more uneven
# Q: what does it mean for a regional species pool to be more uneven??
cv_abund = list(list(cv_abund=1), 
                list(cv_abund=2),
                list(cv_abund=4))


#--------simulate communities-------------------
maps = NULL
maps$agg = lapply(cv_abund, function(x) 
  sim_thomas_community(S_pool, N, 'lnorm', x, fix_s_sim = T))
maps$poi = lapply(cv_abund, function(x) 
  sim_poisson_community(S_pool, N, 'lnorm', x, fix_s_sim = T))




#-------visualise 'stem map' of communities -----
par(mfrow=c(3,2))
for(i in seq_along(cv_abund)) {
  plot(maps$agg[[i]], axes=F, xlab='', ylab='',
       main=paste('Aggregated (CV = ', cv_abund[[i]]$cv_abund,')'))
  plot(maps$poi[[i]], axes=F, xlab='', ylab='',
       main=paste('Random (CV = ', cv_abund[[i]]$cv_abund,')'))
}
dev.off()


#-------sample n_quadrats from the mapped communities------------
# start with a complete sample (all individuals) to remove 'sampling effects'
n_quadrats = 25

comms = lapply(maps, function(x) 
  lapply(x, function(y) 
    sample_quadrats(y, n_quadrats, plot = F, quadrat_area = 0.04,
                    method = 'grid', delta_x = 0.2, delta_y = 0.2, avoid_overlap = T)))

# check what a complete sample looks like...ok?
sample_quadrats(maps$poi[[1]], n_quadrats, plot = T, quadrat_area = 0.04,
                method = 'grid', delta_x = 0.2, delta_y = 0.2, 
                avoid_overlap = T)



#-------aggregate  data into a community and attributes dataframes-------------
spdat = rbind(dplyr::bind_rows(lapply(comms$agg, function(x) x$spec_dat)),
              dplyr::bind_rows(lapply(comms$poi, function(x) x$spec_dat)))

coords = rbind(dplyr::bind_rows(lapply(comms$agg, function(x) x$xy_dat)),
               dplyr::bind_rows(lapply(comms$poi, function(x) x$xy_dat)))

plot_attr = data.frame(coords, 
                       spatial = rep(c('agg', 'poi'), 
                                     each = n_quadrats * length(cv_abund)),
                       SAD_CV = rep(unlist(cv_abund), 
                                    each= n_quadrats, times = 2),
                       Replicate = rep(c(1:n_quadrats), times = length(cv_abund)*2))

plot_attr$group = paste(plot_attr$spatial, plot_attr$SAD_CV, sep='_')

# join the community data frame with the plot attributes
# NB: this data is in a 'wide' format (mobr package uses this format)
comm_dat <- bind_cols(spdat, plot_attr) %>% 
  as_tibble()

# wide to long 
comm_dat_long <- comm_dat %>% 
  gather(species, abundance, species1:species99)


#-------compute diversity metrics--------------
# alpha-scale first 
alpha_metrics <- comm_dat_long %>% 
  # remove the zeroes
  filter(abundance > 0) %>% 
  # calculate metrics for each replicate in each 'group'
  group_by(group, Replicate) %>% 
  summarise(
    # total abundance of individuals
    N = sum(abundance),
    # species richness
    S = n_distinct(species),
    # S_PIE in MoB framework is equal to asympotitic value of diversity with order q = 2
    #  more even communities have higher values
    S_PIE = mobr::calc_PIE(abundance, ENS = T)) %>% 
  ungroup()

# now gamma-scale (we should know what N and S are if we sampled completely, right?)
gamma_metrics <- comm_dat_long %>% 
  # remove species with zero abundance
  filter(abundance > 0) %>% 
  # accumulate the species across all replicates within the groups
  group_by(group, species) %>% 
  summarise(gamma_abundance = sum(abundance)) %>% 
  ungroup() %>% 
  # now, calculate the gamma-scale metrics for each group
  group_by(group) %>% 
  summarise(gamma_N = sum(gamma_abundance),
            gamma_S = n_distinct(species),
            gamma_S_PIE = mobr::calc_PIE(gamma_abundance, ENS = T)) %>% 
  ungroup()

# for beta-S and beta-S_PIE we have what we need:
# first join the alpha- and gamma-scale indicies
beta_calc <- left_join(alpha_metrics,
                       gamma_metrics,
                       by = 'group') %>% 
  # now calculate multiplicative beta-diversity
  mutate(beta_S = gamma_S / S,
         beta_S_PIE = gamma_S_PIE / S_PIE)


# but for beta-Sn we need to 'rarefy' the gamma-scale curve to the alpha-scale N
# first get a vector of the gamma-scale abundances to rarefy
gamma_comms <- comm_dat %>%
  gather(species, abundance, species1:species99) %>%
  as_tibble() %>%
  group_by(group, species) %>%
  summarise(N = sum(abundance)) %>%
  filter(N > 0) %>%
  nest(N) %>% 
  slice(rep(1:n(), each = n_quadrats)) %>%
  group_by(group) %>%
  mutate(Replicate = 1:n()) %>%
  ungroup() %>%
  unite(group, c(group, Replicate), sep = '.')

# now, join these with the alpha-scales abundance that we will rarefy to:
gamma_comms <- inner_join(gamma_comms, 
                          alpha_metrics %>% 
                            unite(group, c(group, Replicate), sep = '.') %>% 
                            group_by(group) %>%
                            mutate(alphaN = N) %>% 
                            nest(alphaN), 
                          by = 'group')

gamma_comms <- inner_join(gamma_comms, 
                          alpha_metrics %>% 
                            unite(group, c(group, Replicate), sep = '.') %>% 
                            group_by(group) %>%
                            mutate(alphaS = N) %>% 
                            nest(alphaS), by = 'group')

# do rarefactions
gamma_S <- gamma_comms %>%
  mutate(gamma_Sn = purrr::map2(data.x, data.y, ~ rarefaction(.x$N, method = 'indiv', effort = 1:.y$alphaN)),
         gamma_S = purrr::map(data.x, ~ rarefaction(.x$N, method = 'indiv')))

# calculate beta_Sn
gamma_Sn <- gamma_S %>% 
  unnest(gamma_Sn) %>% 
  group_by(group) %>% 
  arrange(desc(gamma_Sn)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  separate(group, into = c('group', 'Replicate'), sep = '\\.')

#  put all the discrete-scale diversities together 
diversity <- left_join(beta_calc %>% 
                         # int to character for joining!
                         mutate(Replicate = as.character(Replicate)),
                       gamma_Sn, 
                       by = c('group', 'Replicate')) %>% 
  separate(group, into = c('Distribution', 'CV'), sep = '_', remove = FALSE) %>% 
  mutate(beta_Sn = gamma_Sn / S)



##------some extra work to look at the whole rarefaction curves-------
# get gamma-scale rarefactions ready for plotting
gamma_expandSn <- gamma_S %>%
  unnest(gamma_Sn) %>%
  group_by(group) %>%
  mutate(N = 1:n(), 
         scale = 'gamma') %>%
  ungroup() %>%
  separate(group, into = c('group', 'Replicate'), sep = '\\.') %>%
  separate(group, into = c('Distribution', 'CV'), sep = '_', remove = FALSE)

gamma_expandS <- gamma_S %>%
  unnest(gamma_S) %>%
  group_by(group) %>%
  mutate(N = 1:n(), 
         scale = 'gamma') %>%
  ungroup() %>%
  separate(group, into = c('group', 'Replicate'), sep = '\\.') %>%
  separate(group, into = c('Distribution', 'CV'), sep = '_', remove = FALSE)

# want alpha-scale rarefactions too
alpha_comms <- comm_dat %>%
  gather(species, abundance, species1:species99) %>%
  as_tibble() %>%
  group_by(group, Replicate, species) %>%
  summarise(N = sum(abundance)) %>%
  filter(N > 0) %>%
  nest(N)

alpha_rare <- alpha_comms %>%
  mutate(alpha_rareS = map(data, ~ rarefaction(.x$N, method = 'indiv')))

alpha_expandS <- alpha_rare %>%
  unnest(alpha_rareS) %>%
  group_by(group, Replicate) %>%
  mutate(N = 1:n(),
         scale = 'alpha') %>%
  ungroup() %>%
  separate(group, into = c('Distribution', 'CV'), sep = '_', remove = FALSE)




#----visualisations-----------
# alpha-scale richness
ggplot() +
  # facet_wrap(Distribution~)
  geom_boxplot(data = diversity,
               aes(x = group, y = S))

# alpha-scale evenness (called S_PIE)
ggplot() +
  # facet_wrap(Distribution~)
  geom_boxplot(data = diversity,
               aes(x = group, y = S_PIE))

# we know gamma-scale S with complete sampling, 
# but how did gamma-scale evenness change across our different communities
ggplot() +
  geom_point(data = diversity,
               aes(x = group, y = gamma_S_PIE),
             size = 4)

# before looking at beta-diversity, lets look at the rarefaction curves at two scales together
# combine alpha- and gamma-scale to see the beta_Sn
ggplot() +
  facet_grid(CV ~ Distribution, scales = 'free_x') +
  geom_line(data = gamma_expandSn,
            aes(x = N, y = gamma_Sn, group = interaction(Replicate,CV)),
            size = 1.5, colour = 'black') +
  geom_line(data = alpha_expandS,
            aes(x = N, y = alpha_rareS, group = interaction(Replicate,CV),
                colour = as.factor(CV)), alpha = 0.5) +
  ylab('Species richness') +
  scale_colour_viridis_d() +
  # scale_x_continuous(trans = 'log2') +
  # scale_y_continuous(trans = 'log2') +
  theme_bw() +
  theme(legend.position = 'topleft')


# beta-S
ggplot() +
  geom_boxplot(data = diversity,
               aes(x = group, y = beta_S))

# beta-S_PIE
ggplot() +
  geom_boxplot(data = diversity,
               aes(x = group, y = beta_S_PIE))

# beta-Sn
ggplot() +
  geom_boxplot(data = diversity,
               aes(x = group, y = beta_Sn))


# rarefaction curves underly all these metrics
# alpha-scale
ggplot() +
  facet_wrap( ~ Distribution, scales = 'free_x') +
  geom_line(data = alpha_expandS,
            aes(x = N, y = alpha_rareS, group = interaction(Replicate,CV),
                colour = as.factor(CV)),
            size = 1.5) +
  ylab('Species richness') +
  scale_colour_viridis_d(name = 'CV') +
  # scale_x_continuous(trans = 'log2') +
  # scale_y_continuous(trans = 'log2') +
  theme_bw() +
  theme(legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.background = element_blank())


# gamma-scale
ggplot() +
  facet_wrap( ~ Distribution, scales = 'free_x') +
  geom_line(data = gamma_expandS,
            aes(x = N, y = gamma_S, group = interaction(Replicate,CV),
                colour = as.factor(CV)),
            size = 1.5) +
  ylab('Species richness') +
  scale_colour_viridis_d(name = 'CV') +
  # scale_x_continuous(trans = 'log2') +
  # scale_y_continuous(trans = 'log2') +
  theme_bw() +
  theme(legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.background = element_blank())


