# Exercise: "Components of biodiversity along altitudinal gradients"

# Based on data from:
#     Choi, S.-W. and Thein, P.P. (2018), Distribution breadth and species turnover of night-flying beetles and moths on different mainland and island mountains. Ecol. Res., 33: 237-247. doi:10.1007/s11284-017-1555-z
# Script: Thore Engel


# Load packages
library(tidyverse)
library(vegan)
library(mobr)

# set your current working directory
setwd() # change to your directory

# Read in the data (abundances and sample information) 
adundance_file<-"Choi2018_X_abu.csv" # Change to your filename!
info_file<-"Choi2018_X_info.csv" # Change to your filename!
dat<-read.csv(adundance_file,row.names = 1)
dat_info<-read.csv(info_file)

# Inspect the data!

# 1. Draw individual-based rarefaction curves for each site in the dataset and color them by altitude.
# What can you observe?

rarecurve(dat)

# now lets color this by elevation

# Here is a useful function for the rarefaction curves
# This function returns the rarefaction curves of a all sites in a site-by-species matrix
# in a long data format which is useful for plotting purposes. 
rarefy_long <- function(x) {
    if( is.data.frame(x)) x = as.matrix(x)
    if(is.matrix(x)==F) x=matrix(x,nrow = 1, byrow =T, dimnames= list("x", names(x)))
    alphas <-
        lapply(row.names(x), function(i)
            return(as.numeric(vegan::rarefy(
                x[i, ], sample = 1:sum(x[i, ])
            )))) %>%
        lapply(function(x)
            return(data.frame(
                S_n = as.numeric(x), N = 1:length(x)
            )))
    names(alphas) <- rownames(x)
    alphas <- alphas %>% plyr::ldply(.id = "SiteID")
    out = alphas 
}

# Calculate Rarefaction
curve<- rarefy_long(dat)

# Add sample information (Elevation)
curve <- curve %>%  left_join(dat_info, by = "SiteID")

# Plot 
curve %>% ggplot(aes(x= N, y= S_n, group= SiteID, col= Elevation))+geom_line(size=1) # + coord_trans(x="log", y="log")

# 2. Calculate number of individuals (N), observed richness (S_obs), rarefied richness (S_n)
# and S_PIE and examine their altitudinal trends.

results<-
    data.frame(
    SiteID= rownames(dat),
    S_obs=specnumber(dat),
    N= rowSums(dat),
    S_PIE = calc_PIE(dat, T)
    )

# Rarefy to smallest number of individuals
N_min= min(results$N)
results$S_n<- rarefy(dat, N_min)

# Add sample info
results<-results %>% left_join(dat_info, by = "SiteID")

# plot observed species richness and examine the shape of the relationship
results %>% ggplot(aes(x=Elevation, y= S_obs))+ geom_point()+ 
    geom_smooth(method = 'gam', se = F,formula = y ~ s(x, bs = 'cs', k = 3))

# plot the other metrics adapting the code from above

# Do these metrics fit to the rarefaction curves that you plotted before?

# Repeat for the other data set and compare among them!



    


