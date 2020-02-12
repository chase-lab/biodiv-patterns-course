# Exercise 1 ##
###############

# How do biodiversity components...

#         Total abundance (N)
#         Species abundance distribution (SAD)
#         Intraspecific aggregation (clumping)

# ...influence the scaling-relationships?

#         Rarefaction curve (spec_sample_curve)
#         Species-accumulation curve (spec_sample_curve)
#         Species-area relationship (divar)



#############################
# a) Effect of total abundance
#############################

library(mobsim)
# simulate high N community
sim_n_high <- sim_poisson_community(s_pool = 200, n_sim = 1000, sad_type = "lnorm",
                                   sad_coef = list("cv_abund" = 1))
par(mfrow=c(1,2)) # make two plotting panels 
plot(sim_n_high)
# extract sad
sad_n_high<-community_to_sad(sim_n_high)
plot(sad_n_high,method = "rank")

#simulate low N community
sim_n_low <- sim_poisson_community(s_pool = 200, n_sim = 500, sad_type = "lnorm",
                                    sad_coef = list("cv_abund" = 1))
plot(sim_n_low)
# extract sad
sad_n_low<-community_to_sad(sim_n_low)
plot(sad_n_low,method = "rank")


# Calculate rarefaction/ accumulation curves 
curve_n_high<-spec_sample_curve(sim_n_high)
curve_n_low<-spec_sample_curve(sim_n_low)

# plot rarefaction curve
par(mfrow=c(1,1))
plot.default(curve_n_high[,c(1,3)], type="l", main="species rarefaction curve")
lines(curve_n_low[,c(1,3)], col=2)
legend("bottomright", legend= c("high N", "low N"), col=c(1,2),lty = 1)

# plot accumulation curve
par(mfrow=c(1,1))
plot.default(curve_n_high[,c(1,2)], type="l", main="species accumulation curve")
lines(curve_n_low[,c(1,2)], col=2)
legend("bottomright", legend= c("high N", "low N"), col=c(1,2),lty = 1)

# calculate species area curve
area <- c(0.001,0.002,0.005,0.01,0.02,0.05,0.1,0.2,0.5,1.0)
sar_n_high <- divar(sim_n_high, area,)
sar_n_low <- divar(sim_n_low, area)

plot(m_species~prop_area, data=sar_n_high,type = "b")
lines(m_species~prop_area, data=sar_n_low,type = "b",col=2)
legend("bottomright",c("N high","N low"), col = 1:2, pch = 1)
# how does it look with log-transformed axes? (log="xy")


#############################
#Effect of SAD (evenness)
#############################

# simulate even community
sim_more_even <- sim_poisson_community(s_pool = 20, n_sim = 2000, sad_type = "lnorm",
                                    sad_coef = list("cv_abund" = 0))
par(mfrow=c(1,2)) # make two plotting panels 
plot(sim_more_even)
# extract sad
sad_more_even<-community_to_sad(sim_more_even)
plot(sad_more_even,method = "rank")

#simulate less even community
sim_less_even <- sim_poisson_community(s_pool = 20, n_sim = 2000, sad_type = "lnorm",
                                   sad_coef = list("cv_abund" = 3))
plot(sim_less_even)
# extract sad
sad_less_even<-community_to_sad(sim_less_even)
plot(sad_less_even,method = "rank")

par(mfrow=c(1,2))
pie(sort(sad_more_even, decreasing = T),labels = NA, main="more even")
pie(sort(sad_less_even, decreasing = T),labels = NA, main="less even")

# Calculate rarefaction/ accumulation curves 
curve_more_even<-spec_sample_curve(sim_more_even)
curve_less_even<-spec_sample_curve(sim_less_even)

# plot rarefaction curve
par(mfrow=c(1,1))
plot.default(curve_more_even[,c(1,3)], type="l", main="species rarefaction curve")
lines(curve_less_even[,c(1,3)], col=2)
legend("bottomright", legend= c("more even", "less even"), col=c(1,2),lty = 1)

# plot accumulation curve
par(mfrow=c(1,1))
plot.default(curve_more_even[,c(1,2)], type="l", main="species accumulation curve")
lines(curve_less_even[,c(1,2)], col=2)
legend("bottomright", legend= c("more even", "less even"), col=c(1,2),lty = 1)


# calculate species area curve
area <- c(0.001,0.002,0.005,0.01,0.02,0.05,0.1,0.2,0.5,1.0)
sar_more_even <- divar(sim_more_even, area)
sar_less_even <- divar(sim_less_even, area)

plot(m_species~prop_area, data=sar_more_even,type = "b")
lines(m_species~prop_area, data=sar_less_even,type = "b",col=2)
legend("bottomright",c("more even","less even"), col = 1:2, pch = 1)
# how does it look with log-transformed axes? (log="xy")

#############################
#Effect of aggregation
#############################

# simulate random community
sim_random <- sim_poisson_community(s_pool = 20, n_sim = 300, sad_type = "lnorm",
                                       sad_coef = list("cv_abund" = 1))
par(mfrow=c(1,2)) # make two plotting panels 
plot(sim_random)
# extract sad
sad_random<-community_to_sad(sim_random)
plot(sad_random,method = "rank")

#simulate clustered community
sim_clustered <- sim_thomas_community(s_pool = 20, n_sim = 300, sad_type = "lnorm",
                                       sad_coef = list("cv_abund" = 1),sigma = 0.02)
plot(sim_clustered)
# extract sad
sad_clustered<-community_to_sad(sim_clustered)
plot(sad_clustered,method = "rank")


# Calculate rarefaction/ accumulation curves 
curve_random<-spec_sample_curve(sim_random)
curve_clustered<-spec_sample_curve(sim_clustered)

# plot rarefaction curve
par(mfrow=c(1,1))
plot.default(curve_random[,c(1,3)], type="l", main="species rarefaction curve")
lines(curve_clustered[,c(1,3)], col=2)
legend("bottomright", legend= c("random", "clustered"), col=c(1,2),lty = 1)

# plot accumulation curve
par(mfrow=c(1,1))
plot.default(curve_random[,c(1,2)], type="l", main="species accumulation curve")
lines(curve_clustered[,c(1,2)], col=2)
legend("bottomright", legend= c("random", "clustered"), col=c(1,2),lty = 1)

# plot rarefaction and accumulation in same plot
plot(curve_random)
plot(curve_clustered)

# calculate species area curve
area <- c(0.001,0.002,0.005,0.01,0.02,0.05,0.1,0.2,0.5,1.0)
sar_random <- divar(sim_random, area)
sar_clustered <- divar(sim_clustered, area)

plot(m_species~prop_area, data=sar_random,type = "b")
lines(m_species~prop_area, data=sar_clustered,type = "b",col=2)
legend("bottomright",c("random","clustered"), col = 1:2, pch = 1)
# how does it look with log-transformed axes? (log="xy")


###################################################################################
###################################################################################
# Exercise 2 ##
###############

# How is the detection of biodiversity changes influenced by
# the choice of sampling scale and biodiversity index?


# 1 Simulate two different communities

# control
sim_control<-sim_poisson_community(s_pool = 30, n_sim = 5000, sad_type = "lnorm",
                      sad_coef = list("cv_abund" = 0))
par(mfrow=c(1,2)) # make two plotting panels 
plot(sim_control)
# extract sad
sad_control<-community_to_sad(sim_control)
plot(sad_control,method = "rank")

# changed SAD (less even)

sim_changed<-sim_poisson_community(s_pool = 30, n_sim = 5000, sad_type = "lnorm",
                                   sad_coef = list("cv_abund" = 5))
par(mfrow=c(1,2)) # make two plotting panels 
plot(sim_changed)
# extract sad
sad_changed<-community_to_sad(sim_changed)
plot(sad_changed,method = "rank")

# compare
pie(sort(sad_control, decreasing = T),labels = NA, main="control")
pie(sort(sad_changed, decreasing = T),labels = NA, main="changed")

# Sample both communities in the same way (sample_quadrats)
# small quadrats
par(mfrow=c(1,2))

plot(sad_control,method = "rank")
plot(sad_changed,method = "rank")
sample_small_control <- sample_quadrats(sim_control, n_quadrats = 16, quadrat_area = 0.005,
                           avoid_overlap = T, method= "grid", delta_x = 0.25, delta_y = 0.25)

sample_small_changed <- sample_quadrats(sim_changed, n_quadrats = 16, quadrat_area = 0.005,
                                        avoid_overlap = T, method= "grid", delta_x = 0.25, delta_y = 0.25)

# large quadrats
sample_large_control <- sample_quadrats(sim_control, n_quadrats = 16, quadrat_area = 0.05,
                                        avoid_overlap = T, method= "grid", delta_x = 0.25, delta_y = 0.25)

sample_large_changed <- sample_quadrats(sim_changed, n_quadrats = 16, quadrat_area = 0.05,
                                        avoid_overlap = T, method= "grid", delta_x = 0.25, delta_y = 0.25)

# Calculate species richness per quadrat
library(vegan)

#small quadrats
richness_small_control<-specnumber(sample_small_control$spec_dat)
richness_small_changed<-specnumber(sample_small_changed$spec_dat)
delta_richness_small<-mean(richness_small_control)/mean(richness_small_changed)-1

#large quadrats
richness_large_control<-specnumber(sample_large_control$spec_dat)
richness_large_changed<-specnumber(sample_large_changed$spec_dat)
delta_richness_large<-mean(richness_large_control)/mean(richness_large_changed)-1

#plot
richness_range<-range(c(richness_small_control, richness_small_changed,
                        richness_large_control, richness_large_changed))
boxplot(richness_small_control, richness_small_changed, main="small quadrats",
        ylab= "Richness", names= c("control", "changed"),ylim=richness_range)
boxplot(richness_large_control, richness_large_changed, main="large quadrats",
        ylab= "Richness", names= c("control", "changed"),ylim=richness_range)

# Calculate shannon diversity (ENS) per quadrat

#small quadrats
shannon_small_control<-exp(diversity(sample_small_control$spec_dat))
shannon_small_changed<-exp(diversity(sample_small_changed$spec_dat))
delta_shannon_small<-mean(shannon_small_control)/mean(shannon_small_changed)-1


#large quadrats
shannon_large_control<-exp(diversity(sample_large_control$spec_dat))
shannon_large_changed<-exp(diversity(sample_large_changed$spec_dat))
delta_shannon_large<-mean(shannon_large_control)/mean(shannon_large_changed)-1
shannon_range<-range(c(shannon_small_control, shannon_small_changed,
                       shannon_large_control, shannon_large_changed))
#plot
boxplot(shannon_small_control, shannon_small_changed, main="small quadrats",
        ylab= "shannon", names= c("control", "changed"), ylim=shannon_range)
boxplot(shannon_large_control, shannon_large_changed, main="large quadrats",
        ylab= "shannon", names= c("control", "changed"),ylim=shannon_range)

# Calculate Simpsons diversity (ENS) per quadrat

#small quadrats
simpson_small_control<-diversity(sample_small_control$spec_dat, index="invsimpson")
simpson_small_changed<-diversity(sample_small_changed$spec_dat, index="invsimpson")
delta_simpson_small<-mean(simpson_small_control)/mean(simpson_small_changed)-1


#large quadrats
simpson_large_control<-diversity(sample_large_control$spec_dat, index="invsimpson")
simpson_large_changed<-diversity(sample_large_changed$spec_dat, index="invsimpson")
delta_simpson_large<-mean(simpson_large_control)/mean(simpson_large_changed)-1

#plot
simpson_range<-range(c(simpson_small_control, simpson_small_changed,
                       simpson_large_control, simpson_large_changed))
boxplot(simpson_small_control, simpson_small_changed, main="small quadrats",
        ylab= "ENS_simps", names= c("control", "changed"), ylim=simpson_range)
boxplot(simpson_large_control, simpson_large_changed, main="large quadrats",
        ylab= "ENS_simps", names= c("control", "changed"),ylim=simpson_range)



###### This is extra about the bias correction

# Calculate unbiased ENS diversity per quadrat

# small quadrats
ENS_cor_small_control<-1/(2-rarefy(sample_small_control$spec_dat, 2))
ENS_cor_small_changed<-1/(2-rarefy(sample_small_changed$spec_dat, 2))
delta_ENS_cor_small<-mean(ENS_cor_small_control)/mean(ENS_cor_small_changed)-1

# large quadrats
ENS_cor_large_control<-1/(2-rarefy(sample_large_control$spec_dat, 2))
ENS_cor_large_changed<-1/(2-rarefy(sample_large_changed$spec_dat,2))
delta_ENS_cor_large<-mean(ENS_cor_large_control)/mean(ENS_cor_large_changed)-1

# plot
range_ENS_cor<-range(c(ENS_cor_small_control, ENS_cor_small_changed,
                       ENS_cor_large_control, ENS_cor_large_changed))
boxplot(ENS_cor_small_control, ENS_cor_small_changed, main="small quadrats",
        ylab= "ENS_cor", names= c("control", "changed"), ylim= range_ENS_cor)

boxplot(ENS_cor_large_control, ENS_cor_large_changed, main="large quadrats",
        ylab= "ENS_cor", names= c("control", "changed"), ylim= range_ENS_cor)


# Contrast biased and unbiased ENS
par(mfrow=c(1,1))
ENS<-c(simpson_small_control, simpson_small_changed,simpson_large_control, simpson_large_changed,
          ENS_cor_small_control, ENS_cor_small_changed,ENS_cor_large_control, ENS_cor_large_changed)
metric<-c(rep("biased",64),rep("unbiased",64))
scale<-rep(rep(c("small","large"),each=32),2)
community<-rep(rep(c("control","change"), each=16),4)
df<-data.frame(ENS,metric,scale,community,stringsAsFactors = FALSE)
library(ggplot2)
ggplot(df, aes(scale, ENS, fill=community)) +
  geom_boxplot()+facet_grid(.~metric)

