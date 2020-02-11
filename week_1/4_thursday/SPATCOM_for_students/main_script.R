# Main script SPATCOM metacommunity model 
# for Teaching course "Modeling species distribution and biodiversity patterns"
#
# R code from Duarte Viana, 2018
# additions and modifications by Andrea Mentges, 2020

# 1. Preparations
#================================================================================

# Install packages (only the first time, not needed after this)
install.packages(c("Rcpp", "rJava", "geiger", "gstat", "plotrix", "wrswoR", "dplyr", "sp"))

# Load packages to library (only after start of new session)
library("geiger")
library("gstat")
library("plotrix")
library("wrswoR")
library("dplyr")
library("sp")

# Add your path to the folder with SPATCOM functions here!!
setwd("/Users/am41xite/Nextcloud/Teaching/SPATCOM_for_students")


# 2. Set parameters
#================================================================================
 
# clear plots and variables
plot.new()
rm(list=ls())

# Load the functions
source("SPATCOM_functions.R")

# Landscape parameters
grid.side = 30 # number of cells in each side of the quadrangular grid (default is 30)
R = 60 # species pool (number of species, default is 60)
J = 0.8 # initial proportion of occupied cells (default is 0.8)
cor_env = 30 # width of spatial correlation in the environment (default is 30)

# Model parameters
D = 0.01 # mortality rate (default is 0.01)
M = 0.5 # productivity (proportion of individuals reproducing, default is 0.5)
Z = 10 # number of dispersed propagules per individual (default is 10)
meta.type = "SS"# metacommunity type, "SS" = species-sorting, or "NM" = neutral (default is "SS")
sd.disp = 0.7 # dispersal capacity (log SD of lognormal kernel, default is 0.7)
sd.niche = 10 # niche breadth (SD of normal distribution, default is 10)
G = 1000 # number of time steps (generation times, default is 1000)

# 3. Initialize landscape and species
#================================================================================

# Initialize environment / landscape
set.seed(79) # optional seed: fixes the environment to one specific stochastic outcome
envi = env.set(grid.side=grid.side, R=R, Nenv=1, type="gradient",
               cor.range=cor_env, Nsamples=50)
coords2 = as.data.frame(envi$coords)
xyf = envi$xy
yyf = as.data.frame(envi$yy.data[,1:1])

# Initialize species
set.seed(49) # optional seed: fixes species distribution to specific outcome
spi = sp.set(grid.side=grid.side, R=R, Ji=J, N_env=1, evolve=FALSE, sp.distr="random")


# 4. Run model
#================================================================================

# Run simulation
sp.ini = NaN
set.seed(77) # optional seed: fixes meta-community process outcome
sp.ini = spatcom(sp.i=spi, env.i=envi, D=D, M=M, Z=Z, kern.type="lnorm",
                  peak.disp=0, sd.disp=sd.disp, sd.niche=sd.niche, meta.type=meta.type, G=G)


# 5. Show results
#================================================================================

# Final picture of spatial distribution
plot.new()
dataForEnvPlot = SpatialPixelsDataFrame(coordinates(xyf),as.data.frame(yyf)) # get data for environment
fsample.sp = as.factor(sp.ini) # get data for species
if(all(is.na(sp.ini))) fsample.sp = rep(0,1,length(sp.ini)) # if all NA, replace by zeros for plotting
sp.distr = SpatialPixelsDataFrame(coordinates(xyf),as.data.frame(fsample.sp))
par(mfrow=c(1,2)) # plot environment and species next to each other
a = spplot(dataForEnvPlot, main = "landscape", col.regions=hcl.colors(R, palette = "Greens 2"))
b = spplot(sp.distr, col.regions=hcl.colors(R, palette = "Spectral"), main = "species")
print(a, position = c(0, 0, 0.5, 1), more = TRUE)
print(b, position = c(0.5, 0, 1, 1))

# Regional occupancy at the end (fraction of cells occupied)
sprintf("At the end of the simulation, %i%% of cells are occupied.", round(length(sp.ini[!is.na(sp.ini)])/(grid.side*grid.side)*100))

# Regional richness at the end
sprintf("At the end of the simulation, species richness is %i (of %i, i.e. %i%% of species pool).", length(unique(sp.ini[!is.na(sp.ini)])), R, round(100*length(unique(sp.ini[!is.na(sp.ini)]))/R))

