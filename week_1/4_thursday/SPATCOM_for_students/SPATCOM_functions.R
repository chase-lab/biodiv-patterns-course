#################################################################################
## R code for                                                                  ##
## Spatial scale modulates the inference of metacommunity assembly processes   ##
## Duarte S. Viana & Jonathan M. Chase                                         ##
## Ecology                                                                     ##
#################################################################################

# R code by Duarte Viana, 2018

#---------------------------------------------------------------------------------------

# Helper functions

# Initial conditions: set landscape, environemntal variables and species

# Build landscape 
# "type": autocorrelated (mosaic or gradient) or random environment, 
# as well as discrete (e.g. environmental distribution matches sampling plots)
env.set <- function(grid.side, R, Nenv, type, cor.range, Nsamples){
  grid.size <- grid.side^2
  
  # Define lattice
  xy <- expand.grid(0:(grid.side-1), 0:(grid.side-1))
  names(xy) <- c("x", "y")
  gridded(xy)=~x+y
  coords <- coordinates(xy)
  
  # define environmental variables
  # If gaussian autocorrelated landscape
  if(type == "mosaic" | type == "gradient"){
    lyy <- list()
    for(i in 1:Nenv){
      if(type[i] == "mosaic") g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1, 
                       model=vgm(psill=0.025, model="Exp", range=cor.range[i]), nmax=20)
      # trend in x and y
      if(type[i] == "gradient") g.dummy <- gstat(formula=z~1+x+y, locations=~x+y, dummy=T, 
              beta=c(1, 0.01, 0.01), model=vgm(psill=0.025, range=cor.range[i], model='Exp'), nmax=20)
      # make 1 simulation based on the stat object
      yy <- predict(g.dummy, newdata=xy, nsim=1)
      # Transform environmental variable from normal to uniform
      ecum1 <- ecdf(yy@data[, 1])
      ecum.yy <- ecum1(yy@data[, 1])
      yy@data[, 1] <- ecum.yy
      # Rescale data for a given range (e.g. 0 - 10)
      yy@data[, 1] <- plotrix::rescale(yy@data[, 1], c(0, R))
      yy.data <- yy@data[, 1]
      lyy[[i]] <- yy.data
      sample.id <- NULL
      xy2 <- NULL
    }
  }
  
  if(type == "random"){
    yy <- SpatialPixelsDataFrame(coordinates(xy), as.data.frame(runif(grid.size, 0, 1)))
    lyy <- plotrix::rescale(yy@data[, 1], c(0, R))
    sample.id <- NULL
    xy2 <- NULL
  }
  
  # If environmental distribution matches sampling plots
  if(type == "discrete_mosaic" | type == "discrete_gradient"){
    library(spcosa)
    # Define sampling plots
    grain <- grid.side^2/Nsamples # grain size
    subsample <- stratify(xy, nStrata=Nsamples, nTry=2, equalArea=TRUE)
    sample.id <- attributes(subsample)$stratumId
    xy2 <- getCentroid(subsample)
    lyy <- list()
    for(i in 1:Nenv){
      if(type == "discrete_mosaic") g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1, 
                                                   model=vgm(psill=0.025, model="Exp", cor.range), nmax=20)
      if(type == "discrete_gradient") g.dummy <- gstat(formula=z~1+x+y, locations=~x+y, dummy=T, 
                                                     beta=c(1, 0.01, 0.01), model=vgm(psill=0.025, range=cor.range, model='Exp'), nmax=20)
      # make 1 simulation based on the stat object
      yy0 <- predict(g.dummy, newdata=xy2, nsim=1)
      # disaggregate to species grid
      yy <- SpatialPixelsDataFrame(coords, data.frame(yy=rep(0, nrow(coords))))
      for(j in 1:Nsamples) yy@data[sample.id == j-1, 1] <- yy0@data[j, 1]
      # Transform environmental variable from normal to uniform
      ecum1 <- ecdf(yy@data[, 1])
      ecum.yy <- ecum1(yy@data[, 1])
      yy@data[, 1] <- ecum.yy
      # Rescale data for a given range (e.g. 0 - 10)
      yy@data[, 1] <- plotrix::rescale(yy@data[, 1], c(0, R))
      yy.data <- yy@data[, 1]
      lyy[[i]] <- yy.data
    }
  }
  
  lyy <- as.data.frame(lyy)
  output <- list(xy=xy, coords=coords, yy.data=as.matrix(as.data.frame(lyy)), sample.id=sample.id, mat.coords=xy2)
  return(output)
}


# Build regional species pool
sp.set <- function(grid.side, R, Ji, N_env, evolve=FALSE, sp.distr="random"){
  grid.size <- grid.side^2
  J <- Ji*grid.size # N individuals 
  
  # simulate phylogenetic tree with R species
  tree <- sim.bdtree(b=0.10, d=0.01, stop="taxa", n=R, extinct=FALSE)
  tree <- drop.extinct(tree)
  # define traits
  if(N_env == 1 && !evolve){ 
    simt <- as.data.frame(seq(0.5, R-0.5, 1))
  }
  if(N_env == 1 && evolve){ 
    simt <- sim.char(tree, par=0.02, model="BM", nsim=1)[, , 1]
    simt <- as.data.frame(plotrix::rescale(simt, c(0.5, R-0.5)))
  }
  if(N_env>1){
    corr <- 0 # correlation between traits
    q <- diag(x=1, N_env, N_env) 
    q[q == 0] <- corr
    simt <- sim.char(tree, par=q, model="BM", nsim=1)
    simt <- as.data.frame(simt[, , 1])
    for(i in 1:N_env){
      simt[, i] <- plotrix::rescale(simt[, i], c(0.5, R-0.5))
    }
  }
  
  # initial species distribution 
  # random
  if(sp.distr == "random"){
    sample.sp <- rep(1:R, each=round(J/R))
    pos.rand <- sample(1:grid.size, size=length(sample.sp), replace=FALSE)
    sp.ini <- numeric(grid.size)
    sp.ini[pos.rand] <- sample.sp
    sp.ini[-pos.rand] <- NA
    occup <- which(!is.na(sp.ini))
    sp.ini0 <- sp.ini
  }
  
  output <- list(R=R, opt=as.matrix(simt), sp.ini=sp.ini, occup=occup)
  return(output)
}
  

# Dispersal
library(Rcpp)
# Load dispersal functions
# Function provides the index of the destiny cell directly (not the coordinates)
# the cell index is determined by cell=x + y*gridwidth
sourceCpp("disp2.cpp")

# Colonisation
inv.compet <- function(sp.ini, yy.data, opt, hi, sd, sp, dest, type){
  destj <- unique(dest)
  spj <- na.exclude(c(sp, sp.ini[destj]))
  if(length(spj) == 1) return(spj)
  if(length(spj)>1){
    if(type == "SS") {
      E <- yy.data[destj, ]
      oni <- t(opt[sp, ])
      lambda <- exp(-(E-oni)^2/(2*sd^2))
      if(nrow(lambda) == 1) winprobj <- lambda
      if(nrow(lambda)>1){
        winprobj <- lambda[1, ]
        for(l in 2:nrow(lambda)) winprobj <- winprobj*lambda[l, ]
      }
    }
    if(type == "PD") {
      hii <- hi[spj]
      winprobj <- hii/max(hii)
    }
    if(type == "NM") {
      winprobj <- rep(1, length(spj))
    }
    srand <- sample_int_ccrank(length(spj), 1, winprobj)
    return(spj[srand])
  }
}


#---------------------------------------------------------------------------------------

# Simulation function: assembly of a (meta)community
# according to a given dynamics (meta.type):
# neutral ("NM"), species sorting ("SS"), mass effects ("ME") or stochastic ("STOCHAST") model 

spatcom <- function(sp.i, env.i, D=0.02, M=0.7, Z=10, kern.type, 
                  peak.disp=0, sd.disp, sd.niche, meta.type, G){
  R <- sp.i$R
  sp.ini <- sp.i$sp.ini
  occup <- sp.i$occup
  coords <- env.i$coords
  grid.size <- length(sp.ini)
  grid.side <- sqrt(grid.size)
  mean.disp <- exp(peak.disp+(0.5*sd.disp))
  I <- ifelse(mean.disp<(grid.side/2), 
            ((grid.side*4)*mean.disp)/(pi*(grid.side^2)), 
            ((grid.side*4)*(grid.side/2))/(pi*(grid.side^2)))
  
  if(meta.type == "NM"){
    for(i in 1:G){
      #print(i)
      # remove D individuals from the community randomly
      death.pos <- sample_int_ccrank(length(occup), round(D*length(occup)), rep(1, length(occup)))
      sp.ini[occup[death.pos]] <- NA
      empty <- which(is.na(sp.ini))
      occup <- which(!is.na(sp.ini))
      # Choose mothers
      M.picks <- sample_int_ccrank(length(occup), round(M*length(occup)), rep(1, length(occup)))
      M.pos <- occup[M.picks]
      
      # Generate M draws of Z seeds and Z directions randomly
      # Dispersal events outside grid are discarded (absorbent boundaries)
      # If destination cell is occupied by a resident, there is NO colonization
      if(kern.type == "lnorm") desti <- disp("lnorm", Z, peak.disp, sd.disp, grid.side, coords[M.pos, , drop=F])
      if(kern.type == "exp") desti <- disp("exp", Z, peak.disp, sd.disp, grid.side, coords[M.pos, , drop=F])
      invdf <- data.frame(sp=rep(sp.ini[M.pos], each=Z), dest=desti)
      invdf <- invdf[which(invdf$dest %in% empty), ]
      
      
      # Colonisation
      # immigrants (from outside the lattice)
      imm.sp <- sample(1:R, round(I*length(unique(invdf$dest))), replace=TRUE)
      imm.dest <- sample(empty, round(I*length(unique(invdf$dest))), replace=FALSE)
      if(length(imm.dest)>0) { 
        sp.ini[imm.dest] <- imm.sp
        invdf <- invdf[-which(invdf$dest %in% imm.dest), ]
      }
      if(nrow(invdf)>0){
        # When there is multiple seeds (possibly of multiple species), colonize cell with one seed randomly
        winsp <- invdf %>% group_by(dest) %>% summarize(sp=inv.compet(sp.ini, sp=sp, dest=dest, type="NM"))
        sp.ini[winsp$dest] <- winsp$sp
      }
    }
  }
  
  if(meta.type == "SS"){
    opt <- sp.i$opt
    yy.data <- env.i$yy.data
    sd <- sd.niche
    for(i in 1:G){
      # remove D individuals from the community according to niche adequacy
      # Pick environmental value for each individual
      E <- yy.data[occup, ]
      # Pick optimum niche value for each individual
      oni <- opt[sp.ini[occup], ]
      # Calculate niche match for each individual
      lambda <- exp(-(E-oni)^2/(2*sd^2)) # from Latombe et al. 2015 
      # remove dead individuals
      if(is.numeric(lambda)) prob.lambda <- (1 + max(lambda)) - lambda
      if(is.matrix(lambda)){
        prob.lambda <- lambda[, 1]
        for(l in 2:ncol(lambda)) prob.lambda <- prob.lambda*lambda[, l]
        prob.lambda <- (1 + max(prob.lambda)) - prob.lambda
      }
      death.pos <- sample_int_ccrank(length(occup), size=round(D*length(occup)), prob=prob.lambda)
      sp.ini[occup[death.pos]] <- NA
      empty <- which(is.na(sp.ini))
      occup <- which(!is.na(sp.ini))
      # Choose mothers
      # Pick environmental value for each individual
      E <- yy.data[occup, ]
      # Pick optimum niche value for each individual
      oni <- opt[sp.ini[occup], ]
      # Calculate niche match for each individual
      lambda <- exp(-(E-oni)^2/(2*sd^2)) # from Latombe et al. 2015 
      # Pick mothers
      if(is.numeric(lambda)) prob.lambda <- lambda
      if(is.matrix(lambda)){
        prob.lambda <- lambda[, 1]
        for(l in 2:ncol(lambda)) prob.lambda <- prob.lambda*lambda[, l]
      }
      M.picks <- sample_int_ccrank(length(occup), size=round(M*length(occup)), prob=prob.lambda)
      M.pos <- occup[M.picks]
      
      # Generate M draws of Z seeds and Z directions randomly
      # Dispersal events outside grid are discarded (absorbent boundaries)
      # If destination cell is occupied by a resident, there is NO colonization
      if(kern.type == "lnorm") desti <- disp("lnorm", Z, peak.disp, sd.disp, grid.side, coords[M.pos, , drop=F])
      if(kern.type == "exp") desti <- disp("exp", Z, peak.disp, sd.disp, grid.side, coords[M.pos, , drop=F])
      invdf <- data.frame(sp=rep(sp.ini[M.pos], each=Z), dest=desti)
      invdf <- invdf[which(invdf$dest %in% empty), ]
      # Add immigrants (from outside the lattice)
      invimm <- data.frame(sp=sample(1:R, round(I*length(unique(invdf$dest))), replace=TRUE), 
                         dest=sample(empty, round(I*length(unique(invdf$dest))), replace=TRUE))
      invdf <- bind_rows(invdf, invimm)
      
      # Colonisation
      if(nrow(invdf)>0){
        # When there are multiple candidate species, the species with higher niche match wins
        winsp <- invdf %>% group_by(dest) %>% summarize(sp=inv.compet(
          sp.ini, yy.data=yy.data, opt=opt, sd=sd, sp=sp, dest=dest, type="SS"))
        sp.ini[winsp$dest] <- winsp$sp
      }
    }
  }
  
  if(meta.type == "ME"){
    opt <- sp.i$opt # matrix
    yy.data <- env.i$yy.data # list
    sd <- sd.niche
    for(i in 1:G){
      # remove D individuals from the community according to niche adequacy
      # Pick environmental value for each individual
      E <- yy.data[occup, ]
      # Pick optimum niche value for each individual
      oni <- opt[sp.ini[occup], ]
      # Calculate niche match for each individual
      lambda <- exp(-(E-oni)^2/(2*sd^2)) # from Latombe et al. 2015 
      # remove dead individuals
      if(is.numeric(lambda)) prob.lambda <- (1 + max(lambda)) - lambda
      if(is.matrix(lambda)){
        prob.lambda <- lambda[, 1]
        for(l in 2:ncol(lambda)) prob.lambda <- prob.lambda*lambda[, l]
        prob.lambda <- (1 + max(prob.lambda)) - prob.lambda
      }
      death.pos <- sample_int_ccrank(length(occup), size=round(D*length(occup)), prob=prob.lambda)
      sp.ini[occup[death.pos]] <- NA
      empty <- which(is.na(sp.ini))
      occup <- which(!is.na(sp.ini))
      # Choose mothers
      # Pick environmental value for each individual
      E <- yy.data[occup, ]
      # Pick optimum niche value for each individual
      oni <- opt[sp.ini[occup], ]
      # Calculate niche match for each individual
      lambda <- exp(-(E-oni)^2/(2*sd^2)) # from Latombe et al. 2015 
      # Pick mothers
      if(is.numeric(lambda)) prob.lambda <- lambda
      if(is.matrix(lambda)){
        prob.lambda <- lambda[, 1]
        for(l in 2:ncol(lambda)) prob.lambda <- prob.lambda*lambda[, l]
      }
      M.picks <- sample_int_ccrank(length(occup), size=round(M*length(occup)), prob=prob.lambda)
      M.pos <- occup[M.picks]
      
      # Generate M draws of Z seeds and Z directions randomly
      # Dispersal events outside grid are discarded (absorbent boundaries)
      # If destination cell is occupied by a resident, there is NO colonization
      desti <- disp("unif", Z, peak.disp, sd.disp, grid.side, coords[M.pos, , drop=F])
      invdf <- data.frame(sp=rep(sp.ini[M.pos], each=Z), dest=desti)
      invdf <- invdf[which(invdf$dest %in% empty), ]
      # Add immigrants (from outside the lattice)
      invimm <- data.frame(sp=sample(1:R, round(I*length(unique(invdf$dest))), replace=TRUE), 
                         dest=sample(empty, round(I*length(unique(invdf$dest))), replace=TRUE))
      invdf <- bind_rows(invdf, invimm)
      
      # Colonisation
      if(nrow(invdf)>0){
        # When there are multiple candidate species, the species with higher niche match wins
        winsp <- invdf %>% group_by(dest) %>% summarize(sp=inv.compet(
          sp.ini, yy.data=yy.data, opt=opt, sd=sd, sp=sp, dest=dest, type="SS"))
        sp.ini[winsp$dest] <- winsp$sp
      }
    }
  }
  
  if(meta.type == "STOCHAST"){
    for(i in 1:G){
      # remove D individuals from the community randomly
      death.pos <- sample_int_ccrank(length(occup), round(D*length(occup)), rep(1, length(occup)))
      sp.ini[occup[death.pos]] <- NA
      empty <- which(is.na(sp.ini))
      occup <- which(!is.na(sp.ini))
      # Choose mothers
      M.picks <- sample_int_ccrank(length(occup), round(M*length(occup)), rep(1, length(occup)))
      M.pos <- occup[M.picks]
      
      # Generate M draws of Z seeds and Z directions randomly
      # Dispersal events outside grid are discarded (absorbent boundaries)
      # If destination cell is occupied by a resident, there is NO colonization
      desti <- disp("unif", Z, peak.disp, sd.disp, grid.side, coords[M.pos, , drop=F])
      invdf <- data.frame(sp=rep(sp.ini[M.pos], each=Z), dest=desti)
      invdf <- invdf[which(invdf$dest %in% empty), ]
      
      # Colonisation
      # immigrants (from outside the lattice)
      imm.sp <- sample(1:R, round(I*length(unique(invdf$dest))), replace=TRUE)
      imm.dest <- sample(empty, round(I*length(unique(invdf$dest))), replace=FALSE)
      if(length(imm.dest)>0) { 
        sp.ini[imm.dest] <- imm.sp
        invdf <- invdf[-which(invdf$dest %in% imm.dest), ]
      }
      if(nrow(invdf)>0){
        # When there is multiple seeds (possibly of multiple species), colonize cell with one seed randomly
        winsp <- invdf %>% group_by(dest) %>% summarize(sp=inv.compet(sp.ini, sp=sp, dest=dest, type="NM"))
        sp.ini[winsp$dest] <- winsp$sp
      }
    }
  }
  return(sp.ini)
}
  



  
  
  
  
  









