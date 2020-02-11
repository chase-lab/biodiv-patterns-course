################################
## SPATCOM sampling functions ##
###############################

# Define grid
grid.build<-function(grid.side,edge){
  xy <- expand.grid(0:(grid.side-1), 0:(grid.side-1))
  names(xy) <- c("x","y")
  gridded(xy) = ~x+y
  coords<-coordinates(xy)
  coords2<-as.data.frame(coords)
  edge.min<-min(coords2$x)+edge
  cgrid<-which(coords2$x>=edge&coords2$x<(grid.side-edge)&coords2$y>=edge&coords2$y<(grid.side-edge))
  xyf<-xy[cgrid]
  res<-list(grid=xyf,crop=cgrid)
  return(res)
}

# a<-grid.build(140,20)

# Define sampling plots
sample.design<-function(grid,Nsamples){
  subsample<-stratify(grid,nStrata=Nsamples,nTry=2,equalArea=TRUE)
  sample.id<-(attributes(subsample)$stratumId)+1
  # coordinates matrix
  mat.coords<-attributes(subsample)$centroids
  mat.coords<-as.data.frame(mat.coords)
  res<-list(sample.id=sample.id,mat.coords=mat.coords)
  return(res)
}

# b<-sample.design(a$grid,100)


# Subset sampling plots evenly in space (regular subset)
subsamp.even<-function(grid,samples,Nsamples){
  mat.coords<-samples$mat.coords
  xy.sp<-SpatialPoints(mat.coords)
  sample.id<-samples$sample.id
  coords<-coordinates(grid)
  coords2<-as.data.frame(coords)
  xmin<-range(coords2$x)[1]
  xmax<-range(coords2$x)[2]
  ymin<-range(coords2$y)[1]
  ymax<-range(coords2$y)[2]
  toler<-1
  area<-SpatialPoints(data.frame(x=c(xmin-toler,xmax+toler,xmin-toler,xmax+toler),
                                 y=c(ymax+toler,ymax+toler,ymin-toler,ymin-toler)))
  subsamp<-spsample(area,Nsamples,"regular")
  # pick the point at minimum distance from the original centroids
  subid<-numeric()
  for(i in 1:length(subsamp)){
    pi<-subsamp[i]
    disti<-gDistance(pi,xy.sp,byid=T)
    subid[i]<-which(disti==min(disti))
  }
  subid<-subid[!duplicated(subid)]
  sample.id[-which(sample.id %in% subid)]<-NA
  mat.coords<-mat.coords[subid,]
  res<-list(sample.id=sample.id,mat.coords=mat.coords)
  return(res)
}

# d<-subsamp.even(a$grid,b,4)


# Sample across spatial extent (according to distance classes or buffers)
samp.extents<-function(samples,nclass){
  mat.coords<-as.data.frame(samples$mat.coords)
  xy.sp<-SpatialPoints(mat.coords)
  Nsamples<-nrow(mat.coords)
  dmin<-0
  dmax<-max(dist(mat.coords))
  dxy<-seq(dmin, dmax, le = nclass + 1)
  dxy<-dxy[-1]
  # Build list of buffers with respective sampling plots
  plot.ids<-list()
  for(r in 1:Nsamples){
    origin<-xy.sp[r]
    bufs<-list()
    for(i in 1:length(dxy)){
      buf<-gBuffer(origin,width=dxy[i])
      plots.in<-gIntersection(buf,xy.sp,byid=T,id=row.names(mat.coords))
      plot.id<-as.numeric(row.names(plots.in))
      bufs[[i]]<-plot.id
    }
    len<-unlist(lapply(bufs,length))
    dif<-len[2:length(len)] - len[1:(length(len)-1)]
    exclude<-c(which(len<10),which(dif==0)+1)
    bufs<-bufs[-exclude]
    names(bufs)<-(1:length(dxy))[-exclude]
    plot.ids[[r]]<-bufs
  }
  return(plot.ids)
}

# z<-samp.extents(b,30)


# Function to sample cells at different distance classes
cell.extents<-function(Niter,samples,nclass){
  mat.coords<-coordinates(samples)
  xy.sp<-SpatialPoints(mat.coords)
  Nsamples<-nrow(mat.coords)
  dmin<-0
  dmax<-max(dist(mat.coords))
  dxy<-seq(dmin, dmax, le = nclass + 1)
  dxy<-dxy[-1]
  # Build list of buffers with respective sampling plots
  plot.ids<-list()
  for(r in 1:Niter){
    origin<-xy.sp[sample(1:Nsamples,1)]
    bufs<-list()
    for(i in 1:length(dxy)){
      buf<-gBuffer(origin,width=dxy[i])
      plots.in<-gIntersection(buf,xy.sp,byid=T,id=as.character(1:Nsamples))
      plot.id<-as.numeric(row.names(plots.in))
      bufs[[i]]<-plot.id
    }
    len<-unlist(lapply(bufs,length))
    dif<-len[2:length(len)] - len[1:(length(len)-1)]
    exclude<-c(which(len<10),which(dif==0)+1)
    bufs<-bufs[-exclude]
    names(bufs)<-(1:length(dxy))[-exclude]
    plot.ids[[r]]<-bufs
  }
  return(plot.ids)
}


# Variation partitioning with lm
varpart.lm<-function(Y,X,W){
  abc<-summary(lm(Y ~ ., data=as.data.frame(cbind(X,W))))$adj.r.squared
  ab<-summary(lm(Y ~ ., data=as.data.frame(X)))$adj.r.squared
  bc<-summary(lm(Y ~ ., data=as.data.frame(W)))$adj.r.squared
  a<-abc-bc
  b<-ab+bc-abc
  c<-abc-ab
  d<-1-abc
  return(c(a,b,c,d))
}




