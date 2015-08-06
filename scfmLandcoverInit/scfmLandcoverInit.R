stopifnot(packageVersion("SpaDES") >= "0.99.0")

defineModule(sim, list(
  name="scfmLandcoverInit",
  description="Takes the LCC05 classification of 39 land cover classes, and reclassifies it to flammable and inflammable [1,0]",
  keywords=c("fire", "LCC05", "land cover classification 2005", "BEACONs"),
  authors=c(person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre")),
            person("Steve", "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut"))),
  version=numeric_version("0.1.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c("2005-01-01", NA)),
  timeunit="year",
  citation=list(),
  reqdPkgs=list("raster"),
  parameters=rbind(
    defineParameter(".plotInitialTime", "numeric", NA_real_, NA, NA, desc="Initial time for plotting"),
    defineParameter(".plotInterval", "numeric", NA_real_, NA, NA, desc="Interval between plotting"),
    defineParameter(".saveInitialTime", "numeric", NA_real_,  NA, NA, desc="Initial time for saving"),
    defineParameter(".saveInterval", "numeric", NA_real_, NA, NA, desc="Interval between save events"),
    defineParameter("useCache", "logical", TRUE, NA, NA, desc="Use cache")
    ),
  inputObjects=data.frame(objectName="vegMapLcc",
                          objectClass="RasterLayer",
                          other=NA_character_, stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=c("flammable", "fireMapAttr"), #mapAttr are all things the fir
                           objectClass=c("RasterLayer", "list"),
                           other=rep(NA_character_, 2L), 
                           stringsAsFactors=FALSE)
))

doEvent.scfmLandcoverInit = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    
    sim <- scfmLandcoverInitCacheFunctions(sim)
    sim <- scfmLandcoverInitInit(sim)
    sim <- scheduleEvent(sim, params(sim)$scfmLandcoverInit$.plotInitialTime,
                         "scfmLandcoverInit", "plot")
    sim <- scheduleEvent(sim, params(sim)$scfmLandcoverInit$.saveInitialTime,
                         "scfmLandcoverInit", "save")
  } else if (eventType=="plot") {
    Plot(sim$vegMapLcc05, sim$flammable, new=TRUE)
    # schedule future event(s)
    sim <- scheduleEvent(sim, time(sim) + params(sim)$scfmLandcoverInit$.plotInterval, "scfmLandcoverInit", "plot")
  } else if (eventType=="save") {
    # schedule future event(s)
    sim <- scheduleEvent(sim, time(sim) + params(sim)$scfmLandcoverInit$.saveInterval, "scfmLandcoverInit", "save")
  } else {
      warning(paste("Undefined event type: '", events(sim)[1, "eventType", with=FALSE],
                    "' in module '", events(sim)[1, "moduleName", with=FALSE], "'", sep=""))
  }
  return(invisible(sim))
}

genFireMapAttr<-function(sim){
  #calculate the cell size, total area, and number of flammable cells, etc.
  #browser()
  cellSize<-prod(res(sim$flammable))/1e4
  nFlammable<-table(values(sim$flammable), useNA="no")["1"] #depends on sfcmLandCoverInit
  #to agree of the meaning of 1s
  w<-matrix(c(1,1,1,1,0,1,1,1,1),nrow=3,ncol=3)
  browser()
  #it would be nice to somehow get caching to work on the function argument of focal
  #but I have not been able to make it work.
  tmp<- focal(sim$flammable, w, na.rm=TRUE) #default function is sum(...,na.rm)
  x<-values(tmp)
  x<-x[values(sim$flammable)==1] #only count neighbours for flammable cells!
  nv<-table(x,useNA="no")
  nNbrs<-rep(0,9) #guard against the terrible chance that 
  #not all nNbrs values are realised. 
  nNbrs[as.integer(names(nv))+1]<-nv
  names(nNbrs)<-0:8
  sim$fireMapAttr<-list(cellSize=cellSize,nFlammable=nFlammable,
                        burnyArea=cellSize*nFlammable, nNbrs=nNbrs)
  return(invisible(sim))
}

### template initilization
scfmLandcoverInitInit = function(sim) {
  # these classes are LCC05 specific
  nonFlammClasses<-c(36,37,38,39)
  oldClass <- 0:39
  newClass <- ifelse(oldClass %in% nonFlammClasses,0,1)
  flammableTable <- cbind(oldClass, newClass)
  sim$flammable <- ratify(reclassify(sim$vegMapLcc, flammableTable,count=TRUE))
  #the count options should cause that "a column with frequencies is added. 

  setColors(sim$flammable, n=2) <- c("blue","red")
  
  sim<-genFireMapAttr(sim)
  
  return(invisible(sim))
}

testFun<-function(x) {
  sum(na.omit(x)==1)
}

scfmLandcoverInitCacheFunctions <- function(sim) {
  # for slow functions, add cached versions
  if(params(sim)$scfmLandcoverInit$useCache) {
    sim$cacheLoc <- file.path(outputPath(sim), "scfmLandcoverInitCache") 
    if(!dir.exists(sim$cacheLoc) )
      createEmptyRepo(file.path(outputPath(sim), "scfmLandcoverInitCache"))
    
    sim$focal <- function(...) {
      archivist::cache(cacheRepo=sim$cacheLoc, FUN=raster::focal, ...)
    }
    
    sim$Frabjous <-function(...){
      archivist::cache(cacheRepo=sim$cacheLoc, FUN=sim$testFun, ...)
    }
    
  } else {
    sim$Frabjous <- function(x){sum(na.omit(x)==1)}
    sim$focal <- raster::focal
  }
  
  return(invisible(sim))
}
