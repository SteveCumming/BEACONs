stopifnot(packageVersion("SpaDES") >= "0.99.0")

defineModule(sim, list(
  name="scfmCrop",
  description="A translator module. Crops and reprojects all necessary layer for CONSERV to
  specifed extent. Selects or subsets all spatialised data e.g. fires.
  Uswa cropped RasterLayer, defined by a spatial polygon that defines the area of interest.",
  keywords=c("translator", "lcc05", "Land Cover Classification", "vegetation","Canadian National Fire Database"),
  authors=c(person("Steve", "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut")),
            person(c("Eliot", "J","B"), "McIntire", email="emcintir@nrcan.gc.ca", role=c("aut")),
            person("Pierre", "Vernier", email="pierre.vernier@gmail.com")
            ),
  version=numeric_version("0.1.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timeunit=NA_character_,
  citation=list(),
  reqdPkgs=list("raster","rgeos","sp","archivist"),
  parameters=rbind(
    defineParameter("useCache", "logical", TRUE, NA, NA, desc="Initial time for plotting"),
    defineParameter(".plotInitialTime", "numeric", NA_real_, NA, NA, desc="Initial time for plotting"),
    defineParameter(".plotInterval", "numeric", NA_real_, desc="Interval between plotting"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, desc="Initial time for saving"),
    defineParameter(".saveInterval", "numeric", NA_real_, desc="Interval between save events")),
  inputObjects=data.frame(objectName=c("vegInput", "ageMapInit", "studyArea","firePointsInput","cacheLoc"),
                          objectClass=c("RasterLayer", "RasterLayer", "SpatialPolygons","SpatialPoints", "character"),
                          other=rep(NA_character_, 5L), stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=c("vegMapLcc", "ageMapInit", "firePoints"), #these are the names that matter for dependencies
                           objectClass=c("RasterLayer", "RasterLayer", "SpatialPoints"),
                           other=rep(NA_character_, 3L), stringsAsFactors=FALSE)
))

#Data are in ~stevec/Dropbox/SpaDES/Data/CanadianNationalFireDatabase as of 2015.07.10
#Fire data sources.Rmd  NFDB_point.zip		NFDB_poly.zip

doEvent.scfmCrop = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    
    # do stuff for this event
    sim <- cacheFunctions(sim)
    sim <- scfmCropInit(sim)
    # schedule future event(s)
    sim <- scheduleEvent(sim, params(sim)$scfmCrop$.plotInitialTime, "scfmCrop", "plot")
    sim <- scheduleEvent(sim, params(sim)$scfmCrop$.saveInitialTime, "scfmCrop", "save") 
  } else {
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with=FALSE],
                  "' in module '", events(sim)[1, "moduleName", with=FALSE], "'", sep=""))
  }
  return(invisible(sim))
}

### template initilization
scfmCropInit = function(sim) {
  
  simProjection<-crs(sim$studyArea)  #this would probably be set to be the same as the veg map at an earlier stage.
  
  #if(ncell(sim$vegMap)>5e5) beginCluster(min(parallel::detectCores(),6))
  
  #Project the study area into each input raster, then crop and mask; 
  #Then project result intpo sim projection.
  
  browser()
  
  vegProjection<-crs(sim$vegInput)
  studyAreaTmp <- sim$spTransform(sim$studyArea, CRSobj =vegProjection)
  sim$vegMapLcc <-  sim$crop(sim$vegInput, studyAreaTmp)
  crs(sim$vegMapLcc)<-vegProjection
  sim$vegMapLcc<-sim$mask(sim$vegMapLcc,studyAreaTmp) #
  sim$vegMapLcc<-sim$projectRaster(sim$vegMapLcc,crs=simProjection,method="ngb")
  tmp<-getColors(sim$vegInput)[[1]] # mask removes colors!
  setColors(sim$vegMapLcc, n=length(tmp)) <- tmp 
  
  ageProjection<-crs(sim$ageMapInit)
  studyAreaTmp <- sim$spTransform(sim$studyArea, CRSobj =ageProjection)
  sim$ageMapInit <-  sim$crop(sim$ageMapInit, studyAreaTmp)
  crs(sim$ageMapInit)<-ageProjection
  sim$ageMapInit<-sim$mask(sim$ageMapInit,studyAreaTmp)
  sim$ageMapInit<-sim$projectRaster(sim$ageMapInit,to=sim$vegMapLcc,method="ngb")
  
  fireProjection<-CRS(proj4string(sim$firePointsInput))
  studyAreaTmp <- sim$spTransform(sim$studyArea, CRSobj =fireProjection)
  sim$firePoints <- sim$firePointsInput[studyAreaTmp,]  #note possibly correct syntax A[B,] rather than A[B]
                                                        #https://cran.r-project.org/web/packages/sp/vignettes/over.pdf page 3
  #crs(sim$firePoints) <- fireProjection
  sim$firePoints <- sim$spTransform(sim$firePoints,CRSobj =simProjection)
  
  #endCluster()
    
  # age will not run with projectRaster directly. Instead, project the vegMap to age, then crop, then project back to vegMap
  #vegMap.crsAge <- projectRaster(sim$vegMap, crs=crs(sim$age))
  #age.crsAge <- crop(sim$age, spTransform(sim$studyArea, CRSobj = crs(sim$age)))
  #age.crsAge <- mask(age.crsAge, spTransform(sim$studyArea, CRSobj = crs(sim$age)))
  #sim$ageMapInit <- projectRaster(age.crsAge, to=sim$vegMap, method="ngb")
  
  if(sum(!is.na(getValues(sim$ageMapInit)))==0) stop("There are no age data provided with input age map")
  if(sum(!is.na(getValues(sim$vegMapLcc)))==0) stop("There are no vegatation data provided with input vegatation map")
  setColors(sim$ageMapInit) <- c("light green","dark green")
  
  return(invisible(sim))
}



cacheFunctions <- function(sim) {
  # for slow functions, add cached versions
  if(params(sim)$scfmCrop$useCache) {
    sim$cacheLoc <- file.path(outputPath(sim), "scfmCropCache") 
    if(!dir.exists(sim$cacheLoc) )
      createEmptyRepo(file.path(outputPath(sim), "scfmCropCache"))
    
    sim$mask <- function(...) {
      archivist::cache(cacheRepo=sim$cacheLoc, FUN=raster::mask, ...)
    }
    sim$crop <- function(...) {
      archivist::cache(cacheRepo=sim$cacheLoc, FUN=raster::crop, ...)
    }
    sim$projectRaster <- function(...) {
      archivist::cache(cacheRepo=sim$cacheLoc, FUN=raster::projectRaster, ...)
    }
    sim$spTransform <- function(...) {
      archivist::cache(cacheRepo=sim$cacheLoc, FUN=sp::spTransform,  ...)
    }
  } else {
    sim$mask <- raster::mask
    sim$crop <- raster::crop
    sim$projectRaster <- raster::projectRaster
    sim$spTransform <- sp::spTransform
  }
  
  return(invisible(sim))
}

