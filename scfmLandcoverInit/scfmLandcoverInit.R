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
    defineParameter(".plotInitialTime", "numeric", NA_real_, desc="Initial time for plotting"),
    defineParameter(".plotInterval", "numeric", NA_real_, desc="Interval between plotting"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, desc="Initial time for saving"),
    defineParameter(".saveInterval", "numeric", NA_real_, desc="Interval between save events")),
  inputObjects=data.frame(objectName="vegMapLcc",
                          objectClass="RasterLayer",
                          other=NA_character_, stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=c("flammable"),
                           objectClass=c("RasterLayer"),
                           other=rep(NA_character_, 1L), 
                           stringsAsFactors=FALSE)
))

doEvent.scfmLandcoverInit = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
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

### template initilization
scfmLandcoverInitInit = function(sim) {
  # these classes are LCC05 specific
  nonFlammClasses<-c(26,27,28,29)
  oldClass <- 0:39
  newClass <- ifelse(oldClass %in% nonFlammClasses,0,1)
  flammableTable <- cbind(oldClass, newClass)
  sim$flammable <- ratify(reclassify(sim$vegMapLcc, flammableTable,count=TRUE))
  #the count options should cause that "a column with frequencies is added. 

  setColors(sim$flammable, n=2) <- c("blue","red")
  return(invisible(sim))
}
