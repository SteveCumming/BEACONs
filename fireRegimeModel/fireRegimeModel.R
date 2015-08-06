
defineModule(sim, list(
  name="fireRegimeModel",
  description="estimates fire regime parameters for BEACONs a la Steve's method",
  keywords=c("fire regime", "BEACONs"),
  authors=c(person(c("Steven", "G."), "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut", "cre"))),
  childModules=character(),
  version=numeric_version("0.1.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timeunit="year",
  citation=list(),
  reqdPkgs=list(),
  parameters=rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter("fireCause", "character", c("H","U"), NA, NA, "subset of c(H,H-PB,L,Re,U)"),
    defineParameter("fireEpoch", "numeric", c(1961,1990), NA, NA, "start of normal period")
  ),
  inputObjects=data.frame(
    objectName=c("firePoints","flammable","fireMapAttr"), 
    objectClass=c("SpatialPointsDataFrame", "RasterLayer", "list"),
    other=rep(NA_character_,3),
    stringsAsFactors=FALSE),
  outputObjects=data.frame(
    objectName=c("fireRegimeParameters"),
    objectClass=c("list"),
    other=rep(NA_character_,1),
    stringsAsFactors=FALSE)
))


## event types
#   - type `init` is required for initiliazation

doEvent.fireRegimeModel = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)
    
    # do stuff for this event
    
    #genFireMapAttr() now called in scfmLandcoverInit.R
    fireRegimeModelInit(sim)

    # schedule future event(s)
    scheduleEvent(sim, params(sim)$fireRegimeModel$.plotInitialTime, "fireRegimeModel", "plot")
    scheduleEvent(sim, params(sim)$fireRegimeModel$.saveInitialTime, "fireRegimeModel", "save")
  } else if (eventType=="templateEvent") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # scheduleEvent(sim, time(sim) + increment, "fireRegimeModel", "templateEvent")

    # ! ----- STOP EDITING ----- ! #
    } else {
      warning(paste("Undefined event type: '", events(sim)[1, "eventType", with=FALSE],
                    "' in module '", events(sim)[1, "moduleName", with=FALSE], "'", sep=""))
    }
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initilization


fireRegimeModelInit = function(sim) {

  #browser()
 
  source(paste(paths$modulePath,"TEutilsNew.R",sep="/"),local=TRUE,echo=FALSE)
  
  
  #subset fires by cause and epoch.   
  tmp<-as.data.frame(sim$firePoints)
 
  #extract and validate fireCause spec
  fc<-params(sim)$fireRegimeModel$fireCause
  if(any(!(fc %in% levels(sim$firePointsInput$CAUSE))))
      stop("illegal fireCause: ", fc)
  tmp<-subset(tmp,CAUSE %in% fc)
  
  #extract and validate fireEpoch
  epoch<-params(sim)$fireRegimeModel$fireEpoch
  if (length(epoch)!=2 || !is.numeric(epoch) || any(!is.finite(epoch)) || epoch[1]>epoch[2])
      stop("illegal fireEpoch: ",epoch)
  tmp<-subset(tmp, YEAR>=epoch[1] & YEAR<=epoch[2])
 
  nFires<-dim(tmp)[1]
  epochLength<-epoch[2]-epoch[1]+1  
  rate<-nFires/(epochLength * sim$fireMapAttr$burnyArea)

  #calculate escaped fires 
  xVec<-tmp$SIZE_HA[tmp$SIZE_HA > sim$fireMapAttr$cellSize]
  pEscape<-length(xVec)/nFires
  xBar<-mean(xVec)
  xMax<-max(xVec)
  
  zVec<-log(xVec/sim$fireMapAttr$cellSize)
  if (length(zVec)<100)
    warning("Less than 100 \"large\" fires. That estimates may be unstable.\nConsider using a larger area and/or longer epoch.")

  hdList<-HannonDayiha(zVec)
  maxFireSize<-exp(hdList$That) * sim$fireMapAttr$cellSize  
  
  sim$fireRegimeParameters<-list(ignitionRate=rate, pEscape=pEscape, 
                                 xBar=xBar, xMax=xMax, meanBigFireSize=mean(xVec[xVec>200]),
                                 emfs=maxFireSize)
  return(invisible(sim))
}

### template for save events
fireRegimeModelSave = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
fireRegimeModelPlot = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
fireRegimeModelEvent1 = function(sim) {
  # ! ----- EDIT BELOW ----- ! #



  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


