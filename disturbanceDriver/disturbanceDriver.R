
defineModule(sim, list(
  name="disturbanceDriver",
  description="generate parameters for the generic percolation model",# spades::spread()",
  keywords=c("fire"),
  authors=c(person(c("Steve", "G"), "Cumming", email="stevec@sbf.ulaval.ca", role=c("aut", "cre"))),
  childModules=character(),
  version=numeric_version("0.1.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timeunit="year", 
  citation=list(),
  reqdPkgs=list(),
  parameters=rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur")),
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
  inputObjects=data.frame(objectName=c("spreadCalibrationData","fireRegimeParameters"), objectClass=c("data.frame","list"), other=re2(NA_character_,2), stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=c("spreadParameters"),
                          objectClass=c("list"),
                          other=NA_character_, stringsAsFactors=FALSE)
))



## event types
#   - type `init` is required for initiliazation

doEvent.disturbanceDriver = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    disturbanceDriverInit(sim)

    # schedule future event(s)
    scheduleEvent(sim, params(sim)$disturbanceDriver$.plotInitialTime, "disturbanceDriver", "plot")
    scheduleEvent(sim, params(sim)$disturbanceDriver$.saveInitialTime, "disturbanceDriver", "save")
  } else if (eventType=="templateEvent") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # scheduleEvent(sim, time(sim) + increment, "disturbanceDriver", "templateEvent")

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
disturbanceDriverInit = function(sim) {

  
  #this table contains calibration data for several landscape sizes
  #and several min fire sizes (1 or 2 cells), organised by collumn.
  #The data were made by Steve Cumming in June 2013 for a whole other purpose.
  #I chose the one that seems most appropriate to me
  browser()
  y<-log(sim$spreadCalibrationData[,paste("ls",1e3,"fs",2,sep="")])
  x<-sim$spreadCalibrationData$pjmp
  m.glm<-glm(x~I(log(y)))
  mfs<-sim$fireRegimeParameters$xBar/sim$fireRegimeParameters$cellSize #mean size escaped fires in cells
  pJmp<-sum(m.glm$coeff*c(1,log(mfs)))
 
  spreadParameters<-list(pJmp=pJmp)

  return(invisible(sim))
}

### template for save events
disturbanceDriverSave = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
disturbanceDriverPlot = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
disturbanceDriverEvent1 = function(sim) {
  # ! ----- EDIT BELOW ----- ! #



  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


### add additional events as needed by copy/pasting from above

