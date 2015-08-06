
defineModule(sim, list(
  name="disturbanceDriver",
  description="insert module description here",
  keywords=c("insert key words here"),
  authors=c(person(c("First", "Middle"), "Last", email="email@example.com", role=c("aut", "cre"))),
  childModules=character(),
  version=numeric_version("0.0.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timeunit=NA_character_, # e.g., "year"
  citation=list(),
  reqdPkgs=list(),
  parameters=rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur")),
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
  inputObjects=data.frame(objectName=NA_character_, objectClass=NA_character_, other=NA_character_, stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=NA_character_, objectClass=NA_character_, other=NA_character_, stringsAsFactors=FALSE)
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

  
  
  FC <- read.csv(curvesData, as.is=T)
  predictSpread <- function (fireSize = MFS, landSize=2000, minFireSize=2) {
    lfs <- log(fireSize)
    m <- glm(FC$pjmp ~ log(FC[,paste("ls",landSize,"fs",minFireSize,sep="")]))
    p <- m$coef[1] + m$coef[2] * lfs
    return(p[1])
  }
  
  # Calculate mean size of escaped fires in cells
  MFS <- mean(escapedFires$SIZE_HA) / cellarea
  
  # e.g., landSize=200 corresponds to 40,000 cells
  studyAreaCells <- studyArea / cellarea
  if (sqrt(studyAreaCells) <= 350) {LS <- 200}
  if (sqrt(studyAreaCells) > 350 & sqrt(studyAreaCells) <= 750) {LS <- 500}
  if (sqrt(studyAreaCells) > 750 & sqrt(studyAreaCells) <= 1500) {LS <- 1000}
  if (sqrt(studyAreaCells) > 1500) {LS <- 2000}
  PS <- predictSpread(fireSize=MFS, landSize=LS)
  
  
  # # ! ----- EDIT BELOW ----- ! #



  # ! ----- STOP EDITING ----- ! #

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

### template for your event2
disturbanceDriverEvent2 = function(sim) {
  # ! ----- EDIT BELOW ----- ! #



  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above

