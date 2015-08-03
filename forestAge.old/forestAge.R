###
### name:         forestAge
###
### description:  A basic forest age module based on Canada Land Cover Classes 2005.
###
### keywords:     forest age; LCC05; land cover classification 2005
###
### authors:      Alex M. Chubaty <Alexander.Chubaty@NRCan.gc.ca>
###               Eliot J. B. McIntire <Eliot.McIntire@NRCan.gc.ca>
###               Steve Cumming <Steve.Cumming@sbf.ulaval.ca>
###
### version:      0.2.0
###
### spatialExtent: NA
###
### timeframe:    NA
###
### timestep:     31557600 (1 year)
###
### citation:     NA
###
### reqdPkgs:     raster; RColorBrewer
###
### parameters:   paramName: returnInterval
###               paramClass: logical
###               default: 10.0
###
###               paramName: startTime
###               paramClass: numeric
###               default: 1.0
###
###               paramName: .plotInitialTime
###               paramClass: numeric
###               default: 0
###
###               paramName: .plotInterval
###               paramClass: numeric
###               default: 1
###
###               paramName: .saveInitialTime
###               paramClass: numeric
###               default: NA
###
###               paramName: .saveInterval
###               paramClass: numeric
###               default: NA
###
### inputObjects: objectName: ageMapInit
###               objectClass: RasterLayer
###               other: NA
###
### outputObjects: objectName: ageMap
###                objectClass: RasterLayer
###                other: NA
###
### forestAge module metadata
defineModule(sim, list(
  name="forestAge",
  description="A basic forest age module based on Canada Land Cover Classes 2005.",
  keywords=c("forest age", "LCC05", "land cover classification 2005"),
  authors=c(person(c("Alex", "M"), "Chubaty", email="Alexander.Chubaty@NRCan.gc.ca", role=c("aut", "cre")),
            person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre")),
            person("Steve", "Cumming", email="Steve.Cumming@sbf.ulaval.ca", role=c("aut"))),
  version=numeric_version("0.2.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timeunit="year",
  citation=list(),
  reqdPkgs=list("raster", "RColorBrewer"),
  parameters=rbind(
    defineParameter("returnInterval", "numeric", 1.0,desc=""),
    defineParameter("startTime", "numeric", 1.0,desc=""),
    defineParameter(".plotInitialTime", "numeric", 0,desc=""),
    defineParameter(".plotInterval", "numeric", 1,desc=""),
    defineParameter(".saveInitialTime", "numeric", NA_real_,desc=""),
    defineParameter(".saveInterval", "numeric", NA_real_,desc="")),
  inputObjects=data.frame(objectName=c("ageMapInit", "ageMap"),
                          objectClass=c("RasterLayer", "RasterLayer"),
                          other=rep(NA_character_,2L), stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName="ageMap",
                           objectClass="RasterLayer",
                           other=NA_character_, stringsAsFactors=FALSE)
))

### event functions 
doEvent.forestAge <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for object dependencies:
    ### (use `checkObject` or similar)h

    # do stuff for this event
    sim <- forestAgeInit(sim)
    browser()
    # schedule the next event
    sim <- scheduleEvent(sim, params(sim)$forestAge$startTime, "forestAge", "age")
    sim <- scheduleEvent(sim, params(sim)$forestAge$.saveInterval, "forestAge", "save")
    sim <- scheduleEvent(sim, params(sim)$forestAge$.plotInitialTime, "forestAge", "plot.init")

  } else if (eventType=="age") {
      # do stuff for this event
      sim <- forestAgeAge(sim)

      # schedule the next event
      sim <- scheduleEvent(sim, time(sim) +
                             params(sim)$forestAge$returnInterval,
                           "forestAge", "age")
  } else if (eventType=="plot.init") {
    # do stuff for this event
    Plot(sim$ageMap, legendRange=c(0,200))

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) + params(sim)$forestAge$.plotInterval,
                         "forestAge", "plot")
  } else if (eventType=="plot") {
    # do stuff for this event
    Plot(sim$ageMap, legendRange=c(0,200))

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) + params(sim)$forestAge$.plotInterval,
                         "forestAge", "plot")
  } else {
    warning(paste("Undefined event type: \'", events(sim)[1, "eventType", with=FALSE],
                  "\' in module \'", events(sim)[1, "moduleName", with=FALSE], "\'", sep=""))
  }
  return(invisible(sim))
}

forestAgeInit <- function(sim) {
  #ageMap <- getGlobal("ageMapInit")
  setColors(sim$ageMap,n=201) <- colorRampPalette(c("LightGreen","DarkGreen"))(50)
  #assignGlobal("ageMap", ageMap)
  return(invisible(sim))
}

forestAgeAge <- function(sim) {

  #ageMap <- setValues(ageMap, pmin(200, getValues(getGlobal("ageMap"))+
    #                                 simParams(sim)$forestAge$returnInterval))
  sim$ageMap <- 
  setValues(sim$ageMap,pmin(200, getValues(sim$ageMap) + params(sim)$forestAge$returnInterval))
  
  if(exists("Fires", envir(sim))) {
    ageMap[sim$Fires>0] <- 0
  }
  setColors(sim$ageMap,n=201) <- colorRampPalette(c("LightGreen","darkgreen"))(50)
  #assignGlobal("ageMap", ageMap)

  return(invisible(sim))
}

