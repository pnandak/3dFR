print.hours <- function(seconds){
  
  day <- as.character(floor(seconds/oneDayInSeconds))
  seconds <- seconds %% oneDayInSeconds
  hour <- as.character(floor(seconds / (60*60)))
  seconds <- seconds %% (60*60)
  minute <- as.character(floor(seconds/60))
  seconds <- as.character(seconds %% 60)
  
  (concatenate(c(day, ", ", hour, ":", minute, ":", seconds)))
}

crono.end <- function(start){
  
  now <- getTime()
  duration <- time2seconds(now, start)
  (print.hours(duration))
}

getTime <- function(){
  
  timeStr <- strsplit(as.character(format(Sys.time(), "%y-%m-%d %H:%M:%OS6")), "[ ]")[[1]]
  dateStr <- strsplit(timeStr[1], "[-]")[[1]]
  hourStr <- strsplit(timeStr[2], "[:]")[[1]]
  
  timeObj <- list()
  
  timeObj[["year"]] <- as.numeric(dateStr[1])
  timeObj[["month"]] <- as.numeric(dateStr[2])
  timeObj[["day"]] <- as.numeric(dateStr[3])
  timeObj[["hour"]] <- as.numeric(hourStr[1])
  timeObj[["minute"]] <- as.numeric(hourStr[2])
  timeObj[["second"]] <- as.numeric(hourStr[3])
  
  (timeObj)
}

time2seconds <- function(target, reference=0){
  
  if(length(reference) == 1)
    reference <- target
  
  ref <- reference$second +
         60*reference$minute +
         60*60*reference$hour
  tar <- target$second +
         60*target$minute +
         60*60*target$hour
  days <- target$day - reference$day - 1
  
  ((oneDayInSeconds - ref) + days*oneDayInSeconds + tar)
}

seconds2time <- function(seconds, reference=0){
  
  if(length(reference) == 1)
    reference <- list(year=0, month=0, day=0, hour=0, minute=0, second=0)
  
  day <- floor(seconds/oneDayInSeconds)
  seconds <- seconds %% oneDayInSeconds
  hour <- floor(seconds/(60*60))
  seconds <- seconds %% (60*60)
  minute <- floor(seconds/60)
  seconds <- seconds %% 60
  
  (list(year=reference$year, month=reference$month,
        day=reference$day + day,
        hour=reference$hour + hour,
        minute=reference$minute + minute,
        second=reference$second + seconds))
}

oneDayInSeconds <- 60*60*24
