zileLibere <- function(an = 2017)
{
  an = as.character(an)
  paste = pasteOrtodox(as.numeric(an), asDate = TRUE) + 1
  rusalii = paste + 49
  
  zile_libere = c(
    as.Date(paste0(an, "-01-01")),
    as.Date(paste0(an, "-01-02")),
    as.Date(paste0(an, "-01-24")),
    
    paste,
    rusalii,
    
    as.Date(paste0(an, "-05-01")),
    as.Date(paste0(an, "-08-15")),
    as.Date(paste0(an, "-11-30")),
    as.Date(paste0(an, "-12-01")),
    as.Date(paste0(an, "-12-25")),
    as.Date(paste0(an, "-12-26"))
  )
  
  
  if (an > 2016)
  {
    zile_libere = c(zile_libere, as.Date(paste0(an, "-06-01")))
  }
  
  return(sort(zile_libere))
  
}


zileLibereIntervalAni <- function(anStart = 2005, anStop = 2050)
{
  zileLibereInInterval <- zileLibere(an = anStart)
  
  for (i in (anStart + 1):anStop)
  {
    zileLibereInInterval <- c(zileLibereInInterval, zileLibere(an = i))
    
  }
  
  return(zileLibereInInterval)
  
}


workdays <-  function(iniDate, endDate, holidays) {
  
  theDates = seq(from = as.Date.character(iniDate), to = as.Date.character(endDate), by = "day")
  
  isHoliday = theDates %in% holidays
  
  isWeekend = (as.POSIXlt(theDates)$wday) %in% (c(0, 6))
  
  return (sum(!isHoliday & !isWeekend))
}