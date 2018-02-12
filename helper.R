
### Helper functions


# Trading Period to Hour
tp_to_hour <- function(tp){
  vector_with_hour_min <- format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "30 min"),
                                  "%H%M", tz="GMT")
  time_string <- vector_with_hour_min[tp]
  return(time_string)
}

# DiffTime to predispatch (takes in TimeDiff and TradingPeriod and returns predispatch number)
timediff_to_predispatch <- function(TimeDiff, TradingPeriod = 1){
  Predispatch <- ifelse(TradingPeriod %in% c(seq(1, 45, by = 4)), cut(as.numeric(TimeDiff), breaks = c(seq(0, -3.5, -0.5), seq(-4, -34, -2), -5000)),
                   ifelse(TradingPeriod %in% c(seq(2, 46, by = 4)), cut(as.numeric(TimeDiff), breaks = c(seq(0, -3.5, -0.5), seq(-4.5, -34.5, -2), -5000)),
                     ifelse(TradingPeriod %in% c(seq(3, 47, by = 4)), cut(as.numeric(TimeDiff), breaks = c(seq(0, -3.5, -0.5), seq(-5, -35, -2), -5000)),
                       ifelse(TradingPeriod %in% c(seq(4, 48, by = 4)), cut(as.numeric(TimeDiff), breaks = c(seq(0, -3.5, -0.5), seq(-5.5, -35.5, -2), -5000)), NA) )))
  return(Predispatch)
}

# function that selects data for the graph of the first predispatch
get_first_predispatch <- function(data){
  # Takes in dataframe of one trader's offer data and returns dataframe with data to plot his supply curve submitted before the first predispatch
  fp <- filter(data, Predispatch == 1) %>%
    group_by(Island, PointOfConnection, Unit, ProductType, ProductSubClass, Band) %>%
    arrange(SubmissionOrder) %>%
    slice(n()) %>%
    ungroup() %>%
    group_by(Island, ProductType, ProductSubClass) %>%
    arrange(DollarsPerMegawattHour) %>%
    mutate(cumMegawatt = cumsum(Megawatt)) %>%
    ungroup() %>%
    select(DollarsPerMegawattHour, cumMegawatt, Island) 
  
  fp <- add_row(fp, cumMegawatt = 0, DollarsPerMegawattHour = min(fp$DollarsPerMegawattHour)) %>%
    arrange(cumMegawatt)
  
  return(fp)
}


# function that selects data for the graph of the first predispatch
get_last_predispatch <- function(data){
  # Takes in dataframe of one trader's offer data and returns dataframe with data to plot his supply curve submitted before the last predispatch
  lp <- filter(data, IsLatest == "Y") %>%
    group_by(Island, ProductType, ProductSubClass) %>%
    arrange(DollarsPerMegawattHour) %>%
    mutate(cumMegawatt = cumsum(Megawatt)) %>%
    ungroup() %>%
    select(DollarsPerMegawattHour, cumMegawatt, Island)

  lp <- add_row(lp, cumMegawatt = 0, DollarsPerMegawattHour = min(lp$DollarsPerMegawattHour)) %>%
    arrange(cumMegawatt)
  
  return(lp)
}