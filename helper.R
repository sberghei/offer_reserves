# Helper function
tp_to_hour <- function(tp){
  vector_with_hour_min <- format( seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "30 min"),
                                  "%H%M", tz="GMT")
  time_string <- vector_with_hour_min[tp]
  return(time_string)
}
