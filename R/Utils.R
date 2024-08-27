# Function to create regex string to search for selected indicators
# "standard" has no time window whereas "pa" and "tr" can have a time 
# Make sure to pass in empty string if standard included!
formatIndicatorRegex <- function(modelTypes,windowSizes){
    # Need to add "km" and "aj" so these do not get filtered out
    return(
    paste(c("km","aj", mapply(paste0,modelTypes,windowSizes)),collapse = "|")
    )
}