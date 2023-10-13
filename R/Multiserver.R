Multiserver <- function(Arrivals, ServiceTimes, NumServers = 1) {
  if (any(Arrivals <= 0 | ServiceTimes <= 0) || NumServers <= 0){
    stop("Arrivals, ServiceTimes must be positive & NumServers must be positive" )
  }
  if (length(Arrivals) != length(ServiceTimes)){
    stop("Arrivals and ServiceTimes must have the same length")
  }
# Feed customers through a multiserver queue system to determine each  
# customer's transition times.

NumCust <- length(Arrivals)  #  number of customer arrivals
# When each server is next available (this will be updated as the simulation proceeds):
AvailableFrom <- rep(0, NumServers)
# i.e., when the nth server will next be available

# These variables will be filled up as the simulation proceeds:
ChosenServer <- ServiceBegins <- ServiceEnds <- rep(0, NumCust)  

# ChosenServer = which server the ith customer will use
# ServiceBegins = when the ith customer's service starts
# ServiceEnds = when the ith customer's service ends

# This loop calculates the queue system's "Transitions by Customer":
for (i in seq_along(Arrivals)){
  # go to next available server
  avail <-  min(AvailableFrom)
  ChosenServer[i] <- which.min(AvailableFrom)
  # service begins as soon as server & customer are both ready
  ServiceBegins[i] <- max(avail, Arrivals[i])
  ServiceEnds[i] <- ServiceBegins[i] + ServiceTimes[i]  
  # server becomes available again after serving ith customer
  AvailableFrom[ChosenServer[i]] <- ServiceEnds[i]
}
  out <- data.frame(Arrivals, ServiceBegins, ChosenServer, ServiceEnds)
  return(out)
}


