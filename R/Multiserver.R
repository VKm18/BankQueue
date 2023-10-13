# Import necessary libraries
library(tibble)

#' Multiserver Queue System Simulation
#'
#' Simulate a multi-server queue system to determine each customer's transition times.
#'
#' @param Arrivals A vector of customer arrival times.
#' @param ServiceTimes A vector of service times for each customer.
#' @param NumServers The number of servers available in the queue system. Default is 1.
#' 
#' @return A tibble with columns: Arrivals, ServiceBegins, ChosenServer, and ServiceEnds.
#' 
#' @examples
#' # Example usage:
#' Arrivals <- c(1, 3, 5)
#' ServiceTimes <- c(2, 2, 2)
#' Multiserver(Arrivals, ServiceTimes, 2)
#' 
#' @export
Multiserver <- function(Arrivals, ServiceTimes, NumServers = 1) {
  if (any(Arrivals <= 0 | ServiceTimes <= 0) || NumServers <= 0){
    stop("Arrivals, ServiceTimes must be positive & NumServers must be positive" )
  }
  if (length(Arrivals) != length(ServiceTimes)){
    stop("Arrivals and ServiceTimes must have the same length")
  }
  
  NumCust <- length(Arrivals)  #  number of customer arrivals
  AvailableFrom <- rep(0, NumServers)  # when each server is next available
  
  ChosenServer <- ServiceBegins <- ServiceEnds <- rep(0, NumCust)  
  
  for (i in seq_along(Arrivals)){
    avail <-  min(AvailableFrom)
    ChosenServer[i] <- which.min(AvailableFrom)
    ServiceBegins[i] <- max(avail, Arrivals[i])
    ServiceEnds[i] <- ServiceBegins[i] + ServiceTimes[i]  
    AvailableFrom[ChosenServer[i]] <- ServiceEnds[i]
  }
  
  out <- tibble(Arrivals, ServiceBegins, ChosenServer, ServiceEnds)
  return(out)
}

