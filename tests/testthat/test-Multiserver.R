# Load required libraries
library(testthat)
library(tibble)

context("Multiserver function tests")

test_that("Multiserver returns correct output structure", {
  set.seed(2048)
  arrival_time <- cumsum(rexp(100, 1/60))
  service_time <- rexp(length(arrival_time), 1/150) + 20
  
  result <- Multiserver(arrival_time, service_time)
  
  expect_true(is_tibble(result))
  expect_equal(ncol(result), 4)
  expect_equal(nrow(result), length(arrival_time))
  expect_named(result, c("Arrivals", "ServiceBegins", "ChosenServer", "ServiceEnds"))
})


test_that("Server finishes serving one customer before starting another", {
  set.seed(2048)
  arrival_time <- cumsum(rexp(100, 1/60))
  service_time <- rexp(length(arrival_time), 1/150) + 20
  
  result <- Multiserver(arrival_time, service_time, NumServers = 3)
  
  for (server in 1:3) {
    server_customers <- which(result$ChosenServer == server)
    if (length(server_customers) > 1) {
      for (i in 2:length(server_customers)) {
        # Check that service end time of previous customer is less than or equal to 
        # service start time of next customer for the same server
        expect_true(result$ServiceEnds[server_customers[i - 1]] <= result$ServiceBegins[server_customers[i]])
      }
    }
  }
})
