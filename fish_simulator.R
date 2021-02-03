# Simulates tagged fish detection around fixed detection sites
library("tidyverse")
setwd("~/fish") #change this to your working directory

#tune parameters
detection.prob <- 0.10
move.prob <- 0.05
move.to.prob <- 0.15

resolution <- 10 #10 iterations per day

locations <- c("AMBERJACK", "INNOVATOR", "LENA", "MATTERHORN", "MEDUSA")

tags <- seq(from= 2770, to = 2800)
times <- seq(from =39200, to = 39300, by=1/resolution)

# create an empty tibble to store simulated tag detection
tagdb <- tibble(
  Fish=numeric(),
  Location=character(),
  Time=numeric()
)

# run the simulation
for(tag in tags) {
  loc <- "OCEAN"
  for(time in times) {
    move <- runif(1, min=0, max=resolution)
    if(move <= move.prob*resolution) {
      if(loc == "OCEAN") {
        move.to <- runif(1, min=0, max=resolution)
        if(move.to <= move.to.prob*resolution) {
          loc <- locations[ceiling(runif(1, min=0, max=5))]
        }
      } else {
        loc <- "OCEAN"
      } 
    }
    if(loc != "OCEAN") {
      detect <- runif(1, min=0, max=resolution)
      if(detect <= detection.prob*resolution) {
        tagdb <- tagdb %>% add_row(Fish=tag, Location=loc, Time=time)
      }
    }
  }
}  

# store the results
write_csv(tagdb, "simulated_data.csv")
