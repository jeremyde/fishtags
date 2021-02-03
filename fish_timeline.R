# Plots tag detection events around fixed sites
library("tidyverse")
setwd("~/fish") #change this to your working directory

#load and prep data
fish.data <- read_csv("simulated_data.csv")
fish.data$Time <- as.numeric(fish.data$Time)
fish.data$Location <- as.factor(fish.data$Location)

#make an index of tags (fish) to set plot order
tags <- unique(fish.data$Fish)
tagindex <- data.frame(Fish=tags, tagindex=1:length(tags))
fish.data <- fish.data %>% left_join(tagindex,by="Fish")

#set colors manually for locations (alphabetical order)
location.colors <-c("orange","blue","red","green","yellow")

#set minimum and maximum time to plot
mintime <- round(min(fish.data$Time),0) 
maxtime <- round(max(fish.data$Time),0)
timestep <- 14 # x-axis tick every 2 weeks

#a fish is considered to have left a location after this amount of time
max.absence <- 1 #1 days

#create tibble to store boxes that define when a fish visit begins and ends
boxes <- tibble(
  tagindex=numeric(),
  Location=character(),
  VisitStart=numeric(),
  VisitEnd=numeric()
)

#find fish visit start and stop times
for(tag in 1:length(tags)) {
  tag.data <- fish.data %>% filter(tagindex == tag)
  for(loc in levels(fish.data$Location)) {
    tag.loc.data <- tag.data %>% filter(Location == loc) %>% arrange(Time)
    if (nrow(tag.loc.data) > 0) {
      boxstart <- tag.loc.data$Time[1]
      boxlast <- boxstart
      if (nrow(tag.loc.data) > 1) {
        for (obs in 2:nrow(tag.loc.data)) {
          obs.time <- tag.loc.data$Time[obs]
          if ((obs.time - boxlast) > max.absence) {
            #add a box start-stop record
            boxes <- boxes %>% add_row(tagindex=tag, Location=loc, VisitStart=boxstart, VisitEnd=boxlast)
            boxstart <- obs.time
            boxlast <- boxstart
          } else {
            boxlast <- obs.time
          }
        }
      }
      #deal with last one and single obs
      boxes <- boxes %>% add_row(tagindex=tag, Location=loc, VisitStart=boxstart, VisitEnd=boxlast)      
    }
  }
}

#save calculated fish visit start and stop times to a CSV file
write_csv(boxes %>%  left_join(tagindex,by="tagindex")
          %>% select(Fish,Location,VisitStart,VisitEnd), "visit_start_stop_calcs.csv")

#create the plot
fish.plot <- ggplot() +
  theme_light() +
  theme(panel.grid.major = element_line(size = 0.4),
        text = element_text(size=16)) +
  scale_x_continuous(breaks = seq(mintime, maxtime, by = timestep), minor_breaks=FALSE ) +
  scale_y_continuous(breaks = seq(1, length(tags), by = 1), labels=tags) +
  scale_color_manual(values=location.colors) +
  ggtitle("Fish Tracking Timeline") +
  xlab("Time (days)") +
  ylab("Fish/Tag Number") +
  geom_segment(data=fish.data, aes(x=Time, xend=Time, y = tagindex - 0.49, yend = tagindex + 0.49, color=Location), size=.5) +
  geom_rect(data=boxes, mapping=aes(xmin=VisitStart-0.05, xmax=VisitEnd+0.05, ymin=tagindex - 0.10, ymax=tagindex + 0.10), fill="black") 
  
fish.plot #show the plot

#save a png
png(filename="fishplot.png", width=1200, height=1000)
fish.plot
dev.off()

#save a pdf
pdf(file="fishplot.pdf", width=12, height=10)
fish.plot
dev.off()

