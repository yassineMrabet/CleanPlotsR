
# This script illustrates a simple use of 'rgbif' package
# It generates a distribution map of species from
# GBIF records
# (Global Biodiversity Information Facility) 
# Please visit www.gbif.org for more details



# Optional if above packages were already installed
# if (!require("rgbif")) install.packages("rgbif")
# if (!require("maps")) install.packages("maps")
# if (!require("ggplot2")) install.packages("ggplot2")


# Load required libraries
library(rgbif)
library(maps)
library(ggplot2)


# Import data from GBIF
# I choosed two genera from the Meliaceae Juss. family 
# Melia L. and a close one : Azadirachta A. Juss.
# this may take a while
sp.list <- occ_data(scientificName = c('Melia', 'Azadirachta'),  
                     hasCoordinate = TRUE, limit = 5000)

# Get only the name, key and coordiantes columns
melia.data <- sp.list$Melia$data[,c(1:4)]
# M. volkensii points were overlayed in the map, I decided to
# plot them afterward
melia.volk <- subset(melia.data, name == 'Melia volkensii')
azad.data <- sp.list$Azadirachta$data[,c(1:4)]


# Merge data in a single data frame and remove NA
out <- as.data.frame(rbind(melia.data,azad.data))
out <- out[! is.na(out$name),]


# I introduced additional coordinates collected from
# literature and personal observations
# Check the joined csv. file 
p.add <- read.csv("~/Documents/additional_list.csv",header = TRUE)
out <- as.data.frame(rbind(out,p.add[,c(1:4)])) 
# melia.points <- subset(out, name = 'Melia azedarach' )

# Examine the number of records 
table(out$name)

# General ggplot2 theme for map
# Source: https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
# with slight modifications 
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Helvetica", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#fff2ebff", color = NA), 
      panel.background = element_rect(fill = "#fff2ebff", color = NA), 
      legend.position="bottom",
      legend.text = element_text(face = "italic"),
      legend.background = element_rect(fill = "#fff2ebff", color = NA),
      panel.border = element_blank(),
      ...
    )
}


# Creating the map
world <- map_data("world")

# Setting the native range (Indomalayan realm)
native_range <- subset(world, region == "India" |
                         region == "Bangladesh" |
                         region == "Myanmar" |
                         region == "Nepal" |
                         region == "Malaysia" |
                         region == "Laos" |
                         region =="Indonesia" |
                         region =="Java" | 
                         region =="Bali" |
                         region =="Cambodia" |  
                         region =="Vietnam" |  
                         region =="Sumatra" |  
                         region =="Thailand" |
                         region =="Java" |  
                         region =="Vietnam" |  
                         region =="Borneo")

gg <- ggplot(world, aes(long, lat)) +
      #World polygon
      geom_polygon(aes(group = group), fill = "white", 
               color = "gray40", size = .2)
gg <- gg + geom_polygon(data=native_range, aes(long, lat, group =group), 
                        fill = "#c4ac86ff" , 
                        color = "#c4ac86ff") +
      coord_quickmap()

gg <- gg + geom_point(data = out,
                      aes(x=decimalLongitude, y=decimalLatitude, colour=name, shape=name), 
                      #alpha=0.5, 
                      size = 1.5)

gg <- gg + geom_point(data = melia.volk ,
                      aes(x=decimalLongitude, y=decimalLatitude, colour=name, shape=name), 
                      #alpha=0.5, 
                      size = 1.5)

# gg <- gg + geom_density2d(data = melia.points, aes(x = decimalLongitude, y = decimalLatitude), size = 0.3)

gg <- gg + labs(color = "Species", shape="Species")
gg <- gg + scale_color_brewer(palette="Set1")
gg <- gg + theme_map()


# Display the result 
gg



