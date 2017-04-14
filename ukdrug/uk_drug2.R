# 1. animation of drug prescription from Jan tp Dec 2015 using XYH's animation
# 2. plotly of drug prescription
# 3. write up and reference to Ben Goldacre's article about UK drug prescription

#------------------------------------------------------------------------------#
# Load required libraries and set file directory
library(dplyr)
library(rgdal) # For reading geographic shapefile
library(lubridate)
library(tmap)
library(magick)
library(leaflet)

setwd("./pt/uk_drug")

source("check_ct2.R")
source("search_drug2.R")

# Load list of towns and county
list_towns = read.csv("./list_towns.csv",
                      stringsAsFactors = FALSE,
                      header = TRUE)
all_towns = unique(list_towns$TOWN)
all_counties = unique(list_towns$COUNTY)

# Load county population
c_pop = read.csv("./c_pop.csv",
                 stringsAsFactors = FALSE,
                 header = TRUE)

#------------------------------------------------------------------------------#
# Create empty dataframe to bind the data in the for loop
final_df = data.frame(COUNTY = character(0),
                      T_PRES = numeric(0),
                      T_ITEM = numeric(0),
                      PERIOD = character(0),
                      POP = numeric(0),
                      D_PRES = numeric(0),
                      D_ITEM = numeric(0),
                      stringsAsFactors = FALSE)

for (i in seq(201501, 201512, 1)) {
      
      drugfile = paste0("./drug_data/T",
                        i,
                        "PDPI+BNFT.CSV")
      
      addrfile = paste0("./drug_data/T",
                        i,
                        "ADDR+BNFT.CSV")
      
      source("uk_drug3.R")
      
}

# Write final data frame into csv file for easy reading
# write.csv(x = final_df,
#           file = "final_df.csv",
#           row.names = FALSE)

final_df = read.csv("final_df.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)

#------------------------------------------------------------------------------#
# Converting the names of counties so that they match those from the shapefile
final_df$COUNTY = tools::toTitleCase(tolower(final_df$COUNTY)) %>%
      gsub("Bristol", "County of Bristol", .) %>%
      gsub("London", "Greater London", .)

# Read shape file for coordinates of counties
# https://data.gov.uk/dataset/ceremonial-county-boundaries-of-england
for (i in unique(final_df$PERIOD)) {
      
      # Prepare shapefile data frame
      uks = readOGR(dsn = "./English Ceremonial Counties",
                    layer = "English Ceremonial Counties")
      
      ss = final_df[final_df$PERIOD == i, ]
      
      uks@data = left_join(uks@data, 
                           ss, 
                           by = c("NAME" = "COUNTY"))
      
      # Prepare creation of png image
      mon = as.character(month(dym(i + 1000000),
                               label = TRUE, 
                               abbr = FALSE))
      title = "\nMirtazapine Prescriptions \nper 1,000 people"
      
      img_name = paste0("img", mon, ".png")
      
      # Open png graphic device
      png(filename = img_name,
          width = 800,
          height = 800)
      
      # Print plotting image into png graphic device
      print(tm_shape(uks) +
                  tm_polygons("D_ITEM", 
                              title = "") +
                  tm_layout(paste(as.character(mon), title) ,
                            title.size = 1.6,
                            legend.text.size = 1.3,
                            legend.position = c(0.01, 0.75),
                            title.snap.to.legend = TRUE))
      # Close png graphic device
      dev.off()
      
      # Prepare images for gif animation within the loop
      if (!exists("images")) {
            images = image_read(img_name)
      } else {
            images = c(images, 
                       image_read(img_name))
      }
      
}

# Convert images to a gif animation
images2 = image_morph(images, 
                      frames = 10)
drug_gif = image_animate(images, 
                         fps = 5)
image_write(drug_gif, 
            path = "drug_gif.gif", 
            format = "gif")

#------------------------------------------------------------------------------#
# Creating leaflet interactive map for uk drug prescription
# Summarize prescription for year 2015
leaf_df = final_df %>%
      group_by(COUNTY) %>%
      summarize(tItem = sum(T_ITEM),
                pop = unique(POP)) %>%
      mutate(dItem = round(tItem / pop * 1000)) %>%
      data.frame()

uks = readOGR(dsn = "./English Ceremonial Counties",
              layer = "English Ceremonial Counties")
uks@data = left_join(uks@data,
                     leaf_df,
                     by = c("NAME" = "COUNTY"))

# Create labels for mouse hovering
labels = sprintf("<strong>%s</strong><br/>No. of Prescriptions:<br/>%g",
                 uks@data$NAME, 
                 uks@data$dItem) %>% 
      lapply(htmltools::HTML)

# Create color ranges for the county
bins <- c(0, 20, 50, 100, 150, 200, 250)
pal <- colorBin("YlOrRd", domain = uks@data$dItem, bins = bins)

# Leaflet function
m <- leaflet(uks) %>%
      addTiles() %>%
      addPolygons(fillColor = ~ pal(dItem),
                  weight = 2,
                  opacity = 1,
                  color = "white", 
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                        style = list("font-weight" = "normal",
                                     padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) 

m %>% addLegend(pal = pal, 
                values = ~ dItem,
                opacity = 0.7,
                title = "Mirtazapine Prescriptions per 1,000 People",
                position = "bottomright")
