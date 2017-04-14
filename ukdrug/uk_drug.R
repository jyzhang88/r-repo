# Load required libraries and set file directory
library(dplyr)
library(ggplot2)
library(rgdal)

setwd("./pt/uk_drug")

#------------------------------------------------------------------------------#
# ukdrug01.R
# Load list of drugs prescribed by the clinics around UK
drug = data.table::fread("T201611PDPI+BNFT.CSV",
                         col.names = c("SHA", "PCT", "PRACTICE", "BNF_CODE",
                                       "BNF_NAME", "ITEMS", "NIC", "ACT_COST",
                                       "QUANTITY", "PERIOD", "V11")) %>%
      data.frame() %>%
      select(-V11)

# Subset the columns needed
drug = select(drug, PRACTICE, BNF_NAME, ITEMS, NIC, ACT_COST, QUANTITY, PERIOD)

#------------------------------------------------------------------------------#
# ukdrug02.R
# Load list of clinics situated around UK
clinic = data.table::fread("T201611ADDR+BNFT.CSV",
                           col.names = c("PERIOD", "PRACTICE", 
                                         "CLINIC_ONE", "CLINIC_TWO",
                                         "ROAD_NAME", "TOWN", "COUNTY",
                                         "UNKNOWN")) %>%
      data.frame()

# Remove dashes from names of towns and counties
source("rm_dash.R")
clinic["TOWN"] = apply(clinic["TOWN"], 1, rm_dash)
clinic["COUNTY"] = apply(clinic["COUNTY"], 1, rm_dash)

#------------------------------------------------------------------------------#
# ukdrug03.R
# Load list of towns and counties present in UK
url = "https://en.wikipedia.org/wiki/List_of_towns_in_England"
http = readLines(url)

# Create an empty data frame to collect the names from the 'for' loop
list_towns = data.frame(TOWN = character(0), 
                        COUNTY = character(0),
                        stringsAsFactors = FALSE)

# Manually search for the starting/ending line that carries the names
for (i in 103:5141) {
      
      if (grepl("title=", http[i]) & nchar(http[i]) < 135) {
            
            # Subset string containing town
            start_town = regexpr('\">', http[i])[1] + 2
            end_town = regexpr("</a>", http[i][1]) - 1
            tw = substr(http[i], start_town, end_town) %>%
                  rm_dash() %>%
                  toupper()
            
            # Subset string containing county
            start_ct = 5
            end_ct = regexpr('</td>', http[i+1]) - 1
            ct = substr(http[i+1], start_ct, end_ct) %>%
                  rm_dash() %>%
                  toupper()
            
            # Row bind entry with the list of towns
            entry = data.frame(TOWN = tw, 
                               COUNTY = ct,
                               stringsAsFactors = FALSE)
            list_towns = rbind(list_towns, entry)
            
      }
}

# Convert county names
list_towns$COUNTY = list_towns$COUNTY %>% 
      gsub("GREATER LONDON", "LONDON", .) %>%
      gsub("DURHAM/NORTH YORKSHIRE", "DURHAM", .) %>%
      gsub("COUNTY DURHAM", "DURHAM", .)

# There are a few issues with the TOWN/COUNTY columns
# 1. Some counties are not the listed from the Wiki site.
# 2. Counties falling under the TOWN column
# 3. Missing counties or towns
# To fill in as many counties as possible based on TOWN/COUNTY columns

# Including county Bristol into the list of towns
bristol = clinic[clinic$COUNTY == "BRISTOL", ]
bristol = bristol[!duplicated(bristol$TOWN), ]
bristol_entry = select(bristol, TOWN, COUNTY)
list_towns = rbind(list_towns, bristol_entry)

# Fill in county names under COUNTY column
all_towns = unique(list_towns$TOWN)
all_counties = unique(list_towns$COUNTY)

source("check_ct.R")
clinic$tCOUNTY = apply(clinic, 1, check_ct)
clinic = clinic[!clinic$tCOUNTY == "", ]

# Subset only the columns needed
clinic = select(clinic, PRACTICE, TOWN, tCOUNTY)
colnames(clinic) = c("PRACTICE", "TOWN", "COUNTY")

#------------------------------------------------------------------------------#
# ukdrug04.R
# Get population data of the counties
url2 = "https://en.wikipedia.org/wiki/List_of_ceremonial_counties_of_England"
http = readLines(url2)

c_pop = data.frame(COUNTY = character(0),
                   POP = numeric(0),
                   stringsAsFactors = FALSE)

for (i in seq(64, 534, 10)) {
      start_ct = regexpr('">', http[i])[1] + 2
      end_ct = regexpr('</a>', http[i])[1] - 1
      ct = substr(http[i], start_ct, end_ct) %>%
            rm_dash() %>%
            toupper()
      
      list_pop = gregexpr('[1234567890]', http[i+1])
      pop = substr(http[i+1], 
                   list_pop[[1]][1], 
                   list_pop[[1]][length(list_pop[[1]])]) %>%
            gsub(",", "", .) %>%
            as.numeric()
      
      entry = data.frame(COUNTY = ct,
                         POP = pop,
                         stringsAsFactors = FALSE)
      c_pop = rbind(c_pop, entry)
      
}

c_pop$COUNTY = c_pop$COUNTY %>% 
      gsub("GREATER LONDON", "LONDON", .) %>%
      gsub("DURHAM/NORTH YORKSHIRE", "DURHAM", .) %>%
      gsub("COUNTY DURHAM", "DURHAM", .)

#------------------------------------------------------------------------------#
# ukdrug05.R
# Subset data containing drug of interest
source("search_drug.R")

# This function searches for rows carrying the drug that you're interested in
# It takes a while because the data frame has >10 million rows
req_df = search_drug("mirtazapine")

#------------------------------------------------------------------------------#
# ukdrug06.R
# Merge drug prescription, clinic location and county pop data sets together
d_c = merge(req_df,
            clinic,
            by = "PRACTICE")

pres_df = group_by(d_c, COUNTY) %>%
      summarize(T_PRES = sum(QUANTITY)) %>%
      data.frame() %>%
      merge(c_pop, by = "COUNTY") %>%
      mutate(D_PRES = round(T_PRES / POP * 1000))

# Order according to number of drugs per 1000 people
head(pres_df[order(pres_df$D_PRES, decreasing = TRUE), ], 3)
tail(pres_df[order(pres_df$D_PRES, decreasing = TRUE), ], 3)

#------------------------------------------------------------------------------#
# ukdrug07.R
pres_df2 = group_by(d_c, COUNTY) %>%
      summarize(T_PRES = sum(QUANTITY),
                T_ITEM = sum(ITEMS)) %>%
      data.frame() %>%
      merge(c_pop, by = "COUNTY") %>%
      mutate(D_PRES = round(T_PRES / POP * 1000),
             D_ITEM = round(T_ITEM / POP * 1000))

head(pres_df2[order(pres_df2$D_ITEM, decreasing = TRUE), ], 3)
tail(pres_df2[order(pres_df2$D_ITEM, decreasing = TRUE), ], 3)

#------------------------------------------------------------------------------#
# Read Geoshape file for coordinates of counties
# .shp = https://data.gov.uk/dataset/ceremonial-county-boundaries-of-england
# rgdal = http://gis.stackexchange.com/questions/19064/how-to-open-a-shapefile-in-r
shp = readOGR(dsn = "./English Ceremonial Counties.shp")
ukcoord = read.table("coord_uk_counties.txt",
                     header = FALSE,
                     stringsAsFactors = FALSE,
                     sep = ",")
colnames(ukcoord) = c("COUNTY", "LAT", "LONG")
