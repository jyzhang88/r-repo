# Merge all clinic information together and subset out BRISTOL 
bristol_tws = data.frame(TOWN = character(0),
                         COUNTY = character(0),
                         stringsAsFactors = FALSE)

for (i in seq(201501, 201512, 1)) {
      
      # Read file by file
      addrfile = paste0("./drug_data/T",
                        i,
                        "ADDR+BNFT.CSV")
      
      clinic = data.table::fread(addrfile,
                                 select = c(1:8),
                                 col.names = c("PERIOD", "PRACTICE", 
                                               "CLINIC_ONE", "CLINIC_TWO",
                                               "ROAD_NAME", "TOWN", "COUNTY",
                                               "UNKNOWN")) %>%
            data.frame()
      
      clinic["TOWN"] = apply(clinic["TOWN"], 1, rm_dash)
      clinic["COUNTY"] = apply(clinic["COUNTY"], 1, rm_dash)
      
      # Subset those towns that lies in BRISTOL county
      bristol_entry = clinic[clinic$COUNTY == "BRISTOL", ] %>%
            select(TOWN, COUNTY)
      
      # Row bind all the towns together
      bristol_tws = rbind(bristol_tws, bristol_entry)
      
}

# Remove duplicated towns
bristol_tws = bristol_tws[!duplicated(bristol_tws$TOWN), ]
bristol_tws$TOWN = gsub("  ", " ", bristol_tws$TOWN)

#------------------------------------------------------------------------------#
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

list_towns$COUNTY = list_towns$COUNTY %>% 
      gsub("GREATER LONDON", "LONDON", .) %>%
      gsub("DURHAM/NORTH YORKSHIRE", "DURHAM", .) %>%
      gsub("COUNTY DURHAM", "DURHAM", .)

list_towns = rbind(list_towns, bristol_tws)

# Write into a csv file for future reading
write.csv(list_towns,
          "list_towns.csv",
          row.names = FALSE)

