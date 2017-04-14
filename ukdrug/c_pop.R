# Get population data of the counties
url2 = "https://en.wikipedia.org/wiki/List_of_ceremonial_counties_of_England"
http = readLines(url2)

# Create an empty data frame to collect the names from the 'for' loop
c_pop = data.frame(COUNTY = character(0),
                   POP = numeric(0),
                   stringsAsFactors = FALSE)

for (i in seq(64, 534, 10)) {
      
      # Subset string containing county
      start_ct = regexpr('">', http[i])[1] + 2
      end_ct = regexpr('</a>', http[i])[1] - 1
      ct = substr(http[i], start_ct, end_ct) %>%
            rm_dash() %>%
            toupper()
      
      # Subset string containing numerics
      list_pop = gregexpr('[1234567890]', http[i+1])
      pop = substr(http[i+1], 
                   list_pop[[1]][1], 
                   list_pop[[1]][length(list_pop[[1]])]) %>%
            gsub(",", "", .) %>%
            as.numeric()
      
      # Row bind entry with the c_pop data frame
      entry = data.frame(COUNTY = ct,
                         POP = pop,
                         stringsAsFactors = FALSE)
      c_pop = rbind(c_pop, entry)
      
}

c_pop$COUNTY = c_pop$COUNTY %>% 
      gsub("GREATER LONDON", "LONDON", .) %>%
      gsub("DURHAM/NORTH YORKSHIRE", "DURHAM", .) %>%
      gsub("COUNTY DURHAM", "DURHAM", .)

# Write into a csv file for future reading
write.csv(c_pop,
          "c_pop.csv",
          row.names = FALSE)
