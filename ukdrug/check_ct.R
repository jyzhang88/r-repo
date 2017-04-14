check_ct = function(x) {
      
      # x is the data frame carrying the names
      # Subset the TOWN and COUNTY column 
      tw = x["TOWN"]
      ct = x["COUNTY"]
      
      # If county name present in the list from Wiki, use that county name
      for (i in all_counties) {
            if (grepl(i, ct)) {
                  new_ct = i
            }
      }
      
      # If county name absent in the list from Wiki, use the town name to 
      # determine the county name based on the list from Wiki
      if (!exists("new_ct")) {
            for (i in all_towns) {
                  if (grepl(i, tw)) {
                        new_tw = i
                        ct_index = which(list_towns$TOWN == new_tw)
                        new_ct = list_towns$COUNTY[ct_index]
                  }
            }
      } 
      
      # If both town and county names are missing, return a blank string
      new_ct = ifelse(!exists("new_ct"), "", new_ct)
      
      return(new_ct)
      
}