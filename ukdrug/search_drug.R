search_drug = function(drug_name) {
      
      # Use REGEX to search for rows carrying the drug name
      tf = apply(X = drug["BNF_NAME"], 
                 MARGIN = 1, 
                 FUN = grepl, 
                 pattern = tools::toTitleCase(drug_name))
      
      # Subset out those rows that carry the drugs
      df = drug[tf == TRUE, ]
      
      return(df)
      
}

# This function could be written in a way that takes both the data set carrying
# the drug names and the drug name that you're searching for into the argument
# However in this case, it only takes in the drug name as an argument