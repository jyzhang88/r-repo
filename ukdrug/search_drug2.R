search_drug2 = function(drug_name) {
      
      return(drug[grepl(tools::toTitleCase(drug_name), 
                        drug$BNF_NAME, 
                        fixed = TRUE), ])
      
}