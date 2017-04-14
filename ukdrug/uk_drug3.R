# Few changes made
# - read only necessary columns
# - gsub vector directly instead of using apply() function
# - grepl with fixed = TRUE makes the search faster
# - saved required tables in csv and read directly instead of from site

# Read excel file with drug data
drug = data.table::fread(drugfile,
                         select = c(3, 5, 6, 9, 10),
                         col.names = c("PRACTICE", "BNF_NAME", "ITEMS",
                                       "QUANTITY", "PERIOD")) %>%
      data.frame()
#------------------------------------------------------------------------------#
# Load list of clinics situated around UK
clinic = data.table::fread(addrfile,
                           select = c(2, 6, 7),
                           col.names = c("PRACTICE", "TOWN", "COUNTY")) %>%
      data.frame()

# Remove dashes from names of towns and counties
clinic$COUNTY = gsub("&", "AND", clinic$COUNTY) %>%
      gsub("-", " ", .)
clinic$TOWN = gsub("-", " ", clinic$TOWN)
#------------------------------------------------------------------------------#
# Check if county present, if not, use town name to get county
clinic$COUNTY = apply(clinic, 1, check_ct2)
clinic = clinic[!clinic$COUNTY == "", ]

#------------------------------------------------------------------------------#
# Subset rows that contain mirtazapine
req_df = search_drug2("mirtazapine")

#------------------------------------------------------------------------------#
# Merge drug prescription, clinic location and county pop data sets together
d_c = merge(req_df,
            clinic,
            by = "PRACTICE")

pres_df = group_by(d_c, COUNTY) %>%
      summarize(T_PRES = sum(QUANTITY),
                T_ITEM = sum(ITEMS),
                PERIOD = unique(PERIOD)) %>%
      data.frame() %>%
      merge(c_pop, by = "COUNTY") %>%
      mutate(D_PRES = round(T_PRES / POP * 1000),
             D_ITEM = round(T_ITEM / POP * 1000))

final_df = rbind(final_df, pres_df)

