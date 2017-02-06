# read state code file
us_state_code <- read.delim("us_state_code", 
                            stringsAsFactor = FALSE,
                            header = FALSE)
us_state <- read.delim("us_state", 
                       stringsAsFactor = FALSE, 
                       header = FALSE)
state <- cbind(us_state, us_state_code)
names(state) <- c("state", "state_code")
state$state <- toupper(state$state)

# save data frame as delimited .txt file
write.table(state, file = "state.txt", sep = "\t", row.names = FALSE)