load_census <- function() {
      # read state code txt file
      state <- read.table("state.txt", header = TRUE)
      
      # read census csv files
      # 49 states in 1930-1940
      c1930 <- read.csv("census_1930.csv", header = TRUE, 
                        stringsAsFactor = FALSE)
      c1940 <- read.csv("census_1940.csv", header = TRUE, 
                        stringsAsFactor = FALSE)
      
      # 51 states in 1950-1990
      c1950 <- read.csv("census_1950.csv", header = TRUE, 
                        stringsAsFactor = FALSE)
      c1960 <- read.csv("census_1960.csv", header = TRUE, 
                        stringsAsFactor = FALSE)
      c1970 <- read.csv("census_1970.csv", header = TRUE, 
                        stringsAsFactor = FALSE)
      c1980 <- read.csv("census_1980.csv", header = TRUE, 
                        stringsAsFactor = FALSE)
      c1990 <- read.csv("census_1990.csv", header = TRUE, 
                        stringsAsFactor = FALSE)
      
      # census 2000 is in xls format
      c2000 <- read.xlsx("census_2000.xls", 
                         sheetIndex = 1, 
                         startRow = 10, 
                         endRow = 60,
                         colIndex = c(1, 3:12), 
                         header = FALSE)
      names(c2000) <- c("state", seq(2000, 2009, by = 1))
      c2000$state <- gsub("[.]", "", as.character(c2000$state))
      c2000$state <- toupper(c2000$state)
      
      # merge all census data together into one data frame
      # col bind before merging the datasets with different number of states
      cenone <- cbind(c1930, c1940[, -1])
      centwo <- cbind(c1950, c1960[, -1], 
                      c1970[, -1], c1980[, -1], 
                      c1990[, -1], c2000[, -1])[, -54:-61]
      tcensus <- merge(cenone, centwo, by = "state")
      names(tcensus) <- c("state", seq(1930, 2001, by = 1))
      
      return(tcensus)
}