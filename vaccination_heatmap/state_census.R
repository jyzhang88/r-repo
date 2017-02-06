library(tidyr)
library(data.table)
library(xlsx)

#-----------------------------------------------------------------------------#
# make sure the corners, top and bottom rows are covered
setwd("./vaccination_heatmap")
pageone <- tabulizer::extract_areas("census_1990.pdf", 1)
pone <- data.frame(pageone[[1]])
pone <- pone[c(-1, -42), c(-2, -7)]
pone <- separate(data = pone, 
                 col = X6, 
                 into = c("X6", "X7"), 
                 sep = " ", 
                 remove = TRUE)
names(pone) <- c("state", seq(1990, 2000, by = 1))
for (i in 2:12) {
      pone[, i] <- as.numeric(gsub(",", "", pone[, i]))
}

# make sure the corners, top and bottom rows are covered
pagetwo <- tabulizer::extract_areas("census_1990.pdf", 2)
ptwo <- data.frame(pagetwo[[1]])
ptwo <- ptwo[-c(12:15), -6]
ptwo[, 13] <- NA
for (i in 1:11) {
      ss <- strsplit(as.character(ptwo[i, 1]), split = " ")[[1]]
      if (length(ss) == 3) {
            ptwo[i, 13] <- paste(ss[1], ss[2])
      } else {
            ptwo[i, 13] <- ss[1]
      }
}
ptwo[, 1] <- ptwo[, 13]
ptwo <- ptwo[, -13]
names(ptwo) <- c("state", seq(1990, 2000, by = 1))
for (i in 2:12) {
      ptwo[, i] <- as.numeric(gsub(",", "", ptwo[, i]))
}

# row bind the tables together
census_1990 <- rbind(pone, ptwo)
census_1990$state <- census_1990$state %>% 
      as.character() %>%
      trimws() %>%
      toupper()

# save into a .csv file for reading next time
write.csv(census_1990[, -12], 
          file = "census_1990.csv", 
          row.names = FALSE)

#-----------------------------------------------------------------------------#
# set fixed widths for different census data
# need to multiply the population by 1000 for years 1930-1960
f1930o <- c(2, -15, 9, 9, 9, 9, 9, 9)
f1930t <- c(2, -15, 9, 9, 9, 9) 
yrs <- 1930; yre <- 1939
nskipo <- 23; nskipt <- 82; nrow <- 49
f1940o <- c(2, -15, 9, 9, 9, 9, 9, 9)
f1940t <- c(2, -15, 9, 9, 9, 9)
yrs <- 1940; yre <- 1949
nskipo <- 21; nskipt <- 79; nrow <- 49
f1950o <- c(2, -21, 8, 9, 9, 8, 9)
f1950t <- c(2, -12, 9, 8, 9, 9, 8)
yrs <- 1950; yre <- 1959
nskipo <- 27; nskipt <- 92; nrow <- 51
f1960o <- c(2, -19, 9, 9, 9, 9, 9)
f1960t <- c(2, -10, 9, 9, 9, 9, 9)
yrs <- 1960; yre <- 1969
nskipo <- 24; nskipt <- 86; nrow <- 51
f1970o <- c(-4, 2, 10, 10, 10, 10, 10, 10)
f1970t <- c(-4, 2, 10, 10, 10, 10)
yrs <- 1970; yre <- 1979
nskipo <- 14; nskipt <- 67; nrow <- 51
f1980o <- c(2, -1, 10, 10, 10, 10, 10)
f1980t <- c(2, -1, 10, 10, 10, 10, 10)
yrs <- 1980; yre <- 1989
nskipo <- 11; nskipt <- 70; nrow <- 51

# not writing into functions because the fixed widths and skip rows of the
# census .txt files are all different
# you could put all as arguments, still need to assign width vector

#-----------------------------------------------------------------------------#
# repeat the following codes to extract all the population data for txt files
one <- read.fwf("census_1980.txt", # change file name
                widths = f1980o,
                skip = nskipo,
                n = nrow,
                colClasses = "character")
two <- read.fwf("census_1980.txt", # change file name
                widths = f1980t,
                skip = nskipt,
                n = nrow,
                colClasses = "character")
census <- merge(one, two, by = "V1")
names(census) <- c("state", seq(yrs, yre, by = 1))

for (i in 2:length(census)) {
      census[, i] <- as.numeric(gsub(",", "", trimws(census[, i])))
      census[, i] <- census[, i] * 1000 # comment this for 1970, 1980 data
}

# replace state code in census data into state names
state <- read.table("state.txt", header = TRUE)
census$state <- state[, 1][match(census$state, state[, 2])]
write.csv(census, 
          file = "census_1980.csv", # change file name
          row.names = FALSE) 

#-----------------------------------------------------------------------------#
# read xls file
c2000 <- read.xlsx("census_2000.xls", sheetIndex = 1, 
                   startRow = 10, endRow = 60,
                   colIndex = c(1:12), header = FALSE)
c2000 <- c2000[, -2]
names(c2000) <- c("state", seq(2000, 2009, by = 1))
c2000$state <- gsub("[.]", "", as.character(c2000$state))
c2000$state <- toupper(c2000$state)

#-----------------------------------------------------------------------------#
# two additional states added into the census data from 1950 onwards
# hawaii and alaska
setdiff(c1950$state, c1940$state)
