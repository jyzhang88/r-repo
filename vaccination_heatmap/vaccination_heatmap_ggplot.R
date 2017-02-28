library(ggplot2)
library(reshape2)
library(xlsx)

# setwd("./vaccination_heatmap")

#-----------------------------------------------------------------------------#
# measles csv file from Project Tycho
measles_file <- "MEASLES_Cases_1909-2001_20170125072150.csv"
measles <- read.csv(measles_file,
                    skip = 2,
                    header = TRUE,
                    colClasses = "character")

# last column is empty
measles <- measles[, -62]

# only be using data from 1930 onwards
measles <- measles[measles$YEAR >= 1930, ] 
measles[measles == "-"] <- NA

filldf <- data.frame(YEAR = unique(measles$YEAR))
for (i in names(measles[, 3:61])) {
      fillcol <- numeric()
      for (j in 1930:2001) {
            dat <- subset(measles, select = i, YEAR == j)
            len <- dim(dat)[1]
            if (sum(is.na(dat)) == len) {
                  tot <- NA
                  fillcol <- c(fillcol, tot)
            } else {
                  tot <- sum(as.numeric(dat[, 1]), na.rm = TRUE)    # sum
                  # tot <- mean(as.numeric(dat[, 1]), na.rm = TRUE) # average
                  
                  fillcol <- c(fillcol, tot)
            }
      }
      filldf <- cbind(filldf, fillcol)
}
names(filldf) <- c("YEAR", names(measles[, 3:61]))

# transpose the data frame and changing the states names
mfilldf <- melt(filldf, id.vars = "YEAR")
mfilldf$variable <- gsub("[.]", " ", mfilldf$variable)
dfill <- dcast(mfilldf, variable ~ YEAR, value.var = "value")

#-----------------------------------------------------------------------------#
# load the census data
source("load_census.R")
tcensus <- load_census()

#-----------------------------------------------------------------------------#
# use only states with both cases and census data
extra <- setdiff(dfill$variable, tcensus$state)
for (i in extra) {
      dfill <- subset(dfill, variable != i)
}
den <- cbind(state = dfill$variable, 
             dfill[, 2:73] / (tcensus[, 2:73] / 100000))
mden <- melt(den, id.vars = "state")

# reordering the sequence the states appear in ggplot
mden$state <- with(mden, factor(state, levels = rev(levels(state))))

#-----------------------------------------------------------------------------#
# set color gradient and varying parameters for the heatmap
# color chosen by eyeballing the different heatmaps
cols <- c(colorRampPalette(c("aliceblue", 
                             "cornflowerblue", 
                             "limegreen"))(80), 
          colorRampPalette(c("yellow", 
                             "red"))(520))

bks <- c(0, 1000, 2000, 3000)
lbs <- c("0", "1k", "2k", "3k")
lim <- c(0, 3000)
title <- "Annual Measles Cases (per 100,000)"

# if looking at the weekly average
# cols <- c(colorRampPalette(c("lavenderblush", "pink"))(50),
#           colorRampPalette(c("pink3", "firebrick4"))(750))
# bks <- c(0, 20, 40, 60, 80)
# lbs <- c("0", "20", "40", "60", "80")
# lim <- c(0, 80)
# title <- "Weekly Average Measles Cases (per 100,000)"

#-----------------------------------------------------------------------------#
# using ggplot(), geom_tile() to create a heatmap
# check if variable year is of factor class
ggplot(mden, aes(y = state, x = variable, fill = value)) +
      geom_tile(color = "white", width = 0.9, height = 0.9) +
      theme(panel.grid.major = element_blank(),
            panel.background = element_blank(),
            axis.line.y = element_blank(),
            axis.line.x = element_line(color = "black"),
            axis.ticks.y = element_blank()) +
      scale_fill_gradientn(colours = cols,
                           breaks = bks,
                           labels = lbs,
                           limits = lim,
                           guide = guide_colorbar(ticks = TRUE,
                                                  nbin = 50,
                                                  barheight = 0.6,
                                                  label = TRUE,
                                                  barwidth = 10,
                                                  direction = "horizontal"),
                           na.value = "snow") +
      scale_x_discrete(expand = c(0, 0),
                       breaks = seq(1930, 2000, by = 10)) +
      labs(x = "", y = "", fill = "") +
      ggtitle(title) +
      theme(legend.position = c(0.5, -0.1),
            legend.direction = "horizontal",
            legend.text = element_text(color = "grey20"),
            plot.margin = grid::unit(c(0.5, 1.5, 1.5, 0.5), "cm"),
            axis.text.y = element_text(size = 6,
                                       family = "Helvetica",
                                       hjust = 1),
            axis.text.x = element_text(size = 8),
            title = element_text(face = "bold",
                                 family = "Helvetica",
                                 vjust = 1),
            text = element_text(family = "Helvetica")) +
      geom_vline(xintercept = 34,
                 col = "grey20",
                 lwd = 1) +
      annotate("text", label = "Vaccine introduced",
               x = 35, y = 51,
               vjust = 1, hjust = 0,
               size = I(3.5),
               family = "Helvetica")
