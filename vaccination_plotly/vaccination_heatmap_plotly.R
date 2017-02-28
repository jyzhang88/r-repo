library(plotly)
library(ggplot2)
library(RCurl)
url = "https://raw.githubusercontent.com/jyzhang88/r-repo/master"

# follow up from previous data frame "mden.txt" 
mden_file <- "/vaccination_heatmap/mden.txt"
mden <- read.table(text = getURL(paste0(url, mden_file)), 
                   header = TRUE)
mden$state <- with(mden, factor(state, levels = rev(levels(state))))
mden$variable <- with(mden, factor(variable))
cols <- c(colorRampPalette(c("aliceblue", 
                             "cornflowerblue", 
                             "limegreen"))(80), 
          colorRampPalette(c("yellow", 
                             "red"))(520))
lbs = c("0", "1,000", "2,000", "3,000") # edit to fit plotly
bks <- c(0, 1000, 2000, 3000)
lim <- c(0, 3000)
title <- "Annual Measles Cases (per 100,000)"

# assign the ggplot; there are some adjustments to the ggplot
g = ggplot(mden, aes(y = state, x = variable, fill = value)) +
      geom_tile(color = "white", width = 0.9, height = 0.9) +
      theme(panel.grid.major = element_blank(), # consolidated parameters
            panel.background = element_blank(),
            plot.margin = grid::unit(c(5, 5, 5, 5), "mm"),
            axis.line.y = element_blank(),
            axis.line.x = element_line(color = "black"),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(size = 6,
                                       family = "Helvetica",
                                       hjust = 1),
            axis.text.x = element_text(size = 8),
            legend.text = element_text(color = "grey20"),
            legend.key.width = grid::unit(3, "mm"),
            title = element_text(face = "bold",
                                 family = "Helvetica",
                                 vjust = 1),
            text = element_text(family = "Helvetica")) +
      scale_fill_gradientn(colours = cols, 
                           breaks = bks,
                           labels = lbs,
                           limits = lim,
                           guide = guide_colorbar(ticks = TRUE,
                                                  nbin = 50,
                                                  barheight = 0.6,
                                                  label = TRUE,
                                                  barwidth = 10),
                           na.value = "snow") +
      scale_x_discrete(expand = c(0, 0),
                       breaks = seq(1930, 2000, by = 10)) +
      labs(x = "", y = "", fill = "") +
      ggtitle(title) +
      geom_vline(xintercept = 34,
                 col = "grey20",
                 lwd = 0.5) +
      annotate("text", 
               label = "Vaccine introduced",
               x = 35, y = 50, # adjusted position of the words
               size = I(3),
               family = "Helvetica")

# edit hovering text for the states
hover = mden %>%
      mutate(state = as.character(state),
             variable = as.character(variable)) %>%
      arrange(desc(state), variable)

# rewrite the text that will appear on hovering in the interactive plot
text = c()
for (i in 1:3528) {
      rowbyrow = hover[i, ]
      chunk = paste0("Year: ", 
                     rowbyrow[2],
                     "<br>State: ", 
                     rowbyrow[1],
                     "<br>Annual Cases: ", 
                     round(rowbyrow[3], 2))
      text = c(text, chunk)
}
mtext = matrix(text, nrow = 49, ncol = 72, byrow = TRUE)

# customizing the plot from plotly
p = plotly_build(g)

p$x$data[[1]]$text[1:3, 1:3]  # old text
mtext[1:3, 1:3]               # new text
p$x$data[[1]]$text = mtext    # reassign text

p$x$data[[2]]$y = c(0.4, 49.5)            # adjust margin
p$x$layout$yaxis$range = c(0.4, 50.6)     # adjust margin
p$x$data[[2]]$hoverinfo = "none"          # remove hover for line
p$x$data[[3]]$hoverinfo = "none"          # remove hover for text


# ggplotly(p)           # to draw plotly graph in RStudio Viewer window

plotly_POST(p, "vaccination_heatmap_plotly")    # push plot to plotly cloud
