library(shiny)
library(httr)
library(lubridate)
library(jsonlite)
library(leaflet)

ui <- fluidPage(
   
   titlePanel("Bus Arrival Timing Application"),
   
   fluidRow(
         column(4,
                p("Enter the bus service number and bus stop ID and click the", strong("Submit"), "button once you're done."),
            textInput("bus_no",
                      label = h5("Enter Bus Service Number"),
                      value = "175"),
            textInput("bus_stop",
                      label = h5("Enter Bus Stop ID"),
                      value = "09048"),
            actionButton("drive", "Submit"),
            br(),
            br(),
            p("Bus arrival time is obtained using LTA DataMall API."),
            br(),
            p("Bus icon and 'You are here' icon are made by", a("Freepik", href = "http://www.freepik.com"), "and the icons are obtained from", a("flaticon", href = "www.flaticon.com"), "under", a("Creative Commons BY 3.0", href = "http://creativecommons.org/licenses/by/3.0/"))
            ),
         column(8, 
                tabsetPanel(type = "pills",
                            tabPanel("Introduction",
                                     h4("Bus Arrival Tab"),
                                     p("Enter the bus service number you're interested in and the bus stop ID that you're at into the boxes on the left and click on the", strong("Submit"), "button. A map will appear on the right showing your current location and the location of the next bus. It will also show you the estimated arrival time of the next bus and the terminal at which the bus will be stopping at."),
                                     h4("Bus Arrival Table Tab"),
                                     p("With the bus stop ID, a table will appear under the tab showing you the bus services at the bus stop and the estimated arrival timings of the buses, including the second and the third arriving buses."),
                                     h4("Bus Route Tab"),
                                     p("Unfortunately, the dataset for bus routes from LTA DataMall does not accept request parameters. That means searching for the route of a particular bus service number requires me to pull the entire dataset out and search for the route in R itself. This is extremely time consuming as the dataset is quite huge. So until the DataMall updates the dataset, I shall just make do with an example of", strong("Bus Service Number 10"), ".", em("29/01/2017"))),
                            tabPanel("Bus Arrival", 
                                     h4(textOutput("nextbus")),
                                     textOutput("mymsg"),
                                     leafletOutput("mymap")),
                            tabPanel("Bus Arrival Table",
                                     p(em("Bus arrival timings are expressed in minutes.")),
                                     tableOutput("mytable")),
                            tabPanel("Bus Route", 
                                     p("The route that Bus Service Number 10 takes, including the starting and terminating stations."),
                                     leafletOutput("mymap2")))
                )
         )
   )

url <- "http://datamall2.mytransport.sg/ltaodataservice"
ak <- readLines("./LTA_AccountKey")

# to access the dataset and pull the list of bus stops present in SG  
list_busstop <- function() {
      n <- seq(from = 0, to = 6000, by = 50)
      busstop_list = data.frame()
      for (i in n) {
            resp <- GET(paste0(url,
                               "/BusStops?$skip=",
                               i),
                        add_headers(AccountKey = ak,
                                    accept = "application/json"))
            resp_out <- fromJSON(content(resp,
                                         "text"),
                                 simplifyDataFrame = TRUE)
            busstop_list <- rbind(busstop_list, resp_out$value)
      }
      return(busstop_list)
}

# accessing of the dataset to pull the list of bus stops is not done here as
# it was already done once and the data is saved in a .csv file
# busstops_sg <- list_busstop()
busstops_sg <- read.csv("./busstops_sg.csv",
                        stringsAsFactors = FALSE)[, -1] 


server <- function(input, output) {
      
      # access the dataset and pull out the required data
      bus <- eventReactive(input$drive, {
            
            # check if the bus stop ID entered by the user is valid
            req(input$bus_stop)
            req(input$bus_stop %in% busstops_sg$BusStopCode)
            
            resp <- GET(paste0(url, 
                               "/BusArrival?BusStopID=", 
                               input$bus_stop), 
                        add_headers(AccountKey = ak,
                                    accept = "application/json"))
            arrivalinfo <- fromJSON(content(resp, 
                                            "text"), 
                                    simplifyDataFrame = TRUE)
            bustable <- arrivalinfo$Services
            cbind(bustable[, 1:5], 
                  bustable[, 6],
                  bustable[, 7],
                  bustable[, 8])
            
      })
      
      
      # define functions needed
      
      # convert time to SGT
      convert_time <- function(cl) {
            sgt <- suppressMessages(ymd_hms(cl, 
                                            tz = "Asia/Singapore"))
            sgt <- floor(as.numeric(sgt - Sys.time(), 
                                    units = "mins"))
            return(sgt)
      }
      
      # check wheelchair accessibility
      check_wc <- function(cl) {
            wca <- rep("", length(cl))
            for (i in 1:length(cl)) {
                  if (cl[i] == "WAB") {
                        wca[i] <- "Wheelchair Accessible"
                  } else {
                        wca[i] <- "Not Wheelchair Accessible"
                  }
            }
            return(wca)
      }
      
      # check coordinates of the bus
      check_coor <- function(coor_col) {
            coor <- rep(0, length(coor_col))
            for (i in 1:length(coor_col)) {
                  if (coor_col[i] == "0") {
                        err <- "Satellite cannot find the buses."
                        coor[i] <- NA
                  } else {
                        coor[i] <- as.numeric(coor_col[i])
                  }
            }
            return(coor)
      }
      
      # check arrival timing of the bus; convert to "Arr" if less than one min
      check_arr <- function(cl) {
            tarr <- rep("", length(cl))
            for (i in 1:length(cl)) {
                  if (is.na(cl[i])) {
                        tarr[i] <- NA
                  } else if (cl[i] <= 0) {
                        tarr[i] <- "Arr"
                  } else {
                        tarr[i] <- as.character(cl[i])
                  }
            }
            return(tarr)
      }

      
      # adjust the data frame to a smaller and more managable one
      rbus <- eventReactive(input$drive, {
            
            bustable <- bus()
            abus <- data.frame(apply(bustable[, c(6, 12, 18)], 
                                     2, 
                                     convert_time))
            wbus <- data.frame(apply(bustable[, c(11, 17, 23)], 
                                     2, 
                                     check_wc))
            cbus <- data.frame(apply(bustable[, c(7, 8, 13, 14, 19, 20)], 
                                     2, 
                                     check_coor))
            cbind(ServiceNo = bustable$ServiceNo, 
                  abus, 
                  wbus, 
                  cbus,
                  Terminating = bustable$TerminatingID)
            
      })
      
      
      # message on arrival timing of next bus
      msg <- eventReactive(input$drive, {
            
            req_bus <- subset(rbus(), ServiceNo == input$bus_no)
            terstop <- subset(busstops_sg, 
                              BusStopCode == as.character(req_bus$Terminating))
            terbus <- paste0("This bus terminates at ", 
                             terstop$Description, 
                             ".")
            next_time <- req_bus$EstimatedArrival
            if (length(next_time) == 0) {
                  return()
                  } else if (next_time <= 0) {
                        paste("Next", 
                              input$bus_no, 
                              "bus is arriving.",
                              terbus)
                        } else {
                              paste("Next", 
                                    input$bus_no, 
                                    "bus arriving in", 
                                    next_time, 
                                    "min(s).",
                                    terbus)
                        }
            })
      output$nextbus <- renderText({
            msg()
            })
      
      # draw map showing user and next bus current locations
      leaf <- eventReactive(input$drive, {
            
            req_bus <- subset(rbus(), ServiceNo == input$bus_no)
            text <- " Bus<br>Arriving at your stop in "
            busicon <- makeIcon("./busicon.png")
            youarehere <- makeIcon("./youarehere.png")
            busstop_yah <- subset(busstops_sg,
                                  BusStopCode == input$bus_stop)
            
            leaflet(width = 400, height = 400) %>%
                  addTiles() %>%
                  addMarkers(lng = req_bus[, 9],
                             lat = req_bus[, 8],
                             popup = paste0("Next ",
                                            req_bus[, 1],
                                            text,
                                            req_bus[, 2],
                                            " minute(s)!<br>",
                                            req_bus[, 5]),
                             icon = busicon) %>%
                  addMarkers(lng = busstop_yah$Longitude,
                             lat = busstop_yah$Latitude,
                             popup = "You are here.",
                             icon = youarehere) %>%
                  setView(lng = busstop_yah$Longitude,
                          lat = busstop_yah$Latitude,
                          zoom = 14)
            
            })
      
      
      observeEvent(input$drive, {
            
            lbus <- as.character(subset(rbus(), select = ServiceNo)[, 1])
            req_bus <- subset(rbus(), ServiceNo == input$bus_no)
            
            # if bus service does not come to that bus stop, print text
            if (!as.character(input$bus_no) %in% lbus) {
                  output$mymsg <- renderText({
                        "This bus doesn't come here!"
                        })
                  output$mymap <- renderLeaflet({
                        return()
                        })
                  # if coordinates of bus cannot be found, print text
                  } else if (is.na(req_bus[1, 8])) {
                        output$mymsg <- renderText({
                              paste0("Our satellite can't see this bus! ", 
                                     "But we can still estimate its arrival ", 
                                     "time.")
                              })
                        output$mymap <- renderLeaflet({
                              return()
                              })
                        # else, print map
                        } else {
                              output$mymsg <- renderText({
                                    return()
                                    })
                              output$mymap <- renderLeaflet({
                                    leaf()
                              })
                        }
            
            })

      
      output$mytable <- renderTable({
            
            req_table <- bus()
            abus <- data.frame(apply(req_table[, c(6, 12, 18)], 
                                     2, 
                                     convert_time))
            abus <- apply(abus, 2, check_arr)
            ntable <- data.frame(cbind(ServiceNo = req_table$ServiceNo, 
                                       abus))
            names(ntable) <- c("Service No", 
                               "First Bus", 
                               "Second Bus", 
                               "Third Bus")
            ntable
            
      })
      
      output$mymap2 <- renderLeaflet({
            
            n <- seq(from = 0, to = 500, by = 50)
            busroute <- data.frame()
            
            # access dataset and pull out relevant bus route data
            for (i in n) {
                  resp <- GET(paste0(url,
                                     "/BusRoutes?$skip=",
                                     i), 
                              add_headers(AccountKey = ak,
                                          accept = "application/json"))
                  businfo <- fromJSON(content(resp, "text"),
                                      simplifyDataFrame = TRUE)
                  if ("10" %in% unique(businfo$value$ServiceNo)) {
                        req <- subset(businfo$value, ServiceNo == "10")
                        len_req <- dim(busroute)[1]
                        busroute <- rbind(busroute, req)
                        if (!len_req < dim(busroute)[1]) break
                  }
            }
            
            # merge bus route data with bus stop coordinates to plot route
            dbusroute <- busroute[1:max(busroute$StopSequence), ]
            mbusroute <- merge(dbusroute, busstops_sg, by = "BusStopCode")
            mbusroute <- mbusroute[order(mbusroute$StopSequence), ]
            req_lng <- mbusroute$Longitude
            req_lat <- mbusroute$Latitude
            
            # draw map of bus route with start and end stations
            leaflet(width = 400, height = 400) %>%
                  addTiles() %>%
                  addPolylines(lng = req_lng, 
                               lat = req_lat,
                               col = "black") %>%
                  addPopups(lng = req_lng[1],
                            lat = req_lat[1],
                            popup = mbusroute$Description[1],
                            options = popupOptions(closeButton = FALSE)) %>%
                  addPopups(lng = req_lng[length(req_lng)],
                            lat = req_lat[length(req_lat)],
                            popup = mbusroute$Description[length(req_lng)],
                            options = popupOptions(closeButton = FALSE))
            
            })
}

shinyApp(ui = ui, server = server)
