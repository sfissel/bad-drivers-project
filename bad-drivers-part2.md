Bad Drivers Project - Part 2
================
Stephanie Fissel,
December 11, 2022

``` r
library(rsconnect)
```

    ## Warning: package 'rsconnect' was built under R version 4.1.2

## WORKSPACE PREP

- Data source: [Bad
  Drivers](https://github.com/fivethirtyeight/data/tree/master/bad-drivers)

## Load packages

``` r
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinyWidgets)
library(usdata)
```

## Read in and clean data

- `bad_drivers.csv`  
- `us_states_w_regions.csv`

``` r
# Reading in bad_drivers.csv
bad_drivers <- read.csv("/Users/stephaniefissel/Desktop/ds 2003/bad-drivers.csv")

# Subsetting bad_drivers
bd1 <- subset(bad_drivers, select = c(State,
                                      Number.of.drivers.involved.in.fatal.collisions.per.billion.miles))

# Transforming state names into state abbreviations using usdata library's state2abbr function
# Adding state aabbreviations column 
bd1$States <- state2abbr(bd1$State)

# Eliminating state name column
bd2 <- subset(bd1, select = c(States, 
                                      Number.of.drivers.involved.in.fatal.collisions.per.billion.miles))

# Ordering state abbreviations alphabetically
bd2 <- bd2[with(bd2, order(States, Number.of.drivers.involved.in.fatal.collisions.per.billion.miles)),]

# Reading in us_states_w_regions.csv to add regions by state
region1 <- read.csv(("/Users/stephaniefissel/Documents/GitHub/ds2003Midterm/us_states_w_regions.csv"))

# Subsetting data to just have region
region1 <- subset(region1, select = c(Region))

# Combining datasets
bd2 <- cbind(bd2, Regions = region1$Region)
```

## Building the Shiny App

### Graph 1

``` r
shinyApp(
  ui <- fluidPage(
    # Region Menu
    ## State select input
    selectInput("state", "State:", bd2$States %>% 
                  append("All"), selected = "All"
               ), 
    ## Region select input
    selectInput("region", "Region (State selection must be 'All'):", bd2$Regions %>%
                  append("All"), selected = "All"
    ),
    ## Number of collisions slider range input
    sliderInput("number", "Number of Fatal Collisions per Billion Miles Between:", 
                 min = 5, max = 25, value = c(5, 25), step = 5
                 ),
    

    # Bar Plot
    plotOutput("barplot")
  ),
  
  # Define server logic
  server <- function(input, output) {
    
    # Create bar plot of states vs. number of fatal collisions
    output$barplot <- renderPlot({
      # Filter data based on selected state
      if (input$state != "All") {
        bd2 <- filter(bd2, States == input$state)
      }
      # Filter data based on selected region
      if (input$region != "All") {
        bd2 <- filter(bd2, Regions == input$region)
      }
      # Filter data based on selected range of number of collisions
      bd2 <- filter(bd2, Number.of.drivers.involved.in.fatal.collisions.per.billion.miles >= input$number[1],
                         Number.of.drivers.involved.in.fatal.collisions.per.billion.miles <= input$number[2])
      
      #Bar Plot
      ggplot(bd2, aes(reorder(States, Number.of.drivers.involved.in.fatal.collisions.per.billion.miles), text = paste("Number of Crashes:", Number.of.drivers.involved.in.fatal.collisions.per.billion.miles), color = Regions, fill = Regions)) +
        geom_bar(aes(weight = Number.of.drivers.involved.in.fatal.collisions.per.billion.miles), width = 0.60) + 
        ggtitle("D R I V E R S   I N   F A T A L   C O L L I S I O N S   B Y   S T A T E") +
        xlab("State") +
        ylab("Number of Drivers in Fatal Collisions per Billion Miles") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5,
                                        size = 20,
                                        family = "Arial",
                                        face = "bold"),
              text = element_text(family = "Arial",
                                  size = 14),
              legend.title = element_text(size = 16,
                                          face = "bold",
                                          family = "Arial"),
              legend.text = element_text(size = 14,
                                         family = "Arial"),
              axis.title = element_text(size = 14,
                                        face = "bold"),
              axis.text = element_text(size = 14,
                                       family = "Arial"),
              axis.text.x = element_text(angle = 45,
                                         size = 10)) +
        scale_color_manual(values = c("#26547C", "#EF476F", "#FFD166", "#06D6A0")) +
        scale_fill_manual(values = c("#26547C", "#EF476F", "#FFD166", "#06D6A0"))
      
    })
      }
  )
```

<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>
