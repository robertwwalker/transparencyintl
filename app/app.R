library(shiny)
library(dplyr)
library(tidyr)
library(reactable)
library(plotly)
library(viridis)
library(shinythemes)
library(htmltools)
library(tippy)
library(rnaturalearth)
library(rnaturalearthdata)

# This replaces much of the server side downloading and computation that was originally part of the app
# Because the data will not change, this is an easy way to speed things up.

load(url("https://github.com/robertwwalker/DADMStuff/raw/master/TIShiny.RData"))

# Create a tooltip for the map.

Map.Data <- Map.Data |> mutate(tooltip = paste0(region_un,"<br><b>",country_territory,"</b><br><br>CPI:",CPI,"<br>Rank: ",Rank,"<br>Std. Dev.: ",standard_error, "<br>Sources: ",sources,"<extra></extra>", sep=""))

# Function to create hover for column header from reactable help

with_tooltip <- function(value, tooltip, ...) {
  div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
      tippy(value, tooltip, ...))
}

# Function from reactable for positive and negative bar charts

bar_chart_pos_neg <- function(label, value, max_value = 50, height = "1rem",
                              pos_fill = "green", neg_fill = "red") {
  neg_chart <- div(style = list(flex = "1 1 0"))
  pos_chart <- div(style = list(flex = "1 1 0"))
  width <- paste0(abs(value / max_value) * 100, "%")
  
  if (value < 0) {
    bar <- div(style = list(marginLeft = "0.5rem", background = neg_fill, width = width, height = height))
    chart <- div(
      style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"),
      label,
      bar
    )
    neg_chart <- tagAppendChild(neg_chart, chart)
  } else {
    bar <- div(style = list(marginRight = "0.5rem", background = pos_fill, width = width, height = height))
    chart <- div(style = list(display = "flex", alignItems = "center"), bar, label)
    pos_chart <- tagAppendChild(pos_chart, chart)
  }
  
  div(style = list(display = "flex"), neg_chart, pos_chart)
}

# bar chart function from reactable

bar_chart <- function(label, width = "100%", height = "1rem", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

# A palette for cells

green_pal <- function(x) rgb(colorRamp(c("#aae4cc", "#aab54d"))(x), maxColorValue = 255)

# Define UI for application

ui <- fluidPage(theme=shinytheme("superhero"),
                titlePanel("Corruption Perceptions from Transparency International", windowTitle = "Corruption Perceptions"),
# Initialize the tabset panel for the main outputs.                
  tabsetPanel(
# One tab, the big one, for the plotly.
    tabPanel("Plotly",
    plotlyOutput("distPlot", height="550px")
    ),
# One tab for the table: a reactable
# This started with a datatable and I never bothered to change the iD
  tabPanel("Data",
           textOutput("title"),
    reactableOutput("DT")
  )),
# Using fluid rows puts the inputs at the bottom of the page.
# It draws attention to what I want people to use it for.    
# A 5 width column for the variable selection drop-down
fluidRow(
      column(width=4, 
             selectInput("var",
                         "Variable",
                         choices = list("Corruption Index" = "CPI",
                                        "Rank" = "Rank",
                                        "No. of Sources" = "sources",
                                        "Std. Error" = "standard_error"),
                         selected = "CPI"),
             print(
               HTML("<font color='#a2c4c9'> <small>Corruption Index: Corruption Perceptions Index <br/> 
                      (CPI) Higher values indicate less corruption <br />
                      Rank: Ranking, Best to Worst <br/>
                      No. of Sources: Number of Sources for CPI <br/>
                      Std. Error: Variability of CPI</small> </font>"))),
# A 4 width column to select a year with a slider
        column(width=3,
            sliderInput("year",
                        "Year",
                        min = 2017,
                        max= 2022,
                        value = 2022)
            ),
# a 3 width column with the palette choices from viridis
      column(width=5,
             radioButtons("pal",
                          "Viridis Palette:",
                          choices = c("A: magma" = "A",
                                      "B: inferno" = "B",
                                      "C: plasma" = "C",
                                      "D: viridis" = "D",
                                      "E: cividis" = "E",
                                      "F: rocket" = "F",
                                      "G: mako" = "G",
                                      "H: turbo" = "H"),
                          selected = "G",
                          inline = T
             )
             )
    )
)

server <- function(input, output, session) {
# I need one reactive on the Map data: I need to filter out a single year
# to uniquely determine values for the map.
  Map.Me <- reactive({Map.Data |> filter(year==input$year)})
# A title for the table page to explain a column that is transformed.
  output$title <- renderText("Averages shown for Regions")
# Use the reactive data to draw a table so that only one year is shown at a time.
  output$DT <- renderReactable({reactable(Map.Me(), 
# Group the data by continents/regions
              groupBy = "region_un",
# By default, hide columns, this is chiefly to avoid the geometry needed for the map
              defaultColDef = colDef(show = F),
# Format the columns and their behavior.  I have to explicitly 
# show the columns because of what is above.
              columns = list(
                country_territory = colDef(show=T,
                                           name = "Country Name"),
                year = colDef(show=T, 
                              name = "Year"),
                region_un = colDef(show=T, 
                                   name = "Region"),
                CPI = colDef(aggregate = "mean", 
                             format = colFormat(digits = 1), 
                             show=T, 
                             name="Corruption Perception Index"),
                sCPI = colDef(show=T,
                  header = with_tooltip("Centered CPI <br> [hover]", 
                                        h5("Corruption centered by Region and Year")
                                        ),
# Use the positive and negative bar plot function above for the region/year centerered corruption data
                  cell = function(value) {
                    label <- paste(round(value, digits=2))
                    bar_chart_pos_neg(label, value)
                  }),
# Show the ranks with variable green backgrounds using the palette I defined in the preliminaries
                Rank = colDef(show=T,
                              aggregate = "mean", 
                              format = colFormat(digits = 2),
                              style = function(value) {
                                normalized <- scale(value, scale=FALSE)
                                color <- green_pal(value/181)
                                list(background = color, color = "black")
                              }
                ), 
# Standard error gets no special treatment
                standard_error = colDef(show=T,
                                        aggregate = "mean", format = colFormat(digits = 2),
                                        name = "Standard Deviation of CPI"),
# The number of sources for the corruption data gets a bar plot
                sources = colDef(show=T,
                                 name = "No. of Sources",
                                 aggregate = "mean", format = colFormat(digits = 2),
                                 align = "left", 
                                 cell = function(value) {
                                   width <- paste0(value*10,"%")
                                   bar_chart(value, width = width)
                                 })
                ),
# Theme the reactable
              theme = reactableTheme(
                color = "hsl(233, 9%, 87%)",
                backgroundColor = "hsl(233, 9%, 19%)",
                borderColor = "hsl(233, 9%, 22%)",
                stripedColor = "hsl(233, 12%, 22%)",
                highlightColor = "hsl(233, 12%, 24%)",
                inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
                pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
                )
              )
})
# Produce a plotly choropleth
# Define the tooltip as a hovertemplate
# Add the variables for a choropleth.
# The input palette and the input variable are needed
# get is a base R command for grabbing the variable in this
# piped operation taking base data from the reactive
  output$distPlot <- renderPlotly({
    plot_geo(Map.Me(),
             hovertemplate=~tooltip) |>
      add_trace(
        z = ~get(input$var), 
        locations = ~iso_a3,
        color = ~get(input$var), 
        colors = viridis_pal(option = input$pal)(3)
      ) |> 
      layout(
        geo = list(showframe=FALSE)) |>
      colorbar(title = paste(input$var, "in", input$year))    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
