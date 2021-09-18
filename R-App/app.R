library(shiny)
library(leaflet)
library(magrittr)
library(RColorBrewer)
library(rgdal)
library(sp)
library(htmltools) 
library(dygraphs)
library(highcharter)
library(forecast)

states <- rgdal::readOGR('data/INDIA.geojson')
data = read.csv('data/covid_statewise_full.csv')
f_data1 = c( 41,2,1394,33466,155746,429734,1110507,1995178 , 2621418 , 1871498 , 1278727  ,823900 ,470901  ,354631  ,1109424  ,6943304  ,9010075  ,2236590  ,1244190,1198098)
f_data2 = c(0,0,36,1120,4270,12037,19307,29172,33424,23576,15639,11455,5483,2797,5792,49894,134275,56031,20076,14670)
data$Deaths = c('13,750', '259', '5,600', '9,650', '13,555', '3,186', '10,079', '9,669', '3,578', '4,404', '5,132', '37,184', '19,757', '10,516', '136,355', '1,759', '1,285', '201', '611', '7,493', '16,355', '8,954', '367', '34,761', '791', '22,794', '7,377', '18,383', '3,862', '129', '812', '4', '207', '51', '25,079', '1,809')
data$deaths_num = c(13750, 259, 5600, 9650, 13555, 3186, 10079, 9669, 3578, 4404, 5132, 37184, 19757, 10516, 136355, 1759, 1285, 201, 611, 7493, 16355, 8954, 367, 34761, 791, 22794, 7377, 18383, 3862, 129, 812, 4, 207, 51, 25079, 1809)
data$Confirmed.Cases = c('2,004,590', '52,409', '585,689', '725,605', '1,004,230', '173,357', '825,330', '770,380', '212,260', '324,420', '347,755', '2,941,026', '3,851,984', '792,109', '6,432,649', '111,598', '74,232', '54,057', '29,669', '1,002,323', '600,342', '954,040', '29,477', '2,604,074', '82,384', '1,709,152', '342,786', '1,544,109', '655,732', '7,560', '65,069', '10,659', '20,500', '10,318', '1,437,485', '123,007')
data$confirmed_num = c(2004590, 52409, 585689, 725605, 1004230, 173357, 825330, 770380, 212260, 324420, 347755, 2941026, 3851984, 792109, 6432649, 111598, 74232, 54057, 29669, 1002323, 600342, 954040, 29477, 2604074, 82384, 1709152, 342786, 1544109, 655732, 7560, 65069, 10659, 20500, 10318, 1437485, 123007)
death_data = read.csv('data/death_dataset.csv')
cases_data = read.csv('data/cases_dataset.csv')

pal1 <- colorNumeric('YlOrRd',NULL)
pal2 <- colorNumeric('viridis',NULL)

states$ConfirmedCases = data$Confirmed.Cases
states$CuredCases = data$Cured.Discharged
states$Deaths = data$Deaths

labels1 <- sprintf("<strong>%s</strong><br>Deaths: %s",states$Name,states$Deaths) %>% 
  lapply(htmltools::HTML)

labels2 <- sprintf("<strong>%s</strong><br>Cured: %s",states$Name,states$CuredCases) %>%
  lapply(htmltools::HTML)

l1 <- leaflet(states) %>%
  addLegend(pal = pal1, values = ~data$deaths_num, opacity = 0.7, title = 'Deaths',
            position = "bottomright") %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal1(log10(data$deaths_num)),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 1,
    highlight = highlightOptions(
      weight = 2,
      color = "#0C0C0C",
      dashArray = "",
      fillOpacity = 1,
      bringToFront = TRUE),
    label=labels1,
    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto"))

l2 <- leaflet(states) %>%
  addLegend(pal = pal2, values = ~data$confirmed_num, opacity = 0.7, title = 'Confirmed Cases',
            position = "bottomright") %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal2(log10(data$confirmed_num)),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 1,
    highlight = highlightOptions(
      weight = 2,
      color = "#0C0C0C",
      dashArray = "",
      fillOpacity = 1,
      bringToFront = TRUE),
    label=labels2,
    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto"))

timeser1 <- ts(f_data1,start = c(2020,1),end = c(2021,8),frequency = 12)
fore1 <- forecast::bats(timeser1,12)
hplot1 = hchart(timeser1,name="Current Cases") %>%
  hc_add_series(data = forecast::forecast(fore1),name="Predicted Cases")

timeser2 <- ts(f_data2,start = c(2020,1),end = c(2021,8),frequency = 12)
fore2 <- forecast::snaive(timeser2)
hplot2 = hchart(timeser2,name="Current Deaths") %>%
  hc_add_series(data = forecast::forecast(fore2),name="Predicted Deaths")

splot1 <- hchart(data,"scatter",hcaes(x = Discharge.Ratio,y=Active.Ratio,group = Region)) %>%
  hc_title(text = 'Discharged vs Active Cases Ratio') %>%
  hc_xAxis(title = list(text = "Discharge Ratio (%)"), format = list(text = "Discharge Ratio")) %>%
  hc_yAxis(title = list(text = "Active Ratio (%)"),format = list(text = "Active Ratio"))

splot2 <- hchart(data,"scatter",hcaes(x = Discharge.Ratio,y=Death.Ratio,group = Region)) %>%
  hc_title(text = 'Discharged Cases vs Deaths Ratio') %>%
  hc_xAxis(title = list(text = "Discharge Ratio (%)"), format = list(text = "Discharge Ratio")) %>%
  hc_yAxis(title = list(text = "Death Ratio (%)"),format = list(text = "Death Ratio"))



ui <- shiny::htmlTemplate('www/index.html', 
                          leaflet_map_1 = leafletOutput(outputId = "l1"),
                          
                          leaflet_map_2 = leafletOutput(outputId = "l2"),
                          
                          hplot_1 = highchartOutput(outputId = "hmap1"),
                          
                          hplot_2 = highchartOutput(outputId = "hmap2"),
                          
                          splot_1 = highchartOutput(outputId = "smap1"),
                          
                          splot_2 = highchartOutput(outputId = "smap2"))


server <- function(input, output) {
  
  output$hmap1 <- renderHighchart(hplot1)
  
  output$hmap2 <- renderHighchart(hplot2)
  
  output$smap1 <- renderHighchart(splot1)
  
  output$smap2 <- renderHighchart(splot2)
  
  output$l1 <- renderLeaflet(l1)
  
  output$l2 <- renderLeaflet(l2)
}

shinyApp(ui , server)