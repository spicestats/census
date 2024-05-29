# hc options -------------------------------------------------------------------

hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

# colors -----------------------------------------------------------------------

spcols <- c(purple = "#500778",
            darkblue = "#003057",
            midblue = "#007DBA",
            brightblue = "#00A9E0",
            jade = "#108765",
            green = "#568125",
            magenta = "#B0008E",
            mauve = "#B884CB",
            red = "#E40046",
            orange = "#E87722",
            gold = "#CC8A00",
            mustard = "#DAAA00")


grey <- "#848789"

# ggplot theme -----------------------------------------------------------------

my_ggtheme <- function() {
  theme_minimal() +
    theme(text =(element_text(size = 12)),
          plot.title = element_text(face = "bold", size = 18, family = "Century Gothic"),
          plot.title.position = "plot",
          plot.subtitle = element_text(size=10, face="plain", color="black"),
          strip.text = element_text(size = 12, family = "sans", hjust = 0.5),
          axis.line.x = element_blank(),
          axis.title = element_blank(),
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          plot.margin=unit(c(10, 7, 10, 7), "pt")) 
}


# hc theme ------------------------------------------------------------------------

my_theme <- highcharter::hc_theme(
  
  # * colors ----
  colors = unname(spcols),
  
  # * chart ----
  chart = list(backgroundColor = "white",
               style = list(fontFamily = "Roboto",
                            fontSize = 'medium',
                            color = "grey30")),
  
  # * titles ----
  
  title = list(
    useHTML = TRUE,
    align = "left",
    style = list(fontSize = 'medium',
                 color = "grey30",
                 fontWeight = 'bold')),
  
  subtitle = list(
    useHTML = TRUE,
    align = "left",
    style = list(fontSize = 'medium',
                 color = "grey30",
                 # enable wrapped text for subtitles
                 whiteSpace = 'inherit')),
  
  credits = list(
    style = list(fontSize = 'small',
                 color = "grey80")),
  
  # * y axis ----
  
  yAxis = list(
    title = list(
      style = list(fontSize = 'medium',
                   color = "grey70")),
    labels = list(
      style = list(fontSize = 'small',
                   color = "grey70")),
    useHTML = TRUE,
    lineWidth = 1,
    tickAmount = 5,
    tickmarkPlacement = "on",
    tickLength = 5),
  
  # * x axis ----
  
  xAxis = list(
    title = list(
      style = list(fontSize = 'medium',
                   color = "grey70")),
    labels = list(
      useHTML = TRUE,
      style = list(fontSize = 'small',
                   color = "grey70")),
    tickmarkPlacement = "on",
    tickLength = 0),
  
  # * tooltip ----
  tooltip = list(
    useHTML = TRUE,
    backgroundColor = "white",
    outside = TRUE,
    style = list(color = "grey30")),
  
  # * legend ----
  
  legend = list(
    itemStyle = list(
      color = "grey70",
      fontSize = "small",
      fontWeight = "300"
    )),
  
  plotOptions = list(
    series = list(
      
      # * dataLabels ----
      dataLabels = list(
        style = list(fontSize = "medium",
                     color = "grey30")),
      
      marker = list(fillColor = "white",
                    lineWidth = 2,
                    lineColor = NA)))
)

# make charts ------------------------------------------------------------------

hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

## pay -------------------------------------------------------------------------

make_earnings_chart <- function(df){ 
  
  ids <- unique(df$Area_name)
  
  df %>% 
    hchart("line", hcaes(x = TimePeriod, y = Data, group = Area_name),
           id = ids) %>% 
    hc_add_series(type = "arearange", 
                  data = df, 
                  hcaes(x = TimePeriod, low = Lower, high = Upper, group = Area_name),
                  linkedTo = ids,
                  opacity = 0.2,
                  enableMouseTracking = FALSE,
                  lineColor = "transparent",
                  showInLegend = FALSE) %>% 
    hc_colors(colors = unname(spcols[1:length(ids)])) %>% 
    hc_xAxis(title = NULL) %>% 
    hc_yAxis(title = "", 
             labels = list(format = '\u00A3{value: ,f}')) %>% 
    hc_tooltip(valueDecimals = 0,
               valuePrefix = "\u00A3",
               xDateFormat = '%b %Y') %>% 
    hc_add_theme(my_theme) %>%
    hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
    hc_legend(verticalAlign = "top",
              align = "right",
              floating = TRUE,
              backgroundColor = "white",
              y = 20)
} 

make_region_earnings_chart <- function(df, council_list) {
  
  df %>% 
    filter(Area_name %in% council_list,
           Measure == "Median weekly employee earnings") %>% 
    make_earnings_chart() %>% 
    hc_legend(layout = "proximate",
              align = "right")
}

make_earnings_errorbar_chart <- function(df) {
  
  highchart() %>%
    hc_add_series(df, "scatter", hcaes(x = Area_name, y = Data), showInLegend = FALSE,
                  zIndex = 2, name = paste(month.abb[df$Month[1]], df$Year[1])) %>% 
    hc_add_series(df %>% mutate(Data = ifelse(Area_name == "Scotland", Data * 1.1, NA)), 
                  "column", hcaes(x = Area_name, y = Data), 
                  color = spcols[3], enableMouseTracking = FALSE,
                  zIndex = 1, opacity = 0.5, showInLegend = FALSE) %>% 
    hc_add_series("errorbar", data = df, hcaes(low = Lower, high = Upper),
                  enableMouseTracking = FALSE, showInLegend = FALSE) %>% 
    hc_chart(inverted = TRUE) %>% 
    hc_colors(colors = unname(spcols)) %>% 
    hc_xAxis(title = NULL, type = "category") %>% 
    hc_yAxis(title = "", 
             labels = list(format = '\u00A3{value: ,f}')) %>% 
    hc_tooltip(valueDecimals = 0,
               valuePrefix = "\u00A3",
               headerFormat = '<b>{point.key} </b><br>',
               pointFormat = "{series.name}: <b>{point.y}</b><br/>") %>% 
    hc_add_theme(my_theme)
}

## labour market ---------------------------------------------------------------

make_labourmarket_chart <- function(df){
  
  ids <- unique(df$Measure)

  df %>% 
    hchart("line", hcaes(x = Year, y = Data*100, group = Measure),
           marker = list(enabled = FALSE),
           id = ids) %>% 
    hc_add_series(type = "arearange", 
                  data = df, 
                  hcaes(x = Year, low = Lower*100, high = Upper*100, group = Measure),
                  linkedTo = ids,
                  opacity = 0.3,
                  enableMouseTracking = FALSE,
                  lineColor = "transparent",
                  marker = list(enabled = FALSE),
                  showInLegend = FALSE) %>% 
    hc_colors(colors = unname(spcols[c(3, 6, 7)])) %>% 
    hc_legend(verticalAlign = "top",
              align = "right",
              floating = TRUE,
              y = 15,
              backgroundColor = "white") %>% 
    hc_xAxis(title = NULL) %>% 
    hc_yAxis(title = "",
             softMin = 0,
             softMax = 100,
             maxPadding = 0, 
             labels = list(
               format = '{value}%'),
             accessibility = list(description = "Rate")) %>% 
    hc_add_theme(my_theme) %>%
    hc_tooltip(pointFormat = '{series.name}: <b>{point.y:.1f}%</b>',
               xDateFormat = '%b %Y' )
}

make_labourmarket_errorbar_chart <- function(df) {
  
  highchart() %>%
    hc_add_series(df, "scatter", hcaes(x = Area_name, y = Data*100, group = Age),
                  zIndex = 2, name = paste(month.abb[df$Month[1]], year(df$Year[1])), 
                  showInLegend = FALSE) %>% 
    hc_add_series(df %>% mutate(Data = ifelse(Area_name == "Scotland", Data * 1.1, NA)), 
                  "column", hcaes(x = Area_name, y = Data*100), 
                  color = spcols[3], enableMouseTracking = FALSE,
                  zIndex = 1, opacity = 0.5, showInLegend = FALSE) %>% 
    hc_add_series(df, "errorbar",  
                  hcaes(x = Area_name, low = Lower*100, high = Upper*100),
                  enableMouseTracking = FALSE, showInLegend = FALSE) %>% 
    hc_chart(inverted = TRUE) %>% 
    hc_colors(colors = unname(spcols)) %>% 
    hc_xAxis(title = NULL, type = "category") %>% 
    hc_yAxis(title = "",
             labels = list(format = "{value}%")) %>% 
    hc_add_theme(my_theme) %>%
    hc_tooltip(headerFormat = '<b>{point.key} </b><br>',
               pointFormat = "{series.name}: <b>{point.y:.1f}%</b>")
}

## housing ---------------------------------------------------------------------

make_house_prices_chart <- function(df){
  
  df %>% 
    arrange(desc(Data)) %>% 
    mutate(mycols = ifelse(Area_name == "Scotland", "#007DBA", spcols[1])) %>% 
    hchart("bar", hcaes(x = Area_name, y = Data, color = mycols),
           name = df$Year[1], opacity = 0.8) %>% 
    hc_xAxis(title = NULL) %>% 
    hc_yAxis(title = "", 
             labels = list(format = '\u00A3{value: ,f}')) %>% 
    hc_tooltip(valueDecimals = 0,
               valuePrefix = "\u00A3",
               xDateFormat = '%b %Y') %>% 
    hc_add_theme(my_theme)
}

make_house_prices_chart_ts <- function(df){
  
  spcols <- spcols[1:length(unique(df$Area_name))]
  
  df %>% 
    arrange(TimePeriod, desc(Data)) %>% 
    hchart("line", hcaes(x = TimePeriod, y = Data, group = Area_name)) %>% 
    hc_colors(colors = unname(spcols)) %>% 
    hc_xAxis(title = NULL) %>% 
    hc_yAxis(title = "", 
             labels = list(format = '\u00A3{value: ,f}')) %>% 
    hc_tooltip(valueDecimals = 0,
               valuePrefix = "\u00A3",
               xDateFormat = '%b %Y') %>% 
    hc_add_theme(my_theme) %>% 
    hc_plotOptions(line = list(marker = list(enabled = FALSE))) %>%
    hc_legend(layout = "proximate",
              align = "right",
              backgroundColor = "white") 
}

make_housing_affordability_chart <- function(df){
  
  df %>% 
    arrange(desc(Data)) %>% 
    mutate(mycols = ifelse(Area_name == "Scotland", "#007DBA", spcols[1])) %>% 
    hchart("bar", hcaes(x = Area_name, y = Data, color = mycols),
           name = df$Year[1], opacity = 0.8) %>% 
    hc_xAxis(title = NULL) %>% 
    hc_yAxis(title = "") %>% 
    hc_tooltip(valueDecimals = 1,
               xDateFormat = '%b %Y') %>% 
    hc_add_theme(my_theme)
}

make_tenure_chart <- function(df) {
  df %>%    
    arrange(Measure, desc(Data)) %>% 
    hchart("bar", hcaes(x = Area_name, y = Data*100, group = Measure), 
           opacity = 0.8) %>% 
    hc_colors(colors = unname(spcols)) %>% 
    hc_plotOptions(bar = list(stacking = "normal")) %>% 
    hc_xAxis(title = NULL) %>% 
    hc_yAxis(title = "",
             reversedStacks = FALSE,
             max = 100,
             labels = list(format = '{value}%')) %>% 
    hc_tooltip(headerFormat = '<b>{point.key} </b><br>',
               pointFormat = "{series.name}: <b>{point.y:.1f}%</b>") %>% 
    hc_add_theme(my_theme)
}

## poverty ---------------------------------------------------------------------

make_povertyrate_ts_chart <- function(df) {
  
  ids <- unique(df$Area_name)
  
  df %>% 
    hchart("line", hcaes(x = Year, y = Data*100, group = Area_name),
           id = ids) %>% 
    hc_colors(colors = unname(spcols[1:length(ids)])) %>% 
    hc_xAxis(title = NULL) %>% 
    hc_yAxis(title = "",
             labels = list(format = '{value}%'),
             accessibility = list(description = "Rate")) %>% 
    hc_add_theme(my_theme) %>%
    hc_tooltip(headerFormat = '<b>{point.key}</b></br>',
               pointFormat = '{series.name}: <b>{point.y:.1f}%</b>') 
}

make_povertyrate_barchart <- function(df) {
  
  df %>% 
    arrange(desc(Data)) %>% 
    mutate(mycols = ifelse(Area_name == "Scotland", "#007DBA", spcols[1])) %>% 
    hchart("bar", hcaes(x = Area_name, y = Data*100, color = mycols), 
           name = df$Year[1], opacity = 0.8) %>% 
    hc_chart(inverted = TRUE) %>% 
    hc_xAxis(title = NULL) %>% 
    hc_yAxis(title = "",
             labels = list(format = '{value}%'),
             accessibility = list(description = "Rate")) %>% 
    hc_tooltip(headerFormat = '<b> {point.key} </b><br>',
               pointFormat = '{series.name}: <b>{point.y:.1f}%</b>') %>%
    hc_add_theme(my_theme)
  
}

make_povertynumber_barchart <- function(df) {
  
  df %>% 
    arrange(desc(Data)) %>% 
    mutate(mycols = ifelse(Area_name == "Scotland", "#007DBA", spcols[1])) %>% 
    hchart("bar", hcaes(x = Area_name, y = Data, color = mycols), 
           name = df$Year[1], opacity = 0.8) %>% 
    hc_xAxis(title = NULL) %>% 
    hc_yAxis(title = "", 
             labels = list(format = '{value: ,f}')) %>% 
    hc_tooltip(headerFormat = '<b> {point.key} </b><br>',
               pointFormat = '{series.name}: <b>{point.y} children</b>') %>%
    hc_add_theme(my_theme)
}

make_povertyrate_age_chart <- function(df) {
  
  ids <- unique(df$Age)
  
  highchart() %>%
    hc_add_series(df, "scatter", hcaes(x = Area_name, y = Data*100, group = Age),
                  id = ids, zIndex = 2) %>% 
    hc_add_series(df %>% mutate(Data = ifelse(Area_name == "Scotland" & Age == max(Age), Data * 1.1, NA)), 
                  "column", hcaes(x = Area_name, y = Data*100), 
                  color = spcols[3], enableMouseTracking = FALSE,
                  zIndex = 1, opacity = 0.5, showInLegend = FALSE) %>% 
    hc_chart(inverted = TRUE) %>% 
    hc_colors(colors = unname(spcols[1:length(ids)])) %>% 
    hc_xAxis(title = NULL, type = "category") %>% 
    hc_yAxis(title = "",
             labels = list(format = '{value}%'),
             accessibility = list(description = "Rate")) %>% 
    hc_add_theme(my_theme) %>%
    hc_tooltip(headerFormat = '<b> {point.key} </b><br>',
               pointFormat = '{series.name} year-olds: <b>{point.y:.1f}%</b>') 
}

add_recessionbar <- function(hc) {
  
  hc %>%
    hc_xAxis(
      plotBands = list(
        
        # COVID recession Q1-Q2 2020 (2019/20 - 2020/21)
        list(
          label = list(
            text = "COVID-19",
            style = list(fontSize = "small")),
          from = 5.6,
          to = 6.4,
          color = "#F2F2F2")
      ))
}

# deprivation ------------------------------------------------------------------

make_localshare_map <- function(sf, df){
  
  region_map <- jsonlite::fromJSON(geojsonsf::sf_geojson(sf), simplifyVector = FALSE)
  
  highchart(type = "map") %>%
    hc_add_series_map(map = region_map, 
                      name = "SIMD 2020 local share", 
                      df = df, 
                      value = "Data", 
                      joinBy = "Constituency", 
                      tooltip = list(
                        enabled = TRUE,
                        pointFormat = "{point.Constituency}: {point.value:.1f}%")
    ) %>%
    hc_colorAxis(
      min = 0,
      max = 100,
      maxColor = unname(spcols["purple"]),
      labels = list(format = "{text}%",
                    step = 1)) %>% 
    hc_add_theme(my_theme)
}

make_decile_map <- function(sf, df) {
  
  local_map <- jsonlite::fromJSON(geojsonsf::sf_geojson(sf), simplifyVector = FALSE)
  
  highchart() %>%
    hc_add_series_map(map = local_map, 
                      name = "SIMD 2020", 
                      df = df, 
                      value = "decile", 
                      joinBy = "DataZone", 
                      allAreas = FALSE,
                      tooltip = list(
                        enabled = TRUE,
                        pointFormat = "{point.DataZoneName}: Decile {point.value} (Rank {point.rank})")
    ) %>%
    hc_colorAxis(min = 1, 
                 max = 10,
                 startOnTick = FALSE,
                 tickInterval = 1,
                 stops = list(c(0, unname(spcols["purple"])),
                              c(0.499, "#ede6f1"),
                              c(0.501, "#eef2e9"),
                              c(1, unname(spcols["green"]))),
                 labels = list(step = 1)
    ) %>%
    hc_mapNavigation(enabled = TRUE) %>% 
    hc_add_theme(my_theme)
}



