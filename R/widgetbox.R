#' Ploting Dygraph in R
#'
#' Function to render a Dygraph Widget that plot time series data over customisable axes.
#'
#'@param tsData It will contain either the xts object or a numeric data frame.
#'@param headerText It will show the Header Text Explaining what the Dygraph Plot is about.
#'@param enableDyrange If \code{TRUE}, It will display the DyRange Selector Under the Dygraph.
#'@param enableDyhighlight if \code{TRUE}, It will Display the series as highlighted whenever hovered over.
#'@param legendalignment This will control the Text Alignment for showing Dygraph Plot values over dygraph chart.
#'@param yaxisHeader It will show the Text for the Y-axis Plot.
#'
#' @export
controlChartDygraph <- function(tsData, headerText, enableDyrange = FALSE, enableDyhighlight = FALSE, legendalignment = "centre", yaxisHeader = NULL)  {

  legendwidth <- switch(legendalignment,
                        centre = 40,
                        left = 600,
                        right = 200)
  dg <- dygraph(tsData, main = headerText, ylab = yaxisHeader, width = '100%') %>%
    dyLegend(width = legendwidth)
  if(enableDyrange)   {
    dg <- dg %>% dyRangeSelector(height = 40)
  }
  else if(enableDyhighlight)  {
    dg <- dg %>%  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))
  }

}

#' Plotting Multi-line charts in R
#'
#' Functions to render a plot of Multi-line chart of Metrics Graphics Library.
#'
#'@param multilinedata It will contain the whole dataframe passed by the user to the function.
#'
#'@export
multilineChart <- function(multilinedata) {
  df <- NULL
  colnams <- colnames(multilinedata)
  colnams <- colnams[!(colnams %in% colnams [[1]]) ]
  len <- length(multilinedata)
  for(i in 1:len) {
    if(is.null(df)) {
      df <- multilinedata[i]
      df[paste("A",i,sep = "")] <- multilinedata[i]
    }
    else {

      df[paste("A",i, sep = "")] <-  multilinedata[i]
    }
  }
  switch(len,
         "1" = stop("Data frame has one date type and atleast one numeric type columns"),
         "2" = (df %>% mjs_plot(x = A1, y = A2) %>% mjs_line() %>%  mjs_axis_x(xax_format="date") %>% mjs_add_legend(legend=c("A"))),
         "3" = (df %>% mjs_plot(x = A1, y = A2) %>% mjs_line() %>% mjs_add_line(A3) %>% mjs_axis_x(xax_format="date") %>% mjs_add_legend(legend=colnams)),
         "4" = (df %>% mjs_plot(x = A1, y = A2) %>% mjs_line() %>% mjs_add_line(A3) %>% mjs_add_line(A4) %>%  mjs_axis_x(xax_format="date") %>% mjs_add_legend(legend=colnams)),
         "5" = (df %>% mjs_plot(x = A1, y = A2) %>% mjs_line() %>% mjs_add_line(A3) %>% mjs_add_line(A4) %>% mjs_add_line(A5) %>%  mjs_axis_x(xax_format="date") %>% mjs_add_legend(legend=colnams)),
         "6" = df %>% mjs_plot(x = A1, y = A2) %>% mjs_line() %>% mjs_add_line(A3) %>% mjs_add_line(A4) %>% mjs_add_line(A5) %>% mjs_add_line(A6) %>%  mjs_axis_x(xax_format="date") %>% mjs_add_legend(legend= colnams),
         "7" = df %>% mjs_plot(x = A1, y = A2) %>% mjs_line() %>% mjs_add_line(A3) %>% mjs_add_line(A4) %>% mjs_add_line(A5) %>% mjs_add_line(A6) %>% mjs_add_line(A7) %>%  mjs_axis_x(xax_format="date") %>% mjs_add_legend(legend=colnams),
         "8" = df %>% mjs_plot(x = A1, y = A2) %>% mjs_line() %>% mjs_add_line(A3) %>% mjs_add_line(A4) %>% mjs_add_line(A5) %>% mjs_add_line(A6) %>% mjs_add_line(A7) %>% mjs_add_line(A8) %>%   mjs_axis_x(xax_format="date") %>% mjs_add_legend(legend=colnams),
         "9" = df %>% mjs_plot(x = A1, y = A2) %>% mjs_line() %>% mjs_add_line(A3) %>% mjs_add_line(A4) %>% mjs_add_line(A5) %>% mjs_add_line(A6) %>% mjs_add_line(A7) %>% mjs_add_line(A8) %>% mjs_add_line(A9) %>%  mjs_axis_x(xax_format="date") %>% mjs_add_legend(legend=colnams),
         "10" = df %>% mjs_plot(x = A1, y = A2) %>% mjs_line() %>% mjs_add_line(A3) %>% mjs_add_line(A4) %>% mjs_add_line(A5) %>% mjs_add_line(A6) %>% mjs_add_line(A7) %>% mjs_add_line(A8) %>% mjs_add_line(A9) %>% mjs_add_line(A10) %>%  mjs_axis_x(xax_format="date") %>% mjs_add_legend(legend=colnams)
  )
}

#' Plotting Highcharter Control Charts in R
#'
#' Functions to render a Plot of Highcharter's Library
#'
#'@param title Title for the Highcharts.
#'@param theme Highchart theme to be used in the Highchart Plot.
#'@param seriesData Series to be plotted into the HighChart.
#'@param seriesCategories The X-axis categories to plot on Highchart.
#' @export
highChart <- function(title, theme = "sandsignika", seriesData = NULL, seriesCategories = NULL) {
  hc <- highchart() %>% hc_title(text = title) %>%
    hc_xAxis(categories = seriesCategories) %>%
    hc_yAxis(title = list(text = "Temperature"),
             labels = list(format = "{value}? C"))

  for(i in 2:ncol(seriesData))   {
    hc <- hc %>% hc_add_series(name = colnames(seriesData)[i], data = seriesData[,i], type = "line")
  }
  theme <- switch(theme,
                  economist = hc_theme_economist(),
                  dotabuff = hc_theme_db(),
                  darkunica = hc_theme_darkunica(),
                  gridlight = hc_theme_gridlight(),
                  sandsignika = hc_theme_sandsignika(),
                  sparkline = hc_theme_sparkline()

  )
  hc <- hc %>% hc_add_theme(theme)
  hc <- hc %>% hc_credits(enabled = TRUE, text = "Highcharter")

}

