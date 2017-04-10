#' Ploting Dygraph in R
#'
#' Function to render a Dygraph Widget that plot time series data over customisable axes.
#'
#'@param tsData It will contain either the xts object or a numeric data frame.
#'@param headerText It will show the Header Text Explaining what the Dygraph Plot is about.
#'@param enableDyrange If \code{TRUE}, It will display the DyRange Selector Under the Dygraph.
#'@param enableDyhighlight if \code{TRUE}, It will Display the series as highlighted whenever hovered over.
#'
#' @export
  controlChartDygraph <- function(tsData, headerText, enableDyrange = FALSE, enableDyhighlight = FALSE)  {

    dg <- dygraph(tsData, main = headerText, ylab = "Timings", width = '100%') %>%
                 dyLegend(width = 200)
      if(enableDyrange)    {
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
#'@param multilinePlotingdata It will contain the whole dataframe passed by the user to the function.
#'
#'@export
  multilineChart <- function( multilinePlotingdata) {
    set.seed(1492)

     # mpd <- plotmultilinechart(multilinePlotingdata)

      addlines <- ncol(multilinePlotingdata) - 2
       for( i in 1:addlines) {
          coltoadd <- as.name(colnames(multilinePlotingdata)[i+2])
          print(coltoadd)
         mpd <- plotmultilinechart(multilinePlotingdata) %>% mjs_add_line(coltoadd)
        }
      mpd %>% mjs_axis_x(xax_format="date") %>%
      mjs_add_legend(legend=c("A", "B", "C"))

  }
  plotmultilinechart <- function(multilineDataframe) {
    multilineDataframe %>%
      mjs_plot(x= colnames(multilineDataframe)[1], y= colnames(multilineDataframe)[2]) %>%
      mjs_line()
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
                             sandsignika = hc_theme_sandsignika()
                             )
             hc <- hc %>% hc_add_theme(theme)
             hc <- hc %>% hc_credits(enabled = TRUE, text = "Highcharter")

   }
