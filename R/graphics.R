plot_derivs <- function( tic ){
  compose <- composed_df( tic )
  fig <- plot_ly(data = compose, x = ~scan, y = ~tic, type = 'scatter', mode = 'lines')
  fig <- fig %>% plotly::add_trace( y = ~first, mode = 'lines')
  fig <- fig %>% plotly::add_trace( y = ~second, mode = 'lines')
  return (fig)
}

#' plot_peaks
#'
#' function to run and visualize peakfind
#' @export
plot_peaks <- function( tic , min_diff ){

  peaks <- stepper( tic , min_diff )
  composed <- composed_df( tic )
  fig <- plotly::plot_ly(data = composed, x = ~scan, y = ~tic, type = 'scatter', mode = 'lines')
  fig <- fig %>% plotly::add_trace( y = ~first, mode = 'lines')
  fig <- fig %>% plotly::add_trace( y = ~second, mode = 'lines')
  fig <- fig %>% plotly::add_trace( x = match(peaks, names(tic)), y = composed[['tic']][match(peaks, names(tic))], type = 'scatter', mode = 'markers' )

  return ( fig )

}
