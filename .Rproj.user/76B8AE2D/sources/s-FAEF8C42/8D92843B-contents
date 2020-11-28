plot_derivs <- function(chromatogram, sgolay_order = NULL, sgolay_length = 3){
  compose <- composed_df(chromatogram, sgolay_order, sgolay_length)
  fig <- plot_ly(data = compose, x = ~scan, y = ~tic, type = 'scatter', mode = 'lines')
  fig <- fig %>% add_trace( y = ~first, mode = 'lines')
  fig <- fig %>% add_trace( y = ~second, mode = 'lines')
  return (fig)
}

plot_peaks <- function(chromatogram, min_diff, sgolay_order = NULL, sgolay_length = 3){

  peaks <- stepper(chromatogram, min_diff, sgolay_order, sgolay_length)
  compose <- composed_df(chromatogram, sgolay_order, sgolay_length)
  fig <- plot_ly(data = compose, x = ~scan, y = ~tic, type = 'scatter', mode = 'lines')
  fig <- fig %>% add_trace( y = ~first, mode = 'lines')
  fig <- fig %>% add_trace( y = ~second, mode = 'lines')
  fig <- fig %>% add_trace( x = peaks, y = composed[['tic']][peaks], type = 'scatter', mode = 'markers' )

  return ( fig )

}
