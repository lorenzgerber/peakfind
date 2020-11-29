first_derivative <- function(tic){
  first_derivative <- diff( tic )
  return ( first_derivative )
}

second_derivative <- function(tic){
  second_derivative <- diff(diff(tic))
  return ( second_derivative )
}

composed_df <- function( chromatogram, sgolay_order = NULL, sgolay_length = 3 ){
  tic <- chromatogram::calculate_tic( chromatogram, sgolay_order, sgolay_length )
  first_deriv <- first_derivative( tic )
  second_deriv <- second_derivative( tic )
  stop_index <- dim(chromatogram)[1]-2
  composed <- data.frame('tic' = tic[1:stop_index], 'first' = first_deriv[1:stop_index], 'second' = second_deriv, 'scan' = 1:stop_index)
  return (composed )
}

#' stepper
#'
#' currently the actual peak finder function
#' @export
stepper <- function( chromatogram, min_diff, sgolay_order = NULL, sgolay_length = 3){
  max_diff_index <- NULL
  max_diff_value <-  NULL
  peak_vector <- NULL

  composed <- composed_df(chromatogram, sgolay_order = NULL, sgolay_length = 3)

  for(i in 1:length(composed[['scan']])){
    if(composed[['first']][i] > min_diff){
      max_diff_index <- i
    }

    if(composed[['first']][i] <= 0 && !is.null(max_diff_index) && composed[['second']][i] < 0 ){
      peak_vector <- c(peak_vector, i)
      max_diff_index <- NULL
    }
  }

  peak_vector <- as.numeric(rownames(chromatogram)[peak_vector])

  return ( peak_vector)


}
