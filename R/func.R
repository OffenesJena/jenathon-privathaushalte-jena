reorder_by <- function(x, y) {
  # Order factor levels by values of another coloumn.
  # To change plot order of bars,levels in underlying factor have to be changed.
  # Function can be used within ggplot2::aes() function for x or y args to
  # reorder the values on one of the axes.
  
  # Copied from lib/R/plot.R
  
  # Args:
  #   x: A vector of data. Factor levels of that vector are reorderd or orderd.
  #   y: A vector of data. Factor levels of x are ordered by this vector.
  
  # Returns:
  #   A object of class factor
  
  
  factor(x, levels = unique(x[order(y)]))
  
}
