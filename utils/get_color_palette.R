# Function to define color palette based on the variable and reverse argument
get_color_palette <- function(variable, domain, reverse = FALSE) {
  if (variable %in% c("Tavg", "Tmax", "Tmin")) {
    # Use the RdYlBu palette for temperature variables
    scales::col_numeric(palette = "RdYlBu", domain = domain, reverse = reverse)
  } else {
    # Use the viridis palette for other variables like PREC
    scales::col_numeric(palette = "viridis", domain = domain, reverse = reverse)
  }
}
