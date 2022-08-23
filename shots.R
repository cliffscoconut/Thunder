# Author: Clifford Huang
# Purpose: OKC Thunder Data Science Question

#-------------------------------------------------------------------------------
# Functions for Data Transformation
#-------------------------------------------------------------------------------
# Calculates the distance of each shot from the hoop center. 
get_hoop_dist <- function(x,y) {
  return(sqrt(x^2+y^2))
}

# Converts x,y coordinates to shooting zones. 
xy_to_zone <- function(x,y) {
  hoop_dist <- get_hoop_dist(x,y)
  # The area outside the three point line just below the "break" of the arc
  # and above y=7.8 is incorrectly identified. 
  if (y>7.8 && hoop_dist>23.75) {
    return('NC3')
  } 
  else if (y <= 7.8 && x > 22) {
    return('C3')
  } else {
    return('2PT')
  }
}

# Calculates effective field goal percentage.
calc_eFGP <- function(fgm, tpm, fga){
  return((fgm+(0.5*tpm))/fga)
}

# Returns percent value from decimal value
percent <- function(x, digits = 3, format = "f", ...) {      
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

#-------------------------------------------------------------------------------
# Function to Calculate Shot Distribution and Effective Field Goal Percentage
#-------------------------------------------------------------------------------
get_SD_eFGP <- function(dataset){
  # Add zones to data frame
  for (i in 1:dim(dataset)[1]){
    dataset$fgzone[i] <- xy_to_zone(dataset$x[i], dataset$y[i])
  }
  
  # Add field goal attempts to data frame
  dataset$fga <- rep(1, dim(dataset)[1])
  dataset <- dataset[,-2:-3] # remove x,y coordinates
  
  # Aggregate shots by team and field goal zone
  shotsResults <- aggregate( cbind(fgmade,fga) ~ team+fgzone, data=dataset, FUN=sum)
  
  # Calculate field goal percentage for each team and each field goal zone
  shotsResults$fgp <- percent(shotsResults$fgmade/shotsResults$fga) # add percentages
  
  # Calculate the shot distribution from each zone
  for (i in 1:dim(shotsResults)[1]){
    teamshots <- sum(shotsResults$fga[shotsResults$team==shotsResults$team[i]])
    zoneshots <- shotsResults$fga[i]
    shotsResults$zonepct[i] <- zoneshots/teamshots
  }

  # Calculate effective field goal percentage for each team and each field goal zone
  for (i in 1:dim(shotsResults)[1]){
    fga <- shotsResults[i,'fga']
    fgm <- shotsResults[i,'fgmade']
    if (shotsResults[i, 'fgzone']=="2PT"){
      tpm <- 0
    } else {
      tpm <- fgm
    }
    shotsResults$efgp[i] <- calc_eFGP(fgm, tpm, fga)
  }
  
  print(shotsResults[,-3:-5])
  
}

#-------------------------------------------------------------------------------
# Load Data and Run Script
#-------------------------------------------------------------------------------
fname = file.choose()
shots = read.csv(fname, header = TRUE, sep = ",")
get_SD_eFGP(shots) 
      