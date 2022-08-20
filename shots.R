#-------------------------------------------------------------------------------
# Loading Data 
#-------------------------------------------------------------------------------
# fname = '//Users//seeker//Desktop//Thunder//shots_data.csv'
# shots = read.csv(fname, header = TRUE, sep = ",")

#-------------------------------------------------------------------------------
# Generating Functions for Data Transformation
#-------------------------------------------------------------------------------
# This function calculates the distance of each shot from the hoop center. 
get_hoop_dist <- function(x,y) {
  return(sqrt(x^2+y^2))
}

# This function converts x,y coordinates to shooting zones. 
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

# This function calcualtes effective field goal percentage.
calc_eFGP <- function(fgm, tpm, fga){
  return((fgm+0.5*tpm)/fga)
}

#-------------------------------------------------------------------------------
# Cleaning Dataset
#-------------------------------------------------------------------------------
# Add zones to data frame
for (i in 1:dim(shots)[1]){
  shots$fgzone[i] <- xy_to_zone(shots$x[i], shots$y[i])
}

# Add field goal attempts to data frame
shots$fga <- rep(1, dim(shots)[1])
shots <- shots[,-2:-3] # remove x,y coordinates

#-------------------------------------------------------------------------------
# Show Shot Distribution
#-------------------------------------------------------------------------------
# Calculate field goal percentage for each team and each field goal zone
shotsResults <- aggregate( cbind(fgmade,fga) ~ team+fgzone, data=shots, FUN=sum)
shotsResults$fgp <- shotsResults$fgmade/shotsResults$fga # add percentages
shotsResults

#-------------------------------------------------------------------------------
# Calculate Effective Field Goal Percentage
#-------------------------------------------------------------------------------
# Extract parameters for effective field goal percentage calculation
fga <- aggregate( fga ~ team, data=shotsResults, FUN=sum)
fgm <- aggregate( fgmade ~ team, data=shotsResults, FUN=sum)
tpm <- aggregate( fgmade ~ team, data=shotsResults, FUN=sum
                       , subset = fgzone!="2PT")

# Team_A effective field goal percentage 
eFGP_A <- calc_eFGP(fgm$fgmade[1], tpm$fgmade[1], fga$fga[1])
eFGP_A
# Team B effective field goal percentage
eFGP_B <- calc_eFGP(fgm$fgmade[2], tpm$fgmade[2], fga$fga[2])
eFGP_B
