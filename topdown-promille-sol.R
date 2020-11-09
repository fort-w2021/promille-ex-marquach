
# Function for tell_me_how_drunk 

tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,drinking_time,  drinks) {
  
  # Pre-Stuff
  # proofness of drinks
  proofness <- c("hoibe" = 0.06, "massn" = 0.06,  "schnaps" = 0.4, "wein" = 0.11)
  # mililiters of different alks
  mililiters <- c("hoibe" = 0.5, "massn" = 1, "schnaps" = 0.04, "wein" = 0.2)*1000
  # p Density for alk
  density_alk <- 0.8
  
 #################### Tell me how drunks starts #################
  
  # Calcualte Amount of Alk and give warning for inappropriate Age given consumption
  Calculate_A <- function(age, drinks)
  {
    names_consumed_drinks_by <- names(unlist(drinks))
    number_different_drinks <- sum(by(unlist(drinks), INDICES = names_consumed_drinks_by, sum))
    
    if (age < 16 & number_different_drinks > 0 | ((age >= 16 & age <= 18) &
                                                 ("schnaps" %in% names_consumed_drinks_by)))
      warning("illegal")
    
    consumed_drinks <- by(unlist(drinks), INDICES = names_consumed_drinks_by, sum)
    
    each_A <- consumed_drinks * proofness[names(consumed_drinks)] * mililiters[names(consumed_drinks)]*density_alk
    A <- sum(each_A)
    return(A)
  }
  A <- Calculate_A(age,drinks)
  
  # Calculate Body water after Whatson
  calculated_GKW <- function(sex, age, height, weight)
    {
    if ( sex == "male" | startsWith(toupper(sex),"M")) {
      GKW <- 2.447 - 0.09516*age + 0.1074*height + 0.3362*weight
    } else if ( sex == "female" | startsWith(toupper(sex),"F")) {
      GKW <- 0.203 - 0.07*age +  0.1069*height + 0.2466*weight
    }
    return(GKW)
    }
  GKW <- calculated_GKW(sex, age, height, weight)
  
  
  # Calculate disributionfactor r after watson
  rho_blood <- 1.055 # g/cubic cm
  calculated_r <- function(GKW,weight)
    {
    r <- (rho_blood * GKW)/(0.8 * weight)
    return(r)
  }
  r <- calculated_r(GKW,weight)
  

  calculate_c <- function(A,r, weight,drinking_time)
    {
    #fist jsut calc c without drinking_time knowledge
    c <- (0.8*A)/(rho_blood*GKW)
    
    #Then use knowledge about period of drinks 
    time_diff <- as.numeric(difftime(time1 = drinking_time[2], time2 = drinking_time[1], units = "hours" ))
    
    # Set up if conditions. 
    # Alcohol removement starts if difference of start and end time is bigger then 1. 
    # If difference less then 1 no removement happend.
    # If time for alcohol removement is long enough one is sober.
    if (  ((c/0.15) > time_diff) & (time_diff > 1) ) c_final <- (c - ((time_diff - 1) * 0.15)) else 
      if ( time_diff < 1) c_final <- c else c_final <- 0
  }
  
  c_final <- calculate_c(A,r, weight,drinking_time)
    return(c_final)
}
  