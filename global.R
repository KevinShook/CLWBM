

library(lubridate)

primrose_rating_table <- function() {
  stage <- c(554.52,557.52,562.52,567.52,572.52,577.52,582.52,585.52,587.52,589.52,591.52,593.52,594.52,596.52,597.52,600.52)
  area <- c(3570000,7430000,8900000,12460000,16210000,21680000,24340000,28820000,39840000,60180000,107000000,190000000,330570000,434570000,521000000,555250000)
  volume <- c(0,16500000,57325000,110725000,182400000,277125000,392175000,471915000,540575000,640595000,891365000,1188365000,1448650000,1831220000,2309005000,3923380000)
  
  df <- data.frame(stage, area, volume)
  return(df)
}


cold_rating_table <- function() {
  stage <- c(420.211, 436.611, 439.378, 443.728, 446.892, 448.474, 450.056, 450.847, 451.638, 452.231, 454.011, 455.711, 458.954, 460.141, 462.118, 464.887, 469.831, 471.214, 475.564, 477.146, 480.311, 484.265, 487.231, 491.581, 494.351, 496.525, 498.898, 502.457, 506.016, 509.774, 511.949, 514.915, 516.694, 519.661, 523.615, 528.559, 532.316, 534.293, 535.875, 536.38, 536.88, 537.38, 538.88, 539.88, 540.38, 540.88, 541.88)
  area <- c(0, 1177000, 5103000, 10598000, 17271000, 21982000, 28655000, 34543000, 45142000, 50637000, 62414000, 73405000, 85574000, 87144000, 93817000, 104808000, 124043000, 133856000, 148733000, 158979000, 173112000, 194308000, 208439000, 225318000, 228066000, 241413000, 250441000, 259471000, 271638000, 281059000, 286947000, 294013000, 299901000, 307752000, 319921000, 337978000, 345829000, 360353000, 376447000, 376451000, 376451000, 377290000, 384960000, 390220000, 392900000, 395600000, 401090000)
  volume <- c(0, 9651400, 18340000, 52489000, 96578000, 127630000, 167680000, 192680000, 224190000, 252590000, 353210000, 468650000, 726440000, 828940000, 1007800000, 1282800000, 1848500000, 2026900000, 2641500000, 2884900000, 3410400000, 4136800000, 4734100000, 5677500000, 6305500000, 6815800000, 7399400000, 8306800000, 9251900000, 10290000000, 10908000000, 11770000000, 12298000000, 13199000000, 14440000000, 16067000000, 17351000000, 18049000000, 18632000000, 18821000000, 19008000000, 19197000000, 19769000000, 20156000000, 20352000000, 20549000000, 20947000000)
  
  df <- data.frame(stage, area, volume)
  return(df)
}


cl_outflow_weir <- function(stage, weir, month){
  
  h <- stage - (534.501 + weir)
  
  if (h <= 0) {
    q <- 0
  } else {
    if (month == 12 | month < 3)
      q <- 89.44636817625401 * (h ^ 2) - (0.2889716215213034  * h) + 1.6332980800121997
    else if (month >= 3 & month <= 5 )
      q <- 5.153308189443157 * (h ^ 2) + (36.05000645698398 * h) - 1.6076486028540509
    else if (month >= 6 & month <= 9)  
      q <- 28.250184797822136 * (h ^ 2) + (43.68572544273417 * h) - 6.64653644254948
    else  
      q <- 73.20713384878391  * (h ^ 2) + ( 6.025127584231851  * h) + 1.9428588445596815
  }
  
  q <- pmax(q, 0)
  return(q)
}

weir <- function(elevation, sill, width, constant, exponent){
  if (elevation <= sill)
    discharge <- 0
  else
    discharge <- constant * width * (elevation - sill) ^ exponent
  
  return(discharge)
}

stage2area <- function(rating_curve, stage, minstage, method = "linear"){
  if (stage <= minstage)
    return(0)
  
  if (method == "linear")
    area <- approx(rating_curve$stage, rating_curve$area, xout = stage)
  else 
    area <- spline(rating_curve$stage, rating_curve$area, xout = stage, method = "fmm")
  
  return(area$y)
}

vol2stage <- function(rating_curve, volume, minstage, method = "linear"){
  if (volume <= 0)
    return(minstage)
  
  if (method == "linear")
    stage <- approx(rating_curve$volume, rating_curve$stage, xout = volume)
  else 
    stage <- spline(rating_curve$volume, rating_curve$stage, xout = volume, method = "fmm")
  
  return(stage$y)
}

stage2vol <- function(rating_curve, stage, minstage, method = "linear"){
  if (stage <= minstage)
    return(0)
  if (method == "linear")
    volume <- approx(rating_curve$stage, rating_curve$volume, xout = stage)
  else 
    volume <- spline(rating_curve$stage, rating_curve$volume, xout = stage, method = "fmm")
  return(volume$y)  
}


muskingum <- function(flow, K, x, delt){
  C0 <- (-K * x + 0.5 * delt) / (K * (1 - x) + 0.5 * delt)
  C1 <- (K * x + 0.5 * delt) / (K * (1 - x) + 0.5 * delt)
  C2 <- (K * (1 - x) - 0.5 * delt) / (K * (1 - x) + 0.5 * delt)
  
  C0I2 <- 0
  C1I1 <- 0
  C2O1 <- 0
  O <- flow[1]
  
  tsteps <- length(flow)
  for (i in 2:tsteps) {
    C0I2 <- c(C0I2, (-K * x + 0.5 * delt) / (K * (1 - x) + 0.5  * delt ) * flow[i])
    C1I1 <- c(C1I1, (K * x + 0.5 * delt) / (K * (1 - x)  + 0.5  * delt) * flow[i - 1])
    C2O1 <- c(C2O1, (K * (1 - x) - 0.5 * delt) / (K * (1 - x) + 0.5 * delt) * O[i - 1])
    O <- c(O, C0I2[i] + C1I1[i] + C2O1[i])
  }
  return(pmax(O, 0))
}

test_muskingum <- function(K, x, delt){
  if (2 * K * x <= delt & delt <= 2 * K * (1 - x)) {
    return("Muskingum routing parameters OK")
  } else {
    return("Muskingum routing parameters not OK")
  }
    
}


daily_withdrawals <- function(monthly_withdrawals, start_date, end_date){
  monthly_withdrawals$date <- as.Date(monthly_withdrawals$date)
  monthly_withdrawals$year <- as.numeric(format(monthly_withdrawals$date, format = "%Y"))
  monthly_withdrawals$month <- as.numeric(format(monthly_withdrawals$date, format = "%m"))
  monthly_withdrawals$days <- days_in_month(monthly_withdrawals$date)
  monthly_withdrawals$daily <- monthly_withdrawals$withdrawal / monthly_withdrawals$days
  
  modeldate <- seq.Date(from = start_date, to = end_date, by = 1)
  month <- as.numeric(format(modeldate, format = "%m"))
  year <-  as.numeric(format(modeldate, format = "%Y"))
  daily <- c(0)
  for (j in 1:length(modeldate)){
      daily[j] <- monthly_withdrawals$daily[monthly_withdrawals$month == month[j] 
                                            & monthly_withdrawals$year == year[j]]
  }
  df <- data.frame(modeldate, daily)
  names(df) <- c("date", "daily") 
  return(df)
}
  

run_primrose <- function(start_date, end_date, parameters, primrose_fluxes, runname, rundir){
  current_elevation <- c(0)
  area <- c(0)
  volume <- c(0)
  inflow_vol <- c(0)
  precip_vol <- c(0)
  evap_vol <- c(0)
  outflow_Q <- c(0)
  outflow_vol <- c(0)
  updated_vol <- c(0)
  groundwater_vol <- c(0)
  primrose_rating_curves <- primrose_rating_table()
  min_stage <- min(primrose_rating_curves$stage)
  # set fluxes to specified dates
  primrose_fluxes$date <- as.Date(primrose_fluxes$date)
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  primrose_fluxes <- primrose_fluxes[primrose_fluxes$date >= start_date &
                                       primrose_fluxes$date <= end_date, ]
  
  
  # set specified parameters
  primrose_starting_el <- parameters$Value[1]
  primrose_weir_width <- parameters$Value[3]
  primrose_weir_coefficient <- parameters$Value[4]
  primrose_weir_sill <- parameters$Value[5]
  primrose_evap_mult <- parameters$Value[6]
  primrose_groundwater_q <- parameters$Value[8]
  primrose_inflow_K <- parameters$Value[11]
  primrose_inflow_x <- parameters$Value[12]

  primrose_fluxes$q <- pmax(primrose_fluxes$q, 0)
  
  if (primrose_inflow_K <= 0 | primrose_inflow_x < 0) {
    primrose_fluxes$routed_q <- primrose_fluxes$q
   }
   else{
     # check routing parameters
     routing_check <- test_muskingum(primrose_inflow_K, primrose_inflow_x, 1)
     if (routing_check != "Muskingum routing parameters OK")
       stop(routing_check)
     primrose_fluxes$routed_q <- muskingum(primrose_fluxes$q, primrose_inflow_K, primrose_inflow_x, 1 )
   }

  

  
  # now calculate lake inflows and outflows for each date
  
  primrose_fluxes$inflow_vol <- primrose_fluxes$routed_q * 24 * 3600    # m3/s -> m3
  primrose_weir_exponent <- 1.5
  
  
  # loop through each day calculating inflow and outflow volumes
  num_rows <- nrow(primrose_fluxes)
  elevation <- primrose_starting_el 
  for (j in 1:num_rows) {
    current_elevation[j] <- elevation
    current_area <- stage2area(primrose_rating_curves, elevation, min_stage, method = "linear")
    current_volume <- stage2vol(primrose_rating_curves, elevation, min_stage, method = "linear")
    precip_vol[j] <- (primrose_fluxes$p[j] / 1000) * current_area
    groundwater_vol[j] <- primrose_groundwater_q * 24 * 3600
    evap_vol[j] <- ((primrose_fluxes$evap[j] * primrose_evap_mult) / 1000) * current_area
    outflow_Q[j] <- weir(elevation, primrose_weir_sill, primrose_weir_width, primrose_weir_coefficient, primrose_weir_exponent)
    outflow_vol[j] <- outflow_Q[j] * 24 * 3600           # m3/s ->  m3
    inflow_vol[j] <- primrose_fluxes$inflow_vol[j]
    updated_vol[j] <- current_volume + precip_vol[j] + inflow_vol[j] + groundwater_vol[j] - evap_vol[j] - outflow_vol[j]
    elevation <- vol2stage(primrose_rating_curves, updated_vol[j], min_stage, method = "linear")
  }
  
  df <- data.frame(primrose_fluxes$date, current_elevation, primrose_fluxes$q, primrose_fluxes$routed_q, inflow_vol, precip_vol, evap_vol, groundwater_vol, outflow_Q)
  names(df) <- c("date", "elevation", "loal_inflow_q","routed_local_inflow_q", "local_inflow_vol", "precip+snowmelt_vol", "evap_vol", "groundwater_vol", "outflow_q")
  
  outfile <- paste0(rundir, "/", runname, "_Primrose_Lake.csv")
  write.csv(df, file = outfile, row.names = FALSE)
  return(df)
  
}  

run_interlake <- function(start_date, end_date, parameters, interlake_fluxes, runname, rundir){
  
# set constants
  muskeg_basin_area <- 1562                #km^2
  martineau_below_primrose_area <- 46.2    #km^2
  bridge_basin_area <- 167                 #km^2
  martineau_below_confluence_area <- 123        #km^2  
  total_basin_area <- muskeg_basin_area + martineau_below_primrose_area + 
    bridge_basin_area + martineau_below_confluence_area
  
  muskeg_frac <- muskeg_basin_area / total_basin_area
  martineau_below_primrose_frac <- martineau_below_primrose_area / total_basin_area
  bridge_basin_frac <- bridge_basin_area / total_basin_area
  martineau_below_confluence_frac <- martineau_below_confluence_area / total_basin_area
  
# get routing parameters
  muskeg_K <- parameters$Value[13]
  muskeg_x <- parameters$Value[14]
  martineau_K <- parameters$Value[15]
  martineau_x <- parameters$Value[16]
  

# read in primrose outputs
 
 primrose_file <- paste0(rundir, "/", runname, "_Primrose_Lake.csv")
 primrose_fluxes <- read.csv(file = primrose_file, header = TRUE)
 primrose_fluxes$date <- as.Date(primrose_fluxes$date)
 primrose <- primrose_fluxes[primrose_fluxes$date >= start_date &
                                      primrose_fluxes$date <= end_date, c("date", "outflow_q")]


 interlake_fluxes <- interlake_fluxes[interlake_fluxes$date >= start_date &
                                       interlake_fluxes$date <= end_date, ]
 
 # get local flows by ratios
 interlake_fluxes$primrose_outflow_q <- primrose_fluxes$outflow_q
 interlake_fluxes$muskeg_local_q <- interlake_fluxes$q * muskeg_frac
 interlake_fluxes$martineau_below_primrose_local_q <- interlake_fluxes$q * 
   martineau_below_primrose_frac
 interlake_fluxes$bridge_basin_local_q <- interlake_fluxes$q * bridge_basin_frac
 interlake_fluxes$martineau_below_confluence_local_q <- interlake_fluxes$q * 
   martineau_below_confluence_frac 
 
 # route Muskeg flows
 
 if (muskeg_K <= 0 | muskeg_x < 0) {
   interlake_fluxes$routed_muskeg_q <- interlake_fluxes$muskeg_local_q
 }
 else{
   # check routing parameters
   routing_check <- test_muskingum(muskeg_K, muskeg_x, 1)
   if (routing_check != "Muskingum routing parameters OK")
     stop(routing_check)
   interlake_fluxes$routed_muskeg_q <- muskingum(interlake_fluxes$muskeg_local_q, muskeg_K, muskeg_x, 1 )
 }
 
 # sum flows downstream 
 interlake_fluxes$confluence_q <- interlake_fluxes$primrose_outflow_q + interlake_fluxes$routed_muskeg_q + interlake_fluxes$martineau_below_primrose_local_q
 interlake_fluxes$bridge_q <- interlake_fluxes$confluence_q + interlake_fluxes$bridge_basin_local_q
 
 # route flows to bridge
 if (martineau_K <= 0 | martineau_x < 0) {
   interlake_fluxes$routed_bridge_q <- interlake_fluxes$bridge_q
 }
 else{
   # check routing parameters
   routing_check <- test_muskingum(martineau_K, martineau_x, 1)
   if (routing_check != "Muskingum routing parameters OK")
     stop(routing_check)
   interlake_fluxes$routed_bridge_q <- muskingum(interlake_fluxes$bridge_q, martineau_K, martineau_x, 1 )
 }
 
# add local flows to gauge
 interlake_fluxes$gauge_06AF008_q <- interlake_fluxes$routed_bridge_q + interlake_fluxes$martineau_below_confluence_local_q
 interlake_fluxes <- interlake_fluxes[, -2]

 outfile <- paste0(rundir, "/", runname, "_Interlake.csv")
 write.csv(interlake_fluxes, file = outfile, row.names = FALSE)
 return(interlake_fluxes)
 
}

run_coldlake <- function(start_date, end_date, parameters, coldlakefluxes, monthlywithdrawals, runname, rundir){
  current_elevation <- c(0)
  area <- c(0)
  volume <- c(0)
  inflow_vol <- c(0)
  precip_vol <- c(0)
  evap_vol <- c(0)
  interlake_vol <- c(0)
  outflow_Q <- c(0)
  outflow_vol <- c(0)
  updated_vol <- c(0)
  groundwater_vol <- c(0)

  daily_withdrawal <- daily_withdrawals(monthlywithdrawals, start_date, end_date)
  
  # get parameters
  cold_starting_el <- parameters$Value[2]
  cold_weir_ht <- parameters$Value[10]
  cold_evap_mult <- parameters$Value[7]
  cold_groundwater_q <- parameters$Value[9]
  cold_inflow_K <- parameters$Value[17]
  cold_inflow_x <- parameters$Value[18]
  
  # read in Interlake output
  
  interlake_file <- paste0(rundir, "/", runname, "_Interlake.csv")
  interlake_fluxes <- read.csv(file = interlake_file, header = TRUE)
  interlake_fluxes$date <- as.Date(interlake_fluxes$date)
  interlake_fluxes <- interlake_fluxes[interlake_fluxes$date >= start_date &
                                         interlake_fluxes$date <= end_date,]
  interlake_vol <- interlake_fluxes$gauge_06AF008_q * 24 * 3600    # m3/s -> m3/d
  
  cold_rating_curves <- cold_rating_table()
  cold_min_stage <- min(cold_rating_curves$stage)
  cold_max_vol <- max(cold_rating_curves$volume)
  cold_max_stage <- max(cold_rating_curves$stage)
  
  # set fluxes to specified dates
  coldlakefluxes$date <- as.Date(coldlakefluxes$date)

  
  
  coldlakefluxes <- coldlakefluxes[coldlakefluxes$date >= start_date &
                                     coldlakefluxes$date <= end_date, ]
  
  if (cold_inflow_K <= 0 | cold_inflow_x < 0) {
    coldlakefluxes$routed_q <- coldlakefluxes$q
  }
  else{
    # check routing cold_inflow_K
    routing_check <- test_muskingum(cold_inflow_K, cold_inflow_x, 1)
    if (routing_check != "Muskingum routing parameters OK")
      stop(routing_check)
    coldlakefluxes$routed_q <- muskingum(coldlakefluxes$q, cold_inflow_K, cold_inflow_x, 1 )
  }
  
  coldlakefluxes$routed_inflow_vol <- coldlakefluxes$routed_q * 24 * 3600    # m3/s -> m3/d
  
  # loop through each day calculating inflow and outflow volumes
  num_rows <- nrow(coldlakefluxes)
  elevation <- cold_starting_el
  
  cold_rating_table
  
  for (j in 1:num_rows) {
    date <- coldlakefluxes$date[j]
    year <- as.numeric(format(date, format = "%Y"))
    month <- as.numeric(format(date, format = "%m"))
    current_elevation[j] <- elevation
    
    current_area <- stage2area(cold_rating_curves, elevation, cold_min_stage, method = "linear")
    current_volume <- stage2vol(cold_rating_curves, elevation, cold_min_stage, method = "linear")
    
    groundwater_vol[j] <- cold_groundwater_q * 24 * 3600
    evap_vol[j] <- ((coldlakefluxes$evap[j] * cold_evap_mult) / 1000) * current_area
    
    precip_vol[j] <- (coldlakefluxes$p[j] / 1000) * current_area
    outflow_Q[j] <- cl_outflow_weir(current_elevation[j], cold_weir_ht, month)
    outflow_vol[j] <- outflow_Q[j] * 24 * 3600           # m3/s ->  m3
    updated_vol[j] <- current_volume + precip_vol[j] + coldlakefluxes$routed_inflow_vol[j] + 
      interlake_vol[j] - evap_vol[j] - outflow_vol[j] - daily_withdrawal$daily[j]
    
    # check for exceeding max volume
    if (updated_vol[j] > cold_max_vol) {
      excess_vol <- updated_vol[j] -  cold_max_vol
      outflow_vol[j] <- excess_vol                  # all excess is discharged
      outflow_Q[j] <- outflow_vol[j] / (24 * 3600)
      updated_vol[j] <- cold_max_vol
      elevation <- cold_max_stage
      current_elevation[j] <- cold_max_stage
    }  else {
      elevation <- vol2stage(cold_rating_curves, updated_vol[j], cold_min_stage, method = "linear")
    }
  }

  cold_output_df <- data.frame(coldlakefluxes$date, 
                   current_elevation, 
                   coldlakefluxes$q, 
                   coldlakefluxes$routed_inflow_vol,
                   interlake_vol, 
                   precip_vol, 
                   evap_vol, 
                   groundwater_vol,
                   daily_withdrawal$daily, 
                   outflow_Q)
  
  names(cold_output_df) <- c("date", 
                 "elevation", 
                 "local_inflow_q",
                 "routed_local_inflow_vol", 
                 "interlake_vol", 
                 "precip+snowmelt_vol", 
                 "evap_vol", 
                 "groundwater_vol",
                 "daily_withdrawal_vol", 
                 "outflow_q")
  
  cold_outfile <- paste0(rundir, "/", runname, "_Cold_Lake.csv")
  write.csv(cold_output_df, file = cold_outfile, row.names = FALSE)
  return(cold_output_df)
}