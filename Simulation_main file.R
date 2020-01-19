  #set up
    library(dplyr)
    library(reshape2)
    library(beepr)
    { rm(list=ls())
    source("C:/Users/ASUS/Dropbox/Publication (AV)/Simulation/R file/mode.p synthesis.R")
    source("C:/Users/ASUS/Dropbox/Publication (AV)/Simulation/R file/loc.p synthesis.R")
    
  # Scenario
    scenario <-  list("bus.priority" = 0, "sav.avail" = 1 , "pav.avail"=0 ,
                      "bus.automation"=0, "induc.trip"=0,"mode.split.test"=0) #1 for yes, 0 for no
    scen <- "onlySAV.noaban"
    phase <- 2
    
    if(phase==1){
      scenario[["sav.avail"]] <- 0
      scenario[["pav.avail"]] <-0 }else{NULL}
    

# Initializing ------------------------------------------------------------

  # Parameters 
    
    el.walk <- list("speed_walk_city"=5,"speed_walk_sub"=4,"speed_walk_rur"=4)
    el.bus <- list("fare_pass"=1.38,
                    "acc_bus_city"=5,"acc_bus_sub"=10,"acc_bus_rur"=20,
                    "egg_bus_city"=5,"egg_bus_sub"=10,"egg_bus_rur"=20,
                   "wait_bus_city"=5,"wait_bus_sub"=5,"wait_bus_rur"=15)
    el.car <- list("egg_car"=3,
                        "park.s_city"=5,"park.s_sub"=0,"park.s_rur"=0,
                        "park.c_city"=0,"park.c_sub"=0,"park.c_rur"=0)
    el.sav <- list("sav_fare"=0.32, "wait_sav_city"=5,"wait_sav_sub"=10,"wait_sav_rur"=15)
    para <- list("move.threshold"=0.95, "aban.threshold" = 0.75,"detour"=1.417,
                 "VTTS.car"=10.51,"search.r"=0.5,"car.aban.r"=0,"car.upgrade.r"=0.2)
    RV <- list("car"=1,"walk"=1.06,"bus"=0.72,"pav"=0.69,"sav"=0.89)
    VM <- list("egg_car"=1.67,"search_car"=1.38,"egg_bus"=1.47,"wait_bus"=1.41,"wait_sav"=0.62)


    own.0.cdf <-    switch(scenario[["sav.avail"]]+1,0.00, para[["car.aban.r"]])
    own.c.cdf <- 1 -switch(scenario[["pav.avail"]]+1,0.00, para[["car.upgrade.r"]])
    }
    
  # Read in files 

    {
    if(phase==1){
      setwd("C:/Users/ASUS/Dropbox/Publication (AV)/Simulation/full")
      indi <- read.csv(file="indi_full.csv", header=TRUE, sep=",",stringsAsFactors = FALSE)
      travelcost <- read.csv(file="travelcost_full.csv", header=TRUE, sep=",",stringsAsFactors = FALSE) #built in with vehicle-speed info)
      record <- read.csv(file="record_full.csv", header=TRUE, sep=",",stringsAsFactors = FALSE)
      }else{
      setwd("C:/Users/ASUS/Dropbox/Publication (AV)/Simulation/Simulation result/1121/phase1")
      indi <- read.csv(file="indi_result.csv", header=TRUE, sep=",",stringsAsFactors = FALSE)
      travelcost <- read.csv(file="travelcost_result.csv", header=TRUE, sep=",",stringsAsFactors = FALSE) #built in with vehicle-speed info)
      record <- read.csv(file="record_result.csv", header=TRUE, sep=",",stringsAsFactors = FALSE)}
    
    setwd("C:/Users/ASUS/Dropbox/Publication (AV)/Simulation/")
      loc.p_frame <- read.csv(file="location prob_frame_full.csv", header=TRUE, sep=",")
      distance.lookup <- read.csv(file="distance lookup_full.csv", header=TRUE, sep=",") #it's a constant check-table
      urban.class <- read.csv(file="city_urban_classification.csv", header=TRUE, sep=",")
      trip.c <- read.csv(file="trip count_full.csv", header=TRUE, sep=",",stringsAsFactors = FALSE)
      label <- read.csv(file="label.csv", header=TRUE, sep=",",stringsAsFactors = FALSE)
      density <- read.csv(file="density_full.csv", header=TRUE, sep=",")
      loc.util_o <- read.csv(file="loc.util.O.csv", header=TRUE, sep=",") #other util related to O
  #some primary processing
    
    label <- label$O[1:113]
    if(phase==1){indi <- merge(indi, urban.class[,c("MSOA","env.O")],by.x = "O", by.y = "MSOA")
                 travelcost$vehicle_speed <- 38
                 travelcost <- merge(travelcost, urban.class[,c("MSOA","env.O")], by.x = "O",by.y = "MSOA" )
                 travelcost <- merge(travelcost, urban.class[,c("MSOA","env.D")], by.x = "D",by.y = "MSOA" )
                 indi$original.O <- indi$O }else{NULL}
    travelcost$speed_walk <- sapply(travelcost$env.O, function(x){switch(x+1,el.walk[["speed_walk_rur"]],el.walk[["speed_walk_sub"]],el.walk[["speed_walk_city"]])})
    travelcost$wait_sav <- sapply(travelcost$env.O, function(x){switch(x+1,el.sav[["wait_sav_rur"]],el.sav[["wait_sav_sub"]],el.sav[["wait_sav_city"]])})
    travelcost$wait_bus <- sapply(travelcost$env.O, function(x){switch(x+1,el.bus[["wait_bus_rur"]],el.bus[["wait_bus_sub"]],el.bus[["wait_bus_city"]])})
    travelcost$acc_bus <- sapply(travelcost$env.O, function(x){ switch(x+1,el.bus[["acc_bus_rur"]],el.bus[["acc_bus_sub"]],el.bus[["acc_bus_city"]])})
    travelcost$egg_bus <- sapply(travelcost$env.D, function(x){ switch(x+1,el.bus[["egg_bus_rur"]],el.bus[["egg_bus_sub"]],el.bus[["egg_bus_city"]])})
    travelcost$park.s_car <- sapply(travelcost$env.D, function(x){ switch(x+1,el.car[["park.s_rur"]],el.car[["park.s_sub"]],el.car[["park.s_city"]])})
    travelcost$park.c_car <- sapply(travelcost$env.D, function(x){ switch(x+1,el.car[["park.c_rur"]],el.car[["park.c_sub"]],el.car[["park.c_city"]])})
    
    
    #indi[indi$euclidean.dis==1000,"euclidean.dis"] <- 800
    #travelcost[travelcost$distance==1000,"distance"] <- 800
    #distance.lookup[distance.lookup$euclidean==1000,"euclidean"] <- 800
    #distance.lookup[distance.lookup$euclidean==1000,"dis.commute"] <- 800*1.417
    

  #Prepare travel cost matrix
  
    travelcost$cruise_walk <- 60 * travelcost$distance * para[["detour"]] / (travelcost$speed_walk*1000)
    travelcost$cruise_car <- 60 * travelcost$distance * para[["detour"]] / (travelcost$vehicle_speed*1000) #60minutes,1000meters
    travelcost$cruise_pav <- travelcost$cruise_car
    travelcost$cruise_sav <- travelcost$cruise_car
    travelcost$cruise_bus <- if(scenario[["bus.priority"]] ==1){
          60 * (travelcost$distance * para[["detour"]]) / 29000}else{
          60 * (travelcost$distance * para[["detour"]])/((travelcost$vehicle_speed / 38 ) * 25000)} #38 car initial speed, 25 bus intial speed
    
    #travelcost$expense_bus <- ifelse(travelcost$distance<1600, 1.6,2.5)
    travelcost$expense_car <- travelcost$distance * para[["detour"]] /1000 *0.13
    travelcost$expense_pav <- travelcost$distance * para[["detour"]] /1000 *0.13
    travelcost$expense_sav <- travelcost$distance * para[["detour"]] /1000 *el.sav[["sav_fare"]]
    travelcost$expense_sav[travelcost$expense_sav<2*el.sav[["sav_fare"]]] <- 2*el.sav[["sav_fare"]]
    }


######### Simulation 
    end.t <- switch(phase,11,21)
    #end.t <-2
        t <- switch(phase,1,11)
    while (t < end.t) {
# Residential Relocation ------------------------------------
    
    #Estimating Density
    
    for(x in 1:113){
      density[x,"Sample.tn"] <- sum(indi$O == density[x,"O"], na.rm = TRUE)}
    density[,"multiplier"] <- density$Sample.tn / density$Sample.t0
    density[,"Pop.tn"] <- density[,"multiplier"]*density[,"Pop.t0"]
    density$density.tn <- density[,"Pop.tn"] / density[,"area"]
    
    
    #estimating commute generalized cost
    "car: expense, cruise, egg, park.c, park.s"
    "sav: expense, cruise, wait"
    "bus: expense, cruise, egg, wait"
    "pav: expense, cruise"
    travelcost$gt_walk <- travelcost$cruise_walk
    travelcost$gt_bus <- travelcost$cruise_bus + (travelcost$acc_bus + travelcost$egg_bus) *VM[["egg_bus"]] + travelcost$wait_bus *VM[["wait_bus"]] 
    travelcost$gt_car <- travelcost$cruise_car + el.car[["egg_car"]]*VM[["egg_car"]]+ travelcost$park.s_car*VM[["search_car"]]
    travelcost$gt_pav <- travelcost$cruise_pav
    travelcost$gt_sav <- travelcost$cruise_sav + travelcost$wait_sav*VM[["wait_sav"]] 
    
    travelcost$cgc_walk <- travelcost$gt_walk/60 *RV[["pav"]]*para[["VTTS.car"]]
    travelcost$cgc_bus <-el.bus[["fare_pass"]] +travelcost$gt_bus/60 *RV[["bus"]]*para[["VTTS.car"]]
    travelcost$cgc_car <- travelcost$expense_car + travelcost$gt_car/60* RV[["car"]]*para[["VTTS.car"]]
    travelcost$cgc_pav <- travelcost$expense_pav + travelcost$gt_pav/60 *RV[["pav"]]*para[["VTTS.car"]]
    travelcost$cgc_sav <- travelcost$expense_sav + travelcost$gt_sav/60* RV[["sav"]]*para[["VTTS.car"]] 
    
    if(scenario[["sav.avail"]]==0){
    travelcost$lgc_own.0 <- least.gc(c("walk","bus"))
    travelcost$lgc_own.c <- least.gc(c("walk","bus","car"))
    travelcost$lgc_own.p <- least.gc(c("walk","bus","pav"))
    }else{
    travelcost$lgc_own.0 <- least.gc(c("walk","bus","sav"))
    travelcost$lgc_own.c <- least.gc(c("walk","bus","car","sav"))  
    travelcost$lgc_own.p <- least.gc(c("walk","bus","pav","sav"))   
    }
     
    lgc_own.0 <- dcast(travelcost, O~D, value.var = c("lgc_own.0"))
    lgc_own.c <- dcast(travelcost, O~D, value.var = c("lgc_own.c"))
    lgc_own.p <- dcast(travelcost, O~D, value.var = c("lgc_own.p"))
    
    
    loc.util_o <- merge(loc.util_o[,c("O","util.other")], density[,c("O","density.tn","Sample.tn")],by = "O")
    loc.p_own.0 <- loc.p.synthesis(lgc_own.0)
    loc.p_own.c <- loc.p.synthesis(lgc_own.c)
    loc.p_own.p <- loc.p.synthesis(lgc_own.p)
    loc.util_o[,c("density.tn","Sample.tn")] <- list(NULL)
    
  #Decide who will move
    indi$move.rand <- runif(210783,0,1)
    move.test = indi$move.rand > para[["move.threshold"]]
    move <- indi[move.test,c("ID","D","Did","Car.Ownership")] #把要搬家的挑出來
    move$loc.rand <- runif(nrow(move),0,1)


  #Assign preassumed new residence location (Also it depends on where people work Did=X )
    move.bind <- data.frame(ID=integer(),Did=integer(),D=factor(),mode=factor(),loc.rand=double(),choice_car=factor(),choice_bus=factor(),choice_walk=factor(),choice_pav=factor(),choice_sav=factor())
    
    for(x in 1:113){
      move.sel = move[move$Did == x,]  #把在X區工作的挑出來
      move.sel$choice_own.0 <- cut(move.sel$loc.rand,loc.p_own.0[[x+1]], label, right = TRUE) #指派新居住地點(用車者)
      move.sel$choice_own.c <- cut(move.sel$loc.rand,loc.p_own.c[[x+1]], label, right = TRUE) #指派新居住地點(公車者)
      move.sel$choice_own.p <- cut(move.sel$loc.rand,loc.p_own.p[[x+1]], label, right = TRUE) #指派新居住地點(走路者)
      move.bind <- rbind(move.sel,move.bind)
      }
    
    move <- merge(move,move.bind[,c("ID","choice_own.0","choice_own.c","choice_own.p")],by = "ID")
    
  #final location decision depend on vehicle ownership
    move$O.new <- 0
    move[move$Car.Ownership == 0, "O.new"] <- as.character(move[move$Car.Ownership == 0, "choice_own.0"])
    move[move$Car.Ownership == 1, "O.new"] <- as.character(move[move$Car.Ownership == 1, "choice_own.c"])
    move[move$Car.Ownership == 2, "O.new"] <- as.character(move[move$Car.Ownership == 2, "choice_own.p"])

    move[,"OD"] <- paste(move[,"O.new"],move[,"D"],sep = "") #OD update
    move <- merge(move,distance.lookup, by = "OD",all.x = TRUE) #check distances
    move <- merge(move,urban.class[,c("MSOA","env.O")], by.x = "O.new",by.y="MSOA",all.x = TRUE)
    data.table::setnames(move,"euclidean" , "euclidean.new")
    data.table::setnames(move,"env.O" , "env.O.new")
    
    
    #all residence locations confirmed
    if(scenario[["mode.split.test"]]==1){NULL}else{
    indi <- merge(indi,move[c("ID","O.new","euclidean.new","env.O.new")], by = "ID",all.x = TRUE)
    move.test = indi$move.rand > para[["move.threshold"]]
    indi[move.test,"O"] <- indi[move.test,"O.new"]
    indi[move.test,"euclidean.dis"] <- indi[move.test,"euclidean.new"]
    indi[move.test,"env.O"] <- indi[move.test,"env.O.new"]
    indi[move.test,"OD"] <- paste(indi[move.test,"O"],indi[move.test,"D"],sep = "") #OD update
    indi[,c("O.new","euclidean.new","env.O.new")] <- list(NULL)
    move <- NULL
    }
 # Ownership Decision ------------------------------------------------------

    indi$aban.rand <- runif(210783,0,1)
    aban.test = indi$aban.rand > para[["aban.threshold"]] & indi$Car.Ownership == 1
    own.decision <- indi[aban.test,c("ID","env.O")] #把要棄舊車的挑出來
    own.decision$deci.rand <- runif(nrow(own.decision),0,1)
    own.decision[,"ownership.new"] <- ifelse(own.decision[,"deci.rand"] < own.0.cdf,0,ifelse(own.decision[,"deci.rand"] < own.c.cdf,1,2))
    indi <- merge(indi,own.decision[c("ID","ownership.new")], by = "ID",all.x = TRUE)
    aban.test = indi$aban.rand > para[["aban.threshold"]] & indi$Car.Ownership == 1
    indi[aban.test,"Car.Ownership"] <- indi[aban.test,"ownership.new"]
    own.decision <- NULL
    indi$ownership.new <- NULL
    
# Mode Choice -------------------------------------------------------------

    indi$sel.rand <- runif(210783,0,1)
    m <- 1
  while(m < 5){
    #assign empty trips 
    moto.trips <- subset(indi[,c("mode","O","D")], indi$mode == "car"|indi$mode == "sav"|indi$mode == "pav")
    
    if(scenario[["induc.trip"]]==1){
      moto.trips[,c("car.induc.O","car.induc.D","sav.induc.O","sav.induc.D","pav.induc.O","pav.induc.D")] <-0
      moto.trips[moto.trips$mode =="sav","sav.induc.O"] <- moto.trips[moto.trips$mode =="sav","D"]
      moto.trips[moto.trips$mode =="sav","sav.induc.D"]<- moto.trips[moto.trips$mode =="sav","O"]
      moto.trips[moto.trips$mode =="pav","pav.induc.O"] <- moto.trips[moto.trips$mode =="pav","D"]
      moto.trips[moto.trips$mode =="pav","pav.induc.D"] <- moto.trips[moto.trips$mode =="pav","D"]
      moto.trips[moto.trips$mode =="car","car.induc.O"] <- moto.trips[moto.trips$mode =="car","D"]
      moto.trips[moto.trips$mode =="car","car.induc.D"] <- moto.trips[moto.trips$mode =="car","D"]
    #PAV do not park in city centre
      peri.zones <- c("Leeds 038","Leeds 042","Leeds 048","Leeds 054","Leeds 055","Leeds 056","Leeds 064","Leeds 065","Leeds 070","Leeds 071","Leeds 075","Leeds 081","Leeds 082","Leeds 085","Leeds 110")
      moto.trips[moto.trips$pav.induc.D=="Leeds 111","pav.induc.D"] <- sample(peri.zones, sum(moto.trips$pav.induc.D=="Leeds 111"),TRUE)
      moto.trips[moto.trips$pav.induc.D=="Leeds 063","pav.induc.D"] <- sample(peri.zones, sum(moto.trips$pav.induc.D=="Leeds 063"),TRUE)
      moto.trips[moto.trips$pav.induc.D=="Leeds 112","pav.induc.D"] <- sample(peri.zones, sum(moto.trips$pav.induc.D=="Leeds 112"),TRUE)
    
    #Estimating Demand Factor & Vehicle Speed
    
      trip.c[,"TripO.tn_sum"] <- sapply(trip.c$zone,function(x){sum(moto.trips$O==x)+sum(moto.trips$sav.induc.O==x)+sum(moto.trips$pav.induc.O==x)+sum(moto.trips$car.induc.O==x)*para[["search.r"]]})
      trip.c[,"TripD.tn_sum"] <- sapply(trip.c$zone,function(x){sum(moto.trips$D==x)+sum(moto.trips$sav.induc.D==x)+sum(moto.trips$pav.induc.D==x)+sum(moto.trips$car.induc.D==x)*para[["search.r"]]})
      travelcost <- merge(travelcost,trip.c[,c("zone","TripO.tn_sum","TripO.t0_sum")],by.x = "O",by.y = "zone")
      travelcost <- merge(travelcost,trip.c[,c("zone","TripD.tn_sum","TripD.t0_sum")],by.x = "D",by.y = "zone")
      travelcost$DF <- (travelcost$TripO.tn_sum + travelcost$TripD.tn_sum)*1.2045 / (travelcost$TripO.t0_sum + travelcost$TripD.t0_sum)
      travelcost$vehicle_speed <- 50 / ((travelcost$DF ^ 4)*0.15 +1 ) #free flow speed 50
      travelcost[,c("TripO.tn_sum","TripO.t0_sum","TripD.tn_sum","TripD.t0_sum")] <- list(NULL)
    }else{
      trip.c[,"TripO.tn"] <- sapply(trip.c$zone,function(x){sum(moto.trips$O==x)})
      trip.c[,"TripD.tn"] <- sapply(trip.c$zone,function(x){sum(moto.trips$D==x)})
      travelcost <- merge(travelcost,trip.c[,c("zone","TripO.tn")],by.x = "O",by.y = "zone")
      travelcost <- merge(travelcost,trip.c[,c("zone","TripD.tn")],by.x = "D",by.y = "zone")
      travelcost$DF <- (travelcost$TripO.tn + travelcost$TripD.tn)*1.2045 / (travelcost$TripO.t0 + travelcost$TripD.t0)
      travelcost$vehicle_speed <- 50 / ((travelcost$DF ^ 4)*0.15 +1 ) #free flow speed 50
      travelcost[,c("TripO.tn","TripD.tn")] <- list(NULL)}
    

    #calculate travel time/utility
    travelcost$cruise_car <- 60 * travelcost$distance * para[["detour"]] / (travelcost$vehicle_speed*1000) #60minutes,1000meters
    travelcost$cruise_pav <- travelcost$cruise_car
    travelcost$cruise_sav <- travelcost$cruise_car
    travelcost$cruise_bus <- if(scenario[["bus.priority"]] ==1){
                60 * (travelcost$distance * para[["detour"]]) / 29000}else{
                60 * (travelcost$distance * para[["detour"]])/((travelcost$vehicle_speed / 38 ) * 25000)} #38 car initial speed, 25 bus intial speed
    
    travelcost$gt_bus <- travelcost$cruise_bus + (travelcost$acc_bus + travelcost$egg_bus) *VM[["egg_bus"]] + travelcost$wait_bus *VM[["wait_bus"]] 
    travelcost$gt_car <- travelcost$cruise_car + el.car[["egg_car"]]*VM[["egg_car"]]+ travelcost$park.s_car*VM[["search_car"]]
    travelcost$gt_pav <- travelcost$cruise_pav
    travelcost$gt_sav <- travelcost$cruise_sav + travelcost$wait_sav*VM[["wait_sav"]] 
    
    travelcost$walk.utility <- travelcost$gt_walk * -0.423 +11.9 -4.14 #you forgot to put -4.74 license impact on walking
    travelcost$bus.utility <- travelcost$gt_bus*-0.0825 + el.bus[["fare_pass"]]*1.11*-0.947  -3.39 #PT+1.43
    travelcost$car.utility <- travelcost$gt_car*-0.114 + travelcost$expense_car*1.11*-0.947
    travelcost$pav.utility <- travelcost$gt_pav*-0.0784 + travelcost$expense_pav*1.11*-0.947
    travelcost$sav.utility <- travelcost$gt_sav*-0.102 + travelcost$expense_sav*1.11*-0.947   -1.74
    
    # mode.p template creation
    mode.p <- travelcost[,c("OD","car.utility","sav.utility","pav.utility","bus.utility","walk.utility")]
    if(scenario[["sav.avail"]]==1){
      mode.p.synthesis("own.0","walk","bus","sav")
      mode.p.synthesis("own.c","walk","bus","car","sav")
      mode.p.synthesis("own.p","walk","bus","pav","sav")
    }else{
      mode.p.synthesis("own.0","walk","bus")
      mode.p.synthesis("own.c","walk","bus","car")
      mode.p.synthesis("own.p","walk","bus","pav")}
    
    # 4 quarter mode choice   
    sel.test <- indi$sel.rand >= (-0.25+ m*0.25) & indi$sel.rand < (0+ m*0.25)
    mode.change <- indi[sel.test,c("ID","OD","Car.Ownership")] 
    
    mode.change <- merge(mode.change,mode.p,by = "OD" ,all.x = TRUE)
    mode.change$mode.rand <- runif(length(mode.change$ID),0,1)
    mode.change$mode_own.0 <- ifelse(mode.change$mode.rand < mode.change$walk.cdf_own.0, "walk",ifelse(mode.change$mode.rand < mode.change$bus.cdf_own.0,"bus","sav"))
    mode.change$mode_own.c <- ifelse(mode.change$mode.rand < mode.change$walk.cdf_own.c, "walk",ifelse(mode.change$mode.rand < mode.change$bus.cdf_own.c,"bus",ifelse(mode.change$mode.rand < mode.change$car.cdf_own.c,"car","sav")))
    mode.change$mode_own.p <- ifelse(mode.change$mode.rand < mode.change$walk.cdf_own.p, "walk",ifelse(mode.change$mode.rand < mode.change$bus.cdf_own.p,"bus",ifelse(mode.change$mode.rand < mode.change$pav.cdf_own.p,"pav","sav")))
    
    mode.change$mode.new <- ifelse(mode.change$Car.Ownership == 0, mode.change$mode_own.0, ifelse(mode.change$Car.Ownership == 1,mode.change$mode_own.c,mode.change$mode_own.p)) #switch(mode.change$Car.Ownership+1,mode.change$mode_own.0,mode.change$mode_own.c,mode.change$mode_own.p)
    indi <- merge(indi,mode.change[,c("ID","mode.new")],by = "ID",all.x= TRUE)
    sel.test <- indi$sel.rand >= (-0.25+ m*0.25) & indi$sel.rand < (0+ m*0.25)
    indi[sel.test,"mode"] <- indi[sel.test,"mode.new"]
    indi$mode.new <- NULL

    m <- m+1
    }


    indi <- indi[,c("ID","O","D","Did","OD","Car.Ownership","mode","env.O","euclidean.dis","original.O")]


# Recording ---------------------------------------------------------------

    #record commute distance(mean and s.d.and everything)
      record[t+1,"ave.comm.dist"] <- round(mean(indi[,"euclidean.dis"])*1.417*0.001,3)
      record[t+1,"comm.dis_own.0"] <- round(mean(indi[indi$Car.Ownership==0,"euclidean.dis"])*1.417*0.001,3)
      record[t+1,"comm.dis_own.cp"] <- round(mean(indi[indi$Car.Ownership!=0,"euclidean.dis"])*1.417*0.001,3)
      record[t+1,"sd.comm.dist"] <- round(sd(indi[,"euclidean.dis"]*1.417*0.001),3)
  
    
    #calculate and record mode share
      record[t+1,"walk.share"] <- round(sum(indi$mode == 'walk', na.rm = TRUE) / 2107.83,1)
      record[t+1,"bus.share"] <- round(sum(indi$mode == 'bus', na.rm = TRUE) / 2107.83,1)
      record[t+1,"car.share"] <- round(sum(indi$mode == 'car', na.rm = TRUE) / 2107.83,1)
      record[t+1,"pav.share"] <- round(sum(indi$mode == 'pav', na.rm = TRUE) / 2107.83,1)
      record[t+1,"sav.share"] <- round(sum(indi$mode == 'sav', na.rm = TRUE) / 2107.83,1)
      
      record[t+1,"pav.owner"] <- round(sum(indi$Car.Ownership == 2, na.rm = TRUE) / 2107.83,1)
      record[t+1,"car.owner"] <- round(sum(indi$Car.Ownership == 1, na.rm = TRUE) / 2107.83,1)
      record[t+1,"pav.trans.r"] <- round(sum(indi$Car.Ownership == 2, na.rm = TRUE) / 1432.21,1)
      
   #Net moving individual number
      
      record$num.move[t+1] <- sum(indi$O != indi$original.O)
    
    #counting average speed
      OD.mode <- dcast(indi, OD~mode,length)
      OD.mode$vehicle <- OD.mode %>% select(matches("car|pav|sav")) %>% rowSums(.)
      OD.mode <- merge(OD.mode, travelcost[,c("OD","distance","vehicle_speed","cruise_car","park.s_car","cruise_bus","cruise_walk","wait_sav","acc_bus","egg_bus","wait_bus","cgc_walk","cgc_bus","cgc_car","cgc_sav","cgc_pav")],by = "OD")

      record[t+1,"vehi.speed"] <- round(sum(OD.mode$vehicle * OD.mode$vehicle_speed) /sum(OD.mode$vehicle),1) 
      record[t+1,"walk.time"] <- round(sum(OD.mode$walk * OD.mode$cruise_walk) /sum(OD.mode$walk),1)
      record[t+1,"bus.time"] <-  round(sum(OD.mode$bus * (OD.mode$cruise_bus + OD.mode$acc_bus + OD.mode$egg_bus + OD.mode$wait_bus)) /sum(OD.mode$bus),1)
      record[t+1,"car.time"] <-  round(sum(OD.mode$car * (OD.mode$cruise_car + OD.mode$park.s_car)) /sum(OD.mode$car) + el.car[["egg_car"]],1)
      record[t+1,"pav.time"] <- if(scenario[["pav.avail"]]==1){round(sum(OD.mode$pav * OD.mode$cruise_car) /sum(OD.mode$pav),1)}else{0}
      record[t+1,"sav.time"] <- if(scenario[["sav.avail"]]==1){round(sum(OD.mode$sav * (OD.mode$cruise_car+ OD.mode$wait_sav)) /sum(OD.mode$sav),1)}else{0}
      

      record[t+1,"comm.time"] <-  record[t+1,"car.time"]*as.numeric(record[t+1,"car.share"])*0.01+
                                  record[t+1,"sav.time"]*as.numeric(record[t+1,"sav.share"])*0.01+
                                  record[t+1,"pav.time"]*as.numeric(record[t+1,"pav.share"])*0.01+
                                  record[t+1,"bus.time"]*as.numeric(record[t+1,"bus.share"])*0.01+
                                  record[t+1,"walk.time"]*as.numeric(record[t+1,"walk.share"])*0.01
      
      record[t+1,"ave.cgc"] <- round((sum(OD.mode$walk * OD.mode$cgc_walk) +
                                      sum(OD.mode$bus * OD.mode$cgc_bus) +
                                      sum(OD.mode$car * OD.mode$cgc_car) +
                                      sum(OD.mode$sav * OD.mode$cgc_sav) +
                                      sum(OD.mode$pav * OD.mode$cgc_pav))/ 210783,2)
      
      
      t <- t+1
      
      }

beep("facebook")


#save result   
    date <- "1121"
    print(scen)    
    {record[c(31:39)+switch(phase,0,9),4] <- c(date,
                               paste(names(para),para,sep = ":",collapse = " ; "),
                               paste(names(el.walk),el.walk,sep = ":",collapse = " ; "),
                               paste(names(el.bus),el.bus,sep = ":",collapse = " ; "),
                               paste(names(el.car),el.car,sep = ":",collapse = " ; "),
                               paste(names(el.sav),el.sav,sep = ":",collapse = " ; "),
                               paste(names(VM),VM,sep = ":",collapse = " ; "),
                               paste(names(RV),RV,sep = ":",collapse = " ; "),
                               paste(names(scenario),scenario,sep = ":",collapse = " ; "))
    record[is.na(record)] <- ""
    record[21,1] <- scen
    }
    
    {
      path.date <- paste0("c:/Users/ASUS/Dropbox/Publication (AV)/Simulation/Simulation result/",date)
    path.scen <- paste0(path.date,"/",scen)
    
    if(dir.exists(path.date)){
      summary.rec <- read.csv(file=paste0(path.date,"/summary record.csv"), header=TRUE, sep=",")
    }else{
      dir.create(path.date)
    summary.rec <- read.csv(file="c:/Users/ASUS/Dropbox/Publication (AV)/Simulation/summary record template.csv", header=TRUE, sep=",")  }
    }
    
    summary.rec <- rbind(summary.rec, record[21,])
    write.csv(x = record,file = paste0(path.date,"/record_",scen,".csv"),row.names = FALSE)
    write.csv(x = summary.rec,file = paste0(path.date,"/summary record",".csv"),row.names = FALSE)
    
    
    if(dir.exists(path.scen)){"be careful don't replace it"}else{
    dir.create(path.scen)
    setwd(path.scen)
      write.csv(x = indi,file = "indi_result.csv",row.names = FALSE)
      write.csv(x = record, file = "record_result.csv",row.names = FALSE)
      write.csv(x = travelcost[,1:11], file = "travelcost_result.csv",row.names = FALSE)
      write.csv(x = OD.mode, file = "OD.mode_result.csv",row.names = FALSE)
      }
    #summary.rec <- read.csv(file="summary record template.csv", header=TRUE, sep=",")
    
    