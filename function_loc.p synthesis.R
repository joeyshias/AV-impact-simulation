#least gc
least.gc <-function(mode.set){
  express <- paste0("pmin(",paste(paste0("travelcost$cgc_",mode.set),collapse = ","),")")
  return(eval(parse(text=express)))
  }

#loc.p.synthesis
loc.p.synthesis <- function(cgc_mode){
  loc.util_mode <- merge(x = loc.p_frame[2:114,],y = loc.util_o,by = "O") #providing a frame 
  for (x in 2:114){
    loc.util_mode[,x] <- exp(loc.util_mode[,"util.other"] + loc.util_mode[,"density.tn"]*-0.4811 + cgc_mode[,x]*-0.73)}         #-0.7288                        #other utils, density, vot, expense
  loc.pdf_mode <-  loc.p_frame[2:114,] #create a frame
  for(i in 1:113){
    for(j in 2:114){
      loc.pdf_mode[i,j] <- loc.util_mode[i,j] / sum(loc.util_mode[,j])}}
  
  loc.cdf_mode <- loc.p_frame
  for(i in 1:113){
    loc.cdf_mode[i+1,2:114] <- loc.cdf_mode[i,2:114] + loc.pdf_mode[i,2:114]} #eg:Bradford1cdf = Bradford1 pdf+ previous cdf
  loc.p_mode <- loc.p_frame #make both re-editable
  loc.cdf_mode[,2:114] <- round(loc.cdf_mode[,2:114],digits = 7)
  for(i in 1:113){ #loc.p[1]=0,  loc.p[2] <- cdf[2](if it's larger than loc.p[1]) or loc.p[1]+0.000001, 
    loc.p_mode[i+1,2:114] <- ifelse( round(loc.cdf_mode[i+1,2:114],7) <= round(loc.p_mode[i,2:114],7), loc.p_mode[i,2:114]+0.0000001, loc.cdf_mode[i+1,2:114])}
  loc.p_mode
} 