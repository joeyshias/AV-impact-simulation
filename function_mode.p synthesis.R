#mode.p synthesis------
mode.p.synthesis <- function (pack,mode1,mode2,mode3=0,mode4=0){#define column names
  mode1.utility.n <- paste0(mode1,".utility")
  mode2.utility.n <- paste0(mode2,".utility")
  mode3.utility.n <- paste0(mode3,".utility")
  mode4.utility.n <- paste0(mode4,".utility")
  mode1.cdf.n <- paste0(mode1,".cdf_",pack)
  mode2.cdf.n <- paste0(mode2,".cdf_",pack)
  mode3.cdf.n <- paste0(mode3,".cdf_",pack)
  mode4.cdf.n <- paste0(mode4,".cdf_",pack)
  #sum and pdf
  if(mode3==0){sum <- exp(mode.p[[mode1.utility.n]])+exp(mode.p[[mode2.utility.n]])}else{
    if(mode4==0){sum <- exp(mode.p[[mode1.utility.n]])+exp(mode.p[[mode2.utility.n]])+exp(mode.p[[mode3.utility.n]])
    }else{sum <- exp(mode.p[[mode1.utility.n]])+exp(mode.p[[mode2.utility.n]])+exp(mode.p[[mode3.utility.n]])+exp(mode.p[[mode4.utility.n]])}}
  mode1.pdf <- exp(mode.p[[mode1.utility.n]])/sum
  mode2.pdf <- exp(mode.p[[mode2.utility.n]])/sum
  if(mode3==0){NULL}else{mode3.pdf <- exp(mode.p[[mode3.utility.n]])/sum}
  if(mode4==0){NULL}else{mode4.pdf <- exp(mode.p[[mode4.utility.n]])/sum}
  #cdf
  mode.p[,mode1.cdf.n] <<- mode1.pdf
  mode.p[,mode2.cdf.n] <<- mode1.pdf + mode2.pdf
  if(mode3==0){NULL}else{mode.p[,mode3.cdf.n] <<- mode1.pdf + mode2.pdf + mode3.pdf}
  if(mode4==0){NULL}else{mode.p[,mode4.cdf.n] <<- mode1.pdf + mode2.pdf + mode3.pdf + mode4.pdf}
}