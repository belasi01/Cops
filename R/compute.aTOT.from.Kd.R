# takes Rd and R from COPS data and produce a(lambda) as Equation 8 (or rather, 8')
# of Morel & Marioneta 2001 "Bio-optical properties of oceanic waters: A reappraisal"
# a = Kd 0.90[1+2.25R]^-1 [1-R]
#
# Use COPS data, ex:
# library(Cops) 
# cops=read.COPS(file=paste(path,"COPS_IML4_150710_1348_C_data_004.csv", sep = ""), 2)

compute.aTOT.from.Kd <- function (path, date_station) {
  
  take_down_to_10percent_of_light = TRUE # otherwise, take first 1 m
  
  
  print(paste("IOPs from Kd for",date_station))
  
  station.nb=substring(date_station,18,23) # two extra spaces for case "G604.5"

  #Load wavelenghts from COPS
  lambda = c(340, 412, 443, 465, 490, 510, 532, 555, 560, 589, 625, 665, 670, 683, 694, 710, 765, 780, 875)
  nb_lambda = length(lambda)
  
  #Use processed COPS data, saved 
  files.dat=read.csv(file=paste(path,"L2/",date_station,"/COPS/remove.cops.dat", sep = ""), sep = ";", header = FALSE)
  index_good_casts = which(files.dat$V2==1)
  nb_good_casts = length(index_good_casts)
  R_lambda_casts = array(data=0.0, dim=c(nb_good_casts,nb_lambda))
  Kd_lambda_casts = array(data=0.0, dim=c(nb_good_casts,nb_lambda))
  
  for (c in 1:nb_good_casts) {
    kept_cast = files.dat$V1[index_good_casts[c]]
    load(paste(path,"L2/",date_station,"/COPS/BIN/",kept_cast,".RData",sep = ""))
    
    R_lambda_casts[c,] = cops$R.0m.linear
    
    if (take_down_to_10percent_of_light) {
      #TAKE DEPTHS 0 to z corresponding to 10% of surface radiation
      match_10percent_light = abs(cops$K0.EdZ.fitted[,]/cops$K0.EdZ.fitted[1,] - 0.1)
      z_10 = apply(match_10percent_light,2,which.min) # column-wise minimum
      Kd_lambda_10percent = array(data=0.0, dim=nb_lambda)
      for (i in 1:nb_lambda) {
        Kd_lambda_10percent[i] = mean(cops$K0.EdZ.fitted[1:z_10[i],i], na.rm = TRUE)
      }
      Kd_lambda_casts[c,] = Kd_lambda_10percent
    }
    else {
      #TAKE DEPTHS 0 TO 1 m with rownames(cops$K0.EdZ.fitted)[100]=="1"
      Kd_lambda_1er_m = colMeans(cops$K0.EdZ.fitted[1:100,], na.rm = TRUE)
      Kd_lambda_casts[c,] = Kd_lambda_1er_m
    }
  } # for c
  
  R_lambda = colMeans(R_lambda_casts)
  Kd_lambda = colMeans(Kd_lambda_casts)
  
  #a = Kd 0.90[1+2.25R]^-1 [1-R]
  a.tot = Kd_lambda*0.9*(1-R_lambda)/(1+2.25*R_lambda)
  
  # Plot and save results
  if (station.nb != "409") {
    png(filename = paste(path,"L2/",date_station,"/COPS/absorption.cops.png", sep=""))
    plot(lambda, a.tot, 
         xlab = "Wavelenght", ylab="Absorption", pch=19,
         ylim=c(0, max(a.tot)))
    
    #lines(lambda, a.p.station, col=2, lwd=3)
    #source('./spectral.aw.R', echo=TRUE)
    #lines(300:800, spectral.aw(300:800), col=4, lwd=4)
    #xlines(lambda, a.g.station, col=3, lwd=3)
    dev.off() }
  else {
    print("WARNING: no COPS data for station G409")
  }
  
  # write results in file absorption.cops.dat
  file=paste(path,"L2/",date_station,"/COPS/absorption.cops.dat", sep="")
  df = read.table(file,sep=";")
  nfile = length(df$V1) - 1
  a.mat = matrix(a.tot, nrow=nfile, ncol=19, byrow=T)
  df.a = rbind(lambda,a.mat)
  df.final = as.data.frame(cbind(df$V1, as.data.frame(df.a)))
  
  write.table(df.final, quote=F, file, sep=";", col.names = F, row.names=F)
  
  return(a.tot)
  
}
