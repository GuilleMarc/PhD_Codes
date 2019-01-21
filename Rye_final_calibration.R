###################################################
## APSIM -- FINAL OPTIMIZATION OF PARAMETERS FOR RYE SIMULATIONS
## libraries needed:
library(APSIM)
library(apsimr)
library(ggplot2)
library(dplyr)

## data to be read during the optimization problem:
obs_opt= readRDS(file= 'data_export.Rda')  # observed/predicted @ each date
res                                        # apsim output
dates_index= sapply(obs_opt$Date, function(x){
                grep(x, res$Date, value = FALSE)
              })                           # dates we have data for

## Paths and extensions to read data in:                
apsimExe= "C:/Program Files (x86)/Apsim77-r3615/Model/Apsim.exe"
apsimWd= getwd()
apsimFile= "MaizeWheatSoybeans_2017_V2.apsim"

## Recover zadoks (OUTPUT):
zadoks_by_date= function(X){
  return((X$zadok_stage[dates_index]))
}

## Define parameter matrix (INPUTS):
ap.var= c("tt_floral_initiation", "tt_end_of_juvenile")
obs= res$zadok_stage[dates_index]+rnorm(length(res$zadok_stage[dates_index]),0,1)
ap.values= c(555, 420)

obj_function<- function(prms, odata, apsimVar){
  apsimValue= matrix(rep(prms, length(dates_index)),
                     nrow = length(dates_index),
                     byrow = T
                    )
  uniRes= apsim_vector(X=apsimValue,
                       exe= apsimExe,
                       wd= apsimWd,
                       vars= apsimVar,
                       to.run = apsimFile,
                       to.edit = apsimFile,
                       g= zadoks_by_date)
  
  return(uniRes)
  #rss<-sum(((uniRes)-odata)^2)
  #return(list(apsimValue,uniRes))
}

objfun(ap.values, odata = obs, ap.var)


