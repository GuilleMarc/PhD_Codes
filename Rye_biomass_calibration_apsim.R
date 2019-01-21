########Biomass calibration:  ############################################
library(apsimr)
library(dplyr)
library(ggplot2)
library(tidyr)
####Observed data for biomass calibration:
biomass_15_17= data.frame(Date=c('2015-11-03','2016-04-26', '2017-04-22',
                        '2016-04-18', '2016-11-01'),
                biomass=c(730.41, 3658.35, 1043.85,
                          1955.79, 237.77),
                season=c("fall","spring","spring","spring",
                         "fall"))

biomass_02_15= data.frame(Date= c('2002-04-22','2003-05-06','2009-05-19',
                                  '2011-11-01','2012-04-23','2012-11-02',
                                  '2013-05-15','2013-11-15','2014-05-06',
                                  '2014-11-07','2015-05-13'), 
                          biomass= c(2430,251.22,497.83,
                                     130.0,2517.0,180.0,
                                     1078.73,520.0,874.46,
                                     110.0,2123.90), 
                          season=c('spring','spring','spring',
                                   'fall','spring','fall',
                                   'spring','fall','spring',
                                   'fall','spring'))

obs_opt_biomass= rbind(biomass_02_15, biomass_15_17)
obs_opt_biomass= obs_opt_biomass %>% arrange(Date)
############################################

######################### Optimizer function ###########################################
optim_naive_biomass<- function(observed,values,node1,node2){ 
  
  ## Required libraries:
  require(apsimr); require(ggplot2); require(dplyr)
  
  ## Paths and file descriptions:
  apsimFile= "MaizeWheatSoybeans_2017.apsim"                      
  apsimExe= "C:/Program Files (x86)/Apsim77-r3615/Model/Apsim.exe"
  apsimWd= getwd()
  
  simFile= "Wheat_calibration_2.xml"                                          
  apsimVars= paste(toString(node1), toString(node2), sep = '/')    
  apsimValues= as.list(seq(from= values[1], to= values[2], by= values[3]))
  
  ## iterative process:
  
  res_list= lapply(apsimValues, function(x){
    edit_sim_file(file = simFile,
                  wd= apsimWd,
                  var= apsimVars,
                  value = x,
                  overwrite = TRUE)
    res= apsim(exe= apsimExe, wd= apsimWd, files = apsimFile)
  })
  
  res_vectors= sapply(res_list, function(x){
    cbind(x$biomass[index])
  })
  
  ## errors matrix:
  errors= (observed$biomass - res_vectors)^2
  SSE= apply(errors, 2, sum)
  MSE= apply(errors, 2, mean)
  RMSE= sqrt(MSE)
  RRMSE= RMSE/mean(observed$biomass)
  return(list(SSE= SSE, RMSE= RMSE, RRMSE=RRMSE, res_vectors= res_vectors))
}

###################### Start calibration for biomass ##############################
#split5= obs_opt_biomass %>% slice(c(12:16)) #validation (2015-2017)

obk= data.frame(Date=c('2015-11-03','2016-04-13','2016-04-26', '2017-04-22'),
                biomass=c(660.00, 2017.98, 3658.35, 1043.85))
obi= data.frame(Date=c('2015-10-29', '2016-04-18', '2016-11-01'),
                biomass=c(800.82, 1893.60, 237.77))  

bmass<- rbind(obk,obi) %>% arrange(Date)

## Prepare data: Preapre a k-fold
#Prepare training, testing and validation sets:

#split1= obs_opt_biomass %>% slice(c(4:9)); test1=obs_opt_biomass %>% slice(c(10:11))
#split2= obs_opt_biomass %>% slice(c(4,5,8:11));test2=obs_opt_biomass %>% slice(c(6:7))
#split3= obs_opt_biomass %>% slice(c(4:7,10:11));test3=obs_opt_biomass %>% slice(c(8:9))
#split4= obs_opt_biomass %>% slice(c(6:11));test4=obs_opt_biomass %>% slice(c(4:5))


index= sapply(bmass$Date, function(x){
  grep(x, res$Date, value = FALSE)
}) %>% unlist()  

## pesw_germ (phenology calibrated*)

par_values= c(0.0, 0.50, 0.025)
cat('plant class="yes"', "\n")
node2='pesw_germ'


## Biomass:
## pesw_germ (phenology calibrated)
bm_par_train= optim_naive_biomass(observed= bmass, 
                              values= par_values, 
                              node1 = cat('plant class="yes"', "\n"), 
                              node2 = node2)

plot(bm_par_train$SSE ~ seq(0,0.50,0.025))


bm_par_test= apsim(files = "MaizeWheatSoybeans_2017.apsim" ,
                   exe = apsimExe,
                   wd= apsimWd)

preds_test= bm_par_test[index,]$biomass     #pred 
obs_test= bmass$biomass                     #obs
plot(obs_test,preds_test, xlim = c(0,8000), ylim=c(0,8000))


error_function(obs_test,preds_test)

plot(bm_par_train$SSE ~ seq(0,0.5,0.025))

############
validation= obs_opt_biomass %>% slice(4:11)                  #4:11
write.csv(validation,'validation.csv')

bm_par_test= apsim(files = "MaizeWheatSoybeans_2017.apsim" ,
                   exe = apsimExe,
                   wd= apsimWd)

index= sapply(validation$Date, function(x){
  grep(x, res$Date, value = FALSE)
}) %>% unlist() 

preds_test= bm_par_test[index,]$biomass 
obs_test= validation$biomass 
error_function(obs_test,preds_test)
##############










## Validation set:

index= sapply(split3$Date, function(x){
  grep(x, res$Date, value = FALSE)
}) %>% unlist()  
bm_par_test= apsim(files = "MaizeWheatSoybeans_2017.apsim" ,
                   exe = apsimExe,
                   wd= apsimWd)
preds_test= bm_par_test[index,]$biomass     #pred 
obs_test= split3$biomass                     #obs

error_function(obs_test,preds_test)

