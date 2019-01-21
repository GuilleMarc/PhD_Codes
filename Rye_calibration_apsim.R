##############################################################################
## Calibration of a rye cultivar in a corn-soybean rotation

# A function to minimize error function using discrete steps for a parameter in the APSIM model
##############################################################################

###Required data: out of the function:

optim_naive<- function(observed,values,node1,node2){ 
  
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
    cbind(x$zadok_stage[index])
  })
  
  ## errors matrix:
  errors= (observed$mean_zadok - res_vectors)^2
  SSE= apply(errors, 2, sum)
  MSE= apply(errors, 2, mean)
  RMSE= sqrt(MSE)
  RRMSE= RMSE/mean(observed$mean_zadok)
  return(list(SSE= SSE, RMSE= RMSE, RRMSE=RRMSE, res_vectors= res_vectors))
}

error_function= function(obs,pred){
  errors_sq= (obs-pred)^2
  SSE=sum(errors_sq)
  MSE= mean(errors_sq)
  RMSE= sqrt(MSE)
  RRMSE= RMSE/mean(obs)
  return(list(SSE=SSE, MSE=MSE, RMSE=RMSE, RRMSE=RRMSE))
}


### NOT PRINT:  EXAMPLE OF THE CALIBRATION PROCESS ################################################

obs_opt                                                    # observed data

## The calibration set is very small. No need to randomize

## Split data in two: training and testing: Training (year 1): testing (year 2)
obs_opt= obs_opt %>% arrange(Date)
split1= obs_opt %>% as.data.frame() %>% slice(1:14) 
split2= obs_opt %>% as.data.frame() %>% slice(15:25)

index= sapply(split1$Date, function(x){
  grep(x, res$Date, value = FALSE)
}) %>% unlist()  



### Define the parameter of interest:

#tt_emerg ()

par_values= c(100, 1000, 50)
node1="base_cultivar"
node2='tt_floral_initiation'

#cat('yecora cultivar="yes"', "\n")

tt_par_train= optim_naive(observed= split1, 
                          values= par_values, 
                          node1 = node1, 
                          node2 = node2)

plot(tt_par_train$SSE ~ seq(100,1000,50))


tt_par_test= apsim(files = "MaizeWheatSoybeans_2017.apsim" ,
                         exe = apsimExe,
                         wd= apsimWd)

preds_test= tt_par_test[index,]$zadok_stage     #pred 
obs_test= split1$mean_zadok                     #obs

error_function(obs_test,preds_test)
plot(obs_test,preds_test, xlim = c(0,40), ylim=c(0,40))

#tt_par_test

#####END NOT PRINT ###################################################




















