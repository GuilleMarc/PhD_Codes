########      Cover crop Biomass Sensitivity  #################################
 
# Crop models are valuable tools to simulate agronomic processes but involve multiple
# equations, most of which are non-linear and highly parametrized. Predictions from these
# models are then more sensitive to errors propagating from parameter variation. If 
# we can systematically identify the most significant sources of parameter variation
# then calibration of the model can be more efficient by targeting only such sensitive 
# parameters. 

# I used the agronomic system APSIM to simulate growth of a winter rye cover crop 
# planted between phases of a corn-soybean rotation. The goal was to identify plant
# and soil parameters controlling significant variation in phenology and biomass of 
# the cover crop. 

# Technically, apsim rye predictions are modeled as a function of individual
# parameters and their interactions via generalized additive smoothers (GAMs). GAMS
# are statistical algorithms that fit the response to each predictor and accounts for
# non-linearities and each predictor's contribution to total variability in the response.

# The data streamflow is a s follows:

# 1) A simplified Montecarlo approach to resample and build n-parameter sets
# 2) Run APSIM n-times to obtain biomass outputs at each n-parameter set
# 3) Linearize APSIM outputs via GAM emulators and compute variance-partition indexes
# The final inputs required for sensitivity are
#     dframe with parameters (X), and a vector of apsim outputs(Y)
#     (3) works as a linearized function Y = g(X) + error.    
#                                            g(x) is fitted via smoothers

# The library apsimR (Stanfield, 2014) is a great way to improve communication
# between APSIM and R. However, parameters in APSIM are stored in .xml files
# in intertwined nodes that R sometimes can't read immediately.

## Last revised: 11/29/2017

###############################################################################
# 1) PARAMETER SAMPLING
# Need to create n-modified cover crop files. Each with a different parameter set

simFile<- 'Wheat_Test.xml'                         # original file to modify
apsimWd<- getwd()

simVar<- c("leaf_dm_init",
           "y_node_app_rate",
           "initial_tpla",
           "node_sen_rate",
           "node_no_correction",
           "shoot_lag",
           "shoot_rate",
           "pesw_germ",
           "initial_root_depth")                   # list of parameters to change

x<- vector(mode = "list", length = 800)            # intialize list to be filled in

realizations<- list(1)  
lower.values= list(0.0018, 57, 120, 36, 1, 24, 0.9, 0.05, 60)
upper.values= list(0.0042, 133, 280, 84, 4, 56, 2.1, 0.25, 140)  #low/up thresholds of variation

x1<- lapply(x, function(x) 
  pmap(list(realizations,
            lower.values,
            upper.values),
              function(x,y,z)
                round(runif(x,y,z), 3)             # uniform dist. for parameter perturbation       
                )
               )                                   # fill each element in x list with a parameter set
x1.t<- x1
x1.t<-lapply(x1.t, function(x){
  x[[2]]= rep(x[[2]],2)
  return(x)
})                                                 # y_app_node_rate in wheat_test.xml is duplicated


# Previously, I created 800 copies of the cover crop file to be modified. These 800 copies are stored
# in the same directory where this script should be run.
#for /l %f in (1,1,100) do copy Wheat_Test.xml Wheat_Test_%f.xml (code to be run from a terminal)


simFiles<- list.files( getwd () )[18:817]        # Each copy will get a parameter set from list above
m<-NULL
for(i in 1:length(simFiles)){
  m[i]<- edit_sim_file(file= simFiles[i],
                       wd= apsimWd,
                       var= simVar,
                       value=x1.t[[i]],          #i-th list of parameters for the new i-th xml
                       overwrite = TRUE)
}

## Re-arrange parameters in data.frame format to run apsimr library
x1.mat<- matrix( unlist(x1), nrow = 800, byrow = T)
x1.df<- as.data.frame(x1.mat) 
names(x1.df)<- simVar                            # see line 14

x1.df #X for sensitivity analysis 

###############################################################################
# 2) At this point, I had APSIM to run at each cover crop modified file
#    COLLECT APSIM, BIOMASS AND LAI OUTPUTS

library(APSIM)
library(dplyr)
path<- paste(getwd(), "/", "Sens_out_files/50PP_Density", sep = "")    #directory where outputs are
output_50<- loadApsim(path, 
                      loadAll = TRUE,
                      fill=T)                   #dframe with outputs at each modified cover crop file

output_50<- output_50[complete.cases(output_50),]
output_50<- output_50[,-c(11,13,14)]            # dropping NA's and unnecessary variables

output.sum.50<- output_50 %>% tbl_df() %>% 
  group_by(factor) %>% 
  summarise( meanBiomass = mean(WheatBiomass),
             sdBiomass= sd(WheatBiomass),
             meanLAI= mean(WheatLAI),
             sdLAI= sd(WheatLAI)) %>% 
  arrange(factor)                               # outputs averaged over years of the simulation

output.sum.50<- output.sum.50 %>% 
  mutate(par_set= factor(as.integer(gsub("ini","",factor)))) %>% #recoding number of simulation
  
output.sum.50 #Y for sensitivity analysis

###############################################################################
# 3) SENSITIVITY ANALYSIS: required
library(apsimr)
library(sensitivity)
library(ggplot2)

# Add in biomass and LAI apsim outputs in parameter dataframe
x1.df$biomass_50<- output.sum.50[-1,]$meanBiomass    # initial year not needed. Cover crops
x1.df$LAI_50<- output.sum.50[-1,]$meanLAI            # grow over two seasons (fall-spring)


system.time(emulRes4.Biomass.50<- apsim_emul_sa( y= x1.df$biomass_50,
                                                 X= x1.df[,2:9],
                                                 method = "singleGAM"))

system.time(emulRes4.LAI.50<- apsim_emul_sa( y= x1.df$LAI_50,
                                             X= x1.df[,2:9],
                                             method= "singleGAM")) #run GAM emulator


ggplot(data=emulRes4.LAI.50$Total,                             #visualization
       aes(x=Parameter, y=Est, ymin=Lower, ymax=Upper)) +
       geom_pointrange() + 
       geom_hline(yintercept=1, lty=2) +  
       coord_flip() + 
       xlab("Parameter, 50 PP") + ylab("Sensitivity index (95% CI)") +
       theme_bw()

