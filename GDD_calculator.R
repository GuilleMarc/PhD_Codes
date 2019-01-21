### Cover crop phenology project, sample code  ###################################

# An simple cumulative GDD and Rainfall calculator.
# Inputs required: daily temperature records along with a timestamp
# Degree days correlate well with phenological stages.


## Last revised: 11/25/2017


###############          WEATHER CALCULATOR              ##########################
#         outputs: a dataframe with cumulative GDD and Rainfall
#   inputs(weather dataframe, sowing date, end date, base temperature)

weather<- function(x, sdate, tdate, tb){           
  x$meant= (x$maxt + x$mint)/2                     #daily mean temp
  weather.records= data.frame(date=x$valid,        #timestamp
                              maxt= x$maxt, 
                              mint=x$mint, 
                              meant=x$meant,
                              rain= x$rain)        
              if(sdate > tdate){                   #index###
                  stop("Termination date should be greater than seeding date")
              } else {
                i.start= which(weather.records$date == sdate)
                i.end= which(weather.records$date == tdate)       
              }                                    ########
        gdd= sapply( weather.records$meant[i.start:i.end], function(x){
                                                           ifelse (x > 0, x - tb, 0)
                                                           })
        cgdd= cumsum(gdd)
        
        rain.d= weather.records$rain[i.start:i.end]
        crain= cumsum(rain.d)
         
  return(cbind( data.frame(date= weather.records$date[i.start : i.end],
                           maxt= weather.records$maxt[i.start : i.end],
                           mint= weather.records$mint[i.start : i.end],
                           meant= weather.records$meant[i.start: i.end]),
                gdd, cgdd, rain.d, crain))
}

###############       A wrapper function to get GDD and rain per site per date ##################

data1= read.csv("Weather_15_17.csv", colClasses = c(valid="Date"))
weather.cumul.fn<- function(df, site, sdate){
                   df.tempo=  as.data.frame(df)
                   site.key= df[which(df.tempo$site == as.character(site)),]
                   cgdd.tempo= lapply(as.Date(site.key$date), function(x) {
                                                                weather(data1, 
                                                                sdate = as.character(sdate),
                                                                tdate= x, 
                                                                tb=0)
                                                               }
                                                              )
                   cumul.weather= list(cgdd= unique(sapply(cgdd.tempo, function(x) max(x$cgdd))),
                                       crain= unique(sapply(cgdd.tempo, function(x) max(x$crain)))
                                      )
  return(cumul.weather)
}



##### NOT PRINT: ######################################################################
### An example run for weather recorded at 2 locations (excel file, a location per tab)
# Original source: Iowa Mesonet, 2014                 

# First a function to import data safely

read.data<- function (file, sheet){                   
  require(readxl)
  sheets= readxl::excel_sheets(file)
  sheet.num= which( sheets == sheet)
  return.sheet= readxl::read_excel(file,
                                   sheet = sheet,
                                   na= "na",
                                   col_names = T)
  final.sheet= return.sheet[!is.na(return.sheet$date),]
  return(final.sheet)
}


isuag<- read.data("LeafStage_15_17.xlsx", "isuag")      #label for isuag site
kelly<- read.data("LeafStage_15_17.xlsx", "kelly")      #label for kelly site
both.sites<- rbind(isuag, kelly)


pl.pheno<- both.sites %>%
  separate(season, c("season","year")) %>%              #split season.year
  mutate(year= factor(as.numeric(year) + 2000)) %>%     #add 2000 to two-digit year
  mutate(year.est= factor(year.est)) %>%
  map_df(., function(x) {
    if(typeof(x)=="character"){
      x= type.convert(as.character(x), as.is=FALSE)
    }else{
      x=x
    }
  }
 )    

# Data is from 2 years. Need to limit GDD sum for 1 season only
## example for kelly site for year 1 (2015-2016)
kelly.pheno.year1<- pl.pheno %>%                                        
  unite(season.year, season, year, sep=".", remove=FALSE) %>%
  filter(season.year %in% c("fall.2015","spr.2016")) %>%
  filter(site=="kelly")



######Not run:   #########
# Application: Compute GDDs and Rainfall for year @ kelly site
# weather.cumul.fn(kelly.pheno.year1, "kelly","2015-09-21")
# 

# Note line 46, the wrapper needs more work to allows a user
# to call function on a different weather df
######End not run ########












