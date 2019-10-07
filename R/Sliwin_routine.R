##
##
## Sliding window analysis as implemented in: ##
## 
##  Capilla-Lasheras et al. Altruistic bet-hedging in an arid zone cooperative bird ##
##
##

##libraries
library(dplyr)
library(lme4)
library(doSNOW)
library(data.table)


##### Data frames for analysis #####
Biological_data <- clutch_df
BIOLOGICAL_DATE <- "hatch_date"
Weather_data <- weather

##
## In the code that follows, change 'RAINFALL' (name of the variable with rainfall data in Weather_data)
## and 'RESPONSE' to make it run with your own data
##

##### Setting up windows of interest #####

# total length 80 in intervals of 1 (days, weeks, etc)
start <- seq(0,80, 1) 
stop <- seq(0,80,1)
df00 <- expand.grid(start = start, stop = stop)
df00 <- df00 %>% 
  filter(stop < start) %>%  # removes redundant windows
  filter(start - stop >=4)  # removes windows shorter than 4 days

matrix.window <- df00       # final table with windows for analysis
matrix.window$AIC_model <- as.vector(NA) # variable to store AIC values


##### Seting up cluster for parallel computation #####
n_cores <- 4                             # number of nodes
parallel_cluster <- makeCluster(n_cores) # initiate cluster
registerDoSNOW(parallel_cluster)
getDoParWorkers()

# attaching librares to cluster library 
clusterEvalQ(cl = parallel_cluster, library(dplyr))
clusterEvalQ(cl = parallel_cluster, library(data.table))
#clusterEvalQ(cl = parallel_cluster, library(lubridate))
clusterEvalQ(cl = parallel_cluster, library(doSNOW))

## IN THIS EXAMPLE, A RAINFALL INDEX IS CALCULATED (I.E. TOTAL RAINFALL)
Biological_data$RAINFALL_INDEX <- NA

# exporting data to cluster
clusterExport(parallel_cluster, 
              c("Biological_data", 
                "BIOLOGICAL_DATE",
                "matrix.window", 
                "Weather_data"))

##### Sliding window routine #####
model_output_sliwin <- as.list(NA) # list to store models
pbar <- txtProgressBar(min = 1,    # progress bar
                       max = nrow(matrix.window), 
                       initial = 1, 
                       style = 3)

# loop to iterate over every window
for (m in 1:nrow(matrix.window)){
  rainfall_start <- matrix.window$start[m]
  rainfall_stop <- matrix.window$stop[m]
  
  clusterExport(parallel_cluster, 
                c("rainfall_start", 
                  "rainfall_stop"))
  
  par <- parLapply(cl = parallel_cluster, 
                   x = as.list(1:nrow(Biological_data)), 
                   fun = function(x) {
                     
                     ref.date <- ymd(Biological_data[[BIOLOGICAL_DATE]])[x]
                     
                     # appropiatte short window
                     Biological_data$RAINFALL_INDEX[x] <- {Weather_data %>% 
                         filter(date >= (ref.date-rainfall_start)) %>%
                         filter(date <= (ref.date-rainfall_stop)) %>%
                         summarise(new_rainfall = sum(RAINFALL))}$new_rainfall
                     return(Biological_data[x,])
                   })
  new_data <- rbindlist(par)
  par <- NULL
  
  # model with new rainfall index
  sliwin_model <- lm(RESPONSE ~ RAINFALL_INDEX, data = new_data)
                               
  
  # storing model and AIC value
  matrix.window$AIC_model[m] <- AIC(sliwin_model) # AIC
  model_output_sliwin[[m]] <- sliwin_model        # full model output  

  # clean working dir for next iteration
  
  model_output_sliwin <- as.list(NA)
  sliwin_model <- NULL

  # progress bar update
  setTxtProgressBar(pb = pbar, value = m)
}
stopCluster(parallel_cluster) # stop cluster


##### NULL MODEL #####
sliwin_model_null <- lm(RESPONSE ~ 1, data = new_data)
matrix.window$AIC_null <- AIC(sliwin_model_null)

##
##
## Results are stored in 'matrix.window' and 'model_output_sliwin'
##
##




