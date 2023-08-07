## Mendota - Monona ## 

library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)
library(zoo)
library(imputeTS)

# Vollenweider Equation # ====================
	# Estimation of P load: TPout = TPin/(1+oTw); Tpout*(1+oTw) = TPin
		# Mendota 
				# TPout = Mendota outflow into Monona 
		# Monona TPout = Monona outflow into Kegonsa 

# Preparation for Vollenweider Equations Estimates # =======================
	# Need outlet P from Mendota 
	# Need to estimate Monona Outlet P 

# Mendota TP Out + Monona TP In #=========================

# Mendota surface P concentrations # 
setwd("C:/Users/tjbut/Box Sync/Active/Active Dota-Nona Invasives")
setwd("J:/Box Sync/Active/Active Dota-Nona Invasives")
mendota_chem = read_csv('ntl1_v11_surfacenut.csv') %>% 
	filter(lakeid == 'ME') %>% 
	select(lakeid, year4, sampledate, depth, rep, totpuf, totpf, totpuf_sloh, drp_sloh, flagtotpuf, flagtotpf, flagtotpuf_sloh, flagdrp_sloh)
mendota_chem # 1995 - 2021 

# get surface nutrients # 
mendota_chem_surface = mendota_chem %>% 
  filter(depth == 0 )
mendota_chem_surface

# get annual average concentration 
mendota_totp = mendota_chem_surface %>% 
  group_by(year4) %>%
  summarize(tp = mean(totpuf_sloh, na.rm = T), 
            tp_sd = sd(totpuf_sloh, na.rm = T),
            tp2 = mean(totpuf, na.rm = T), 
            tp2_sd = sd(totpuf, na.rm = T)) %>% 
  ungroup() %>%
  mutate(tp_ugL = tp*1000, 
         tp_sd_ugL = tp_sd*1000, 
         tp2_ugL = tp2*1000,
         tp2_sd_ugL = tp2_sd*1000)
mendota_totp

# Monona Discharge 
setwd("C:/Users/tjbut/Box Sync/Active/Active Dota-Nona Invasives/gage data")
setwd("J:/Box Sync/Active/Active Dota-Nona Invasives/gage data")
monona_discharge = read_csv('ME-OUTLET.csv') %>% 
	mutate(datetime = mdy(datetime)) %>% 
	mutate(year = year(datetime))
monona_discharge

# Get annual discharge estimate # 
monona_dis_annum = monona_discharge %>% 
  group_by(year) %>% 
  summarize(dc_ft3_s = mean(discharge_ft3_s, na.rm = T), 
            dc_sd = sd(discharge_ft3_s, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(dc_L_yr = dc_ft3_s*(28.3168)*(86400)*365,
         dc_sd_L_yr = dc_sd*(28.3168)*(86400)*365)
monona_dis_annum

# Combine surface P + Discharge: 2003 - current # 
chem_join = mendota_totp %>% 
	filter(year4 > 2002) %>% 
	select(year4, tp_ugL, tp_sd_ugL)
chem_join

discharge_join = monona_dis_annum %>% 
	rename(year4 = year) %>% 
	select(year4, dc_L_yr, dc_sd_L_yr)

outlet_join = left_join(chem_join, discharge_join, by = 'year4') %>% 
	mutate(tp_lbs_L = tp_ugL/(1000000*453.592)) %>% 
	mutate(tp_kg_L = tp_lbs_L/2.20462)
outlet_join

mendota_tpout = outlet_join %>% 
	mutate(load_kg_yr = tp_kg_L*dc_L_yr)
mendota_tpout

reduced_mendota_tpout = mendota_tpout %>% 
	select(year4, load_kg_yr)

# add on 1995 - 2002 load from Lathrop & Carpenter 2014 # 
setwd("C:/Users/tjbut/Box Sync/Active/Active Dota-Nona Invasives")
derived_mendota_tpout = read_csv('Outlet P Load_Mendota.csv') %>% 
	rename(year4 = year, 
				 load_kg_yr = outletP_kg_yr) %>% 
	filter(year4 > 1994 & year4 < 2003)
derived_mendota_tpout

# Up to date Monona P load # 
mendota_tpout = rbind(reduced_mendota_tpout, derived_mendota_tpout) %>% 
	arrange(year4)
mendota_tpout

plot(load_kg_yr~year4, type = 'o', data = mendota_tpout, lwd = 2, col = 'seagreen')
mtext(side=3, 'Mendota TP out')

monona_tpin = mendota_tpout # Mendota tp out equals Monona tp in 

# Monona TP Out Estimation #=========================
# Daily Mendota Outlet Discharge # 
setwd("C:/Users/tjbut/Box Sync/Active/Active Dota-Nona Invasives/gage data")
setwd("J:/Box Sync/Active/Active Dota-Nona Invasives/gage data")
mendota_outlet_discharge = read_csv('ME-OUTLET.csv') %>% 
	mutate(datetime = mdy(datetime)) %>% 
	mutate(year = year(datetime)) %>% 
	mutate(outlet = 'mendota')
mendota_outlet_discharge

# Daily Waubesa Outlet Discharge # 
setwd("C:/Users/tjbut/Box Sync/Active/Active Dota-Nona Invasives/gage data")
setwd("J:/Box Sync/Active/Active Dota-Nona Invasives/gage data")
waubesa_outlet_discharge = read_csv('WAUB-OUTLET.csv') %>% 
	mutate(datetime = mdy(datetime)) %>%
	mutate(year = year(datetime)) %>% 
	mutate(outlet = 'waubesa')
waubesa_outlet_discharge

# Regression between Mendota x Waubesa discharge to back-interpolate 1995 - 2003 Mendota Outlet Discharge 
mendota_out_reduce = mendota_outlet_discharge %>% 
	select(outlet, year, datetime, discharge_ft3_s) %>% 
	rename(mendota_outlet_ft3_s = discharge_ft3_s)
mendota_out_reduce

waubesa_out_reduce = waubesa_outlet_discharge %>%
	select(outlet, year, datetime, discharge_ft3_s) %>%
	rename(waubesa_outlet_ft3_s = discharge_ft3_s)
waubesa_out_reduce

outlet_regression = left_join(mendota_out_reduce, waubesa_out_reduce, by = 'datetime') %>% 
	rename(year = year.x) %>% 
	select(year, datetime, mendota_outlet_ft3_s, waubesa_outlet_ft3_s)
outlet_regression

mod1 = lm(mendota_outlet_ft3_s~waubesa_outlet_ft3_s, data = outlet_regression)
summary(mod1)
#plot(mod1) Looks pretty decent actually 

plot(mendota_outlet_ft3_s~waubesa_outlet_ft3_s, data = outlet_regression, pch = 19)
abline(mod1, col = 'red')
summary(mod1)
	# Mendota discharge (ft3/s) = Waubesa discharge (ft3/s)*(0.580242) + 10.516636

# Calculate Mendota discharge from 1995 - 2003-11-07 
waubesa_out_reduce

derived_mendota_discharge = waubesa_out_reduce %>% 
	filter(datetime <= as.Date('2003-11-07')) %>% 
	rename(outlet.origin = outlet) %>% 
	mutate(derived_mendota_ft3_s = (waubesa_outlet_ft3_s*0.580242) + (10.516636)) %>% 
	mutate(outlet = 'mendota') %>% 
	select(outlet, year, datetime, derived_mendota_ft3_s) %>% 
	rename(mendota_outlet_ft3_s = derived_mendota_ft3_s)
derived_mendota_discharge

mendota_out_reduce_filledin = rbind(derived_mendota_discharge, mendota_out_reduce)
mendota_out_reduce_filledin # Esimtated and recorded (2003-11-08 on) Mendota outlet discharge 
plot(mendota_out_reduce_filledin$mendota_outlet_ft3_s~mendota_out_reduce_filledin$datetime, type = 'l')

# Monona Discharge is 48% of the flow gained between the Mendota and Waubesa outflow (McDonal & Lathrop 2017)
	# So will combine mendota and waubesa outflow, take 48% of the waubesa outflow to get Monona outflow
	# That will give dataset with outflow of all three lakes 
plot(waubesa_out_reduce$waubesa_outlet_ft3_s ~ waubesa_out_reduce$datetime, type = 'l')
mendota_out_reduce_filledin
waubesa_join = waubesa_out_reduce %>% 
	select(datetime, waubesa_outlet_ft3_s)
waubesa_join

dota_besa_join = left_join(mendota_out_reduce_filledin, waubesa_join, by = 'datetime') %>% 
	select(!(outlet)) # information is given in column names
dota_besa_join

discharge_out = dota_besa_join %>% 
	mutate(monona_outlet_ft3_s = (waubesa_outlet_ft3_s+mendota_outlet_ft3_s)*0.48, 
				 monona_outlet_lower = (waubesa_outlet_ft3_s+mendota_outlet_ft3_s)*0.43, # lower range 
				 monona_outlet_upper = (waubesa_outlet_ft3_s+mendota_outlet_ft3_s)*0.52) %>% # upper range
	select(year, datetime, mendota_outlet_ft3_s, monona_outlet_ft3_s, waubesa_outlet_ft3_s) # include upper lower if wanted 
discharge_out

# Monona outlet discharge 
monona_outlet_discharge = discharge_out %>% 
	select(year, datetime, monona_outlet_ft3_s)
monona_outlet_discharge


# join with nutrient data # 
setwd("C:/Users/tjbut/Box Sync/Active/Active Dota-Nona Invasives")
setwd("J:/Box Sync/Active/Active Dota-Nona Invasives")
monona_chem = read_csv('ntl1_v11_surfacenut.csv') %>% 
	filter(lakeid == 'MO') %>% 
	select(lakeid, year4, sampledate, depth, totpuf, totpuf_sloh) %>% 
	# totpuf = ug/L; totpuf_sloh = mg/L 
	filter(depth == 0) %>% 
	mutate(totpuf_sloh = totpuf_sloh*1000) %>% # Now both are ug/L 
	mutate(totpuf_comb = coalesce(totpuf, totpuf_sloh)) %>% 
	mutate(datetime = mdy(sampledate)) %>% 
	select(!(sampledate))
monona_chem # 1995 - 2021 

monona_chem_reduce = monona_chem %>% 
	select(datetime, totpuf_comb)

monona_chemjoin = full_join(monona_outlet_discharge, monona_chem_reduce, by = 'datetime')
monona_chemjoin

monona_monthly = monona_chemjoin %>% 
	mutate(year.mon = as.yearmon(datetime, '%b-%Y')) %>% 
	group_by(year.mon) %>%
	summarize(totpuf_comb = mean(totpuf_comb, na.rm = T), 
						sd_totpuf = sd(totpuf_comb, na.rm = T), 
						monona_outlet_ft3_s = mean(monona_outlet_ft3_s, na.rm = T),
						sd_outlet = sd(monona_outlet_ft3_s, na.rm = T)) %>% 
	ungroup()
monona_monthly

plot(totpuf_comb~year.mon, data = monona_monthly)

mo.nut.ts = ts(monona_monthly$totpuf_comb, start = c(1995, 1), end = c(2020, 12), frequency = 12)
mo.nut.ts.interp = na_interpolation(mo.nut.ts) # NAs in time series need to fill - impute with Kalman Filter 
plot(mo.nut.ts.interp)

# Flow weighted # =================================

# Vollenweider to predict annual mean TP concentration in the lake # 
	#TP = TPin/(1 + tw^0.5)

# Mendota In 
	# Need: Sum(Daily discharge*Daily P Load) 
	# Need: Sum(Discharge)

# Mendota In Raw Values # =======================

# packages # 
library(tidyverse)
library(lubridate)  
  
# sum together tributaries when possible # 
	# PB solo: 1992 - 2001 
	# PB + YR: 2002 - 2011
	# All: 2012 - 2022 

# Perform following interpolation as suggested by USGS Todd 
	# Yahara River Loads: Regression of HWY 113 & Windsor Gage at Yahara River 2003 - 2019 
		## Yahara P load = 0.9319*(P load at Windsor YYYY) + 7400.9

	# Dorn Creek Loads (1990 - 2012): Regression between Windsor annual loads 2013 - 2019 
		## Dorn Creek M P load = 6E-06*(P load at Windsor YYYY)^2 + 0.2642*(P load at Windsor YYYY) + 952.68

	# Sixmile Creek (Dorn Creek Q) P Loads (1990 - 2012): 2E-05*(P load at Windsor YYYY)^2+0.558*(Windsor YYYY)+6658.3  

# Windsor loads 
setwd("C:/Users/tjbut/Box Sync/Active/Active Dota-Nona Invasives/gage data")
windsor_load = read_csv('W-YR.csv') %>% # 1995 - 2011
	mutate(totp_kg_d = total_p_lbs_d/2.20462) %>% 
	mutate(year = year(datetime)) %>% 
	filter(year > 1994 & year < 2012)
windsor_load

# Stuntebeck Loads # 
setwd("C:/Users/tjbut/Box Sync/Active/Active Dota-Nona Invasives")
stuntebeck_estimated_loads = read_csv('Stuntebeck_ME_estimate.csv') %>% 
	filter(year > 1994 & year < 2013)
stuntebeck_estimated_loads

# Join Mendota P load # 
mendota_pload_gage = mendota_pload_annum_gages %>% # Directly estimated from the gages 
	select(year, load_kg_yr) %>% 
	filter(year > 2012)
mendota_pload_gage
stuntebeck_estimated_loads = stuntebeck_estimated_loads %>% # Stuntebeck back estimated through regressions with Windsor load 
	mutate(load_kg_yr = pload_lbs/2.20462) %>% 
	select(year, load_kg_yr)
stuntebeck_estimated_loads

mendota_load_final = rbind(stuntebeck_estimated_loads, mendota_pload_gage)
mendota_load_final

windows(height = 4, width = 6)
plot(load_kg_yr~year, data = mendota_load_final, type = 'o',
		 ylim = c(0, 75000), lwd = 2, col = 'black', 
		 ylab = 'P load (kg)', xlab = 'Year')

## Gage Data 1995 - present ##=======================
library(here)
here('gage_dat')
setwd(here('gage_dat'))

# Yahara River (Windsor): 1995 - present 
yr_windsor = read_csv('yr_windsor_gage.csv')
yr_windsor

# Yahara River (HWY 113): 2002 - present
yr_hwy113 = read_csv('yr_hwy113_gage.csv')
yr_hwy113

# Pheasant Branch: 1995 - present 
pb = read_csv('pb_gage.csv')
pb

# Dorn Creek: 2012 - present 
dc = read_csv('dc_gage.csv')
dc

# Sixmile Creek: 2012 - present 
smc = read_csv('smc_gage.csv')
smc

# Perform following interpolation as suggested by USGS Todd - Discharge #==========================

yr_hwy113_dis = yr_hwy113 %>% 
	mutate(gage = 'yr_hwy113') %>%
	select(gage, datetime, discharge_ft3_s, discharge_flag) %>% 
	mutate(datetime = mdy(datetime))
yr_hwy113_dis

yr_windsor_dis = yr_windsor %>% 
	mutate(gage = 'yr_windsor') %>%
	select(gage, datetime, discharge_ft3_s, discharge_flag) %>% 
	filter(datetime > '2001-12-31')
yr_windsor_dis

yr_join = rbind(yr_hwy113_dis, yr_windsor_dis)
yr_join

library(zoo)
yr_wide = yr_join %>% 
	filter(discharge_ft3_s >0) %>%
	pivot_wider(names_from = gage, 
							values_from = discharge_ft3_s) %>% 
	drop_na()
yr_wide

library(ggplot2)
ggplot(yr_wide, aes(x = yr_windsor, y = yr_hwy113)) + 
	geom_point()

# Randomly shuffle data # 
df.shuffled = yr_wide[sample(nrow(yr_wide)), ]

# define number of folds to use for k-fold cross-validation 
K = 10

# Define degree of polynomials to fit 
degree = 5

# Create k equal-sized folds 
folds = cut(seq(1, nrow(df.shuffled)), breaks = K, labels = F)

# Create object to hold MSE's of models 
mse = matrix(data = NA, nrow = K, ncol = degree)

# Perform K-fold cross-validation 
for(i in 1:K){
    
    #define training and testing data
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- df.shuffled[testIndexes, ]
    trainData <- df.shuffled[-testIndexes, ]
    
    #use k-fold cv to evaluate models
    for (j in 1:degree){
        fit.train = lm(log(yr_hwy113) ~ poly(log(yr_windsor), j), data=trainData)
        fit.test = predict(fit.train, newdata=testData)
        mse[i,j] = mean((fit.test-testData$yr_hwy113)^2) 
    }
}

# find MSE for each degree 
colMeans(mse) # linear model = lowest MSE 

# Analyze the Final Model # 
best = lm(log(yr_hwy113) ~ log(yr_windsor), data = yr_wide)
summary(best)
plot(best)
plot(log(yr_hwy113) ~ log(yr_windsor), data = yr_wide)
abline(best, col = 'red')

# Back-calculate Yahara discharge # 
	## log(yr_hwy113_dis) = 0.742445*log(yr_windsor) + 1.898055 

yr_hwy113_dis
yr_hwy113_backdate = yr_windsor %>% 
	filter(datetime < '2002-01-01') %>% 
	mutate(backdate = (0.742445*log(discharge_ft3_s)) + 1.898055) %>% 
	select(datetime, backdate) %>% 
	rename(discharge_ft3_s = backdate) %>% 
	mutate(discharge_flag = 'A:e', 
				 gage = 'interpolated_windsor') %>% 
	mutate(discharge_ft3_s = exp(discharge_ft3_s)) %>% 
	select(gage, datetime, discharge_ft3_s, discharge_flag)
yr_hwy113_backdate

yr_hwy113_dis_interp = rbind(yr_hwy113_dis, yr_hwy113_backdate) %>% arrange(datetime) %>% 
yr_hwy113_dis_interp

plot(discharge_ft3_s ~ datetime, data = yr_hwy113_dis_interp, type = 'l')
abline(h = 0)

# Calculate interpolated Yahara load #

yr_hwy113_p = yr_hwy113 %>% 
	mutate(gage = 'yr_hwy113') %>%
	select(gage, datetime, p_lbs_d, p_flag) %>% 
	mutate(datetime = mdy(datetime))
yr_hwy113_p

yr_windsor_p = yr_windsor %>% 
	mutate(gage = 'yr_windsor') %>%
	select(gage, datetime, p_lbs_d, p_flag) %>% 
	filter(datetime > '2001-12-31')
yr_windsor_p

yr_join = rbind(yr_hwy113_p, yr_windsor_p)
yr_join

library(zoo)
yr_wide = yr_join %>% 
	filter(p_lbs_d >0) %>%
	pivot_wider(names_from = gage, 
							values_from = p_lbs_d) %>% 
	drop_na() %>% 
	mutate(lg.yr_windsor = log(yr_windsor), 
				 lg.yr_hwy113 = log(yr_hwy113))
yr_wide

library(ggplot2)
ggplot(yr_wide, aes(x = lg.yr_windsor, y = lg.yr_hwy113)) + 
	geom_point()

# Randomly shuffle data # 
df.shuffled = yr_wide[sample(nrow(yr_wide)), ]

# define number of folds to use for k-fold cross-validation 
K = 10

# Define degree of polynomials to fit 
degree = 5

# Create k equal-sized folds 
folds = cut(seq(1, nrow(df.shuffled)), breaks = K, labels = F)

# Create object to hold MSE's of models 
mse = matrix(data = NA, nrow = K, ncol = degree)

# Perform K-fold cross-validation 
for(i in 1:K){
    
    #define training and testing data
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- df.shuffled[testIndexes, ]
    trainData <- df.shuffled[-testIndexes, ]
    
    #use k-fold cv to evaluate models
    for (j in 1:degree){
        fit.train = lm(yr_hwy113 ~ poly(yr_windsor, j), data=trainData)
        fit.test = predict(fit.train, newdata=testData)
        mse[i,j] = mean((fit.test-testData$yr_hwy113)^2) 
    }
}

# find MSE for each degree 
colMeans(mse) # linear model = lowest MSE 

# Analyze the Final Model # 
best = lm(lg.yr_hwy113 ~ lg.yr_windsor, data = yr_wide)
summary(best)
plot(best)
plot(lg.yr_hwy113 ~ lg.yr_windsor, data = yr_wide)
abline(best, col = 'red')

yr_hwy113_back_p = yr_windsor %>% 
	select(datetime, p_lbs_d, p_flag) %>% 
	filter(datetime < '2002-01-01') %>% 
	mutate(lg.p_lbs_d = (0.576899*p_lbs_d) + 2.045695) %>%
	mutate(p_lbs_d = exp(lg.p_lbs_d))
yr_hwy113_back_p
plot(p_lbs_d~datetime, data = yr_hwy113_back_p, ylim = c(0, 60))

# Dorn Creek Loads (1990 - 2012): Regression between Windsor annual loads 2013 - 2019 
		## Dorn Creek M P load = 6E-06*(P load at Windsor YYYY)^2 + 0.2642*(P load at Windsor YYYY) + 952.68

	# Sixmile Creek (Dorn Creek Q) P Loads (1990 - 2012): 2E-05*(P load at Windsor YYYY)^2+0.558*(Windsor YYYY)+6658.3  

