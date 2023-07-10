## Mendota P loading estimate ##========================

# packages # 
library(tidyverse)
library(lubridate)  
  
# Current location of the data # 
setwd("C:/Users/tjbut/Box Sync/Active/Active Dota-Nona Invasives/gage data")
setwd("J:/Box Sync/Active/Active Dota-Nona Invasives/gage data")
pload = read_csv('USGS_gages_compiled.csv') %>% 
	mutate(date = mdy(date)) %>% 
	mutate(year = year(date)) %>% 
	select(gage, date, year, total_p_lbs_d)
pload

pload_annum_lbs = pload %>%
	group_by(gage, year) %>% 
	summarize(annual_load = mean(total_p_lbs_d), 
						annual_load_sd = sd(total_p_lbs_d)) %>%
	ungroup() %>% 
	mutate(annual_load_lbs_yr = (annual_load*365)) %>%
	mutate(annual_load_lbs_yr = (annual_load*365))
pload_annum_lbs 

# sum together tributaries when possible # 
	# PB solo: 1992 - 2001 
	# PB + YR: 2002 - 2011
	# All: 2012 - 2022 

mendota_pload_annum_gages = pload_annum_lbs %>% 
	group_by(year) %>% 
	summarize(sum_annual_load_lbs_yr = sum(annual_load_lbs_yr)) %>% 
	ungroup() %>% 
	filter(year > 2011) %>% 
	mutate(load_kg_yr = sum_annual_load_lbs_yr/2.20462)
mendota_pload_annum_gages # only accurate to 2012 - 2022 when all gages are used 

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

## Monona P loading estimate ##===================

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

monona_load = outlet_join %>% 
	mutate(load_kg_yr = tp_kg_L*dc_L_yr)
monona_load

reduced_monona_load = monona_load %>% 
	select(year4, load_kg_yr)

# add on 1995 - 2002 load from Lathrop & Carpenter 2014 # 
setwd("C:/Users/tjbut/Box Sync/Active/Active Dota-Nona Invasives")
derived_monona_load = read_csv('Outlet P Load_Mendota.csv') %>% 
	rename(year4 = year, 
				 load_kg_yr = outletP_kg_yr) %>% 
	filter(year4 > 1994 & year4 < 2003)
derived_monona_load

# Up to date Monona P load # 
monona_load_final = rbind(reduced_monona_load, derived_monona_load) %>% 
	arrange(year4)
monona_load_final

points(load_kg_yr~year4, type = 'o', data = monona_load_final, lwd = 2, col = 'seagreen')
legend('topleft', legend = c('Mendota', 'Monona'), col = c('black', 'seagreen'), pch = 1, lwd = 2)

## Consolidate Loads - convert to volumetric loads ## =========================
mendota_load_final
monona_load_final

mendota_vol_load = mendota_load_final %>%
	mutate(load_g_m3 = (load_kg_yr*1000)/(505*10^6))
mendota_vol_load

monona_vol_load = monona_load_final %>%
	mutate(load_g_m3 = (load_kg_yr*1000)/(110*10^6))
monona_vol_load

windows(height = 4, width = 6)
plot(load_g_m3~year, data = mendota_vol_load, type = 'o',
		 ylim = c(0, 0.4), lwd = 2, col = 'black', 
		 ylab = 'P load (g/m^3)', xlab = 'Year')
points(load_g_m3~year4, type = 'o', data = monona_vol_load, lwd = 2, col = 'seagreen')
legend('topleft', legend = c('Mendota', 'Monona'), col = c('black', 'seagreen'), pch = 1, lwd = 2)

# Create Dataset #============================
join1 = mendota_vol_load %>% 
	mutate(lake = 'mendota')
join1

join2 = monona_vol_load %>% 
	mutate(lake = 'monona') %>%
	rename(year = year4)
join2

phosphorus_load = rbind(join1, join2) %>% arrange(lake)
phosphorus_load

# write .csv # 
#setwd("C:/Users/tjbut/Box Sync/Active/Active Dota-Nona Invasives/derived data")
#write_csv(phosphorus_load, 'pload.csv')

