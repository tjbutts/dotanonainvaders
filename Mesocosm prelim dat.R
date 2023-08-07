## Mesocosm Data Viz ##=======================
library(lubridate)
library(tidyverse)

## Calculate Mendota P load ## 
phosphorus_load = read_csv('pload.csv')
phosphorus_load

# Mean annual load of g_m3 # 
me_annum_pload = phosphorus_load %>% 
	filter(lake == 'mendota') %>% 
	summarize(load_g_m3 = mean(load_g_m3))
me_annum_pload # 0.0562 g/m3 

# 0.154 ug/L per day 
# Add 1.386 ug to each 9 L mesocosm per day 

## Mesocosm Prelim Data ##====================
prelim_dat = read_csv('mesocosm_dat.csv')
prelim_dat

dat = prelim_dat %>% 
	mutate(date = mdy(date)) %>%
	mutate(julian = yday(date)) %>% 
	as.data.frame()

plot(dat[dat$mesocosm == 1, 'julian'], dat[dat$mesocosm == 1, 'chl_rfu'], 
		 xlab = 'doy', ylab = 'Chlorophyll-a (RFU)', type = 'o', lwd = 3, ylim = c(0, 1500))
points(dat[dat$mesocosm == 2, 'julian'], dat[dat$mesocosm == 2, 'chl_rfu'],
			 type = 'o', lwd = 3, col = 'seagreen3')
points(dat[dat$mesocosm == 3, 'julian'], dat[dat$mesocosm == 3, 'chl_rfu'],
			 type = 'o', lwd = 3, col = 'dodgerblue3')
points(dat[dat$mesocosm == 4, 'julian'], dat[dat$mesocosm == 4, 'chl_rfu'],
			 type = 'o', lwd = 3, col = 'orchid4')
abline(h = 0)

plot(dat[dat$mesocosm == 1, 'julian'], dat[dat$mesocosm == 1, 'pc_rfu'], 
		 xlab = 'doy', ylab = 'Phycocyanin (RFU)', type = 'o', lwd = 3, ylim = c(0, 5000))
points(dat[dat$mesocosm == 2, 'julian'], dat[dat$mesocosm == 2, 'pc_rfu'],
			 type = 'o', lwd = 3, col = 'seagreen3')
points(dat[dat$mesocosm == 3, 'julian'], dat[dat$mesocosm == 3, 'pc_rfu'],
			 type = 'o', lwd = 3, col = 'dodgerblue3')
points(dat[dat$mesocosm == 4, 'julian'], dat[dat$mesocosm == 4, 'pc_rfu'],
			 type = 'o', lwd = 3, col = 'orchid4')
abline(h = 0)
