## LTER covariates (water quality) ## 

## packages ## 
library(tidyverse)
library(lubridate)
library(car)

## P loading data ##================ 

pload = read_csv('pload.csv')
pload

dota_load = pload %>% filter(lake == 'mendota')
nona_load = pload %>% filter(lake == 'monona')

# Assess ##=======================
	# Annual Mean 
	# Annual Standard Deviation 
	# Average Monthly Maximum 
	# Average Monthly Standard Deviation 

## LTER data ##=======================

### Relationship with phytoplankton biomass ###=======================
# Package ID: knb-lter-ntl.88.31 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Phytoplankton - Madison Lakes Area 1995 - current.
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/88/31/f2de15b2fff6ae962a04c150c0a1c510" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "lakeid",     
                    "year4",     
                    "sampledate",     
                    "sta",     
                    "depth_range",     
                    "division",     
                    "taxa_name",     
                    "gald",     
                    "cells_per_nu",     
                    "nu_per_ml",     
                    "cells_per_ml",     
                    "biovolume_conc",     
                    "biomass_conc",     
                    "relative_total_biovolume",     
                    "genus"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt1$sta)!="factor") dt1$sta<- as.factor(dt1$sta)
if (class(dt1$depth_range)!="factor") dt1$depth_range<- as.factor(dt1$depth_range)
if (class(dt1$division)!="factor") dt1$division<- as.factor(dt1$division)
if (class(dt1$taxa_name)!="factor") dt1$taxa_name<- as.factor(dt1$taxa_name)
if (class(dt1$gald)=="factor") dt1$gald <-as.numeric(levels(dt1$gald))[as.integer(dt1$gald) ]               
if (class(dt1$gald)=="character") dt1$gald <-as.numeric(dt1$gald)
if (class(dt1$cells_per_nu)=="factor") dt1$cells_per_nu <-as.numeric(levels(dt1$cells_per_nu))[as.integer(dt1$cells_per_nu) ]               
if (class(dt1$cells_per_nu)=="character") dt1$cells_per_nu <-as.numeric(dt1$cells_per_nu)
if (class(dt1$nu_per_ml)=="factor") dt1$nu_per_ml <-as.numeric(levels(dt1$nu_per_ml))[as.integer(dt1$nu_per_ml) ]               
if (class(dt1$nu_per_ml)=="character") dt1$nu_per_ml <-as.numeric(dt1$nu_per_ml)
if (class(dt1$cells_per_ml)=="factor") dt1$cells_per_ml <-as.numeric(levels(dt1$cells_per_ml))[as.integer(dt1$cells_per_ml) ]               
if (class(dt1$cells_per_ml)=="character") dt1$cells_per_ml <-as.numeric(dt1$cells_per_ml)
if (class(dt1$biovolume_conc)=="factor") dt1$biovolume_conc <-as.numeric(levels(dt1$biovolume_conc))[as.integer(dt1$biovolume_conc) ]               
if (class(dt1$biovolume_conc)=="character") dt1$biovolume_conc <-as.numeric(dt1$biovolume_conc)
if (class(dt1$biomass_conc)=="factor") dt1$biomass_conc <-as.numeric(levels(dt1$biomass_conc))[as.integer(dt1$biomass_conc) ]               
if (class(dt1$biomass_conc)=="character") dt1$biomass_conc <-as.numeric(dt1$biomass_conc)
if (class(dt1$relative_total_biovolume)=="factor") dt1$relative_total_biovolume <-as.numeric(levels(dt1$relative_total_biovolume))[as.integer(dt1$relative_total_biovolume) ]               
if (class(dt1$relative_total_biovolume)=="character") dt1$relative_total_biovolume <-as.numeric(dt1$relative_total_biovolume)
if (class(dt1$genus)!="factor") dt1$genus<- as.factor(dt1$genus)
                
phyto_biomass = as_tibble(dt1) %>% 
	filter(lakeid == 'ME' | lakeid == 'MO') %>%
	rename(year = year4) %>% 
	select(lakeid, sampledate, year, depth_range, division, taxa_name, genus, gald, biomass_conc)
phyto_biomass

total_daily_phyto = phyto_biomass %>% 
	group_by(lakeid, sampledate, year, depth_range) %>%
	summarize(biomass_conc = sum(biomass_conc), 
						mean_gald = mean(gald, na.rm = T), 
						gald_sd = sd(gald, na.rm = T)) %>% 
	ungroup()
total_daily_phyto

total_daily_phyto_ME = total_daily_phyto %>%
	filter(lakeid == 'ME')
total_daily_phyto_MO = total_daily_phyto %>%
	filter(lakeid == 'MO')

plot(biomass_conc~sampledate, type = 'l', data = total_daily_phyto_ME, ylim = c(0,20))
plot(biomass_conc~sampledate, type = 'l', data = total_daily_phyto_MO, ylim = c(0,20))

plot(mean_gald~sampledate, type = 'o', data = total_daily_phyto_ME)
plot(mean_gald~sampledate, type = 'o', data = total_daily_phyto_MO)

# biomass_conc = biomass concentration (mg/L)
# gald = Average greatest axial linear dimension (um)

# Mess around with time series statistics - attempt to deseasonalize #=====================

## Don't know if I'm doing this correctly 
library(forecast)
library(ggplot2)
library(zoo)
library(imputeTS)

# Phytoplankton data - Mendota #==================================
phyto.grouped = phyto_biomass %>% 
	filter(lakeid == 'ME') %>% 
	mutate(year.mon = as.yearmon(sampledate, '%b-%Y')) %>% 
	group_by(year.mon) %>%
	summarize(community_biomass = sum(biomass_conc)) %>% 
	ungroup() %>%
	complete(year.mon = seq(min(year.mon), max(year.mon), 1/12))
	# group_by(year) %>%
	# complete(month = seq(min(month), max(month), 1/12)) %>% 
	# ungroup()
phyto.grouped


phyto.ts = ts(phyto.grouped$community_biomass, start = c(1995, 1), end = c(2020, 12), frequency = 12)
phyto.ts.kal = na_kalman(phyto.ts) # NAs in time series need to fill - impute with Kalman Filter 
plot(phyto.ts.kal)

# Decompose phyto time series # 
phyto.decomp = decompose(phyto.ts.kal)
plot(phyto.decomp)

# Seasonally adjust data # 
phyto.ts.kal_seasadjust = phyto.ts.kal - phyto.decomp$seasonal
plot(phyto.ts.kal_seasadjust)

#### ANCOVA Analysis #========================

# phytoplankton #=============================
phyto.annum.avg.ME = phyto.grouped %>% 
	mutate(year = year(year.mon), 
				 month = month(year.mon, label = T)) %>% 
	drop_na() %>% 
	group_by(year) %>% 
	summarize(mean_biomass = mean(community_biomass, na.rm = T), 
						sd_biomass = sd(community_biomass, na.rm = T), 
						n = n()) %>% 
	ungroup() %>% 
	mutate(se_biomass = sd_biomass/sqrt(n)) %>% 
	mutate(t_score = qt(p=0.05/2, df = n - 1, lower.tail = F)) %>%
	mutate(marg.err = t_score*se_biomass) %>% 
	mutate(l95 = mean_biomass - marg.err, 
				 u95 = mean_biomass + marg.err)
phyto.annum.avg.ME

ggplot(phyto.annum.avg.ME, aes(x = year, y = mean_biomass)) + 
			 	geom_point(size = 4) + 
	geom_errorbar(aes(ymax = u95, ymin = l95))

# ANCOVA # 
dota_load_phyto = filter(dota_load, year < 2021) # phyto data only goes to 2020
dota_load_phyto

phyto.annum.avg.ME

load_phyto.annum = left_join(dota_load_phyto, phyto.annum.avg.ME, by = 'year')
load_phyto.annum

# Initial linear model # 
lm1 = lm(mean_biomass~load_g_m3, data = load_phyto.annum)
summary(lm1)
#plot(lm1) # pretty skewed - will try log-log 

lm1.log = lm(log(mean_biomass)~log(load_g_m3), data = load_phyto.annum)
summary(lm1.log)
#plot(lm1.log) # diagnostics 

windows(height = 4, width = 5)
par(mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
plot(log(mean_biomass)~log(load_g_m3), data = load_phyto.annum, yaxt = 'n', xaxt = 'n', 
		 xlim = c(log(0.009), log(0.2)), ylim = c(log(1), log(20)), 
		 pch = 19, ylab = ' log[phytoplankton biomass (mg/L)]', xlab = 'log[P load (g/m3)]')
abline(lm1.log)
text(-2.2, log(13), 'p = 0.015')
text(-2.2, log(16), 'AdjustedR = 19.18%')
text(-2.2, log(20), 'Mendota', font = 2)
axis(side = 1, 
		 at = c(log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
		 			 log(0.1), log(0.2)), 
		 labels = c('0.01', '0.02', '','0.04','','0.06','','0.08','','0.1', '0.2'), 
		 las = 3
)
axis(side = 2, 
		 at = c(log(1), log(2), log(3), log(4), log(5), log(6), log(7), log(8), log(9), log(10),
		 			 log(20)), 
		 labels = c('1', '2','','4','','6','','8','','10','20'))
mtext('log[phytoplankton biomass (mg/L)]', side = 2, line = 2)
mtext('log[P load (g/m3)', side = 1, line = 2.5)

# Add grouping data # 
load_phyto.annum = mutate(load_phyto.annum, group = case_when(year < 2009 ~ "base",
																															year > 2008 & year < 2015 ~ '+SWF', 
																															year > 2014 ~ 'SWF+ZM')) %>% 
	mutate(log.mean_biomass = log(mean_biomass), 
				 log.load_g_m3 = log(load_g_m3)) %>% 
	mutate(group = as.factor(group))
load_phyto.annum

anc1 = lm(log.mean_biomass~log.load_g_m3*factor(group), data = load_phyto.annum) # interactions n.s.
summary(anc1)
Anova(anc1)

library(emmeans)
library(cowplot)
devtools::install_github("Ryo-N7/tvthemes")
library(tvthemes)
library(scales)

# Want to compare the slope of the predictor across the levels of the predictor 
emmeans(anc1, specs = 'group')

anc1.ph = emtrends(anc1, specs = 'group', var = 'log.load_g_m3')
pairs(anc1.ph)

windows(height = 4, width = 6)
qplot(data = load_phyto.annum, 
			x=load_g_m3, 
			y=mean_biomass,
			geom = 'point', 
			colour = group, 
			shape = group) + 
	scale_x_continuous(trans = log_trans(), 
										 breaks = exp(-5:-1), 
										 labels = label_math(e^.x, format = log)) + 
	scale_y_continuous(trans = log_trans(), 
										 breaks = exp(0:20), 
										 labels = label_math(e^.x, format = log)) +
	xlab('Log[P load (g/m3)]') + 
	ylab('Log[Phytoplankton biomass (mg/L)]') +
	geom_smooth(method = 'lm', se=T) + 
	scale_color_attackOnTitan() + 
	theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Phytoplankton data - Monona #==================================
phyto.grouped = phyto_biomass %>% 
	filter(lakeid == 'MO') %>% 
	mutate(year.mon = as.yearmon(sampledate, '%b-%Y')) %>% 
	group_by(year.mon) %>%
	summarize(community_biomass = sum(biomass_conc)) %>% 
	ungroup() %>%
	complete(year.mon = seq(min(year.mon), max(year.mon), 1/12))
	# group_by(year) %>%
	# complete(month = seq(min(month), max(month), 1/12)) %>% 
	# ungroup()
phyto.grouped

phyto.ts = ts(phyto.grouped$community_biomass, start = c(1995, 1), end = c(2020, 12), frequency = 12)
phyto.ts.kal = na_kalman(phyto.ts) # NAs in time series need to fill - impute with Kalman Filter 
plot(phyto.ts.kal)

# Decompose phyto time series # 
phyto.decomp = decompose(phyto.ts.kal)
plot(phyto.decomp)

#### ANCOVA Analysis #========================

# phytoplankton #=============================
phyto.annum.avg.MO = phyto.grouped %>% 
	mutate(year = year(year.mon), 
				 month = month(year.mon, label = T)) %>% 
	drop_na() %>% 
	group_by(year) %>% 
	summarize(mean_biomass = mean(community_biomass, na.rm = T), 
						sd_biomass = sd(community_biomass, na.rm = T), 
						n = n()) %>% 
	ungroup() %>% 
	mutate(se_biomass = sd_biomass/sqrt(n)) %>% 
	mutate(t_score = qt(p=0.05/2, df = n - 1, lower.tail = F)) %>%
	mutate(marg.err = t_score*se_biomass) %>% 
	mutate(l95 = mean_biomass - marg.err, 
				 u95 = mean_biomass + marg.err)
phyto.annum.avg.MO

ggplot(phyto.annum.avg.MO, aes(x = year, y = mean_biomass)) + 
			 	geom_point(size = 4) + 
	geom_errorbar(aes(ymax = u95, ymin = l95))

# ANCOVA # 
nona_load_phyto = filter(nona_load, year < 2021) # phyto data only goes to 2020
nona_load_phyto

phyto.annum.avg.MO

load_phyto.annum = left_join(nona_load_phyto, phyto.annum.avg.MO, by = 'year')
load_phyto.annum

# Initial linear model # 
lm1 = lm(mean_biomass~load_g_m3, data = load_phyto.annum)
summary(lm1)
#plot(lm1) # pretty skewed - will try log-log 

lm1.log = lm(log(mean_biomass)~log(load_g_m3), data = load_phyto.annum)
summary(lm1.log)
#plot(lm1.log) # diagnostics 

windows(height = 4, width = 5)
par(mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
plot(log(mean_biomass)~log(load_g_m3), data = load_phyto.annum, yaxt = 'n', xaxt = 'n', 
		 xlim = c(log(0.045), log(0.3)), ylim = c(log(1), log(21)), 
		 pch = 19, ylab = ' log[phytoplankton biomass (mg/L)]', xlab = 'log[P load (g/m3)]')
abline(lm1.log)
text(-1.5, log(13), 'p = 0.57')
text(-1.5, log(16), 'AdjustedR = ngl')
text(-1.5, log(20), 'Monona', font = 2)
axis(side = 1, 
		 at = c(log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
		 			 log(0.1), log(0.2), log(0.3)), 
		 labels = c('0.01', '0.02', '','0.04','','0.06','','0.08','','0.1', '0.2', '0.3'), 
		 las = 3
)
axis(side = 2, 
		 at = c(log(1), log(2), log(3), log(4), log(5), log(6), log(7), log(8), log(9), log(10),
		 			 log(20), log(21)), 
		 labels = c('1', '2','','4','','6','','8','','10','20', '21'))
mtext('log[phytoplankton biomass (mg/L)]', side = 2, line = 2)
mtext('log[P load (g/m3)]', side = 1, line = 2.5)

# Add grouping data # 
load_phyto.annum = mutate(load_phyto.annum, group = case_when(year < 2009 ~ "base",
																															year > 2008 & year < 2015 ~ '+SWF', 
																															year > 2014 ~ 'SWF+ZM')) %>% 
	mutate(log.mean_biomass = log(mean_biomass), 
				 log.load_g_m3 = log(load_g_m3)) %>% 
	mutate(group = as.factor(group))
load_phyto.annum

anc1 = lm(log.mean_biomass~log.load_g_m3*factor(group), data = load_phyto.annum) # interactions n.s.
summary(anc1)
Anova(anc1)

library(emmeans)
library(cowplot)
devtools::install_github("Ryo-N7/tvthemes")
library(tvthemes)
library(scales)

# Want to compare the slope of the predictor across the levels of the predictor 
emmeans(anc1, specs = 'group')

anc1.ph = emtrends(anc1, specs = 'group', var = 'log.load_g_m3')
pairs(anc1.ph)

windows(height = 4, width = 6)
qplot(data = load_phyto.annum, 
			x=load_g_m3, 
			y=mean_biomass,
			geom = 'point', 
			colour = group, 
			shape = group) + 
	scale_x_continuous(trans = log_trans(), 
										 breaks = exp(-5:-1), 
										 labels = label_math(e^.x, format = log)) + 
	scale_y_continuous(trans = log_trans(), 
										 breaks = exp(0:20), 
										 labels = label_math(e^.x, format = log)) +
	xlab('Log[P load (g/m3)]') + 
	ylab('Log[Phytoplankton biomass (mg/L)]') +
	geom_smooth(method = 'lm', se=T) + 
	scale_color_attackOnTitan() + 
	theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


### Relationship with cyanobacteria biomass ###=======================

# select for cyano taxa 
cyano_biomass = phyto_biomass %>% 
	as_tibble() %>% 
	group_by(lakeid, sampledate, year, division) %>% 
	summarize(biomass_conc = sum(biomass_conc)) %>% 
	ungroup() %>% 
	filter(division == 'Cyanophyta')
cyano_biomass	

# Mendota # ===========================
cyano.grouped = cyano_biomass %>% 
	filter(lakeid == 'ME') %>% 
	mutate(year.mon = as.yearmon(sampledate, '%b-%Y')) %>% 
	group_by(year.mon) %>%
	summarize(community_biomass = sum(biomass_conc)) %>% 
	ungroup() %>%
	complete(year.mon = seq(min(year.mon), max(year.mon), 1/12))
	# group_by(year) %>%
	# complete(month = seq(min(month), max(month), 1/12)) %>% 
	# ungroup()
cyano.grouped 

cyano.ts = ts(cyano.grouped$community_biomass, start = c(1995, 1), end = c(2020, 12), frequency = 12)
cyano.ts.kal = na_kalman(cyano.ts) # NAs in time series need to fill - impute with Kalman Filter 
plot(cyano.ts.kal)

# Decompose phyto time series # 
cyano.decomp = decompose(cyano.ts.kal)
plot(cyano.decomp)

cyano.annum.avg.ME = cyano.grouped %>% 
	mutate(year = year(year.mon), 
				 month = month(year.mon, label = T)) %>% 
	drop_na() %>% 
	group_by(year) %>% 
	summarize(mean_biomass = mean(community_biomass, na.rm = T), 
						sd_biomass = sd(community_biomass, na.rm = T), 
						n = n()) %>% 
	ungroup() %>% 
	mutate(se_biomass = sd_biomass/sqrt(n)) %>% 
	mutate(t_score = qt(p=0.05/2, df = n - 1, lower.tail = F)) %>%
	mutate(marg.err = t_score*se_biomass) %>% 
	mutate(l95 = mean_biomass - marg.err, 
				 u95 = mean_biomass + marg.err)
cyano.annum.avg.ME

ggplot(cyano.annum.avg.ME, aes(x = year, y = mean_biomass)) + 
			 	geom_point(size = 4) + 
	geom_errorbar(aes(ymax = u95, ymin = l95))

# ANCOVA # 
dota_load_phyto = filter(dota_load, year < 2021) # phyto data only goes to 2020
dota_load_phyto

cyano.annum.avg.ME

load_cyano.annum = left_join(dota_load_phyto, cyano.annum.avg.ME, by = 'year')
load_cyano.annum

# Initial linear model # 
lm1 = lm(mean_biomass~load_g_m3, data = load_cyano.annum)
summary(lm1)
#plot(lm1) # Some high leverage points will try log-log

lm1.log = lm(log(mean_biomass)~log(load_g_m3), data = load_cyano.annum)
summary(lm1.log)
#plot(lm1.log) # diagnostics 

windows(height = 4, width = 5)
par(mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
plot(log(mean_biomass)~log(load_g_m3), data = load_cyano.annum, yaxt = 'n', xaxt = 'n', 
		 xlim = c(log(0.01), log(0.2)), ylim = c(log(1), log(10)), 
		 pch = 19, ylab = ' log[cyano biomass (mg/L)]', xlab = 'log[P load (g/m3)]')
abline(lm1.log)
text(-1.9, log(1), 'p = 0.002')
text(-2.1, log(1.18), 'AdjustedR = 30%')
text(-1.9, log(1.4), 'Mendota', font = 2)
axis(side = 1, 
		 at = c(log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
		 			 log(0.1), log(0.2)), 
		 labels = c('0.01', '0.02', '','0.04','','0.06','','0.08','','0.1', '0.2'), 
		 las = 3
)
axis(side = 2, 
		 at = c(log(1), log(2), log(3), log(4), log(5), log(6), log(7), log(8), log(9), log(10)), 
		 labels = c('1', '2','','4','','6','','8','','10'))
mtext('log[Cyano biomass (mg/L)]', side = 2, line = 2)
mtext('log[P load (g/m3)]', side = 1, line = 2.5)

# Add grouping data # 
load_cyano.annum = mutate(load_cyano.annum, group = case_when(year < 2009 ~ "base",
																															year > 2008 & year < 2015 ~ '+SWF', 
																															year > 2014 ~ 'SWF+ZM')) %>% 
	mutate(log.mean_biomass = log(mean_biomass), 
				 log.load_g_m3 = log(load_g_m3)) %>% 
	mutate(group = as.factor(group))
load_cyano.annum

anc1 = lm(log.mean_biomass~log.load_g_m3*factor(group), data = load_cyano.annum) # interactions n.s.
summary(anc1)
Anova(anc1)

library(emmeans)
library(cowplot)
devtools::install_github("Ryo-N7/tvthemes")
library(tvthemes)
library(scales)

# Want to compare the slope of the predictor across the levels of the predictor 
emmeans(anc1, specs = 'group')

anc1.ph = emtrends(anc1, specs = 'group', var = 'log.load_g_m3')
pairs(anc1.ph)

windows(height = 4, width = 6)
qplot(data = load_cyano.annum, 
			x=load_g_m3, 
			y=mean_biomass,
			geom = 'point', 
			colour = group, 
			shape = group) + 
	scale_x_continuous(trans = log_trans(), 
										 breaks = exp(-5:-1), 
										 labels = label_math(e^.x, format = log)) + 
	scale_y_continuous(trans = log_trans(), 
										 breaks = exp(0:20), 
										 labels = label_math(e^.x, format = log)) +
	xlab('Log[P load (g/m3)]') + 
	ylab('Log[Cyano biomass (mg/L)]') +
	geom_smooth(method = 'lm', se=T) + 
	scale_color_attackOnTitan() + 
	theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Monona # ===========================
cyano.grouped = cyano_biomass %>% 
	filter(lakeid == 'MO') %>% 
	mutate(year.mon = as.yearmon(sampledate, '%b-%Y')) %>% 
	group_by(year.mon) %>%
	summarize(community_biomass = sum(biomass_conc)) %>% 
	ungroup() %>%
	complete(year.mon = seq(min(year.mon), max(year.mon), 1/12))
	# group_by(year) %>%
	# complete(month = seq(min(month), max(month), 1/12)) %>% 
	# ungroup()
cyano.grouped 

cyano.ts = ts(cyano.grouped$community_biomass, start = c(1995, 1), end = c(2020, 12), frequency = 12)
cyano.ts.kal = na_kalman(cyano.ts) # NAs in time series need to fill - impute with Kalman Filter 
plot(cyano.ts.kal)

# Decompose phyto time series # 
cyano.decomp = decompose(cyano.ts.kal)
plot(cyano.decomp)

cyano.annum.avg.MO = cyano.grouped %>% 
	mutate(year = year(year.mon), 
				 month = month(year.mon, label = T)) %>% 
	drop_na() %>% 
	group_by(year) %>% 
	summarize(mean_biomass = mean(community_biomass, na.rm = T), 
						sd_biomass = sd(community_biomass, na.rm = T), 
						n = n()) %>% 
	ungroup() %>% 
	mutate(se_biomass = sd_biomass/sqrt(n)) %>% 
	mutate(t_score = qt(p=0.05/2, df = n - 1, lower.tail = F)) %>%
	mutate(marg.err = t_score*se_biomass) %>% 
	mutate(l95 = mean_biomass - marg.err, 
				 u95 = mean_biomass + marg.err)
cyano.annum.avg.MO

ggplot(cyano.annum.avg.MO, aes(x = year, y = mean_biomass)) + 
			 	geom_point(size = 4) + 
	geom_errorbar(aes(ymax = u95, ymin = l95))

# ANCOVA # 
nona_load_phyto = filter(nona_load, year < 2021) # phyto data only goes to 2020
nona_load_phyto

cyano.annum.avg.MO

load_cyano.annum = left_join(nona_load_phyto, cyano.annum.avg.MO, by = 'year')
load_cyano.annum

# Initial linear model # 
lm1 = lm(mean_biomass~load_g_m3, data = load_cyano.annum)
summary(lm1)
#plot(lm1) # Some high leverage points will try log-log

lm1.log = lm(log(mean_biomass)~log(load_g_m3), data = load_cyano.annum)
summary(lm1.log)
#plot(lm1.log) # diagnostics 

windows(height = 4, width = 5)
par(mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
plot(log(mean_biomass)~log(load_g_m3), data = load_cyano.annum, #yaxt = 'n', xaxt = 'n', 
		 # xlim = c(log(0.01), log(0.2)), ylim = c(log(1), log(10)), 
		 pch = 19, ylab = ' log[cyano biomass (mg/L)]', xlab = 'log[P load (g/m3)]')
abline(lm1.log)
# text(-1.9, log(1), 'p = 0.002')
# text(-2.1, log(1.18), 'AdjustedR = 30%')
# text(-1.9, log(1.4), 'Monona', font = 2)
# axis(side = 1, 
# 		 at = c(log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
# 		 			 log(0.1), log(0.2)), 
# 		 labels = c('0.01', '0.02', '','0.04','','0.06','','0.08','','0.1', '0.2'), 
# 		 las = 3
# )
# axis(side = 2, 
# 		 at = c(log(1), log(2), log(3), log(4), log(5), log(6), log(7), log(8), log(9), log(10)), 
# 		 labels = c('1', '2','','4','','6','','8','','10'))
mtext('log[Cyano biomass (mg/L)]', side = 2, line = 2)
mtext('log[P load (g/m3)]', side = 1, line = 2.5)

# Add grouping data # 
load_cyano.annum = mutate(load_cyano.annum, group = case_when(year < 2009 ~ "base",
																															year > 2008 & year < 2015 ~ '+SWF', 
																															year > 2014 ~ 'SWF+ZM')) %>% 
	mutate(log.mean_biomass = log(mean_biomass), 
				 log.load_g_m3 = log(load_g_m3)) %>% 
	mutate(group = as.factor(group))
load_cyano.annum

anc1 = lm(log.mean_biomass~log.load_g_m3*factor(group), data = load_cyano.annum) # interactions n.s.
summary(anc1)
Anova(anc1)

library(emmeans)
library(cowplot)
devtools::install_github("Ryo-N7/tvthemes")
library(tvthemes)
library(scales)

# Want to compare the slope of the predictor across the levels of the predictor 
emmeans(anc1, specs = 'group')

anc1.ph = emtrends(anc1, specs = 'group', var = 'log.load_g_m3')
pairs(anc1.ph)

windows(height = 4, width = 6)
qplot(data = load_cyano.annum, 
			x=load_g_m3, 
			y=mean_biomass,
			geom = 'point', 
			colour = group, 
			shape = group) + 
	scale_x_continuous(trans = log_trans(), 
										 breaks = exp(-5:-1), 
										 labels = label_math(e^.x, format = log)) + 
	scale_y_continuous(trans = log_trans(), 
										 breaks = exp(0:20), 
										 labels = label_math(e^.x, format = log)) +
	xlab('Log[P load (g/m3)]') + 
	ylab('Log[Cyano biomass (mg/L)]') +
	geom_smooth(method = 'lm', se=T) + 
	scale_color_attackOnTitan() + 
	theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

### Relationship with non-cyano GALD ###==============================

# filter out cyanobacteria biomass 
non.cyano_gald = phyto_biomass %>% 
	as_tibble() %>% 
	filter(division != 'Cyanophyta') %>% 
	group_by(lakeid, sampledate, year) %>% 
	summarize(max_gald = max(gald)) %>% 
	ungroup() 
non.cyano_gald

# Mendota # ===========================
phyto.gald.grouped = non.cyano_gald %>% 
	filter(lakeid == 'ME') %>% 
	mutate(year.mon = as.yearmon(sampledate, '%b-%Y')) %>% 
	group_by(year.mon) %>%
	summarize(max_gald = max(max_gald)) %>% 
	ungroup() %>%
	complete(year.mon = seq(min(year.mon), max(year.mon), 1/12))
	# group_by(year) %>%
	# complete(month = seq(min(month), max(month), 1/12)) %>% 
	# ungroup()
phyto.gald.grouped

phyto.gald.grouped.ts = ts(phyto.gald.grouped$max_gald, start = c(1995, 1), end = c(2020, 12), frequency = 12)
phyto.gald.ts.kal = na_kalman(phyto.gald.grouped.ts) # NAs in time series need to fill - impute with Kalman Filter 
plot(phyto.gald.ts.kal)

# Decompose phyto time series # 
phyto.gald.decomp = decompose(phyto.gald.ts.kal)
plot(phyto.gald.decomp)

phyto.gald.annum.avg.ME = phyto.gald.grouped %>% 
	mutate(year = year(year.mon), 
				 month = month(year.mon, label = T)) %>% 
	drop_na() %>% 
	group_by(year) %>% 
	summarize(mean_maxgald = mean(max_gald, na.rm = T), 
						sd_maxgald = sd(max_gald, na.rm = T), 
						n = n()) %>% 
	ungroup() %>% 
	mutate(se_maxgald = sd_maxgald/sqrt(n)) %>% 
	mutate(t_score = qt(p=0.05/2, df = n - 1, lower.tail = F)) %>%
	mutate(marg.err = t_score*se_maxgald) %>% 
	mutate(l95 = mean_maxgald - marg.err, 
				 u95 = mean_maxgald + marg.err)
phyto.gald.annum.avg.ME

ggplot(phyto.gald.annum.avg.ME, aes(x = year, y = mean_maxgald)) + 
			 	geom_point(size = 4) + 
	geom_errorbar(aes(ymax = u95, ymin = l95))

# ANCOVA # 
dota_load_phyto = filter(dota_load, year < 2021) # phyto data only goes to 2020
dota_load_phyto

phyto.gald.annum.avg.ME

load_phyto.gald.annum = left_join(dota_load_phyto, phyto.gald.annum.avg.ME, by = 'year')
load_phyto.gald.annum

# Initial linear model # 
lm1 = lm(mean_maxgald~load_g_m3, data = load_phyto.gald.annum)
summary(lm1)
#plot(lm1) # Some high leverage points will try log-log

lm1.log = lm(log(mean_maxgald)~log(load_g_m3), data = load_phyto.gald.annum)
summary(lm1.log)
#plot(lm1.log) # diagnostics 

windows(height = 4, width = 5)
par(mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
plot(log(mean_maxgald)~log(load_g_m3), data = load_phyto.gald.annum, yaxt = 'n', xaxt = 'n', 
		 xlim = c(log(0.01), log(0.2)), ylim = c(log(50), log(500)), 
		 pch = 19, ylab = ' log[phyto GALD (um)]', xlab = 'log[P load (g/m3)]')
abline(lm1.log)
text(-2.1, log(60), 'p = 0.451')
text(-2.1, log(70), 'Mendota', font = 2)
axis(side = 1, 
		 at = c(log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
		 			 log(0.1), log(0.2)), 
		 labels = c('0.01', '0.02', '','0.04','','0.06','','0.08','','0.1', '0.2'), 
		 las = 3
)
axis(side = 2, 
		 at = c(log(50), log(60), log(70), log(80), log(90), log(100), log(200), log(300),
		 			 log(400), log(500)), 
		 labels = c('50', '','','','','100',
		 					 '','','','500'))
mtext('log[Phyto mean monthly max GALD (um)]', side = 2, line = 2)
mtext('log[P load (g/m3)]', side = 1, line = 2.5)

# Add grouping data # 
load_phyto.gald.annum = mutate(load_phyto.gald.annum, group = case_when(year < 2009 ~ "base",
																															year > 2008 & year < 2015 ~ '+SWF', 
																															year > 2014 ~ 'SWF+ZM')) %>% 
	mutate(log.mean_maxgald = log(mean_maxgald), 
				 log.load_g_m3 = log(load_g_m3)) %>% 
	mutate(group = as.factor(group))
load_phyto.gald.annum

anc1 = lm(log.mean_maxgald~log.load_g_m3*factor(group), data = load_phyto.gald.annum) # interactions n.s.
summary(anc1)
Anova(anc1)

library(emmeans)
library(cowplot)
devtools::install_github("Ryo-N7/tvthemes")
library(tvthemes)
library(scales)

# Want to compare the slope of the predictor across the levels of the predictor 
emmeans(anc1, specs = 'group')

anc1.ph = emtrends(anc1, specs = 'group', var = 'log.load_g_m3')
pairs(anc1.ph)

windows(height = 4, width = 6)
qplot(data = load_phyto.gald.annum, 
			x=load_g_m3, 
			y=mean_maxgald,
			geom = 'point', 
			colour = group, 
			shape = group) + 
	scale_x_continuous(trans = log_trans(), 
										 breaks = exp(-5:-1), 
										 labels = label_math(e^.x, format = log)) + 
	scale_y_continuous(trans = log_trans(), 
										 breaks = exp(0:20), 
										 labels = label_math(e^.x, format = log)) +
	xlab('Log[P load (g/m3)]') + 
	ylab('Log[Mean Max GALD (um)]') +
	geom_smooth(method = 'lm', se=T) + 
	scale_color_attackOnTitan() + 
	theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Monona # ===========================
phyto.gald.grouped = non.cyano_gald %>% 
	filter(lakeid == 'MO') %>% 
	mutate(year.mon = as.yearmon(sampledate, '%b-%Y')) %>% 
	group_by(year.mon) %>%
	summarize(max_gald = max(max_gald)) %>% 
	ungroup() %>%
	complete(year.mon = seq(min(year.mon), max(year.mon), 1/12))
	# group_by(year) %>%
	# complete(month = seq(min(month), max(month), 1/12)) %>% 
	# ungroup()
phyto.gald.grouped

phyto.gald.grouped.ts = ts(phyto.gald.grouped$max_gald, start = c(1995, 1), end = c(2020, 12), frequency = 12)
phyto.gald.ts.kal = na_kalman(phyto.gald.grouped.ts) # NAs in time series need to fill - impute with Kalman Filter 
plot(phyto.gald.ts.kal)

# Decompose phyto time series # 
phyto.gald.decomp = decompose(phyto.gald.ts.kal)
plot(phyto.gald.decomp)

phyto.gald.annum.avg.MO = phyto.gald.grouped %>% 
	mutate(year = year(year.mon), 
				 month = month(year.mon, label = T)) %>% 
	drop_na() %>% 
	group_by(year) %>% 
	summarize(mean_maxgald = mean(max_gald, na.rm = T), 
						sd_maxgald = sd(max_gald, na.rm = T), 
						n = n()) %>% 
	ungroup() %>% 
	mutate(se_maxgald = sd_maxgald/sqrt(n)) %>% 
	mutate(t_score = qt(p=0.05/2, df = n - 1, lower.tail = F)) %>%
	mutate(marg.err = t_score*se_maxgald) %>% 
	mutate(l95 = mean_maxgald - marg.err, 
				 u95 = mean_maxgald + marg.err)
phyto.gald.annum.avg.MO

ggplot(phyto.gald.annum.avg.MO, aes(x = year, y = mean_maxgald)) + 
			 	geom_point(size = 4) + 
	geom_errorbar(aes(ymax = u95, ymin = l95))

# ANCOVA # 
nona_load_phyto = filter(nona_load, year < 2021) # phyto data only goes to 2020
nona_load_phyto

phyto.gald.annum.avg.MO

load_phyto.gald.annum = left_join(nona_load_phyto, phyto.gald.annum.avg.MO, by = 'year')
load_phyto.gald.annum

# Initial linear model # 
lm1 = lm(mean_maxgald~load_g_m3, data = load_phyto.gald.annum)
summary(lm1)
#plot(lm1) # Some high leverage points will try log-log

lm1.log = lm(log(mean_maxgald)~log(load_g_m3), data = load_phyto.gald.annum)
summary(lm1.log)
#plot(lm1.log) # diagnostics 

windows(height = 4, width = 5)
par(mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
plot(log(mean_maxgald)~log(load_g_m3), data = load_phyto.gald.annum, yaxt = 'n', xaxt = 'n', 
		 xlim = c(log(0.05), log(0.3)), ylim = c(log(50), log(200)), 
		 pch = 19, ylab = ' log[phyto GALD (um)]', xlab = 'log[P load (g/m3)]')
abline(lm1.log)
text(-1.5, log(50), 'p = 0.387')
text(-1.5, log(55), 'AdjustedR = ngl')
text(-1.5, log(60), 'Monona', font = 2)
axis(side = 1, 
		 at = c(log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
		 			 log(0.1), log(0.2), log(0.3)), 
		 labels = c('0.01', '0.02', '','0.04','','0.06','','0.08','','0.1', '0.2', '0.3'), 
		 las = 3
)
axis(side = 2, 
		 at = c(log(50), log(60), log(70), log(80), log(90), log(100), log(200), log(300),
		 			 log(400), log(500)), 
		 labels = c('50', '','','','','100',
		 					 '','','','500'))
mtext('log[Phyto mean monthly max GALD (um)]', side = 2, line = 2)
mtext('log[P load (g/m3)]', side = 1, line = 2.5)

# Add grouping data # 
load_phyto.gald.annum = mutate(load_phyto.gald.annum, group = case_when(year < 2009 ~ "base",
																															year > 2008 & year < 2015 ~ '+SWF', 
																															year > 2014 ~ 'SWF+ZM')) %>% 
	mutate(log.mean_maxgald = log(mean_maxgald), 
				 log.load_g_m3 = log(load_g_m3)) %>% 
	mutate(group = as.factor(group))
load_phyto.gald.annum

anc1 = lm(log.mean_maxgald~log.load_g_m3*factor(group), data = load_phyto.gald.annum) # interactions n.s.
summary(anc1)
Anova(anc1)

library(emmeans)
library(cowplot)
devtools::install_github("Ryo-N7/tvthemes")
library(tvthemes)
library(scales)

# Want to compare the slope of the predictor across the levels of the predictor 
emmeans(anc1, specs = 'group')

anc1.ph = emtrends(anc1, specs = 'group', var = 'log.load_g_m3')
pairs(anc1.ph)

windows(height = 4, width = 6)
qplot(data = load_phyto.gald.annum, 
			x=load_g_m3, 
			y=mean_maxgald,
			geom = 'point', 
			colour = group, 
			shape = group) + 
	scale_x_continuous(trans = log_trans(), 
										 breaks = exp(-5:-1), 
										 labels = label_math(e^.x, format = log)) + 
	scale_y_continuous(trans = log_trans(), 
										 breaks = exp(0:20), 
										 labels = label_math(e^.x, format = log)) +
	xlab('Log[P load (g/m3)]') + 
	ylab('Log[Mean Max GALD (um)]') +
	geom_smooth(method = 'lm', se=T) + 
	scale_color_attackOnTitan() + 
	theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

### Relationship with cyano GALD ###============================== 

# filter to cyanobacteria biomass 
cyano_gald = phyto_biomass %>% 
	as_tibble() %>% 
	filter(division == 'Cyanophyta') %>% 
	group_by(lakeid, sampledate, year) %>% 
	summarize(max_gald = max(gald)) %>% 
	ungroup() 
cyano_gald

# Mendota # ===========================
cyano.gald.grouped = cyano_gald %>% 
	filter(lakeid == 'ME') %>% 
	mutate(year.mon = as.yearmon(sampledate, '%b-%Y')) %>% 
	group_by(year.mon) %>%
	summarize(max_gald = max(max_gald)) %>% 
	ungroup() %>%
	complete(year.mon = seq(min(year.mon), max(year.mon), 1/12))
	# group_by(year) %>%
	# complete(month = seq(min(month), max(month), 1/12)) %>% 
	# ungroup()
cyano.gald.grouped

cyano.gald.grouped.ts = ts(cyano.gald.grouped$max_gald, start = c(1995, 1), end = c(2020, 12), frequency = 12)
cyano.gald.ts.kal = na_kalman(cyano.gald.grouped.ts) # NAs in time series need to fill - impute with Kalman Filter 
plot(cyano.gald.ts.kal)

# Decompose cyano time series # 
cyano.gald.decomp = decompose(cyano.gald.ts.kal)
plot(cyano.gald.decomp)

cyano.gald.annum.avg.ME = cyano.gald.grouped %>% 
	mutate(year = year(year.mon), 
				 month = month(year.mon, label = T)) %>% 
	drop_na() %>% 
	group_by(year) %>% 
	summarize(mean_maxgald = mean(max_gald, na.rm = T), 
						sd_maxgald = sd(max_gald, na.rm = T), 
						n = n()) %>% 
	ungroup() %>% 
	mutate(se_maxgald = sd_maxgald/sqrt(n)) %>% 
	mutate(t_score = qt(p=0.05/2, df = n - 1, lower.tail = F)) %>%
	mutate(marg.err = t_score*se_maxgald) %>% 
	mutate(l95 = mean_maxgald - marg.err, 
				 u95 = mean_maxgald + marg.err)
cyano.gald.annum.avg.ME

ggplot(cyano.gald.annum.avg.ME, aes(x = year, y = mean_maxgald)) + 
			 	geom_point(size = 4) + 
	geom_errorbar(aes(ymax = u95, ymin = l95))

# ANCOVA # 
dota_load_cyano = filter(dota_load, year < 2021) # cyano data only goes to 2020
dota_load_cyano

cyano.gald.annum.avg.ME

load_cyano.gald.annum = left_join(dota_load_cyano, cyano.gald.annum.avg.ME, by = 'year')
load_cyano.gald.annum

# Initial linear model # 
lm1 = lm(mean_maxgald~load_g_m3, data = load_cyano.gald.annum)
summary(lm1)
#plot(lm1) # Some high leverage points will try log-log

lm1.log = lm(log(mean_maxgald)~log(load_g_m3), data = load_cyano.gald.annum)
summary(lm1.log)
plot(lm1.log) # diagnostics 

windows(height = 4, width = 5)
par(mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
plot(log(mean_maxgald)~log(load_g_m3), data = load_cyano.gald.annum, yaxt = 'n', xaxt = 'n', 
		 xlim = c(log(0.01), log(0.2)), ylim = c(log(50), log(800)), 
		 pch = 19, ylab = ' log[cyano GALD (um)]', xlab = 'log[P load (g/m3)]')
abline(lm1.log)
text(-2.1, log(60), 'p = 0.249')
text(-2.1, log(70), 'Mendota', font = 2)
axis(side = 1, 
		 at = c(log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
		 			 log(0.1), log(0.2)), 
		 labels = c('0.01', '0.02', '','0.04','','0.06','','0.08','','0.1', '0.2'), 
		 las = 3
)
axis(side = 2, 
		 at = c(log(50), log(60), log(70), log(80), log(90), log(100), log(200), log(300),
		 			 log(400), log(500), log(600), log(700), log(800)), 
		 labels = c('50', '','','','','100',
		 					 '','','','500','','',''))
mtext('log[cyano mean monthly max GALD (um)]', side = 2, line = 2)
mtext('log[P load (g/m3)]', side = 1, line = 2.5)

# Add grouping data # 
load_cyano.gald.annum = mutate(load_cyano.gald.annum, group = case_when(year < 2009 ~ "base",
																															year > 2008 & year < 2015 ~ '+SWF', 
																															year > 2014 ~ 'SWF+ZM')) %>% 
	mutate(log.mean_maxgald = log(mean_maxgald), 
				 log.load_g_m3 = log(load_g_m3)) %>% 
	mutate(group = as.factor(group))
load_cyano.gald.annum

anc1 = lm(log.mean_maxgald~log.load_g_m3*factor(group), data = load_cyano.gald.annum) # interactions n.s.
summary(anc1)
Anova(anc1)

library(emmeans)
library(cowplot)
devtools::install_github("Ryo-N7/tvthemes")
library(tvthemes)
library(scales)

# Want to compare the slope of the predictor across the levels of the predictor 
emmeans(anc1, specs = 'group')

anc1.ph = emtrends(anc1, specs = 'group', var = 'log.load_g_m3')
pairs(anc1.ph)

windows(height = 4, width = 6)
qplot(data = load_cyano.gald.annum, 
			x=load_g_m3, 
			y=mean_maxgald,
			geom = 'point', 
			colour = group, 
			shape = group) + 
	scale_x_continuous(trans = log_trans(), 
										 breaks = exp(-5:-1), 
										 labels = label_math(e^.x, format = log)) + 
	scale_y_continuous(trans = log_trans(), 
										 breaks = exp(0:20), 
										 labels = label_math(e^.x, format = log)) +
	xlab('Log[P load (g/m3)]') + 
	ylab('Log[Mean Max GALD (um)]') +
	geom_smooth(method = 'lm', se=T) + 
	scale_color_attackOnTitan() + 
	theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Monona # ===========================
cyano.gald.grouped = cyano_gald %>% 
	filter(lakeid == 'MO') %>% 
	mutate(year.mon = as.yearmon(sampledate, '%b-%Y')) %>% 
	group_by(year.mon) %>%
	summarize(max_gald = max(max_gald)) %>% 
	ungroup() %>%
	complete(year.mon = seq(min(year.mon), max(year.mon), 1/12))
	# group_by(year) %>%
	# complete(month = seq(min(month), max(month), 1/12)) %>% 
	# ungroup()
cyano.gald.grouped

cyano.gald.grouped.ts = ts(cyano.gald.grouped$max_gald, start = c(1995, 1), end = c(2020, 12), frequency = 12)
cyano.gald.ts.kal = na_kalman(cyano.gald.grouped.ts) # NAs in time series need to fill - impute with Kalman Filter 
plot(cyano.gald.ts.kal)

# Decompose cyano time series # 
cyano.gald.decomp = decompose(cyano.gald.ts.kal)
plot(cyano.gald.decomp)

cyano.gald.annum.avg.MO = cyano.gald.grouped %>% 
	mutate(year = year(year.mon), 
				 month = month(year.mon, label = T)) %>% 
	drop_na() %>% 
	group_by(year) %>% 
	summarize(mean_maxgald = mean(max_gald, na.rm = T), 
						sd_maxgald = sd(max_gald, na.rm = T), 
						n = n()) %>% 
	ungroup() %>% 
	mutate(se_maxgald = sd_maxgald/sqrt(n)) %>% 
	mutate(t_score = qt(p=0.05/2, df = n - 1, lower.tail = F)) %>%
	mutate(marg.err = t_score*se_maxgald) %>% 
	mutate(l95 = mean_maxgald - marg.err, 
				 u95 = mean_maxgald + marg.err)
cyano.gald.annum.avg.MO

ggplot(cyano.gald.annum.avg.MO, aes(x = year, y = mean_maxgald)) + 
			 	geom_point(size = 4) + 
	geom_errorbar(aes(ymax = u95, ymin = l95))

# ANCOVA # 
nona_load_cyano = filter(nona_load, year < 2021) # cyano data only goes to 2020
nona_load_cyano

cyano.gald.annum.avg.MO

load_cyano.gald.annum = left_join(nona_load_cyano, cyano.gald.annum.avg.MO, by = 'year')
load_cyano.gald.annum

# Initial linear model # 
lm1 = lm(mean_maxgald~load_g_m3, data = load_cyano.gald.annum)
summary(lm1)
#plot(lm1) # Some high leverage points will try log-log

lm1.log = lm(log(mean_maxgald)~log(load_g_m3), data = load_cyano.gald.annum)
summary(lm1.log)
#plot(lm1.log) # diagnostics 

windows(height = 4, width = 5)
par(mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
plot(log(mean_maxgald)~log(load_g_m3), data = load_cyano.gald.annum, yaxt = 'n', xaxt = 'n', 
		 xlim = c(log(0.05), log(0.3)), ylim = c(log(100), log(800)), 
		 pch = 19, ylab = ' log[cyano GALD (um)]', xlab = 'log[P load (g/m3)]')
abline(lm1.log)
text(-1.5, log(100), 'p = 0.277')
text(-1.5, log(115), 'AdjustedR = ngl')
text(-1.5, log(130), 'Monona', font = 2)
axis(side = 1, 
		 at = c(log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
		 			 log(0.1), log(0.2), log(0.3)), 
		 labels = c('0.01', '0.02', '','0.04','','0.06','','0.08','','0.1', '0.2', '0.3'), 
		 las = 3
)
axis(side = 2, 
		 at = c(log(50), log(60), log(70), log(80), log(90), log(100), log(200), log(300),
		 			 log(400), log(500), log(600), log(700), log(800)), 
		 labels = c('50', '','','','','100',
		 					 '','','','500','','','800'))
mtext('log[cyano mean monthly max GALD (um)]', side = 2, line = 2)
mtext('log[P load (g/m3)]', side = 1, line = 2.5)

# Add grouping data # 
load_cyano.gald.annum = mutate(load_cyano.gald.annum, group = case_when(year < 2009 ~ "base",
																															year > 2008 & year < 2015 ~ '+SWF', 
																															year > 2014 ~ 'SWF+ZM')) %>% 
	mutate(log.mean_maxgald = log(mean_maxgald), 
				 log.load_g_m3 = log(load_g_m3)) %>% 
	mutate(group = as.factor(group))
load_cyano.gald.annum

anc1 = lm(log.mean_maxgald~log.load_g_m3*factor(group), data = load_cyano.gald.annum) # interactions n.s.
summary(anc1)
Anova(anc1)

library(emmeans)
library(cowplot)
devtools::install_github("Ryo-N7/tvthemes")
library(tvthemes)
library(scales)

# Want to compare the slope of the predictor across the levels of the predictor 
emmeans(anc1, specs = 'group')

anc1.ph = emtrends(anc1, specs = 'group', var = 'log.load_g_m3')
pairs(anc1.ph)

windows(height = 4, width = 6)
qplot(data = load_cyano.gald.annum, 
			x=load_g_m3, 
			y=mean_maxgald,
			geom = 'point', 
			colour = group, 
			shape = group) + 
	scale_x_continuous(trans = log_trans(), 
										 breaks = exp(-5:-1), 
										 labels = label_math(e^.x, format = log)) + 
	scale_y_continuous(trans = log_trans(), 
										 breaks = exp(0:20), 
										 labels = label_math(e^.x, format = log)) +
	xlab('Log[P load (g/m3)]') + 
	ylab('Log[Mean Max GALD (um)]') +
	geom_smooth(method = 'lm', se=T) + 
	scale_color_attackOnTitan() + 
	theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

### Relationship with phyto GALD variance ###================================== 

# filter out cyanobacteria biomass 
non.cyano_gald = phyto_biomass %>% 
	as_tibble() %>% 
	filter(division != 'Cyanophyta') %>% 
	group_by(lakeid, sampledate, year) %>% 
	summarize(max_gald = max(gald)) %>% 
	ungroup() 
non.cyano_gald

# Mendota # ===========================
phyto.gald.grouped = non.cyano_gald %>% 
	filter(lakeid == 'ME') %>% 
	mutate(year.mon = as.yearmon(sampledate, '%b-%Y')) %>% 
	group_by(year.mon) %>%
	summarize(max_gald = max(max_gald)) %>% 
	ungroup() %>%
	complete(year.mon = seq(min(year.mon), max(year.mon), 1/12))
# group_by(year) %>%
# complete(month = seq(min(month), max(month), 1/12)) %>% 
# ungroup()
phyto.gald.grouped

phyto.gald.grouped.ts = ts(phyto.gald.grouped$max_gald, start = c(1995, 1), end = c(2020, 12), frequency = 12)
phyto.gald.ts.kal = na_kalman(phyto.gald.grouped.ts) # NAs in time series need to fill - impute with Kalman Filter 
plot(phyto.gald.ts.kal)

# Decompose phyto time series # 
phyto.gald.decomp = decompose(phyto.gald.ts.kal)
plot(phyto.gald.decomp)

phyto.gald.annum.avg.ME = phyto.gald.grouped %>% 
	mutate(year = year(year.mon), 
				 month = month(year.mon, label = T)) %>% 
	drop_na() %>% 
	group_by(year) %>% 
	summarize(mean_maxgald = mean(max_gald, na.rm = T), 
						sd_maxgald = sd(max_gald, na.rm = T), 
						n = n()) %>% 
	ungroup() %>% 
	mutate(se_maxgald = sd_maxgald/sqrt(n)) %>% 
	mutate(t_score = qt(p=0.05/2, df = n - 1, lower.tail = F)) %>%
	mutate(marg.err = t_score*se_maxgald) %>% 
	mutate(l95 = mean_maxgald - marg.err, 
				 u95 = mean_maxgald + marg.err)
phyto.gald.annum.avg.ME

ggplot(phyto.gald.annum.avg.ME, aes(x = year, y = sd_maxgald)) + 
	geom_point(size = 4) + 
	geom_errorbar(aes(ymax = u95, ymin = l95))

# ANCOVA # 
dota_load_phyto = filter(dota_load, year < 2021) # phyto data only goes to 2020
dota_load_phyto

phyto.gald.annum.avg.ME

load_phyto.gald.annum = left_join(dota_load_phyto, phyto.gald.annum.avg.ME, by = 'year')
load_phyto.gald.annum

# Initial linear model # 
lm1 = lm(sd_maxgald~load_g_m3, data = load_phyto.gald.annum)
summary(lm1)
#plot(lm1) # Some high leverage points will try log-log

lm1.log = lm(log(sd_maxgald)~log(load_g_m3), data = load_phyto.gald.annum)
summary(lm1.log)
#plot(lm1.log) # diagnostics 

windows(height = 4, width = 5)
par(mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
plot(log(sd_maxgald)~log(load_g_m3), data = load_phyto.gald.annum, yaxt = 'n', xaxt = 'n', 
		 xlim = c(log(0.01), log(0.2)), ylim = c(log(50), log(500)), 
		 pch = 19, ylab = ' log[phyto GALD (um)]', xlab = 'log[P load (g/m3)]')
abline(lm1.log)
text(-2.1, log(60), 'p = 0.451')
text(-2.1, log(70), 'Mendota', font = 2)
axis(side = 1, 
		 at = c(log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
		 			 log(0.1), log(0.2)), 
		 labels = c('0.01', '0.02', '','0.04','','0.06','','0.08','','0.1', '0.2'), 
		 las = 3
)
axis(side = 2, 
		 at = c(log(50), log(60), log(70), log(80), log(90), log(100), log(200), log(300),
		 			 log(400), log(500)), 
		 labels = c('50', '','','','','100',
		 					 '','','','500'))
mtext('log[Phyto mean monthly sd GALD (um)]', side = 2, line = 2)
mtext('log[P load (g/m3)]', side = 1, line = 2.5)

# Add grouping data # 
load_phyto.gald.annum = mutate(load_phyto.gald.annum, group = case_when(year < 2009 ~ "base",
																																				year > 2008 & year < 2015 ~ '+SWF', 
																																				year > 2014 ~ 'SWF+ZM')) %>% 
	mutate(log.sd_maxgald = log(sd_maxgald), 
				 log.load_g_m3 = log(load_g_m3)) %>% 
	mutate(group = as.factor(group))
load_phyto.gald.annum

anc1 = lm(log.sd_maxgald~log.load_g_m3*factor(group), data = load_phyto.gald.annum) # interactions n.s.
summary(anc1)
Anova(anc1)

library(emmeans)
library(cowplot)
devtools::install_github("Ryo-N7/tvthemes")
library(tvthemes)
library(scales)

# Want to compare the slope of the predictor across the levels of the predictor 
emmeans(anc1, specs = 'group')

anc1.ph = emtrends(anc1, specs = 'group', var = 'log.load_g_m3')
pairs(anc1.ph)

windows(height = 4, width = 6)
qplot(data = load_phyto.gald.annum, 
			x=load_g_m3, 
			y=sd_maxgald,
			geom = 'point', 
			colour = group, 
			shape = group) + 
	scale_x_continuous(trans = log_trans(), 
										 breaks = exp(-5:-1), 
										 labels = label_math(e^.x, format = log)) + 
	scale_y_continuous(trans = log_trans(), 
										 breaks = exp(0:20), 
										 labels = label_math(e^.x, format = log)) +
	xlab('Log[P load (g/m3)]') + 
	ylab('Log[Mean sd GALD (um)]') +
	geom_smooth(method = 'lm', se=T) + 
	scale_color_attackOnTitan() + 
	theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
										 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Monona # ===========================
phyto.gald.grouped = non.cyano_gald %>% 
	filter(lakeid == 'MO') %>% 
	mutate(year.mon = as.yearmon(sampledate, '%b-%Y')) %>% 
	group_by(year.mon) %>%
	summarize(max_gald = max(max_gald)) %>% 
	ungroup() %>%
	complete(year.mon = seq(min(year.mon), max(year.mon), 1/12))
# group_by(year) %>%
# complete(month = seq(min(month), max(month), 1/12)) %>% 
# ungroup()
phyto.gald.grouped

phyto.gald.grouped.ts = ts(phyto.gald.grouped$max_gald, start = c(1995, 1), end = c(2020, 12), frequency = 12)
phyto.gald.ts.kal = na_kalman(phyto.gald.grouped.ts) # NAs in time series need to fill - impute with Kalman Filter 
plot(phyto.gald.ts.kal)

# Decompose phyto time series # 
phyto.gald.decomp = decompose(phyto.gald.ts.kal)
plot(phyto.gald.decomp)

phyto.gald.annum.avg.MO = phyto.gald.grouped %>% 
	mutate(year = year(year.mon), 
				 month = month(year.mon, label = T)) %>% 
	drop_na() %>% 
	group_by(year) %>% 
	summarize(mean_maxgald = mean(max_gald, na.rm = T), 
						sd_maxgald = sd(max_gald, na.rm = T), 
						n = n()) %>% 
	ungroup() %>% 
	mutate(se_maxgald = sd_maxgald/sqrt(n)) %>% 
	mutate(t_score = qt(p=0.05/2, df = n - 1, lower.tail = F)) %>%
	mutate(marg.err = t_score*se_maxgald) %>% 
	mutate(l95 = mean_maxgald - marg.err, 
				 u95 = mean_maxgald + marg.err)
phyto.gald.annum.avg.MO

ggplot(phyto.gald.annum.avg.MO, aes(x = year, y = sd_maxgald)) + 
	geom_point(size = 4) + 
	geom_errorbar(aes(ymax = u95, ymin = l95))

# ANCOVA # 
nona_load_phyto = filter(nona_load, year < 2021) # phyto data only goes to 2020
nona_load_phyto

phyto.gald.annum.avg.MO

load_phyto.gald.annum = left_join(nona_load_phyto, phyto.gald.annum.avg.MO, by = 'year')
load_phyto.gald.annum

# Initial linear model # 
lm1 = lm(sd_maxgald~load_g_m3, data = load_phyto.gald.annum)
summary(lm1)
#plot(lm1) # Some high leverage points will try log-log

lm1.log = lm(log(sd_maxgald)~log(load_g_m3), data = load_phyto.gald.annum)
summary(lm1.log)
#plot(lm1.log) # diagnostics 

windows(height = 4, width = 5)
par(mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
plot(log(sd_maxgald)~log(load_g_m3), data = load_phyto.gald.annum, yaxt = 'n', xaxt = 'n', 
		 xlim = c(log(0.05), log(0.3)), ylim = c(log(50), log(200)), 
		 pch = 19, ylab = ' log[phyto GALD (um)]', xlab = 'log[P load (g/m3)]')
abline(lm1.log)
text(-1.5, log(50), 'p = 0.387')
text(-1.5, log(55), 'AdjustedR = ngl')
text(-1.5, log(60), 'Monona', font = 2)
axis(side = 1, 
		 at = c(log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
		 			 log(0.1), log(0.2), log(0.3)), 
		 labels = c('0.01', '0.02', '','0.04','','0.06','','0.08','','0.1', '0.2', '0.3'), 
		 las = 3
)
axis(side = 2, 
		 at = c(log(50), log(60), log(70), log(80), log(90), log(100), log(200), log(300),
		 			 log(400), log(500)), 
		 labels = c('50', '','','','','100',
		 					 '','','','500'))
mtext('log[Phyto mean monthly sd GALD (um)]', side = 2, line = 2)
mtext('log[P load (g/m3)]', side = 1, line = 2.5)

# Add grouping data # 
load_phyto.gald.annum = mutate(load_phyto.gald.annum, group = case_when(year < 2009 ~ "base",
																																				year > 2008 & year < 2015 ~ '+SWF', 
																																				year > 2014 ~ 'SWF+ZM')) %>% 
	mutate(log.sd_maxgald = log(sd_maxgald), 
				 log.load_g_m3 = log(load_g_m3)) %>% 
	mutate(group = as.factor(group))
load_phyto.gald.annum

anc1 = lm(log.sd_maxgald~log.load_g_m3*factor(group), data = load_phyto.gald.annum) # interactions n.s.
summary(anc1)
Anova(anc1)

library(emmeans)
library(cowplot)
devtools::install_github("Ryo-N7/tvthemes")
library(tvthemes)
library(scales)

# Want to compare the slope of the predictor across the levels of the predictor 
emmeans(anc1, specs = 'group')

anc1.ph = emtrends(anc1, specs = 'group', var = 'log.load_g_m3')
pairs(anc1.ph)

windows(height = 4, width = 6)
qplot(data = load_phyto.gald.annum, 
			x=load_g_m3, 
			y=sd_maxgald,
			geom = 'point', 
			colour = group, 
			shape = group) + 
	scale_x_continuous(trans = log_trans(), 
										 breaks = exp(-5:-1), 
										 labels = label_math(e^.x, format = log)) + 
	scale_y_continuous(trans = log_trans(), 
										 breaks = exp(0:20), 
										 labels = label_math(e^.x, format = log)) +
	xlab('Log[P load (g/m3)]') + 
	ylab('Log[Mean sd GALD (um)]') +
	geom_smooth(method = 'lm', se=T) + 
	scale_color_attackOnTitan() + 
	theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
										 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


### Relationship with cyano GALD variance ###=================================

# filter to cyanobacteria biomass 
cyano_gald = phyto_biomass %>% 
	as_tibble() %>% 
	filter(division == 'Cyanophyta') %>% 
	group_by(lakeid, sampledate, year) %>% 
	summarize(max_gald = max(gald)) %>% 
	ungroup() 
cyano_gald

# Mendota # ===========================
cyano.gald.grouped = cyano_gald %>% 
	filter(lakeid == 'ME') %>% 
	mutate(year.mon = as.yearmon(sampledate, '%b-%Y')) %>% 
	group_by(year.mon) %>%
	summarize(max_gald = max(max_gald)) %>% 
	ungroup() %>%
	complete(year.mon = seq(min(year.mon), max(year.mon), 1/12))
# group_by(year) %>%
# complete(month = seq(min(month), max(month), 1/12)) %>% 
# ungroup()
cyano.gald.grouped

cyano.gald.grouped.ts = ts(cyano.gald.grouped$max_gald, start = c(1995, 1), end = c(2020, 12), frequency = 12)
cyano.gald.ts.kal = na_kalman(cyano.gald.grouped.ts) # NAs in time series need to fill - impute with Kalman Filter 
plot(cyano.gald.ts.kal)

# Decompose cyano time series # 
cyano.gald.decomp = decompose(cyano.gald.ts.kal)
plot(cyano.gald.decomp)

cyano.gald.annum.avg.ME = cyano.gald.grouped %>% 
	mutate(year = year(year.mon), 
				 month = month(year.mon, label = T)) %>% 
	drop_na() %>% 
	group_by(year) %>% 
	summarize(mean_maxgald = mean(max_gald, na.rm = T), 
						sd_maxgald = sd(max_gald, na.rm = T), 
						n = n()) %>% 
	ungroup() %>% 
	mutate(se_maxgald = sd_maxgald/sqrt(n)) %>% 
	mutate(t_score = qt(p=0.05/2, df = n - 1, lower.tail = F)) %>%
	mutate(marg.err = t_score*se_maxgald) %>% 
	mutate(l95 = sd_maxgald - marg.err, 
				 u95 = sd_maxgald + marg.err)
cyano.gald.annum.avg.ME

ggplot(cyano.gald.annum.avg.ME, aes(x = year, y = sd_maxgald)) + 
	geom_point(size = 4) + 
	geom_errorbar(aes(ymax = u95, ymin = l95))

# ANCOVA # 
dota_load_cyano = filter(dota_load, year < 2021) # cyano data only goes to 2020
dota_load_cyano

cyano.gald.annum.avg.ME

load_cyano.gald.annum = left_join(dota_load_cyano, cyano.gald.annum.avg.ME, by = 'year')
load_cyano.gald.annum

# Initial linear model # 
lm1 = lm(sd_maxgald~load_g_m3, data = load_cyano.gald.annum)
summary(lm1)
#plot(lm1) # Some high leverage points will try log-log

lm1.log = lm(log(sd_maxgald)~log(load_g_m3), data = load_cyano.gald.annum)
summary(lm1.log)
#plot(lm1.log) # diagnostics 

windows(height = 4, width = 5)
par(mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
plot(log(sd_maxgald)~log(load_g_m3), data = load_cyano.gald.annum, yaxt = 'n', xaxt = 'n', 
		 xlim = c(log(0.01), log(0.2)), ylim = c(log(50), log(800)), 
		 pch = 19, ylab = ' log[cyano GALD (um)]', xlab = 'log[P load (g/m3)]')
abline(lm1.log)
text(-2.1, log(60), 'p=0.281')
text(-2.1, log(70), 'Mendota', font = 2)
axis(side = 1, 
		 at = c(log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
		 			 log(0.1), log(0.2)), 
		 labels = c('0.01', '0.02', '','0.04','','0.06','','0.08','','0.1', '0.2'), 
		 las = 3
)
axis(side = 2, 
		 at = c(log(50), log(60), log(70), log(80), log(90), log(100), log(200), log(300),
		 			 log(400), log(500), log(600), log(700), log(800)), 
		 labels = c('50', '','','','','100',
		 					 '','','','500','','',''))
mtext('log[cyano mean monthly sd GALD (um)]', side = 2, line = 2)
mtext('log[P load (g/m3)]', side = 1, line = 2.5)

# Add grouping data # 
load_cyano.gald.annum = mutate(load_cyano.gald.annum, group = case_when(year < 2009 ~ "base",
																																				year > 2008 & year < 2015 ~ '+SWF', 
																																				year > 2014 ~ 'SWF+ZM')) %>% 
	mutate(log.sd_maxgald = log(sd_maxgald), 
				 log.load_g_m3 = log(load_g_m3)) %>% 
	mutate(group = as.factor(group))
load_cyano.gald.annum

anc1 = lm(log.sd_maxgald~log.load_g_m3*factor(group), data = load_cyano.gald.annum) # interactions n.s.
summary(anc1)
Anova(anc1)

library(emmeans)
library(cowplot)
devtools::install_github("Ryo-N7/tvthemes")
library(tvthemes)
library(scales)

# Want to compare the slope of the predictor across the levels of the predictor 
emmeans(anc1, specs = 'group')

anc1.ph = emtrends(anc1, specs = 'group', var = 'log.load_g_m3')
pairs(anc1.ph)

windows(height = 4, width = 6)
qplot(data = load_cyano.gald.annum, 
			x=load_g_m3, 
			y=sd_maxgald,
			geom = 'point', 
			colour = group, 
			shape = group) + 
	scale_x_continuous(trans = log_trans(), 
										 breaks = exp(-5:-1), 
										 labels = label_math(e^.x, format = log)) + 
	scale_y_continuous(trans = log_trans(), 
										 breaks = exp(0:20), 
										 labels = label_math(e^.x, format = log)) +
	xlab('Log[P load (g/m3)]') + 
	ylab('Log[Mean sd GALD (um)]') +
	geom_smooth(method = 'lm', se=T) + 
	scale_color_attackOnTitan() + 
	theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
										 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Monona # ===========================
cyano.gald.grouped = cyano_gald %>% 
	filter(lakeid == 'MO') %>% 
	mutate(year.mon = as.yearmon(sampledate, '%b-%Y')) %>% 
	group_by(year.mon) %>%
	summarize(max_gald = max(max_gald)) %>% 
	ungroup() %>%
	complete(year.mon = seq(min(year.mon), max(year.mon), 1/12))
# group_by(year) %>%
# complete(month = seq(min(month), max(month), 1/12)) %>% 
# ungroup()
cyano.gald.grouped

cyano.gald.grouped.ts = ts(cyano.gald.grouped$max_gald, start = c(1995, 1), end = c(2020, 12), frequency = 12)
cyano.gald.ts.kal = na_kalman(cyano.gald.grouped.ts) # NAs in time series need to fill - impute with Kalman Filter 
plot(cyano.gald.ts.kal)

# Decompose cyano time series # 
cyano.gald.decomp = decompose(cyano.gald.ts.kal)
plot(cyano.gald.decomp)

cyano.gald.annum.avg.MO = cyano.gald.grouped %>% 
	mutate(year = year(year.mon), 
				 month = month(year.mon, label = T)) %>% 
	drop_na() %>% 
	group_by(year) %>% 
	summarize(sd_maxgald = mean(max_gald, na.rm = T), 
						sd_maxgald = sd(max_gald, na.rm = T), 
						n = n()) %>% 
	ungroup() %>% 
	mutate(se_maxgald = sd_maxgald/sqrt(n)) %>% 
	mutate(t_score = qt(p=0.05/2, df = n - 1, lower.tail = F)) %>%
	mutate(marg.err = t_score*se_maxgald) %>% 
	mutate(l95 = sd_maxgald - marg.err, 
				 u95 = sd_maxgald + marg.err)
cyano.gald.annum.avg.MO

ggplot(cyano.gald.annum.avg.MO, aes(x = year, y = sd_maxgald)) + 
	geom_point(size = 4) + 
	geom_errorbar(aes(ymax = u95, ymin = l95))

# ANCOVA # 
nona_load_cyano = filter(nona_load, year < 2021) # cyano data only goes to 2020
nona_load_cyano

cyano.gald.annum.avg.MO

load_cyano.gald.annum = left_join(nona_load_cyano, cyano.gald.annum.avg.MO, by = 'year')
load_cyano.gald.annum

# Initial linear model # 
lm1 = lm(sd_maxgald~load_g_m3, data = load_cyano.gald.annum)
summary(lm1)
#plot(lm1) # Some high leverage points will try log-log

lm1.log = lm(log(sd_maxgald)~log(load_g_m3), data = load_cyano.gald.annum)
summary(lm1.log)
#plot(lm1.log) # diagnostics 

windows(height = 4, width = 5)
par(mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
plot(log(sd_maxgald)~log(load_g_m3), data = load_cyano.gald.annum, yaxt = 'n', xaxt = 'n', 
		 xlim = c(log(0.05), log(0.3)), ylim = c(log(100), log(800)), 
		 pch = 19, ylab = ' log[cyano GALD (um)]', xlab = 'log[P load (g/m3)]')
abline(lm1.log)
text(-1.5, log(100), 'p = 0.164')
text(-1.5, log(115), 'AdjustedR = ngl')
text(-1.5, log(130), 'Monona', font = 2)
axis(side = 1, 
		 at = c(log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
		 			 log(0.1), log(0.2), log(0.3)), 
		 labels = c('0.01', '0.02', '','0.04','','0.06','','0.08','','0.1', '0.2', '0.3'), 
		 las = 3
)
axis(side = 2, 
		 at = c(log(50), log(60), log(70), log(80), log(90), log(100), log(200), log(300),
		 			 log(400), log(500), log(600), log(700), log(800)), 
		 labels = c('50', '','','','','100',
		 					 '','','','500','','','800'))
mtext('log[cyano mean monthly sd GALD (um)]', side = 2, line = 2)
mtext('log[P load (g/m3)]', side = 1, line = 2.5)

# Add grouping data # 
load_cyano.gald.annum = mutate(load_cyano.gald.annum, group = case_when(year < 2009 ~ "base",
																																				year > 2008 & year < 2015 ~ '+SWF', 
																																				year > 2014 ~ 'SWF+ZM')) %>% 
	mutate(log.sd_maxgald = log(sd_maxgald), 
				 log.load_g_m3 = log(load_g_m3)) %>% 
	mutate(group = as.factor(group))
load_cyano.gald.annum

anc1 = lm(log.sd_maxgald~log.load_g_m3*factor(group), data = load_cyano.gald.annum) # interactions n.s.
summary(anc1)
Anova(anc1)

library(emmeans)
library(cowplot)
devtools::install_github("Ryo-N7/tvthemes")
library(tvthemes)
library(scales)

# Want to compare the slope of the predictor across the levels of the predictor 
emmeans(anc1, specs = 'group')

anc1.ph = emtrends(anc1, specs = 'group', var = 'log.load_g_m3')
pairs(anc1.ph)

windows(height = 4, width = 6)
qplot(data = load_cyano.gald.annum, 
			x=load_g_m3, 
			y=sd_maxgald,
			geom = 'point', 
			colour = group, 
			shape = group) + 
	scale_x_continuous(trans = log_trans(), 
										 breaks = exp(-5:-1), 
										 labels = label_math(e^.x, format = log)) + 
	scale_y_continuous(trans = log_trans(), 
										 breaks = exp(0:20), 
										 labels = label_math(e^.x, format = log)) +
	xlab('Log[P load (g/m3)]') + 
	ylab('Log[Mean sd GALD (um)]') +
	geom_smooth(method = 'lm', se=T) + 
	scale_color_attackOnTitan() + 
	theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
										 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


## Nutrient Data ###====================================
# Package ID: knb-lter-ntl.1.59 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Chemical Limnology of Primary Study Lakes: Nutrients, pH and Carbon 1981 - current.
# Data set creator:  John Magnuson - University of Wisconsin-Madison 
# Data set creator:  Stephen Carpenter - University of Wisconsin-Madison 
# Data set creator:  Emily Stanley - University of Wisconsin-Madison 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/1/59/0ff1fd13116d6097376e3745194cdc5f" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
							 ,skip=1
							 ,sep=","  
							 ,quot='"' 
							 , col.names=c(
							 	"lakeid",     
							 	"year4",     
							 	"daynum",     
							 	"sampledate",     
							 	"depth",     
							 	"rep",     
							 	"sta",     
							 	"event",     
							 	"ph",     
							 	"phair",     
							 	"alk",     
							 	"dic",     
							 	"tic",     
							 	"doc",     
							 	"toc",     
							 	"no3no2",     
							 	"no2",     
							 	"nh4",     
							 	"totnf",     
							 	"totnuf",     
							 	"totpf",     
							 	"totpuf",     
							 	"drsif",     
							 	"brsif",     
							 	"brsiuf",     
							 	"tpm",     
							 	"totnuf_sloh",     
							 	"no3no2_sloh",     
							 	"nh4_sloh",     
							 	"kjdl_n_sloh",     
							 	"totpuf_sloh",     
							 	"drp_sloh",     
							 	"drsif_sloh",     
							 	"flagdepth",     
							 	"flagph",     
							 	"flagphair",     
							 	"flagalk",     
							 	"flagdic",     
							 	"flagtic",     
							 	"flagdoc",     
							 	"flagtoc",     
							 	"flagno3no2",     
							 	"flagno2",     
							 	"flagnh4",     
							 	"flagtotnf",     
							 	"flagtotnuf",     
							 	"flagtotpf",     
							 	"flagtotpuf",     
							 	"flagdrsif",     
							 	"flagbrsif",     
							 	"flagbrsiuf",     
							 	"flagtpm",     
							 	"flagtotnuf_sloh",     
							 	"flagno3no2_sloh",     
							 	"flagnh4_sloh",     
							 	"flagkjdl_n_sloh",     
							 	"flagtotpuf_sloh",     
							 	"flagdrp_sloh",     
							 	"flagdrsif_sloh"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]               
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$rep)!="factor") dt1$rep<- as.factor(dt1$rep)
if (class(dt1$sta)!="factor") dt1$sta<- as.factor(dt1$sta)
if (class(dt1$event)!="factor") dt1$event<- as.factor(dt1$event)
if (class(dt1$ph)=="factor") dt1$ph <-as.numeric(levels(dt1$ph))[as.integer(dt1$ph) ]               
if (class(dt1$ph)=="character") dt1$ph <-as.numeric(dt1$ph)
if (class(dt1$phair)=="factor") dt1$phair <-as.numeric(levels(dt1$phair))[as.integer(dt1$phair) ]               
if (class(dt1$phair)=="character") dt1$phair <-as.numeric(dt1$phair)
if (class(dt1$alk)=="factor") dt1$alk <-as.numeric(levels(dt1$alk))[as.integer(dt1$alk) ]               
if (class(dt1$alk)=="character") dt1$alk <-as.numeric(dt1$alk)
if (class(dt1$dic)=="factor") dt1$dic <-as.numeric(levels(dt1$dic))[as.integer(dt1$dic) ]               
if (class(dt1$dic)=="character") dt1$dic <-as.numeric(dt1$dic)
if (class(dt1$tic)=="factor") dt1$tic <-as.numeric(levels(dt1$tic))[as.integer(dt1$tic) ]               
if (class(dt1$tic)=="character") dt1$tic <-as.numeric(dt1$tic)
if (class(dt1$doc)=="factor") dt1$doc <-as.numeric(levels(dt1$doc))[as.integer(dt1$doc) ]               
if (class(dt1$doc)=="character") dt1$doc <-as.numeric(dt1$doc)
if (class(dt1$toc)=="factor") dt1$toc <-as.numeric(levels(dt1$toc))[as.integer(dt1$toc) ]               
if (class(dt1$toc)=="character") dt1$toc <-as.numeric(dt1$toc)
if (class(dt1$no3no2)=="factor") dt1$no3no2 <-as.numeric(levels(dt1$no3no2))[as.integer(dt1$no3no2) ]               
if (class(dt1$no3no2)=="character") dt1$no3no2 <-as.numeric(dt1$no3no2)
if (class(dt1$no2)=="factor") dt1$no2 <-as.numeric(levels(dt1$no2))[as.integer(dt1$no2) ]               
if (class(dt1$no2)=="character") dt1$no2 <-as.numeric(dt1$no2)
if (class(dt1$nh4)=="factor") dt1$nh4 <-as.numeric(levels(dt1$nh4))[as.integer(dt1$nh4) ]               
if (class(dt1$nh4)=="character") dt1$nh4 <-as.numeric(dt1$nh4)
if (class(dt1$totnf)=="factor") dt1$totnf <-as.numeric(levels(dt1$totnf))[as.integer(dt1$totnf) ]               
if (class(dt1$totnf)=="character") dt1$totnf <-as.numeric(dt1$totnf)
if (class(dt1$totnuf)=="factor") dt1$totnuf <-as.numeric(levels(dt1$totnuf))[as.integer(dt1$totnuf) ]               
if (class(dt1$totnuf)=="character") dt1$totnuf <-as.numeric(dt1$totnuf)
if (class(dt1$totpf)=="factor") dt1$totpf <-as.numeric(levels(dt1$totpf))[as.integer(dt1$totpf) ]               
if (class(dt1$totpf)=="character") dt1$totpf <-as.numeric(dt1$totpf)
if (class(dt1$totpuf)=="factor") dt1$totpuf <-as.numeric(levels(dt1$totpuf))[as.integer(dt1$totpuf) ]               
if (class(dt1$totpuf)=="character") dt1$totpuf <-as.numeric(dt1$totpuf)
if (class(dt1$drsif)=="factor") dt1$drsif <-as.numeric(levels(dt1$drsif))[as.integer(dt1$drsif) ]               
if (class(dt1$drsif)=="character") dt1$drsif <-as.numeric(dt1$drsif)
if (class(dt1$brsif)=="factor") dt1$brsif <-as.numeric(levels(dt1$brsif))[as.integer(dt1$brsif) ]               
if (class(dt1$brsif)=="character") dt1$brsif <-as.numeric(dt1$brsif)
if (class(dt1$brsiuf)=="factor") dt1$brsiuf <-as.numeric(levels(dt1$brsiuf))[as.integer(dt1$brsiuf) ]               
if (class(dt1$brsiuf)=="character") dt1$brsiuf <-as.numeric(dt1$brsiuf)
if (class(dt1$tpm)=="factor") dt1$tpm <-as.numeric(levels(dt1$tpm))[as.integer(dt1$tpm) ]               
if (class(dt1$tpm)=="character") dt1$tpm <-as.numeric(dt1$tpm)
if (class(dt1$totnuf_sloh)=="factor") dt1$totnuf_sloh <-as.numeric(levels(dt1$totnuf_sloh))[as.integer(dt1$totnuf_sloh) ]               
if (class(dt1$totnuf_sloh)=="character") dt1$totnuf_sloh <-as.numeric(dt1$totnuf_sloh)
if (class(dt1$no3no2_sloh)=="factor") dt1$no3no2_sloh <-as.numeric(levels(dt1$no3no2_sloh))[as.integer(dt1$no3no2_sloh) ]               
if (class(dt1$no3no2_sloh)=="character") dt1$no3no2_sloh <-as.numeric(dt1$no3no2_sloh)
if (class(dt1$nh4_sloh)=="factor") dt1$nh4_sloh <-as.numeric(levels(dt1$nh4_sloh))[as.integer(dt1$nh4_sloh) ]               
if (class(dt1$nh4_sloh)=="character") dt1$nh4_sloh <-as.numeric(dt1$nh4_sloh)
if (class(dt1$kjdl_n_sloh)=="factor") dt1$kjdl_n_sloh <-as.numeric(levels(dt1$kjdl_n_sloh))[as.integer(dt1$kjdl_n_sloh) ]               
if (class(dt1$kjdl_n_sloh)=="character") dt1$kjdl_n_sloh <-as.numeric(dt1$kjdl_n_sloh)
if (class(dt1$totpuf_sloh)=="factor") dt1$totpuf_sloh <-as.numeric(levels(dt1$totpuf_sloh))[as.integer(dt1$totpuf_sloh) ]               
if (class(dt1$totpuf_sloh)=="character") dt1$totpuf_sloh <-as.numeric(dt1$totpuf_sloh)
if (class(dt1$drp_sloh)=="factor") dt1$drp_sloh <-as.numeric(levels(dt1$drp_sloh))[as.integer(dt1$drp_sloh) ]               
if (class(dt1$drp_sloh)=="character") dt1$drp_sloh <-as.numeric(dt1$drp_sloh)
if (class(dt1$drsif_sloh)=="factor") dt1$drsif_sloh <-as.numeric(levels(dt1$drsif_sloh))[as.integer(dt1$drsif_sloh) ]               
if (class(dt1$drsif_sloh)=="character") dt1$drsif_sloh <-as.numeric(dt1$drsif_sloh)
if (class(dt1$flagdepth)!="factor") dt1$flagdepth<- as.factor(dt1$flagdepth)
if (class(dt1$flagph)!="factor") dt1$flagph<- as.factor(dt1$flagph)
if (class(dt1$flagphair)!="factor") dt1$flagphair<- as.factor(dt1$flagphair)
if (class(dt1$flagalk)!="factor") dt1$flagalk<- as.factor(dt1$flagalk)
if (class(dt1$flagdic)!="factor") dt1$flagdic<- as.factor(dt1$flagdic)
if (class(dt1$flagtic)!="factor") dt1$flagtic<- as.factor(dt1$flagtic)
if (class(dt1$flagdoc)!="factor") dt1$flagdoc<- as.factor(dt1$flagdoc)
if (class(dt1$flagtoc)!="factor") dt1$flagtoc<- as.factor(dt1$flagtoc)
if (class(dt1$flagno3no2)!="factor") dt1$flagno3no2<- as.factor(dt1$flagno3no2)
if (class(dt1$flagno2)!="factor") dt1$flagno2<- as.factor(dt1$flagno2)
if (class(dt1$flagnh4)!="factor") dt1$flagnh4<- as.factor(dt1$flagnh4)
if (class(dt1$flagtotnf)!="factor") dt1$flagtotnf<- as.factor(dt1$flagtotnf)
if (class(dt1$flagtotnuf)!="factor") dt1$flagtotnuf<- as.factor(dt1$flagtotnuf)
if (class(dt1$flagtotpf)!="factor") dt1$flagtotpf<- as.factor(dt1$flagtotpf)
if (class(dt1$flagtotpuf)!="factor") dt1$flagtotpuf<- as.factor(dt1$flagtotpuf)
if (class(dt1$flagdrsif)!="factor") dt1$flagdrsif<- as.factor(dt1$flagdrsif)
if (class(dt1$flagbrsif)!="factor") dt1$flagbrsif<- as.factor(dt1$flagbrsif)
if (class(dt1$flagbrsiuf)!="factor") dt1$flagbrsiuf<- as.factor(dt1$flagbrsiuf)
if (class(dt1$flagtpm)!="factor") dt1$flagtpm<- as.factor(dt1$flagtpm)
if (class(dt1$flagtotnuf_sloh)!="factor") dt1$flagtotnuf_sloh<- as.factor(dt1$flagtotnuf_sloh)
if (class(dt1$flagno3no2_sloh)!="factor") dt1$flagno3no2_sloh<- as.factor(dt1$flagno3no2_sloh)
if (class(dt1$flagnh4_sloh)!="factor") dt1$flagnh4_sloh<- as.factor(dt1$flagnh4_sloh)
if (class(dt1$flagkjdl_n_sloh)!="factor") dt1$flagkjdl_n_sloh<- as.factor(dt1$flagkjdl_n_sloh)
if (class(dt1$flagtotpuf_sloh)!="factor") dt1$flagtotpuf_sloh<- as.factor(dt1$flagtotpuf_sloh)
if (class(dt1$flagdrp_sloh)!="factor") dt1$flagdrp_sloh<- as.factor(dt1$flagdrp_sloh)
if (class(dt1$flagdrsif_sloh)!="factor") dt1$flagdrsif_sloh<- as.factor(dt1$flagdrsif_sloh)


chem_data = dt1
chem_data

### Relationship with total phosphorus ###============================ 

# Mendota surface P concentrations # 
mendota_chem = chem_data %>% 
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
				 tp2_sd_ugL = tp2_sd*1000) %>% 
	mutate(tp_ugL_comb = coalesce(tp_ugL, tp2_ugL), 
				 tp_sd_comb = coalesce(tp_sd_ugL, tp2_sd_ugL)) %>% 
	select(year4, tp_ugL_comb, tp_sd_comb) %>% 
	mutate(lake = 'mendota')
mendota_totp

plot(tp_ugL_comb~year4, data = mendota_totp, type = 'o')

#### ANCOVA Analysis #========================
totp_annum.ME = mendota_totp %>% 
	rename(year = year4) %>% 
	mutate(u_sd = tp_ugL_comb + tp_sd_comb, 
				 l_sd = tp_ugL_comb - tp_sd_comb)
totp_annum.ME

ggplot(totp_annum.ME, aes(x = year, y = tp_ugL_comb)) + 
	geom_point(size = 4) + 
	geom_errorbar(aes(ymax = u_sd, ymin = l_sd))

# ANCOVA # 
dota_load = filter(dota_load, year < 2022) # nutrient data only goes to 2021
dota_load

load_surftotp.annum = left_join(dota_load, totp_annum.ME, by = 'year')
load_surftotp.annum

# Initial linear model # 
lm1 = lm(tp_ugL_comb~load_g_m3, data = load_surftotp.annum)
summary(lm1)
plot(lm1) # not bad 

lm1.log = lm(log(tp_ugL_comb)~log(load_g_m3), data = load_surftotp.annum)
summary(lm1.log)
#plot(lm1.log) # diagnostics 

windows(height = 4, width = 5)
par(mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
plot(tp_ugL_comb~load_g_m3, data = load_surftotp.annum, 
		 xlim = c(0.009, 0.2), ylim = c(50, 150), 
		 pch = 19, ylab = ' phytoplankton biomass (mg/L)', xlab = 'P load (g/m3)')
abline(lm1)
text(0.18, 50, 'p = 0.006')
text(0.18, 53, 'AdjustedR = 23.48%')
text(0.18, 56, 'Mendota', font = 2)
mtext('Surface TP (ug/L)', side = 2, line = 2)
mtext('P load (g/m3)', side = 1, line = 2.5)

# Add grouping data # 
load_surftotp.annum = mutate(load_phyto.annum, group = case_when(year < 2009 ~ "base",
																															year > 2008 & year < 2015 ~ '+SWF', 
																															year > 2014 ~ 'SWF+ZM')) %>% 
	mutate(log.mean_biomass = log(mean_biomass), 
				 log.load_g_m3 = log(load_g_m3)) %>% 
	mutate(group = as.factor(group))
load_phyto.annum

anc1 = lm(log.mean_biomass~log.load_g_m3*factor(group), data = load_phyto.annum) # interactions n.s.
summary(anc1)
Anova(anc1)

library(emmeans)
library(cowplot)
devtools::install_github("Ryo-N7/tvthemes")
library(tvthemes)
library(scales)

# Want to compare the slope of the predictor across the levels of the predictor 
emmeans(anc1, specs = 'group')

anc1.ph = emtrends(anc1, specs = 'group', var = 'log.load_g_m3')
pairs(anc1.ph)

windows(height = 4, width = 6)
qplot(data = load_phyto.annum, 
			x=load_g_m3, 
			y=mean_biomass,
			geom = 'point', 
			colour = group, 
			shape = group) + 
	scale_x_continuous(trans = log_trans(), 
										 breaks = exp(-5:-1), 
										 labels = label_math(e^.x, format = log)) + 
	scale_y_continuous(trans = log_trans(), 
										 breaks = exp(0:20), 
										 labels = label_math(e^.x, format = log)) +
	xlab('Log[P load (g/m3)]') + 
	ylab('Log[Phytoplankton biomass (mg/L)]') +
	geom_smooth(method = 'lm', se=T) + 
	scale_color_attackOnTitan() + 
	theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
										 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


### Relationship with soluble reactive phosphorus ###==================

### Relationship with nitrate ###====================

### Relationship with ammonium ###===================