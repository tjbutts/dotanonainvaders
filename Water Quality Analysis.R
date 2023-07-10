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
plot(lm1) # pretty skewed - will try log-log 

lm1.log = lm(log(mean_biomass)~log(load_g_m3), data = load_phyto.annum)
summary(lm1.log)
plot(lm1.log) # diagnostics 

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
plot(lm1) # pretty skewed - will try log-log 

lm1.log = lm(log(mean_biomass)~log(load_g_m3), data = load_phyto.annum)
summary(lm1.log)
plot(lm1.log) # diagnostics 

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
plot(lm1) # Some high leverage points will try log-log

lm1.log = lm(log(mean_biomass)~log(load_g_m3), data = load_cyano.annum)
summary(lm1.log)
plot(lm1.log) # diagnostics 

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
plot(lm1) # Some high leverage points will try log-log

lm1.log = lm(log(mean_biomass)~log(load_g_m3), data = load_cyano.annum)
summary(lm1.log)
plot(lm1.log) # diagnostics 

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

### Relationship with total phosphorus ###============================

### Relationship with soluble reactive phosphorus ###==================

### Relationship with nitrate ###====================

### Relationship with ammonium ###===================