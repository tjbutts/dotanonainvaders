## Phytoplankton GALD distribution ##=============

library(tidyverse)
library(lubridate)
library(zoo)
library(mgcv)

# Phyto data # ========================
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

                   
phyto <-read.csv(infile1,header=F 
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
                
if (class(phyto$lakeid)!="factor") phyto$lakeid<- as.factor(phyto$lakeid)
if (class(phyto$year4)=="factor") phyto$year4 <-as.numeric(levels(phyto$year4))[as.integer(phyto$year4) ]               
if (class(phyto$year4)=="character") phyto$year4 <-as.numeric(phyto$year4)                                   
# attempting to convert phyto$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(phyto$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){phyto$sampledate <- tmp1sampledate } else {print("Date conversion failed for phyto$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(phyto$sta)!="factor") phyto$sta<- as.factor(phyto$sta)
if (class(phyto$depth_range)!="factor") phyto$depth_range<- as.factor(phyto$depth_range)
if (class(phyto$division)!="factor") phyto$division<- as.factor(phyto$division)
if (class(phyto$taxa_name)!="factor") phyto$taxa_name<- as.factor(phyto$taxa_name)
if (class(phyto$gald)=="factor") phyto$gald <-as.numeric(levels(phyto$gald))[as.integer(phyto$gald) ]               
if (class(phyto$gald)=="character") phyto$gald <-as.numeric(phyto$gald)
if (class(phyto$cells_per_nu)=="factor") phyto$cells_per_nu <-as.numeric(levels(phyto$cells_per_nu))[as.integer(phyto$cells_per_nu) ]               
if (class(phyto$cells_per_nu)=="character") phyto$cells_per_nu <-as.numeric(phyto$cells_per_nu)
if (class(phyto$nu_per_ml)=="factor") phyto$nu_per_ml <-as.numeric(levels(phyto$nu_per_ml))[as.integer(phyto$nu_per_ml) ]               
if (class(phyto$nu_per_ml)=="character") phyto$nu_per_ml <-as.numeric(phyto$nu_per_ml)
if (class(phyto$cells_per_ml)=="factor") phyto$cells_per_ml <-as.numeric(levels(phyto$cells_per_ml))[as.integer(phyto$cells_per_ml) ]               
if (class(phyto$cells_per_ml)=="character") phyto$cells_per_ml <-as.numeric(phyto$cells_per_ml)
if (class(phyto$biovolume_conc)=="factor") phyto$biovolume_conc <-as.numeric(levels(phyto$biovolume_conc))[as.integer(phyto$biovolume_conc) ]               
if (class(phyto$biovolume_conc)=="character") phyto$biovolume_conc <-as.numeric(phyto$biovolume_conc)
if (class(phyto$biomass_conc)=="factor") phyto$biomass_conc <-as.numeric(levels(phyto$biomass_conc))[as.integer(phyto$biomass_conc) ]               
if (class(phyto$biomass_conc)=="character") phyto$biomass_conc <-as.numeric(phyto$biomass_conc)
if (class(phyto$relative_total_biovolume)=="factor") phyto$relative_total_biovolume <-as.numeric(levels(phyto$relative_total_biovolume))[as.integer(phyto$relative_total_biovolume) ]               
if (class(phyto$relative_total_biovolume)=="character") phyto$relative_total_biovolume <-as.numeric(phyto$relative_total_biovolume)
if (class(phyto$genus)!="factor") phyto$genus<- as.factor(phyto$genus)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(phyto)                            
attach(phyto)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(lakeid)
summary(year4)
summary(sampledate)
summary(sta)
summary(depth_range)
summary(division)
summary(taxa_name)
summary(gald)
summary(cells_per_nu)
summary(nu_per_ml)
summary(cells_per_ml)
summary(biovolume_conc)
summary(biomass_conc)
summary(relative_total_biovolume)
summary(genus) 
                # Get more details on character variables
                 
summary(as.factor(phyto$lakeid)) 
summary(as.factor(phyto$sta)) 
summary(as.factor(phyto$depth_range)) 
summary(as.factor(phyto$division)) 
summary(as.factor(phyto$taxa_name)) 
summary(as.factor(phyto$genus))
detach(phyto)               
phyto = as_tibble(phyto)     
phyto

# Monthly median GALD # #=============================
gald_ME = phyto %>% 
	select(lakeid, year4, sampledate, division, gald, cells_per_ml) %>% 
	filter(lakeid == 'ME') %>% 
	group_by(sampledate) %>% 
	summarize(median_gald = median(gald, na.rm = T)) %>% 
	ungroup() %>% 
	mutate(doy = yday(sampledate), 
				 year = year(sampledate), 
				 year.mon = as.yearmon(sampledate)) %>% 
	mutate(group = case_when(year < 2009 ~ 'base', 
													 year > 2008 & year < 2016 ~ 'SWF', 
													 year > 2015 ~ 'ZM'))
gald_ME

# Because we will be plotting by period, make some separate data frames to make life easier
gald_base = gald_ME %>% 
	filter(group == 'base')
gald_base

gald_SWF = gald_ME %>% 
	filter(group == 'SWF') 

gald_ZM = gald_ME %>% 
	filter(group == 'ZM')


# Base GAM
gald_base_gam <- gam(median_gald ~ s(doy, k = 40, bs = 'cs') ,  data = gald_base, method = 'REML') # Cubic Regression Spline
summary(gald_base_gam) 
gam.check(gald_base_gam)

gald_SWF_gam <- gam(median_gald ~ s(doy, k = 40, bs = 'cs') ,  data = gald_SWF, method = 'REML') # Cubic Regression Spline
summary(gald_SWF_gam) 
gam.check(gald_SWF_gam)

gald_ZM_gam <- gam(median_gald ~ s(doy, k = 40, bs = 'cs') ,  data = gald_ZM, method = 'REML') # Cubic Regression Spline
summary(gald_ZM_gam) 
gam.check(gald_ZM_gam)

#Plotting Colors
#Colors for data visualization
#Ref: lty = 3, pulse: lty = 1
low_col_B = rgb(74, 166, 81, max = 255, alpha = 180) #Pond B, Pond F
low_col_F = rgb(74, 166, 81, max = 255, alpha = 100) #Pond B, Pond F
low_col = rgb(74, 166, 81, max = 255, alpha = 255) #Pond B, Pond F
ref_col = rgb(155, 155, 155, max=255, alpha = 100) # Reference
black_col = rgb(0,0,0, max=255, alpha = 100) # Black

int_col_A = rgb(44, 127, 184, max = 255, alpha = 180) #Pond A, pond D
int_col_D = rgb(44, 127, 184, max = 255, alpha = 100) #Pond A, pond D
int_col = rgb(44, 127, 184, max = 255, alpha = 255) #Pond A, pond D

high_col_C = rgb(8, 29, 88, max = 255, alpha = 180) #Pond C, Pond E
high_col_E = rgb(8, 29, 88, max = 255, alpha = 100) #Pond C, Pond E
high_col = rgb(8, 29, 88, max = 255, alpha = 255) #Pond C, Pond E

#============================
# Set up Blank Plot 
windows(height = 5, width = 7)
par(omi = c(0.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

#Plot of the Total P GAM for POND B ===============
plot(gald_base_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(gald_base_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = low_col_F, xlim = c(0,366), ylim = c(0, 50),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = low_col, 
		 xlab = "", ylab = "", cex.axis= 1.2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot

plot(gald_SWF_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(gald_SWF_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = high_col_E, xlim = c(0,366), ylim = c(0, 50),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = high_col, 
		 xlab = "", ylab = "", cex.axis= 1.2)

par(new = TRUE) #add new smooth to the same plot 

plot(gald_ZM_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(gald_ZM_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = int_col_D, xlim = c(0,366), ylim = c(0, 50),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = int_col, 
		 xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 2.5, "Median GALD", cex = 1.25)
mtext(side = 1, line = 2.5, 'Day of Year', cex = 1.25)
legend('topleft', legend = c('Pre', '+SWF', '+ZM'), pch = 19, col = c(low_col, high_col, int_col), 
			 bg = 'white')

## Monona ##================================
gald_MO = phyto %>% 
	select(lakeid, year4, sampledate, division, gald, cells_per_ml) %>% 
	filter(lakeid == 'MO') %>% 
	group_by(sampledate) %>% 
	summarize(median_gald = median(gald, na.rm = T)) %>% 
	ungroup() %>% 
	mutate(doy = yday(sampledate), 
				 year = year(sampledate), 
				 year.mon = as.yearmon(sampledate)) %>% 
	mutate(group = case_when(year < 2009 ~ 'base', 
													 year > 2008 & year < 2016 ~ 'SWF', 
													 year > 2015 ~ 'ZM'))
gald_MO

# Because we will be plotting by period, make some separate data frames to make life easier
gald_base = gald_MO %>% 
	filter(group == 'base')
gald_base

gald_SWF = gald_MO %>% 
	filter(group == 'SWF') 

gald_ZM = gald_MO %>% 
	filter(group == 'ZM')


# Base GAM
gald_base_gam <- gam(median_gald ~ s(doy, k = 40, bs = 'cs') ,  data = gald_base, method = 'REML') # Cubic Regression Spline
summary(gald_base_gam) 
gam.check(gald_base_gam)

gald_SWF_gam <- gam(median_gald ~ s(doy, k = 40, bs = 'cs') ,  data = gald_SWF, method = 'REML') # Cubic Regression Spline
summary(gald_SWF_gam) 
gam.check(gald_SWF_gam)

gald_ZM_gam <- gam(median_gald ~ s(doy, k = 40, bs = 'cs') ,  data = gald_ZM, method = 'REML') # Cubic Regression Spline
summary(gald_ZM_gam) 
gam.check(gald_ZM_gam)

#Plotting Colors
#Colors for data visualization
#Ref: lty = 3, pulse: lty = 1
low_col_B = rgb(74, 166, 81, max = 255, alpha = 180) #Pond B, Pond F
low_col_F = rgb(74, 166, 81, max = 255, alpha = 100) #Pond B, Pond F
low_col = rgb(74, 166, 81, max = 255, alpha = 255) #Pond B, Pond F
ref_col = rgb(155, 155, 155, max=255, alpha = 100) # Reference
black_col = rgb(0,0,0, max=255, alpha = 100) # Black

int_col_A = rgb(44, 127, 184, max = 255, alpha = 180) #Pond A, pond D
int_col_D = rgb(44, 127, 184, max = 255, alpha = 100) #Pond A, pond D
int_col = rgb(44, 127, 184, max = 255, alpha = 255) #Pond A, pond D

high_col_C = rgb(8, 29, 88, max = 255, alpha = 180) #Pond C, Pond E
high_col_E = rgb(8, 29, 88, max = 255, alpha = 100) #Pond C, Pond E
high_col = rgb(8, 29, 88, max = 255, alpha = 255) #Pond C, Pond E

#============================
# Set up Blank Plot 
windows(height = 5, width = 7)
par(omi = c(0.5,0.5,0.5,0.1), mai = c(0.3,0.3,0.1,0.1))

#Plot of the Total P GAM for POND B ===============
plot(gald_base_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(gald_base_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = low_col_F, xlim = c(0,366), ylim = c(0, 50),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = low_col, 
		 xlab = "", ylab = "", cex.axis= 1.2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot

plot(gald_SWF_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(gald_SWF_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = high_col_E, xlim = c(0,366), ylim = c(0, 50),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = high_col, 
		 xlab = "", ylab = "", cex.axis= 1.2)

par(new = TRUE) #add new smooth to the same plot 

plot(gald_ZM_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(gald_ZM_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = int_col_D, xlim = c(0,366), ylim = c(0, 50),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = int_col, 
		 xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 2.5, "Median GALD", cex = 1.25)
mtext(side = 1, line = 2.5, 'Day of Year', cex = 1.25)

# ggridges plot - mean value per taxa weighted by number of cells per ml  ##==================
library(ggridges)
gald_dist_ME = phyto %>% 
	filter(lakeid == 'ME') %>%
	select(sampledate, division, taxa_name, gald, cells_per_ml) %>% 
	group_by(sampledate, division) %>% 
	summarize(median_gald = median(gald), 
						sd_gald = sd(gald),
						median_cell = median(cells_per_ml)) %>% 
	ungroup() %>% 
	mutate(year = year(sampledate), 
				 doy = yday(sampledate), 
				 month = month(sampledate)) %>% 
	mutate(group = case_when(year < 2009 ~ 'base', 
													 year > 2008 & year < 2016 ~ 'SWF', 
													 year > 2015 ~ 'ZM')) %>% 
	mutate(month = as.factor(month))
gald_dist_ME

# scales 
mins = min(gald_dist_ME$median_gald)
maxs = max(gald_dist_ME$median_gald)

#install.packages('hrbrthemes')
library(hrbrthemes)

base_dist = gald_dist_ME %>% 
	filter(group == 'base')
swf_dist = gald_dist_ME %>%
	filter(group == 'SWF')
zm_dist = gald_dist_ME %>%
	filter(group == 'ZM')

windows(height = 6, width = 7)
ggplot(base_dist, aes(x = median_gald, y = month, height = ..density.., fill = group))+
	geom_density_ridges(scale = 3) + 
	scale_x_continuous(limits = c(0,50)) + 
	scale_fill_manual(values = 'seagreen3') +
	theme_ipsum(grid = F) + 
	theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.y = element_text(angle = 180, hjust = 1))

ggplot(swf_dist, aes(x = median_gald, y = month, height = ..density.., fill = group))+
	geom_density_ridges(scale = 3) + 
	scale_x_continuous(limits = c(0,50)) + 
	scale_fill_manual(values = 'dodgerblue3') +
	theme_ipsum(grid = F) + 
	theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.y = element_text(angle = 180, hjust = 1))

ggplot(zm_dist, aes(x = median_gald, y = month, height = ..density.., fill = group))+
	geom_density_ridges(scale = 3) + 
	scale_x_continuous(limits = c(0,50)) + 
	scale_fill_manual(values = 'gray80') +
	theme_ipsum(grid = F) + 
	theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.y = element_text(angle = 180, hjust = 1))

# Playing around with ggplot styles #===================
cols = c('#E69F00', '#56B4E9', '#009E73')
cols_dark = c('#C18511', '#289ACF', '#0D8561')

ggplot(iris, aes(x=Sepal.Length, y=Species, fill = Species)) + 
	geom_density_ridges(aes(point_color = Species, point_fill = Species, point_shape = Species), 
											alpha = 0.2, jittered_points = TRUE) +
	scale_y_discrete(expand = c(0.01, 0)) +
	scale_point_color_hue(l=40) +
	scale_discrete_manual(aesthetic = 'point_shape', values = c(21,22,23)) +
	theme_ridges(grid = F, center = T)

gald_growing_ME = gald_dist_ME %>% 
	mutate(month = as.double(month)) %>% 
	filter(month > 4 & month < 10) %>% 
	mutate(month = as.factor(month))
gald_growing_ME

ggplot(gald_growing_ME, aes(x=median_gald, y=month, fill = group)) + 
	geom_density_ridges(aes(point_color = group, point_fill = group, point_shape = group),
											alpha = 0.5, jittered_points = F, scale = 0.95, rel_min_height = 0.01) + 
	scale_fill_manual(values = c('seagreen3', 'dodgerblue3', 'black')) + 
	scale_y_discrete(expand = c(0.01, 0)) + 
	xlim(0, 50) + 
	scale_point_color_hue(aesthetics = 'point_color') + 
	scale_discrete_manual(aesthetic = 'point_shape', values = c(21,22,23)) + 
	theme_ridges(grid = F, center = T)
