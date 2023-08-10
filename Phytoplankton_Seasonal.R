# Seasonal Plotting - GAMs # 

library(tidyverse)
library(lubridate)
library(zoo)

# Phytoplankton Seasonal #=========================
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

# Phyto Biomass_Sum #============================

# Mendota #========================
phyto_ME_sum = phyto %>% 
	filter(lakeid == 'ME') %>% 
	group_by(sampledate) %>% 
	summarize(biomass_mgL = sum(biomass_conc)) %>% 
	ungroup() %>% 
	mutate(doy = yday(sampledate), 
				 year = year(sampledate)) %>% 
	mutate(group = case_when(year < 2009 ~ 'base', 
													 year > 2008 & year < 2016 ~ 'SWF', 
													 year > 2015 ~ 'ZM'))
phyto_ME_sum

# Because we will be plotting by period, make some separate data frames to make life easier
phyto_base = phyto_ME_sum %>% 
	filter(group == 'base')
phyto_base

phyto_SWF = phyto_ME_sum %>% 
	filter(group == 'SWF') 

phyto_ZM = phyto_ME_sum %>% 
	filter(group == 'ZM')

# GAM-ing the secchi for pattern - not analysis...

# Base GAM
phyto_base_gam <- gam(biomass_mgL ~ s(doy, k = 150, bs = 'cc') , Gamma(link = 'log'), data = phyto_base, method = 'REML') # Cubic Regression Spline
summary(phyto_base_gam) 
gam.check(phyto_base_gam)

phyto_SWF_gam <- gam(biomass_mgL ~ s(doy, k = 80, bs = 'cc') , Gamma(link = 'log'), data = phyto_SWF, method = 'REML') # Cubic Regression Spline
summary(phyto_SWF_gam) 
gam.check(phyto_SWF_gam)

phyto_ZM_gam <- gam(biomass_mgL ~ s(doy, k = 60, bs = 'cc') , Gamma(link = 'log'), data = phyto_ZM, method = 'REML') # Cubic Regression Spline
summary(phyto_ZM_gam) 
gam.check(phyto_ZM_gam)

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
plot(phyto_base_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(phyto_base_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = low_col_F, xlim = c(0,366), ylim = c(log(0.0001), log(25)),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = low_col, yaxt = 'n',
		 xlab = "", ylab = "", cex.axis= 1.2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot

plot(phyto_SWF_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(phyto_SWF_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = high_col_E, xlim = c(0,366), ylim = c(log(0.0001),log(25)),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = high_col, yaxt = 'n',
		 xlab = "", ylab = "", cex.axis= 1.2)

par(new = TRUE) #add new smooth to the same plot 

plot(phyto_ZM_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(phyto_ZM_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = int_col_D, xlim = c(0,366), ylim = c(log(0.0001),log(25)),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = int_col, yaxt = 'n',
		 xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 2.5, "Phytoplankton Biomass (mg/L)", cex = 1.25)
mtext(side = 1, line = 2.5, 'Day of Year', cex = 1.25)

axis(side = 2, 
		 at = c(log(0.0001), log(0.0002), log(0.0003), log(0.0004), log(0.0005), log(0.0006), log(0.0007), log(0.0008), log(0.0009),
		 			  log(0.001), log(0.002), log(0.003), log(0.004), log(0.005), log(0.006), log(0.007), log(0.008), log(0.009),
		 			  log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
		 			  log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9),
		 			  log(1), log(2), log(3), log(4), log(5), log(6), log(7), log(8), log(9),
		 			  log(10), log(20), log(30), log(40), log(50), log(60), log(70), log(80), log(90),
		 			  log(100)), 
		 labels = c('0.0001','','','','','','','','',
							'0.001','','','','','','','','',
							'0.01','','','','','','','','',
							'0.1','','','','','','','','',
							'1','','','','','','','','',
							'10','','','','','','','','',
							'100'))
legend(x = 290, y = log(0.004), legend = c('Pre', '+SWF', '+ZM'), pch = 19, col = c(low_col, high_col, int_col), 
			 bg = 'white')


# Monona #==========================
phyto_MO_sum = phyto %>% 
	filter(lakeid == 'MO') %>% 
	group_by(sampledate) %>% 
	summarize(biomass_mgL = sum(biomass_conc)) %>% 
	ungroup() %>% 
	mutate(doy = yday(sampledate), 
				 year = year(sampledate)) %>% 
	mutate(group = case_when(year < 2009 ~ 'base', 
													 year > 2008 & year < 2016 ~ 'SWF', 
													 year > 2015 ~ 'ZM'))
phyto_MO_sum

# Because we will be plotting by period, make some separate data frames to make life easier
phyto_base = phyto_MO_sum %>% 
	filter(group == 'base')
phyto_base

phyto_SWF = phyto_MO_sum %>% 
	filter(group == 'SWF') 

phyto_ZM = phyto_MO_sum %>% 
	filter(group == 'ZM')

# GAM-ing the secchi for pattern - not analysis...

# Base GAM
phyto_base_gam <- gam(biomass_mgL ~ s(doy, k = 80, bs = 'cc') , Gamma(link = 'log'), data = phyto_base, method = 'REML') # Cubic Regression Spline
summary(phyto_base_gam) 
gam.check(phyto_base_gam)

phyto_SWF_gam <- gam(biomass_mgL ~ s(doy, k = 60, bs = 'cc') , Gamma(link = 'log'), data = phyto_SWF, method = 'REML') # Cubic Regression Spline
summary(phyto_SWF_gam) 
gam.check(phyto_SWF_gam)

phyto_ZM_gam <- gam(biomass_mgL ~ s(doy, k = 50, bs = 'cc') , Gamma(link = 'log'), data = phyto_ZM, method = 'REML') # Cubic Regression Spline
summary(phyto_ZM_gam) 
gam.check(phyto_ZM_gam)

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
plot(phyto_base_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(phyto_base_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = low_col_F, xlim = c(0,366), ylim = c(log(0.0001), log(25)),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = low_col, yaxt = 'n',
		 xlab = "", ylab = "", cex.axis= 1.2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot

plot(phyto_SWF_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(phyto_SWF_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = high_col_E, xlim = c(0,366), ylim = c(log(0.0001),log(25)),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = high_col, yaxt = 'n',
		 xlab = "", ylab = "", cex.axis= 1.2)

par(new = TRUE) #add new smooth to the same plot 

plot(phyto_ZM_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(phyto_ZM_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = int_col_D, xlim = c(0,366), ylim = c(log(0.0001),log(25)),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = int_col, yaxt = 'n',
		 xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 2.5, "Phytoplankton Biomass (mg/L)", cex = 1.25)
mtext(side = 1, line = 2.5, 'Day of Year', cex = 1.25)

axis(side = 2, 
		 at = c(log(0.0001), log(0.0002), log(0.0003), log(0.0004), log(0.0005), log(0.0006), log(0.0007), log(0.0008), log(0.0009),
		 			  log(0.001), log(0.002), log(0.003), log(0.004), log(0.005), log(0.006), log(0.007), log(0.008), log(0.009),
		 			  log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
		 			  log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9),
		 			  log(1), log(2), log(3), log(4), log(5), log(6), log(7), log(8), log(9),
		 			  log(10), log(20), log(30), log(40), log(50), log(60), log(70), log(80), log(90),
		 			  log(100)), 
		 labels = c('0.0001','','','','','','','','',
							'0.001','','','','','','','','',
							'0.01','','','','','','','','',
							'0.1','','','','','','','','',
							'1','','','','','','','','',
							'10','','','','','','','','',
							'100'))
legend(x = 290, y = log(0.004), legend = c('Pre', '+SWF', '+ZM'), pch = 19, col = c(low_col, high_col, int_col), 
			 bg = 'white')

# Cyanophyte Biomass Sum #============================

# Mendota #========================
cyano_ME_sum = phyto %>% 
	filter(lakeid == 'ME',
				 division == 'Cyanophyta') %>% 
	group_by(sampledate) %>% 
	summarize(biomass_mgL = sum(biomass_conc)) %>% 
	ungroup() %>% 
	mutate(doy = yday(sampledate), 
				 year = year(sampledate)) %>% 
	mutate(group = case_when(year < 2009 ~ 'base', 
													 year > 2008 & year < 2016 ~ 'SWF', 
													 year > 2015 ~ 'ZM'))
cyano_ME_sum

# Because we will be plotting by period, make some separate data frames to make life easier
cyano_base = cyano_ME_sum %>% 
	filter(group == 'base')
cyano_base

cyano_SWF = cyano_ME_sum %>% 
	filter(group == 'SWF') 

cyano_ZM = cyano_ME_sum %>% 
	filter(group == 'ZM')

# GAM-ing the secchi for pattern - not analysis...

# Base GAM
cyano_base_gam <- gam(biomass_mgL ~ s(doy, k = 150, bs = 'cc') , Gamma(link = 'log'), data = cyano_base, method = 'REML') # Cubic Regression Spline
summary(cyano_base_gam) 
gam.check(cyano_base_gam)

cyano_SWF_gam <- gam(biomass_mgL ~ s(doy, k = 80, bs = 'cc') , Gamma(link = 'log'), data = cyano_SWF, method = 'REML') # Cubic Regression Spline
summary(cyano_SWF_gam) 
gam.check(cyano_SWF_gam)

cyano_ZM_gam <- gam(biomass_mgL ~ s(doy, k = 60, bs = 'cc') , Gamma(link = 'log'), data = cyano_ZM, method = 'REML') # Cubic Regression Spline
summary(cyano_ZM_gam) 
gam.check(cyano_ZM_gam)

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
plot(cyano_base_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(cyano_base_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = low_col_F, xlim = c(0,366), ylim = c(log(0.0001), log(25)),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = low_col, yaxt = 'n',
		 xlab = "", ylab = "", cex.axis= 1.2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot

plot(cyano_SWF_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(cyano_SWF_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = high_col_E, xlim = c(0,366), ylim = c(log(0.0001),log(25)),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = high_col, yaxt = 'n',
		 xlab = "", ylab = "", cex.axis= 1.2)

par(new = TRUE) #add new smooth to the same plot 

plot(cyano_ZM_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(cyano_ZM_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = int_col_D, xlim = c(0,366), ylim = c(log(0.0001),log(25)),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = int_col, yaxt = 'n',
		 xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 2.5, "Cyanophyta Biomass (mg/L)", cex = 1.25)
mtext(side = 1, line = 2.5, 'Day of Year', cex = 1.25)

axis(side = 2, 
		 at = c(log(0.0001), log(0.0002), log(0.0003), log(0.0004), log(0.0005), log(0.0006), log(0.0007), log(0.0008), log(0.0009),
		 			 log(0.001), log(0.002), log(0.003), log(0.004), log(0.005), log(0.006), log(0.007), log(0.008), log(0.009),
		 			 log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
		 			 log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9),
		 			 log(1), log(2), log(3), log(4), log(5), log(6), log(7), log(8), log(9),
		 			 log(10), log(20), log(30), log(40), log(50), log(60), log(70), log(80), log(90),
		 			 log(100)), 
		 labels = c('0.0001','','','','','','','','',
		 					 '0.001','','','','','','','','',
		 					 '0.01','','','','','','','','',
		 					 '0.1','','','','','','','','',
		 					 '1','','','','','','','','',
		 					 '10','','','','','','','','',
		 					 '100'))
legend(x = 290, y = log(0.004), legend = c('Pre', '+SWF', '+ZM'), pch = 19, col = c(low_col, high_col, int_col), 
			 bg = 'white')


# Monona #==========================
cyano_MO_sum = phyto %>% 
	filter(lakeid == 'MO',
				 division == 'Cyanophyta') %>% 
	group_by(sampledate) %>% 
	summarize(biomass_mgL = sum(biomass_conc)) %>% 
	ungroup() %>% 
	mutate(doy = yday(sampledate), 
				 year = year(sampledate)) %>% 
	mutate(group = case_when(year < 2009 ~ 'base', 
													 year > 2008 & year < 2016 ~ 'SWF', 
													 year > 2015 ~ 'ZM'))
cyano_MO_sum

# Because we will be plotting by period, make some separate data frames to make life easier
cyano_base = cyano_MO_sum %>% 
	filter(group == 'base')
cyano_base

cyano_SWF = cyano_MO_sum %>% 
	filter(group == 'SWF') 

cyano_ZM = cyano_MO_sum %>% 
	filter(group == 'ZM')

# GAM-ing the secchi for pattern - not analysis...

# Base GAM
cyano_base_gam <- gam(biomass_mgL ~ s(doy, k = 100, bs = 'cc') , Gamma(link = 'log'), data = cyano_base, method = 'REML') # Cubic Regression Spline
summary(cyano_base_gam) 
gam.check(cyano_base_gam)

cyano_SWF_gam <- gam(biomass_mgL ~ s(doy, k = 60, bs = 'cc') , Gamma(link = 'log'), data = cyano_SWF, method = 'REML') # Cubic Regression Spline
summary(cyano_SWF_gam) 
gam.check(cyano_SWF_gam)

cyano_ZM_gam <- gam(biomass_mgL ~ s(doy, k = 50, bs = 'cc') , Gamma(link = 'log'), data = cyano_ZM, method = 'REML') # Cubic Regression Spline
summary(cyano_ZM_gam) 
gam.check(cyano_ZM_gam)

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
plot(cyano_base_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(cyano_base_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = low_col_F, xlim = c(0,366), ylim = c(log(0.0001), log(25)),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = low_col, yaxt = 'n',
		 xlab = "", ylab = "", cex.axis= 1.2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot

plot(cyano_SWF_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(cyano_SWF_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = high_col_E, xlim = c(0,366), ylim = c(log(0.0001),log(25)),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = high_col, yaxt = 'n',
		 xlab = "", ylab = "", cex.axis= 1.2)

par(new = TRUE) #add new smooth to the same plot 

plot(cyano_ZM_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(cyano_ZM_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = int_col_D, xlim = c(0,366), ylim = c(log(0.0001),log(25)),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = int_col, yaxt = 'n',
		 xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 2.5, "Cyanophyta Biomass (mg/L)", cex = 1.25)
mtext(side = 1, line = 2.5, 'Day of Year', cex = 1.25)

axis(side = 2, 
		 at = c(log(0.0001), log(0.0002), log(0.0003), log(0.0004), log(0.0005), log(0.0006), log(0.0007), log(0.0008), log(0.0009),
		 			 log(0.001), log(0.002), log(0.003), log(0.004), log(0.005), log(0.006), log(0.007), log(0.008), log(0.009),
		 			 log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
		 			 log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9),
		 			 log(1), log(2), log(3), log(4), log(5), log(6), log(7), log(8), log(9),
		 			 log(10), log(20), log(30), log(40), log(50), log(60), log(70), log(80), log(90),
		 			 log(100)), 
		 labels = c('0.0001','','','','','','','','',
		 					 '0.001','','','','','','','','',
		 					 '0.01','','','','','','','','',
		 					 '0.1','','','','','','','','',
		 					 '1','','','','','','','','',
		 					 '10','','','','','','','','',
		 					 '100'))
legend(x = 290, y = log(0.004), legend = c('Pre', '+SWF', '+ZM'), pch = 19, col = c(low_col, high_col, int_col), 
			 bg = 'white')

# Diatom Biomass Sum #============================

# Mendota #========================
dia_ME_sum = phyto %>% 
	filter(lakeid == 'ME',
				 division == 'Bacillariophyta') %>% 
	group_by(sampledate) %>% 
	summarize(biomass_mgL = sum(biomass_conc)) %>% 
	ungroup() %>% 
	mutate(doy = yday(sampledate), 
				 year = year(sampledate)) %>% 
	mutate(group = case_when(year < 2009 ~ 'base', 
													 year > 2008 & year < 2016 ~ 'SWF', 
													 year > 2015 ~ 'ZM'))
dia_ME_sum

# Because we will be plotting by period, make some separate data frames to make life easier
dia_base = dia_ME_sum %>% 
	filter(group == 'base')
dia_base

dia_SWF = dia_ME_sum %>% 
	filter(group == 'SWF') 

dia_ZM = dia_ME_sum %>% 
	filter(group == 'ZM')

# GAM-ing the secchi for pattern - not analysis...

# Base GAM
dia_base_gam <- gam(biomass_mgL ~ s(doy, k = 100, bs = 'cc') , Gamma(link = 'log'), data = dia_base, method = 'REML') # Cubic Regression Spline
summary(dia_base_gam) 
gam.check(dia_base_gam)

dia_SWF_gam <- gam(biomass_mgL ~ s(doy, k = 60, bs = 'cc') , Gamma(link = 'log'), data = dia_SWF, method = 'REML') # Cubic Regression Spline
summary(dia_SWF_gam) 
gam.check(dia_SWF_gam)

dia_ZM_gam <- gam(biomass_mgL ~ s(doy, k = 60, bs = 'cc') , Gamma(link = 'log'), data = dia_ZM, method = 'REML') # Cubic Regression Spline
summary(dia_ZM_gam) 
gam.check(dia_ZM_gam)

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
plot(dia_base_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(dia_base_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = low_col_F, xlim = c(0,366), ylim = c(log(0.0001), log(25)),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = low_col, yaxt = 'n',
		 xlab = "", ylab = "", cex.axis= 1.2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot

plot(dia_SWF_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(dia_SWF_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = high_col_E, xlim = c(0,366), ylim = c(log(0.0001),log(25)),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = high_col, yaxt = 'n',
		 xlab = "", ylab = "", cex.axis= 1.2)

par(new = TRUE) #add new smooth to the same plot 

plot(dia_ZM_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(dia_ZM_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = int_col_D, xlim = c(0,366), ylim = c(log(0.0001),log(25)),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = int_col, yaxt = 'n',
		 xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 2.5, "Diatom Biomass (mg/L)", cex = 1.25)
mtext(side = 1, line = 2.5, 'Day of Year', cex = 1.25)

axis(side = 2, 
		 at = c(log(0.0001), log(0.0002), log(0.0003), log(0.0004), log(0.0005), log(0.0006), log(0.0007), log(0.0008), log(0.0009),
		 			 log(0.001), log(0.002), log(0.003), log(0.004), log(0.005), log(0.006), log(0.007), log(0.008), log(0.009),
		 			 log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
		 			 log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9),
		 			 log(1), log(2), log(3), log(4), log(5), log(6), log(7), log(8), log(9),
		 			 log(10), log(20), log(30), log(40), log(50), log(60), log(70), log(80), log(90),
		 			 log(100)), 
		 labels = c('0.0001','','','','','','','','',
		 					 '0.001','','','','','','','','',
		 					 '0.01','','','','','','','','',
		 					 '0.1','','','','','','','','',
		 					 '1','','','','','','','','',
		 					 '10','','','','','','','','',
		 					 '100'))
legend(x = 290, y = log(0.004), legend = c('Pre', '+SWF', '+ZM'), pch = 19, col = c(low_col, high_col, int_col), 
			 bg = 'white')


# Monona #==========================
dia_MO_sum = phyto %>% 
	filter(lakeid == 'MO',
				 division == 'Bacillariophyta') %>% 
	group_by(sampledate) %>% 
	summarize(biomass_mgL = sum(biomass_conc)) %>% 
	ungroup() %>% 
	mutate(doy = yday(sampledate), 
				 year = year(sampledate)) %>% 
	mutate(group = case_when(year < 2009 ~ 'base', 
													 year > 2008 & year < 2016 ~ 'SWF', 
													 year > 2015 ~ 'ZM'))
dia_MO_sum

# Because we will be plotting by period, make some separate data frames to make life easier
dia_base = dia_MO_sum %>% 
	filter(group == 'base')
dia_base

dia_SWF = dia_MO_sum %>% 
	filter(group == 'SWF') 

dia_ZM = dia_MO_sum %>% 
	filter(group == 'ZM')

# GAM-ing the secchi for pattern - not analysis...

# Base GAM
dia_base_gam <- gam(biomass_mgL ~ s(doy, k = 100, bs = 'cc') , Gamma(link = 'log'), data = dia_base, method = 'REML') # Cubic Regression Spline
summary(dia_base_gam) 
gam.check(dia_base_gam)

dia_SWF_gam <- gam(biomass_mgL ~ s(doy, k = 50, bs = 'cc') , Gamma(link = 'log'), data = dia_SWF, method = 'REML') # Cubic Regression Spline
summary(dia_SWF_gam) 
gam.check(dia_SWF_gam)

dia_ZM_gam <- gam(biomass_mgL ~ s(doy, k = 30, bs = 'cc') , Gamma(link = 'log'), data = dia_ZM, method = 'REML') # Cubic Regression Spline
summary(dia_ZM_gam) 
gam.check(dia_ZM_gam)

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
plot(dia_base_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(dia_base_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = low_col_F, xlim = c(0,366), ylim = c(log(0.0001), log(25)),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = low_col, yaxt = 'n',
		 xlab = "", ylab = "", cex.axis= 1.2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot

plot(dia_SWF_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(dia_SWF_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = high_col_E, xlim = c(0,366), ylim = c(log(0.0001),log(25)),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = high_col, yaxt = 'n',
		 xlab = "", ylab = "", cex.axis= 1.2)

par(new = TRUE) #add new smooth to the same plot 

plot(dia_ZM_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(dia_ZM_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = int_col_D, xlim = c(0,366), ylim = c(log(0.0001),log(25)),
		 cex = .75, pch = 20, lwd = 0.5, lty = 1, col = int_col, yaxt = 'n',
		 xlab = "", ylab = "", cex.axis= 1.2)
mtext(side = 2, line = 2.5, "Diatom Biomass (mg/L)", cex = 1.25)
mtext(side = 1, line = 2.5, 'Day of Year', cex = 1.25)

axis(side = 2, 
		 at = c(log(0.0001), log(0.0002), log(0.0003), log(0.0004), log(0.0005), log(0.0006), log(0.0007), log(0.0008), log(0.0009),
		 			 log(0.001), log(0.002), log(0.003), log(0.004), log(0.005), log(0.006), log(0.007), log(0.008), log(0.009),
		 			 log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
		 			 log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9),
		 			 log(1), log(2), log(3), log(4), log(5), log(6), log(7), log(8), log(9),
		 			 log(10), log(20), log(30), log(40), log(50), log(60), log(70), log(80), log(90),
		 			 log(100)), 
		 labels = c('0.0001','','','','','','','','',
		 					 '0.001','','','','','','','','',
		 					 '0.01','','','','','','','','',
		 					 '0.1','','','','','','','','',
		 					 '1','','','','','','','','',
		 					 '10','','','','','','','','',
		 					 '100'))
legend(x = 290, y = log(0.004), legend = c('Pre', '+SWF', '+ZM'), pch = 19, col = c(low_col, high_col, int_col), 
			 bg = 'white')

