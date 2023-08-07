### Combine MeMO LTER data ### 

library(tidyverse)
library(lubridate)
library(zoo)

# Secchi Depth Data # ==============================
# Package ID: knb-lter-ntl.31.32 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Secchi Disk Depth; Other Auxiliary Base Crew Sample Data 1981 - current.
# Data set creator:  John Magnuson - University of Wisconsin-Madison 
# Data set creator:  Stephen Carpenter - University of Wisconsin-Madison 
# Data set creator:  Emily Stanley - University of Wisconsin-Madison 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/31/32/d01c782e0601d2217b94dd614444bd33" 
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
                    "sta",     
                    "secview",     
                    "secnview",     
                    "timeon",     
                    "timeoff",     
                    "airtemp",     
                    "windir",     
                    "windspd",     
                    "waveht",     
                    "cloud",     
                    "ice"    ), check.names=TRUE)
               
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
if (class(dt1$sta)!="factor") dt1$sta<- as.factor(dt1$sta)
if (class(dt1$secview)=="factor") dt1$secview <-as.numeric(levels(dt1$secview))[as.integer(dt1$secview) ]               
if (class(dt1$secview)=="character") dt1$secview <-as.numeric(dt1$secview)
if (class(dt1$secnview)=="factor") dt1$secnview <-as.numeric(levels(dt1$secnview))[as.integer(dt1$secnview) ]               
if (class(dt1$secnview)=="character") dt1$secnview <-as.numeric(dt1$secnview)
if (class(dt1$timeon)=="factor") dt1$timeon <-as.numeric(levels(dt1$timeon))[as.integer(dt1$timeon) ]               
if (class(dt1$timeon)=="character") dt1$timeon <-as.numeric(dt1$timeon)
if (class(dt1$timeoff)=="factor") dt1$timeoff <-as.numeric(levels(dt1$timeoff))[as.integer(dt1$timeoff) ]               
if (class(dt1$timeoff)=="character") dt1$timeoff <-as.numeric(dt1$timeoff)
if (class(dt1$airtemp)=="factor") dt1$airtemp <-as.numeric(levels(dt1$airtemp))[as.integer(dt1$airtemp) ]               
if (class(dt1$airtemp)=="character") dt1$airtemp <-as.numeric(dt1$airtemp)
if (class(dt1$windir)!="factor") dt1$windir<- as.factor(dt1$windir)
if (class(dt1$windspd)=="factor") dt1$windspd <-as.numeric(levels(dt1$windspd))[as.integer(dt1$windspd) ]               
if (class(dt1$windspd)=="character") dt1$windspd <-as.numeric(dt1$windspd)
if (class(dt1$waveht)=="factor") dt1$waveht <-as.numeric(levels(dt1$waveht))[as.integer(dt1$waveht) ]               
if (class(dt1$waveht)=="character") dt1$waveht <-as.numeric(dt1$waveht)
if (class(dt1$cloud)=="factor") dt1$cloud <-as.numeric(levels(dt1$cloud))[as.integer(dt1$cloud) ]               
if (class(dt1$cloud)=="character") dt1$cloud <-as.numeric(dt1$cloud)
if (class(dt1$ice)!="factor") dt1$ice<- as.factor(dt1$ice)
                
# Convert Missing Values to NA for non-dates

secchi = as_tibble(dt1) # Secnview = Secchi depth without assisted view (m) 
secchi_lter = secchi %>% 
	filter(lakeid == 'ME' | lakeid == 'MO')
secchi_lter

# MEMO Secchi Depth Data # ========================
# Package ID: knb-lter-ntl.416.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Lake Mendota Microbial Observatory Secchi Disk Measurements 2012-present.
# Data set creator:  Robin Rohwer - North Temperate Lakes LTER 
# Data set creator:  Katherine McMahon - North Temperate Lakes LTER 
# Contact:  Katherine McMahon -    - trina.mcmahon@wisc.edu
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/416/2/6b4502e8093d7ef8eb9a7a647b4ed28b" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
							 ,skip=1
							 ,sep=","  
							 ,quot='"' 
							 , col.names=c(
							 	"Year",     
							 	"Month",     
							 	"Day",     
							 	"Secchi.Depth.m",     
							 	"s.Secchi.Depth.m",     
							 	"People",     
							 	"Notes.Secchi"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Year)=="factor") dt1$Year <-as.numeric(levels(dt1$Year))[as.integer(dt1$Year) ]               
if (class(dt1$Year)=="character") dt1$Year <-as.numeric(dt1$Year)
if (class(dt1$Month)=="factor") dt1$Month <-as.numeric(levels(dt1$Month))[as.integer(dt1$Month) ]               
if (class(dt1$Month)=="character") dt1$Month <-as.numeric(dt1$Month)
if (class(dt1$Day)=="factor") dt1$Day <-as.numeric(levels(dt1$Day))[as.integer(dt1$Day) ]               
if (class(dt1$Day)=="character") dt1$Day <-as.numeric(dt1$Day)
if (class(dt1$Secchi.Depth.m)=="factor") dt1$Secchi.Depth.m <-as.numeric(levels(dt1$Secchi.Depth.m))[as.integer(dt1$Secchi.Depth.m) ]               
if (class(dt1$Secchi.Depth.m)=="character") dt1$Secchi.Depth.m <-as.numeric(dt1$Secchi.Depth.m)
if (class(dt1$s.Secchi.Depth.m)=="factor") dt1$s.Secchi.Depth.m <-as.numeric(levels(dt1$s.Secchi.Depth.m))[as.integer(dt1$s.Secchi.Depth.m) ]               
if (class(dt1$s.Secchi.Depth.m)=="character") dt1$s.Secchi.Depth.m <-as.numeric(dt1$s.Secchi.Depth.m)
if (class(dt1$People)!="factor") dt1$People<- as.factor(dt1$People)
if (class(dt1$Notes.Secchi)!="factor") dt1$Notes.Secchi<- as.factor(dt1$Notes.Secchi)

# Convert Missing Values to NA for non-dates

dt1$s.Secchi.Depth.m <- ifelse((trimws(as.character(dt1$s.Secchi.Depth.m))==trimws("NA")),NA,dt1$s.Secchi.Depth.m)               
suppressWarnings(dt1$s.Secchi.Depth.m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$s.Secchi.Depth.m))==as.character(as.numeric("NA"))),NA,dt1$s.Secchi.Depth.m))
dt1$Notes.Secchi <- as.factor(ifelse((trimws(as.character(dt1$Notes.Secchi))==trimws("NA")),NA,as.character(dt1$Notes.Secchi)))


# Here is the structure of the input data frame:
str(dt1)                      

secchi_memo = as_tibble(dt1)
secchi_memo

## Integrate Secchi Datasets ##=========================
secchi_memo
secchi_lter

# Create date object in memo data # 

secchi_memo$sampledate = as.Date(with(secchi_memo, paste(Year, Month, Day, sep = '-')), '%Y-%m-%d')
secchi_memo_select = secchi_memo %>%
	select(sampledate, Secchi.Depth.m) %>% 
	rename(secchi_m = Secchi.Depth.m)
secchi_lter_select = secchi_lter %>% 
	select(sampledate, secnview) %>% 
	rename(secchi_m = secnview)

secchi_comb_ME = rbind(secchi_memo_select, secchi_lter_select) %>% arrange(sampledate) %>% 
	mutate(year = year(sampledate), 
				 month = month(sampledate), 
				 year.mon = as.yearmon(sampledate)) %>% 
	mutate(group = case_when(year < 2009 ~ 'base', 
													 year > 2008 & year < 2016 ~ 'SWF', 
													 year > 2015 ~ 'ZM'))
secchi_comb_ME

ggplot(data = secchi_comb, aes(x = month, y = secchi_m, col = group)) + 
	geom_point() + 
	scale_color_manual(values = c('seagreen', 'dodgerblue', 'gray30'))

# GAM Plots # 
# ========= PACKAGES ========== #
if (!require(mgcv)) install.packages('mgcv')
library(mgcv)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

## secchi by period ## ===========================

# Because we will be plotting by period, make some separate data frames to make life easier
secc_base = secchi_comb_ME %>% 
	filter(group == 'base') %>%
	transform(ndate = as.numeric(sampledate))
secc_base

secc_SWF = secchi_comb_ME %>% 
	filter(group == 'SWF') 

secc_ZM = secchi_comb_ME %>% 
	filter(group == 'ZM')

# GAM-ing the secchifor pattern - not analysis...

# Base GAM
secc_base_gam <- gam(secchi_m ~ s(month, k = 10, bs = 'cs') , data = secc_base, method = 'REML') # Cubic Regression Spline
summary(secc_base_gam) 
gam.check(secc_base_gam)

secc_SWF_gam <- gam(secchi_m ~ s(month, k = 10, bs = 'cs') , data = secc_SWF, method = 'REML') # Cubic Regression Spline
summary(secc_SWF_gam) 
gam.check(secc_SWF_gam)

secc_ZM_gam <- gam(secchi_m ~ s(month, k = 10, bs = 'cs') , data = secc_ZM, method = 'REML') # Cubic Regression Spline
summary(secc_ZM_gam) 
gam.check(secc_ZM_gam)

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
plot(secc_base_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(secc_base_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = low_col_F, ylim = rev(c(0,12)),
		 cex = .75, pch = 19, lwd = 0.5, lty = 1, col = low_col,
		 xlab = "", ylab = "", cex.axis= 1.2)

#Plot of the Total P GAM for POND F ===============
par(new = TRUE) #add new smooth to the same plot

plot(secc_SWF_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(secc_SWF_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = high_col_E, ylim = rev(c(0,12)),
		 cex = .75, pch = 17, lwd = 0.5, lty = 1, col = high_col,
		 xlab = "", ylab = "", cex.axis= 1.2)

par(new = TRUE) #add new smooth to the same plot 

plot(secc_ZM_gam, select = 1, 
		 seWithMean = FALSE, shift = coef(secc_ZM_gam)[1],
		 se = TRUE, residuals = TRUE, all.terms = TRUE, shade = TRUE, rug = T,
		 shade.col = int_col_D, ylim = rev(c(0,12)),
		 cex = .75, pch = 15, lwd = 0.5, lty = 1, col = int_col,
		 xlab = "", ylab = "", cex.axis= 1.2)


mtext(side = 2, line = 3, "Chlorophyll-a (ug/L)", cex = 1.25)
#mtext(side = 3, line = 0.5, "Low Coupling", cex = 1.25)
#rect(185,-2,190,50, col=col, border=NA)


