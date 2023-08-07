## GAM Analysis ##===================

mcycle <- MASS::mcycle

# Examine the mcycle data frame
head(mcycle)
plot(mcycle)

# Fit a linear model
lm_mod <- lm(accel~times, data = mcycle)

# Visualize the model
termplot(lm_mod, partial.resid = TRUE, se = TRUE)

# Load mgcv 
library(mgcv)

# Fit the model 
gam_mod = gam(accel ~ s(times), data = mcycle)

# Plot the Results # 
plot(gam_mod, residuals = T, pch = 1)

# Extract the model coefficients 
gam_mod = mgcv::gam(accel ~ s(times), data = mcycle)
coef(gam_mod)

## Setting complexity of the motorcycle model ##======================

# Fit a GAM with 3 basis functions 
gam_mod_k3 = gam(accel ~ s(times, k = 3), data = mcycle)

# Fit with 20 basis functions 
gam_mod_k20 = gam(accel ~ s(times, k = 20), data = mcycle)

# Visualize the GAMs 
par(mfrow = c(1,2))
plot(gam_mod_k3, residuals = TRUE, pch = 1)
plot(gam_mod_k20, residuals = TRUE, pch = 1)

# Extract the smoothing parameter 
gam_mod_k3$sp
gam_mod_k20$sp

# Fix the smoothing parameter at 0.1 
gam_mod_s1 = gam(accel ~ s(times), data = mcycle, sp = 0.1)

# Fix the smoothing parameter at 0.0001 
gam_mod_s2 = gam(accel ~ s(times), data = mcycle, sp = 0.0001)

# Plot both models # 
par(mfrow = c(1,2))
plot(gam_mod_s1, residuals = TRUE, pch = 1)
plot(gam_mod_s2, residuals = TRUE, pch = 1)

# Complexity and smoothing together # 
gam_mod_sk = gam(accel ~ s(times, k = 20), data = mcycle, sp = 0.1)

# Visual the model 
plot(gam_mod_sk, residuals = TRUE, pch = 1)

# Multivariate GAMs #======================
install.packages('gamair')
library(gamair)
data('mpg', package = 'gamair')

# Examine the data 
head(mpg)
str(mpg)

# Fit the model 
mod_city = gam(city.mpg ~ s(weight)+s(length)+s(price), data = mpg, method = 'REML')
plot(mod_city, pages = 1)

# Fit the model with categorical terms 
mod_city2 = gam(city.mpg ~ s(weight) + s(length) + s(price) + fuel + drive + style, data = mpg,
								method = 'REML')
plot(mod_city2, all.terms = TRUE, pages = 1)

# Interpreting and visualizing GAMs #=============================

# Fit the model 
mod_city4 = gam(city.mpg ~ s(weight) + s(length) + s(price) + s(rpm) + s(width), 
								data = mpg, method = 'REML')
summary(mod_city4)

# edf > 5 = non-linear 
# edf close to 1 = linear 

# plot the motorcycle crash model and data # 
mod = gam(accel ~ s(times), data = mcycle, method = 'REML')

# Make the plot with residuals 
plot(mod, residuals = T)

# Change the shape of the residuals 
plot(mod, residuals = T, pch = 1, cex = 1)

# Plotting multiple auto performance variables 
mod = gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + comp.ratio, 
					data = mpg, method = 'REML')

# plot the price effect # 
plot(mod, select = 3)

# plot all effects 
plot(mod, pages = 1, all.terms = T)

# Visualizing uncertainty #=========================
# Fit the model
mod <- gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + comp.ratio, 
           data = mpg, method = "REML")

# Plot the weight effect
plot(mod, shade = TRUE, shade.col = "hotpink",
		 seWithMean = F, select = 1,  shift = coef(mod)[1],
		 se = T, residuals = T, pch = 19, cex = 1)

# Make another plot adding the intercept value and uncertainty
plot(mod, select = 1, shade = TRUE, shade.col = "hotpink", 
     shift = coef(mod)[1], seWithMean = TRUE)

