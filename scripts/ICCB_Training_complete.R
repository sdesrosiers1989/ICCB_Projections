################################################################
################ Processing Climate Data with R ################
################################################################

## Install packages
install.packages("terra")
install.packages("dplyr")
install.packages("sf")
install.packages("ggplot2")
install.packages("dismo")
install.packages("rasterVis")
install.packages("reshape")

## Import packages
library(terra)
library(dplyr)
library(sf)
library(ggplot2)
library(dismo)
library(rasterVis)
library(reshape)
library(RColorBrewer)


# Part 1: Temperature Data (one model)

## Set working directory
setwd("C:/R Code/Training/ICCB_training/data/annual/")
getwd()                   # get work directory
dir()                     # list files in the work directory

# Retrieve file names in the directory
files=dir()
files[1]

# Load file and query data (working with one model)
tas = rast("tas_GFDL-ESM4_ssp370_r1i1p1f1_CCAM10_aus-10i_10km_sem_1981-2100.nc")
tas 

# Adding missing year values to the data
dates = seq(as.Date("1981-01-01"), as.Date("2100-12-01"), by="year")
names(tas) = dates  # fixing the time data in the NetCDF
tas

# Sub-setting and plotting the data
tas[[1]]
plot(tas[[1]])
plot(tas[[1:4]])
plot(tas[[117:120]])

# Calculating climatology for baseline (1981-2010)
tas_base = mean(tas[[1:30]])
plot(tas_base)

# Calculating climatology for future (2071-2100)
tas_fut = mean(tas[[91:120]])
plot(tas_fut)

# Change in future temperature (future - base)
tas_dif = tas_fut - tas_base
plot(tas_dif)

# Can you make this plot nicer? Add a title and cha

# Extracting out point data (timeseries)
tas[50,50]
df = melt(tas[50,50])

# Basic plot
plot(df, xlab = "Year", ylab = "Temperature (degC)")

# Calculating spatial average of all data
spat_ave = global(tas, fun=mean, na.rm=TRUE)
spat_ave$date = dates

# ggplot
ggplot(data = spat_ave, aes(y=mean, x=date))+ 
  ylab('Temperature (degC)') + xlab('Year') +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm") +
  theme_bw()


# Part 2: Rainfall Data (multiple models)

# Working with multiple Models
pr_files <- list.files(pattern = "pr", full.names = FALSE)

pr_data = rast(pr_files)*365.25  # daily mean to annual total
pr_data

# Repeating the year names multiple times to correspond with multiple models
years = seq(1981,2100)
years_rep = rep(years, times =3)
names(pr_data) = years_rep

# Calculating the model average
pr_modavg = tapp(pr_data, years, fun = mean)

# Calculating climatology for baseline (1981-2010)
pr_base = mean(pr_modavg[[1:30]])  # Converting from daily mean to annual mean
levelplot(pr_base)

# Calculating climatology for future (2071-2100)
pr_fut = mean(pr_modavg[[91:120]])  # Converting from daily mean to annual mean
levelplot(pr_fut)

# Cutting data to Queensland
qld_shp = vect('C:/R Code/Training/ICCB_training/data/shp/QLD_State_Mask.shp')
pr_dif = pr_fut - pr_base
pr_dif_masked <- crop(pr_dif, qld_shp, mask = TRUE)
levelplot(pr_dif_masked)

# Plotting the percent change 
pr_pdif = (pr_fut - pr_base ) / pr_base *100  #Percent difference
pr_pdif_masked <- crop(pr_pdif, qld_shp, mask = TRUE)
levelplot(pr_pdif_masked)

# Specifying plotting bins and colours
my.at <- seq(-20, 20, length.out = 10)
my.at = c(-Inf, my.at, Inf)
levelplot(pr_pdif_masked, at = my.at, cuts=11, pretty=T,
                col.regions=((brewer.pal(11,"RdBu"))))
  


# Part 3: Calculating BioClim Indices 

# now working with monthly data
setwd("C:/R Code/Training/ICCB_training/data/monthly/")
getwd() # get work directory
dir() # list files in the work directory
lga_shp = vect('C:/R Code/Training/ICCB_training/data/shp/SunshineCoast.shp')

tmax = rast("tasmax_GFDL-ESM4_ssp370_r1i1p1f1_CCAM10_aus-10i_10km_mon_1981-2100.nc")
tmin = rast("tasmin_GFDL-ESM4_ssp370_r1i1p1f1_CCAM10_aus-10i_10km_mon_1981-2100.nc")
pr = rast("pr_GFDL-ESM4_ssp370_r1i1p1f1_CCAM10_aus-10i_10km_mon_1981-2100.nc" )

#interogate data
tmax

dates <- seq(as.Date("1981-01-01"), as.Date("2100-12-01"), by="month")
names(tmax) = dates
names(tmin) = dates
names(pr) = dates

# Plot the data
levelplot(mean(tmin[[0:360]]), margin = FALSE, par.settings = YlOrRdTheme, main = 'TMIN')

# Masking data to Sunshine Coast
pr_masked <- crop(pr, lga_shp, mask = TRUE)
tmin_masked <- crop(tmin, lga_shp, mask = TRUE)
tmax_masked <- crop(tmax, lga_shp, mask = TRUE)

# Spatial average
pr_ave = global(pr_masked, fun=mean, na.rm=TRUE)
tmin_ave = global(tmin_masked, fun=mean, na.rm=TRUE)
tmax_ave = global(tmax_masked, fun=mean, na.rm=TRUE)

# Rename header from mean to variable for merging
colnames(pr_ave)[1] <- "pr"
colnames(tmin_ave)[1] <- "tmin"
colnames(tmax_ave)[1] <- "tmax"

# Adding date column for merging
pr_ave$date <- rownames(pr_ave)
tmin_ave$date <- rownames(tmin_ave)
tmax_ave$date <- rownames(tmax_ave)

# Merge data into a dataframe
df <- merge(pr_ave, tmin_ave, by = "date", all = TRUE)
df <- merge(df, tmax_ave, by = "date", all = TRUE)


# Select the baseline (1981-2010)
df$date <- as.Date(df$date)
df$year <- as.numeric(format(df$date, "%Y"))
df_base = subset(df, year >= 1981 & year <= 2010)
df_fut = subset(df, year >= 2071 & year <= 2100)


# Using dismo package to calculate the biovars
bio_base = biovars(df_base$pr, df_base$tmin, df_base$tmax)
bio_fut = biovars(df_fut$pr, df_fut$tmin, df_fut$tmax)

print(bio_base[, c("bio5", "bio6", "bio7", "bio12", "bio15")])
print(bio_fut[, c("bio5", "bio6", "bio7", "bio12", "bio15")])



