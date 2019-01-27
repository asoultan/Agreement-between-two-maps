# Agreement Coefficient (AC) evaluates the total agreement, which includes the systematic (ACs) and unsystematic agreement (ACu) between two datasets (images or raster maps (i.e. two distribution  maps obtained by two different approachs, or when compare the current and the predicted future/past distribution)
# If the agreement between the first image/raster(r1) and the second image/raster(r2) is perfect and AC = 1

library(raster)

r1<-raster("*.tif")# path to the first map/raster
r2<-raster("*.tif")# path to the second map/raster

x<-getValues(r1)
x<-na.omit(x)
y<-getValues(r2)
y<-na.omit(y)
n<-length(x)
xm<-mean(x)
ym<-mean(y)
SSD<-sum((x-y)^2)#sum of square difference (SSD), which indicates the degree of disagreement between r1 and r2. it should be zero if there is a perfect agreement between the two maps
MSD<-SSD/n # The arithmetic mean of SSD
RMSD<-sqrt(MSD)# root mean square difference
xm_ym<-abs(xm-ym)
x_xm<-abs(x-xm)
y_ym<-abs(y-ym)
SPOD<-sum((xm_ym+x_xm)*(xm_ym+y_ym))#  sum of potential difference (SPOD) used to standardize SSD
AC<-1-(SSD/SPOD)#The agreement coefficient (AC)
#temporal transferability of wildlife habitat models:implications for habitat monitoring. page 1516
##################
AC
#####################
lmy<- lm(y~x)# lm(dependent~independent)

lm<- lm(x~y)

a<-abs(lm$residuals)# residuals is the difference betweent the estimated y (depentent) and the observed
b<-abs(lmy$residuals)
# temporal transferability of wildlife habitat models:implications for habitat monitoring. page 1516

####################################################
PSD<-1-((sum(a*b))/SSD)# proportion of systematic disagreement
#####################################################

#An agreement coeffeciant of image comparision
SPDu<-sum(abs(lm$residuals)*abs(lmy$residuals))# The unsystematic sum of product-difference (SPDu)
MPDu<-SPDu/n#mean differences
SPDs<-SSD-SPDu # systematic sum of product-difference (SPDs)
MPDs<-SPDs/n
#############################
ACs<-1-(SPDs/SPOD) #systematic agreement Coefficient

###########################
ACu<-1-(SPDu/SPOD) # unsystematic agreement Coefficient


# Ji, L. & Gallo, K. (2006) An agreement coefficient for image comparison. Photogrammetric Engineering and Remote Sensing, 72, 823–833.
# https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1365-2699.2011.02479.x 
