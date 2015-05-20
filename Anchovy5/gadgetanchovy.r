setwd("~/GADGET/RGADGET/Anchovy3")
source('../rgadget-master/gadgetFileIO.R')
source('../rgadget-master/gadgetfunctions.R')
source('../rgadget-master/gadgetClass.R')
source('../rgadget-master/gadgetMethods.R')
source('../rgadget-master/function.R')
library(plyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
tmp<-gadget.iterative(grouping=list(catch=c('ldist.seine')), 
params.file = 'params.in',
optinfofile = 'optfile',
wgts = 'WGTS',rew.sI=TRUE)
library(Rgadget)
fit <- gadget.fit()
plot(fit,data='summary')
plot(fit,data='catchdist.fleets')
plot(fit,data='res.by.year',type='total')

