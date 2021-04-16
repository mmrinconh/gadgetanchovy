#devtools::install_github('hafro/rgadget')
install.packages('devtools')
devtools::install_github('hafro/rgadget', ref = '0949c54')
library(Rgadget)
tmp<-callGadget(s=1,main='main',i='params.in',o='lik.out',ignore.stderr=FALSE, log='tmp')
lik <- read.gadget.likelihood('likelihood')
tmp<-gadget.iterative(params.file = 'params.in',
                      optinfofile = 'optfile',
                      wgts = 'WGTS',
                      rew.sI=TRUE,
                      cv.floor=0.05,
                      grouping = list(sind=lik$surveyindices$name, seine = c('ldist.seine','ldist.alkseine')))
fit<-gadget.fit(wgts = "WGTS", printfile.printatstart = 0, printfile.steps = 2)
#fit<-gadget.fit(wgts = "WGTS")
st <- Rgadget:::read.gadget.stockfiles('Modelfiles/anch')
params <- read.gadget.parameters('WGTS/params.final')
rec <- Rgadget:::get.gadget.recruitment(st,params,collapse=FALSE)
SS<-read.gadget.lik.out('WGTS/lik.final')
lik <- read.gadget.likelihood('likelihood')
lik.data<-read.gadget.data(lik)
save(params,rec,st,lik.data,SS,file="Output.Rdata")
source("gadget_fit4_2step_2019.r")

#############################################################################33
setwd("WGTS")

Files2copy<-list.files()
list.files<-Files2copy[1:(length(Files2copy)-1)]

dir.create("../WGTS1")
file.copy(from=list.files, to = "../WGTS1")

setwd("..")
fit1 <- gadget.fit(wgts = 'WGTS1', printfile.printatstart = 0, printfile.steps = 1)

setwd("WGTS")

Files2copy<-list.files()
list.files<-Files2copy[1:(length(Files2copy)-1)]

dir.create("../WGTSall")
file.copy(from=list.files, to = "../WGTSall")

setwd("..")
fitall <- gadget.fit(wgts = 'WGTSall', printfile.printatstart = 0, printfile.steps = c(1,2,3,4))



#fit1<-gadget.fit(wgts = "WGTS", printfile.printatstart = 0, printfile.steps = c(1,2,3,4), fit.folder = 'FIT1')

#function(wgts = 'WGTS', 
 #        main.file = NULL,
  #       fleet.predict = NULL,
   #      mat.par=NULL, 
    #     params.file=NULL,
     #    f.age.range=NULL, 
      #   fit.folder = 'FIT',
       #  printfile.printatstart = 1, 
        # printfile.steps = 1,
         #rec.len.param = FALSE,
         #gd = NULL)
#source("gadget_fit4_2step_2019.r")










#fit<-gadget.fit(wgts = "WGTS_end", printfile.printatstart = 0, printfile.steps = 2)


#setwd("WGTS")
#F12<-data.frame(stock="anch",age.min=1,age.max=2)
#F1<-data.frame(stock="anch",age.min=1,age.max=1)
#F2<-data.frame(stock="anch",age.min=2,age.max=2)
#Files2copy<-list.files()
#list.files<-Files2copy[1:(length(Files2copy)-1)]

#dir.create("../WGTS1")
#dir.create("../WGTS2")
#dir.create("../WGTS12")
#dir.create("../WGTSseine")
#dir.create("../WGTSpelago")
#dir.create("../WGTSecocadiz")

#HIce WGTS1 a mano en el 44porque necesita casi todos los archivos
#file.copy(from=list.files, to = "../WGTS1")
#file.copy(from=list.files, to = "../WGTS2")
# file.copy(from=list.files, to = "../WGTS12")
# file.copy(from=list.files, to = "../WGTSseine")
# file.copy(from=list.files, to = "../WGTSpelago")
# file.copy(from=list.files, to = "../WGTSecocadiz")

#setwd("..")
#fit1 <- gadget.fit(wgts = 'WGTS1', 
                  # f.age.range=F1)

#fit2 <- gadget.fit(wgts = 'WGTS2', 
                  # f.age.range=F2)
#F12<-data.frame(stock="anch",age.min=1,age.max=2)
#fit12 <- gadget.fit(wgts = 'WGTS12', 
                    #f.age.range=F12)





#retro<-gadget.retro(mainfile = 'WGTS/main.final', iterative=TRUE)
