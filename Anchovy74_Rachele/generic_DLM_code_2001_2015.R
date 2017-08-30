library(DLMtool)
library(ggplot2)
library(reshape2)
#Running TAC estimation with DLMobject
#access example data objects
for(i in 1:length(DLMdat))assign(DLMdat[[i]]@Name,DLMdat[[i]])
#avail('DLM_data') 
#Ex_dlm.data<-new("DLM_data",stock="C:/Users/Jason.Cope/Documents/GitHub/Data-limited-tools/Shiny_DLMtool/DLM_objects_examples/Example_datafile.csv")
Ex_dlm.data2<-new("DLM_data",stock="/home/marga/Documents/Data_limited course/DLM/Example_datafile_anchovy_2001_2015.csv")
Ex_dlm.data2@L95<-15
Ex_dlm.data2@MPs<-c("AvC","BK_CC","CC1",      "CC4" ,"Fdem_CC",  "SBT1" ,    "SPMSY" )
#,  "DDe",       "DDe75",      "matlenlim",  "matlenlim2", "MRnoreal",   "MRreal",     "slotlim"  #valen DDe y DDe75
funcs <- Ex_dlm.data2@MPs
temp <- DLMtool:::getTAC(Ex_dlm.data2, MPs = funcs, 1000)
TACa <- temp[[1]]
Ex_dlm.data2.TAC <- temp[[2]]
Ex_dlm.data2.TAC@TAC <- TACa
TAC.out<-Ex_dlm.data2.TAC@TAC[,,1]

TAC.df<-data.frame(t(TAC.out))
colnames(TAC.df)<-Ex_dlm.data2@MPs#MP.labs()

#Para después de incluir el Gadget
#TAC.df.melt<-melt(TAC.df)



######################################################################3333
#en el cesga he corrido:
#load("/mnt/EMC/Home_SVG/home/csic/mdp/mrh/mnt/store/GADGET_backup/Anchovy74/WGTS/WGTS.Rdata")
# directorio Anchovy75
# load("WGTS/WGTS.Rdata")
# fit<-out
# pre.fleet<-filter(fit$fleet.info,year==2015) %>% select(fleet, ratio = harv.rate)

#correr manualmente en el cesga gadget_forward_MR.r (Directorio Anchovy74 y PRE2 (res2)) parar en la línea 196 que da un error en llply y  #check anch file y cambiar normalparam por normalcond
#####################################################################3
#aquí
Run<-74
load(paste("/run/user/1000/gvfs/sftp:host=svg.cesga.es,user=csmdpmrh/mnt/EMC/Home_SVG/home/csic/mdp/mrh/mnt/store/GADGET_backup/Anchovy",Run,"/PRE2/out.Rdata",sep=""))
progn <- out
# prueba<-gadget.forward(params.file='WGTS/params.final',
#                          effort=seq(0.5,1.5,by = 0.05),
#                          fleets=pre.fleet[3,],
#                          years = 100,
#                          num.trials = 10, rec.window=c(2001,2015), gd = list(dir = ".", rel.dir = "PRE5"))
# prueba<-gadget.forward(params.file='WGTS/params.final',
#                        effort=seq(0.5,1.5,by = 0.05),
#                        fleets=data.frame(fleet = "seine", ratio = 0.6),
#                        years = 100,
#                        num.trials = 10, rec.window=c(2001,2015), gd = list(dir = ".", rel.dir = "PRE5"))
# 
# 
# 
# prueba<-gadget.forward(params.file='WGTS/params.final',
#                        effort=seq(0.5,1.5,by = 0.05),
#                        fleets=pre.fleet[3,],
#                        years = 100,
#                        num.trials = 10, gd = list(dir = ".", rel.dir = "PRE2"))
# #Run
# #Run

progn.ssb <- progn$lw
res <-
  left_join(progn$catch %>%
              group_by(year,trial,effort) %>%
              summarise(catch=sum(biomass.consumed)),
            progn.ssb) %>%
  filter(year > 2050) %>%
  ungroup() %>%
  group_by(stock,effort,trial) %>%
  summarise(catch=mean(catch),
            total.bio = mean(total.bio)) %>%
  ungroup() %>% 
  group_by(stock,effort) %>%
  summarise(catch.m=mean(catch),
            catch.u=quantile(catch,0.975),
            catch.l=quantile(catch,0.025),
            ssb.m=mean(total.bio),
            ssb.u=quantile(total.bio,0.975),
            ssb.l=quantile(total.bio,0.025))

save(res,file=paste("/run/user/1000/gvfs/sftp:host=svg.cesga.es,user=csmdpmrh/mnt/EMC/Home_SVG/home/csic/mdp/mrh/mnt/store/GADGET_backup/Anchovy",Run,"/res2.Rdata",sep=""))


#Aquí
Run<-74
load(paste("/run/user/1000/gvfs/sftp:host=svg.cesga.es,user=csmdpmrh/mnt/EMC/Home_SVG/home/csic/mdp/mrh/mnt/store/GADGET_backup/Anchovy",Run,"/res2.Rdata",sep=""))

Fmsy <- 
  res %>% 
  filter(catch.m==max(catch.m))
#Prueba_Anchovy74 PRE
# stock effort      catch.m      catch.u      catch.l        ssb.m        ssb.u        ssb.l
# <chr> <fctr>        <dbl>        <dbl>        <dbl>        <dbl>        <dbl>        <dbl>
#   1  anch    0.5 2.022195e+85 2.027537e+85 2.021256e+85 6.899173e+84 6.917399e+84 6.895971e+84

#0.4 en la última
FMSY<--log(1-as.numeric(as.character(Fmsy$effort)))
#FMSY=0.5108, mensual 0.0425
##############################################33


#by quarters biomass and recruitment Run 73 modificando printfiles para que saliera por cuartos! al final no me sirve para el MSE

# library(Rgadget)
# plot(out$res.by.year$total.number)
# plot(out,data='res.by.year',type='F')
# plot(out$res.by.year$F[14:28],out$res.by.year$num.catch[14:28])

# Run<-73
# outi <- read.printfiles(paste("/run/user/1000/gvfs/sftp:host=svg.cesga.es,user=csmdpmrh/mnt/EMC/Home_SVG/home/csic/mdp/mrh/mnt/store/GADGET_backup/Anchovy",Run,"/WGTS/out.fit",sep="")) #de aquí se puede sacar la información por cuartos
# #F anual así estaba
# 
# # f.by.year <- out[[sprintf("%s.prey", x)]] %>% 
# #   group_by(year, area) %>% summarise(catch = sum(biomass.consumed), 
# #                                      num.catch = sum(number.consumed), F = mean(mortality[age >= 
# #                                                                                             min(f.age.range) & age <= max(f.age.range)]))
# f.age.range <- c(max(outi[["anch.prey"]]$age), max(outi[["anch.prey"]]$age))
# f.by.quarter <- outi[["anch.prey"]] %>% 
#   group_by(year, step, area) %>% summarise(catch = sum(biomass.consumed), 
#                                            num.catch = sum(number.consumed), F = mean(mortality[age >= 
#                                                                                                   min(f.age.range) & age <= max(f.age.range)]))
# #super plot en la que se ve que la mortalidad por edad es casi igual
# outi[["anch.prey"]] %>% 
#   +     group_by(year, step, age) %>% ggplot(aes(year+step,mortality))+geom_point()+facet_wrap(~age)
# outi[["anch.prey"]] %>%  filter(step==2) %>%
#    group_by(year, step, age) %>% ggplot(aes(year+step,mortality))+geom_point()+facet_wrap(~age)
# 
# bio.by.month<-outi[["anch.full"]] %>% 
#   group_by(year, step, area) %>% summarize(total.number = sum(number))
# biomass.by.month<-outi[["anch.full"]] %>% 
#   group_by(year, step, area) %>% summarize(total.biomass = sum(number*mean.weight))
# 
# biomass.by.year<-outi[["anch.full"]] %>% 
#   group_by(year, area) %>% summarize(total.biomass = sum(number*mean.weight))
# #Esta biomasa difiere de out$res.by.year$total.biomass en que esta es la suma de la biomasa de todo el año, mientras la de Bjarki es la biomasa al principio del año, que creo que es mejor para calcular la TAC
# 
# 
# CVgadget<-sd(biomass.by.year$total.biomass)/mean(biomass.by.year$total.biomass)
# Cvgadget2<-sd(out$res.by.year$total.biomass)/mean(out$res.by.year$total.biomass)
# MuC<-mean(biomass.by.year$total.biomass[23:28]*0.001)#0.001 para pasar de kilogramos a toneladas
# 
# MuC2<-mean(out$res.by.year$total.biomass[23:28]*0.001)#0.001 para pasar de kilogramos a toneladas
# Biomgadget <- DLMtool:::trlnorm(100, MuC, CVgadget)

########################################

Run<-74
load(paste("/run/user/1000/gvfs/sftp:host=svg.cesga.es,user=csmdpmrh/mnt/EMC/Home_SVG/home/csic/mdp/mrh/mnt/store/GADGET_backup/Anchovy",Run,"/WGTS/WGTS.Rdata",sep=""))

fit<-out
Cvgadget<-sd(fit$res.by.year$num.catch)/mean(out$res.by.year$num.catch)# CV de capturas toda la serie
MuC<-mean(fit$res.by.year$total.biomass[11:15]*0.001)#media de biomasa de los últimos 5 años
Biomgadget <- DLMtool:::trlnorm(1000, MuC, Cvgadget)

TAC.df$Gadget<-Biomgadget*FMSY

#Para después de incluir el Gadget


TAC.df.melt<-melt(TAC.df)

box.only <- function(x) {
  r <- quantile(x, probs = c(0.25, 0.25, 0.5, 0.75, 0.75))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


#TAC.plot<-ggplot(data=TAC.df.melt,aes(as.factor(variable),value))+geom_boxplot()+ coord_flip()+labs(x="Method",y="TAC")+ylim(0,quantile(TAC.out,0.95,na.rm=T))

TAC.plot.01.15<-ggplot(data=TAC.df.melt,aes(as.factor(variable),value, fill=variable))+stat_summary(fun.data = box.only, geom = "boxplot")+ coord_flip(ylim = c(0,quantile(TAC.df,0.95,na.rm=T)))+labs(x="Method",y="TAC (tonnes)") +scale_fill_manual(breaks=as.character(unique(TAC.df.melt$variable)),values=c(rep("white",7),"dark grey"))+ggtitle("TAC from 2001-2015 data")+guides(fill=FALSE)+theme_Publication()
Ref<-MuC*FMSY
TAC.plot.01.15
pdf("TAC_plot_01_15_realdata_fv.pdf", paper='A4r',width=9.27, height=11.69)
print(TAC.plot.01.15)
dev.off()
ggsave("TAC_plot_01_15_realdata_ggsave_fv.pdf",  width=190, height=120, units="mm",device=cairo_pdf)
ggsave("TAC_plot_01_15_realdata_ggsave_fv.jpg",  width=190, height=120, units="mm")

