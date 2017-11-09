
#desde aquí!
#Calculo de Fmsy en cesga
devtools::install_github('hafro/rgadget')

library(Rgadget)
#place in the Gadget folder in CESGA
load("WGTS/WGTS.Rdata")
fit<-out
pre.fleet <- filter(fit$fleet.info,year==2013) %>% 
  select(fleet, ratio = harv.rate) %>% filter(fleet=="seine")
pre.fleet[,c(4,5)]#data.frame(fleet='seine',ratio = 1.407891)


FMSYcal<-gadget.forward(effort=seq(0.1,1.5,by = 0.05),years = 100,
                        params.file = "WGTS/params.final",
                        main.file = "main",num.trials = 10,
                        fleets =  data.frame(fleet='seine',ratio = 1.407891),
                        rec.window =c(2000,2013),
                        gd=list(dir='.',rel.dir='FMSY_fvvv'),
                        method = 'AR1')
progn<-FMSYcal
progn.ssb <- filter(progn$lw,year>=1989)%>% mutate(total.bio=number*mean_weight)#grepl('mat',stock))
res <-
  left_join(progn$catch %>%
              group_by(year,trial,effort) %>%
              summarise(catch=sum(biomass_consumed)),
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

Fmsy <- 
  res %>% 
  group_by(stock) %>%
  filter(catch.m==max(catch.m))
Fmsy #0.55


statusquohv<-pre.fleet$ratio
gadget.forward(years = 32 ,params.file = "WGTS/params.final",
               main.file = 'main', num.trials = 10,
               fleets = pre.fleet[,c(4,5)],
               effort = statusquohv,
               rec.scalar = NULL,
               check.previous = FALSE,
               save.results = TRUE,
               stochastic = TRUE,
               rec.window = c(2000,2013),
               gd=list(dir='.',rel.dir='simfsq'),
               method = 'AR1',
               ref.years=c(2000,2013),
               custom.print=NULL,
               prj.func = NULL)

Fmsy<-0.55
gadget.forward(years = 32 ,params.file = "WGTS/params.final",
               main.file = 'main', num.trials = 10,
               fleets = pre.fleet[,c(4,5)],
               effort = Fmsy,
               rec.scalar = NULL,
               check.previous = FALSE,
               save.results = TRUE,
               stochastic = TRUE,
               rec.window = c(2000,2013),
               gd=list(dir='.',rel.dir='simFmsyfv'),
               method = 'AR1',
               ref.years=c(2000,2013),
              custom.print=NULL,
               prj.func = NULL)


FMSY<--log(1-Fmsy) 
harvest005<-1-exp(-(FMSY+0.05))
gadget.forward(years = 32 ,params.file = "WGTS/params.final",
               main.file = 'main', num.trials = 10,
               fleets = pre.fleet[,c(4,5)],
               effort = harvest005,
               rec.scalar = NULL,
               check.previous = FALSE,
               save.results = TRUE,
               stochastic = TRUE,
               rec.window = c(2000,2013),
               gd=list(dir='.',rel.dir='simFmsy005'),
               method = 'AR1',
               ref.years=c(2000,2013),
               custom.print=NULL,
               prj.func = NULL)
               #...)
harvest01<-1-exp(-(FMSY+0.1))
gadget.forward(years = 32 ,params.file = "WGTS/params.final",
               main.file = 'main', num.trials = 10,
               fleets = pre.fleet[,c(4,5)],
               effort = harvest01,
               rec.scalar = NULL,
               check.previous = FALSE,
               save.results = TRUE,
               stochastic = TRUE,
               rec.window = c(2000,2013),
               gd=list(dir='.',rel.dir='simFmsy01'),
               method = 'AR1',
               ref.years=c(2000,2013),
               custom.print=NULL,
               prj.func = NULL)

harvest015<-1-exp(-(FMSY+0.15))
gadget.forward(years = 32 ,params.file = "WGTS/params.final",
               main.file = 'main', num.trials = 10,
               fleets = pre.fleet[,c(4,5)],
               effort = harvest015,
               rec.scalar = NULL,
               check.previous = FALSE,
               save.results = TRUE,
               stochastic = TRUE,
               rec.window = c(2000,2013),
               gd=list(dir='.',rel.dir='simFmsy015'),
               method = 'AR1',
               ref.years=c(2000,2013),
               custom.print=NULL,
               prj.func = NULL)

harvest_005<-1-exp(-(FMSY-0.05))
gadget.forward(years = 32 ,params.file = "WGTS/params.final",
               main.file = 'main', num.trials = 10,
               fleets = pre.fleet[,c(4,5)],
               effort = harvest_005,
               rec.scalar = NULL,
               check.previous = FALSE,
               save.results = TRUE,
               stochastic = TRUE,
               rec.window = c(2000,2013),
               gd=list(dir='.',rel.dir='simFmsy_005'),
               method = 'AR1',
               ref.years=c(2000,2013),
               custom.print=NULL,
               prj.func = NULL)

harvest_01<-1-exp(-(FMSY-0.1))
gadget.forward(years = 32 ,params.file = "WGTS/params.final",
               main.file = 'main', num.trials = 10,
               fleets = pre.fleet[,c(4,5)],
               effort = harvest_01,
               rec.scalar = NULL,
               check.previous = FALSE,
               save.results = TRUE,
               stochastic = TRUE,
               rec.window = c(2000,2013),
               gd=list(dir='.',rel.dir='simFmsy_01'),
               method = 'AR1',
               ref.years=c(2000,2013),
               custom.print=NULL,
               prj.func = NULL)
harvest_015<-1-exp(-(FMSY-0.15))
gadget.forward(years = 32 ,params.file = "WGTS/params.final",
               main.file = 'main', num.trials = 10,
               fleets = pre.fleet[,c(4,5)],
               effort = harvest_015,
               rec.scalar = NULL,
               check.previous = FALSE,
               save.results = TRUE,
               stochastic = TRUE,
               rec.window = c(2000,2013),
               gd=list(dir='.',rel.dir='simFmsy_015'),
               method = 'AR1',
               ref.years=c(2000,2013),
               custom.print=NULL,
               prj.func = NULL)

#Take out data in my PC
#"ecosystem","model","species","year","SSB","catch","F","isFmsy","isFsq"
load("/run/user/1000/gvfs/sftp:host=svg.cesga.es,user=csmdpmrh/mnt/EMC/Home_SVG/home/csic/mdp/mrh/mnt/store/GADGET_backup/Anchovy2017_3abenchmark/WGTS/WGTS.Rdata")
fit<-out
firstpart<-right_join(fit$res.by.year%>%filter(year>1999, year<2013)%>% select(year,total.biomass,catch),fit$fleet.info%>% filter(year>1999,year<2013, fleet=="seine")%>%select(year,harv.rate))%>%ungroup()%>% select(year,total.biomass,catch,harv.rate)
names(firstpart)=c("year","B","catch","F")

scen<-c('simfsq',"simFmsyfv","simFmsy005", "simFmsy01",'simFmsy015','simFmsy_005','simFmsy_01','simFmsy_015')
a<-1
for (i in scen){
load(paste0("/run/user/1000/gvfs/sftp:host=svg.cesga.es,user=csmdpmrh/mnt/EMC/Home_SVG/home/csic/mdp/mrh/mnt/store/GADGET_backup/Anchovy2017_3abenchmark/",i,"/out.Rdata"))
progn<-out
progn.ssb <- filter(progn$lw,year>=2013,trial==1)%>% mutate(total.bio=number*mean_weight)%>%group_by(year,effort) %>%
  summarise(B=sum(total.bio))
#grepl('mat',stock))

 PROGN<- bind_rows(firstpart,left_join(progn$catch %>% filter(year>=2013,trial==1) %>%
              group_by(year) %>%
              summarise(catch=sum(biomass_consumed)),
            progn.ssb) %>% ungroup() %>% select(year,B,catch,F=effort))
   
assign(paste0("PROGN",a),PROGN) 
a<-a+1
}

prelisummary<-bind_rows(PROGN1%>%mutate(isFmsy=NA,isFsq=1),PROGN2%>%mutate(isFmsy=1,isFsq=NA),bind_rows(PROGN3,PROGN4,PROGN5,PROGN6,PROGN7,PROGN8)%>%mutate(isFmsy=NA,isFsq=NA))

Summary_scenarios<-prelisummary%>%mutate(B=B*0.001,catch=catch*0.001)#in tonnes
Summary_scenarios$model<-"Gadget"
Summary_scenarios$ecosystem<-"South Western Waters, Gulf of Cádiz (ICES IXa South)"
Summary_scenarios$species<-"EngraulisEncrasicolus"
Summary_scenarios_fv<-Summary_scenarios%>%filter(year<2046)%>%select(ecosystem,model,species,year,B,catch,F,isFmsy,isFsq)
head(Summary_scenarios_fv)   
write_csv(Summary_scenarios_fv, path = "data_output/surveys_complete.csv")
 
 
 
 
 
 
 
 
 
 
 
   
   
   
   
 