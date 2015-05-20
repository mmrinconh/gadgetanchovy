#script to run and output iceland imm and mat cod stocks in 01-cod \01initial
library(Rgadget)
library(gridExtra)
## interacting with gadget
tmp<-callGadget(s=1,main='main',i='params.in',
                o='lik.out',ignore.stderr=FALSE)

## reading in the likelihood output (scores, weights etc.)
lik.out <- read.gadget.lik.out()
#scores by component
lik.out$data
#weights of each component
lik.out$weights

lik <- read.gadget.likelihood('likelihood')
lik.dat <- read.gadget.data(lik)
lik.dat$df
names(lik.dat$dat$catchdistribution)
head(lik.dat$dat$catchdistribution$alkeys.aut)

## quick optimization
tmp<-callGadget(l=1,main='main',i='params.in',p='params.opt'
                o='lik.out',opt='optinfofile',ignore.stderr=FALSE)

## model fitting:
if(FALSE){
  cl <- makeCluster( detectCores() -1)
  
  tmp <- gadget.iterative(rew.sI=TRUE,
                          main='main',
                          grouping=list(
                            igfs=c('si.gp1','si.gp2','si.gp3'),
                            aut=c('si.gp1a','si.gp2a','si.gp3a')),
                          cv.floor = 0.01,
                          cl=cl)
  close(cl)
}

## collect statistics on fit (i.e. how well does the model predict the past)
fit <- gadget.fit()

## predict into the future
progn <- gadget.forward(params.file='WGTS/params.final',effort=0.2)


## simple plots
## survey indices
plot(fit)

## likelihood summary
plot(fit,data='summary')

## lengthdists
ldist.plot <- plot(fit,data='catchdist.fleets')

## ICES plots (used for management)
grid.arrange(plot(fit,data='res.by.year',type='total')+
               theme(legend.position='none'),
             plot(fit,data='res.by.year',type='F')+
               theme(legend.position='none'),
             plot(fit,data='res.by.year',type='rec')+
               theme(legend.position='none'),
             plot(fit,data='res.by.year',type='catch')+
               theme(legend.position='none'))

## prognosis plots (no default plots defined yet)
progn.ssb <- filter(progn$lw,stock=='codmat')

progn.by.year <-
  left_join(progn$catch %>%
              group_by(year,trial) %>%
              summarise(catch=sum(biomass.consumed)),
            progn.ssb)


prog.bio.plot <-
  ggplot(progn.by.year,aes(year,total.bio/1e6,lty=as.factor(trial))) +
  geom_rect(aes(xmin=max(fit$res.by.year$year),
                xmax=Inf,ymin=-Inf,ymax=Inf),
            fill = 'gray90', alpha=0.1) +
  geom_line() +
  theme_bw() + xlab('Year') + ylab('SSB (\'000 tons)') +
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        legend.title = element_blank(),
        legend.position = 'none')


prog.catch.plot <-
  ggplot(filter(progn.by.year,year<2032),
         aes(year,catch/1e6,lty=as.factor(trial))) +
  geom_rect(aes(xmin=max(fit$res.by.year$year),
                xmax=Inf,ymin=-Inf,ymax=Inf),
            fill = 'gray90', alpha=0.1) +
  geom_line() +
  theme_bw() + xlab('Year') + ylab('Catch (\'000 tons)') +
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        legend.title = element_blank(),
        legend.position = 'none')


grid.arrange(prog.bio.plot,prog.catch.plot,ncol=1)
