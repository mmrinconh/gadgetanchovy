#debug in CESGA
rm(list = ls())
library(Rgadget)
source_data("https://github.com/mmrinconh/gadgetanchovy/blob/master/Anchovybenchmark_allnumbers_59/WGTS.Rdata?raw=True")
#load("WGTS/WGTS.Rdata")
fit<-out
#filter(fit$fleet.info,year>=2015)
pre.fleet <- filter(fit$fleet.info,year==2016) %>% 
  select(fleet, ratio = harv.rate) %>% filter(fleet=="seine")
pre.fleet[,c(4,5)]#data.frame(fleet='seine',ratio = 1.407891)
#load("Output.Rdata")
#source_data("https://github.com/mmrinconh/gadgetanchovy/blob/master/Anchovybenchmark_allnumbers_59/Output.Rdata?raw=True")

#require(EnvStats)
# rec <- 
#   rec %>% 
#   #    dplyr::group_by(stock,year) %>% 
#   #    dplyr::summarise(recruitment = sum(recruitment)) %>% 
#   dplyr::arrange(stock,year,step)
# Proj.rec<-function(recu=recu,...){
#   require(EnvStats)
#   recu %>% dplyr::filter(year>2011) %>%
#     dplyr::group_by(stock,year) %>% 
#     dplyr::summarise(recruitmentpre = sum(recruitment))%>% dplyr::summarise(recruitment = geoMean(recruitmentpre))  %>% dplyr::mutate(stock="anch", year="2017",trial=1,step=2)%>% dplyr::select(stock,year,trial,recruitment,step)
# }

#setwd("/run/user/1000/gvfs/sftp:host=ft2.cesga.es,user=csmdpmrh/mnt/netapp1/Store_CSIC/home/csic/mdp/mrh/GADGET_backup/Anchovy2018_benchmark_allnumbers_59")



years = 1;params.file = "WGTS/params.final";
main.file = 'main'; num.trials = 1;
fleets = pre.fleet;
effort = pre.fleet$ratio;
rec.scalar = NULL;
check.previous = FALSE;
save.results = TRUE;
stochastic = FALSE;
rec.window = c(2012:2017);
gd=list(dir='.',rel.dir='PRE_final');
method = 'custom';
ref.years=c(1989:2016);
custom.print=NULL;
#prj.func = Proj.rec;
#recu=rec
  ## helper function
readoutput <- function(x) {
  tmp <- readLines(x)
  file.remove(x)
  preamble <- tmp[grepl(";", tmp)]
  body <- tmp[!grepl(";", tmp)]
  header <- preamble[grepl("year-step-area", preamble)] %>% 
    gsub("; (*)", "\\1", .) %>% stringr::str_split("-") %>% 
    unlist() %>% gsub(" ", "_", .)
  body %>% paste(collapse = "\n") %>% read.table(text = ., 
                                                 col.names = header, fill = TRUE, stringsAsFactors = FALSE) %>% 
    dplyr::mutate(trial = cut(1:length(year), c(0, which(diff(year) < 
                                                           0), 1e+09), labels = FALSE) - 1) %>% tibble::as_tibble()
}
pre <- paste(gd$dir, gd$rel.dir, sep = "/")
if (check.previous) {
  if (file.exists(sprintf("%s/out.Rdata", pre))) {
    load(sprintf("%s/out.Rdata", pre))
    return(out)
  }
}
dir.create(pre, showWarnings = FALSE, recursive = TRUE)
dir.create(sprintf("%s/Aggfiles", pre), showWarnings = FALSE)
main <- Rgadget:::read.gadget.main(file = main.file)
stocks <- Rgadget:::read.gadget.stockfiles(main$stockfiles)
time <- Rgadget:::read.gadget.time(main$timefile)
area <- Rgadget:::read.gadget.area(main$areafile)
fleet <- Rgadget:::read.gadget.fleet(main$fleetfiles)
all.fleets <- paste(fleet$fleet$fleet, collapse = " ")
params <- Rgadget:::read.gadget.parameters(params.file)
rec <- Rgadget:::get.gadget.recruitment(stocks, params, collapse = FALSE) %>% 
  na.omit()
if (is.null(ref.years)) {
  ref.years <- min(rec$year)
}
rec <- rec %>% dplyr::arrange(stock, year, step)
plyr::l_ply(stocks, function(x) {
  Rgadget:::writeAggfiles(x, folder = sprintf("%s/Aggfiles", pre))
})
sim.begin <- time$lastyear + 1
rec <- rec %>% dplyr::filter(year < sim.begin)
if (nrow(rec) == 0) 
  stop("No recruitment info found")
time$lastyear <- sim.begin + years - 1
Rgadget:::write.gadget.time(time, file = sprintf("%s/time.pre", pre))
main$timefile <- sprintf("%s/time.pre", pre)
time.grid <- expand.grid(year = time$firstyear:time$lastyear, 
                         step = 1:length(time$notimesteps), area = area$areas)
area$temperature <- dplyr::mutate(time.grid, temperature = 5)
main$areafile <- sprintf("%s/area", pre)
Rgadget:::write.gadget.area(area, file = sprintf("%s/area", pre))
fleet <- plyr::llply(fleet, function(x) {
  tmp <- subset(x, fleet %in% fleets$fleet)
})
fleet$fleet <- dplyr::mutate(fleet$fleet, fleet = sprintf("%s.pre", 
                                                          fleet), multiplicative = "#rgadget.effort", amount = sprintf("%s/fleet.pre", 
                                                                                                                       pre), type = "linearfleet")
fleet$prey <- dplyr::mutate(fleet$prey, fleet = sprintf("%s.pre", 
                                                        fleet))
fleet.predict <- time.grid %>% dplyr::filter((year >= sim.begin | 
                                                (year == (sim.begin - 1) & step > time$laststep)) & area %in% 
                                               fleet$fleet$livesonareas)
if ("year" %in% names(fleets) | "step" %in% names(fleets)) {
  fleet.predict <- fleet.predict %>% dplyr::left_join(fleets)
} else {
  fleet.predict <- fleets %>% split(.$fleet) %>% purrr::map(~cbind(fleet.predict, 
                                                                   .)) %>% dplyr::bind_rows()
}
fleet.predict <- fleet.predict %>% dplyr::mutate(fleet = paste(fleet, 
                                                               "pre", sep = "."))
Rgadget:::write.gadget.table(dplyr::arrange(fleet.predict[c("year", 
                                                  "step", "area", "fleet", "ratio")], year, step, area), 
                   file = sprintf("%s/fleet.pre", pre), col.names = FALSE, 
                   row.names = FALSE, quote = FALSE)
main$fleetfiles <- c(main$fleetfiles, sprintf("%s/fleet", 
                                              pre))
Rgadget:::write.gadget.fleet(fleet, file = sprintf("%s/fleet", pre))
if (!is.null(rec.window)) {
  if (length(rec.window) == 1) {
    tmp <- rec %>% dplyr::ungroup() %>% dplyr::filter(as.numeric(year) < 
                                                        rec.window)
  } else if (class(rec.window) == "data.frame") {
    tmp <- rec %>% dplyr::ungroup() %>% dplyr::left_join(rec.window, 
                                                         by = "stock") %>% dplyr::filter(as.numeric(year) < 
                                                                                           upper, as.numeric(year) > lower)
  } else {
    tmp <- rec %>% dplyr::ungroup() %>% dplyr::filter(as.numeric(year) <= 
                                                        max(rec.window) & as.numeric(year) >= min(rec.window))
  }
} else {
  tmp <- rec %>% dplyr::ungroup()
}

if (stochastic) {
  if (tolower(method) == "bootstrap") {
    prj.rec <- tmp %>% dplyr::group_by(stock, year) %>% 
      dplyr::summarise(recruitment = sum(recruitment)) %>% 
      split(.$stock) %>% purrr::map(~dplyr::select(., 
                                                   recruitment) %>% dplyr::slice(plyr::rlply(ceiling(num.trials * 
                                                                                                       years/nrow(tmp)), c(sample(rec.window[2]:rec.window[3] - 
                                                                                                                                    rec.window[1] + 1, replace = TRUE), sample(rec.window[1]:rec.window[2] - 
                                                                                                                                                                                 rec.window[1] + 1, replace = TRUE))) %>% unlist())) %>% 
      purrr::map(~dplyr::slice(., 1:(num.trials * years))) %>% 
      purrr::map(~tibble::data_frame(year = rep((sim.begin):(sim.begin + 
                                                               years - 1), num.trials), trial = rep(1:num.trials, 
                                                                                                    each = years), recruitment = .$recruitment)) %>% 
      dplyr::bind_rows(.id = "stock") %>% dplyr::select(stock, 
                                                        year, trial, recruitment) %>% dplyr::mutate(step = (rec$step[rec$year == 
                                                                                                                       min(ref.years)])[1])
  }
  else if (tolower(method) == "ar1") {
    prj.rec <- tmp %>% dplyr::group_by(stock, year) %>% 
      dplyr::summarise(recruitment = sum(recruitment)) %>% 
      split(.$stock) %>% purrr::map(~lm(head(.$recruitment, 
                                             -1) ~ tail(.$recruitment, -1))) %>% purrr::map(~dplyr::bind_cols(broom::glance(.), 
                                                                                                              as.data.frame(t(broom::tidy(.)$estimate)))) %>% 
      purrr::map(~dplyr::rename(., a = V1, b = V2)) %>% 
      purrr::map(~data.frame(year = rep((sim.begin):(sim.begin + 
                                                       years - 1), num.trials), trial = rep(1:num.trials, 
                                                                                            each = years), rec = pmax(arima.sim(years * 
                                                                                                                                  num.trials, model = list(ar = .$b), sd = .$sigma) + 
                                                                                                                        .$a, 0))) %>% dplyr::bind_rows(.id = "stock") %>% 
      dplyr::mutate(rec = ifelse(is.na(rec), x, rec)) %>% 
      select(stock, year, trial, recruitment = rec) %>% 
      dplyr::mutate(step = (rec$step[rec$year == min(ref.years)])[1])
  }
  else if (tolower(method) == "custom") {
    if (!is.function(prj.func)) {
      prj.rec <- tmp %>% prj.func(...)
      if (!("data.frame" %in% class(prj.rec))) 
        stop("prj.func does not return a data.frame")
      if (!(c("stock", "year", "trial", "recruitment", 
              "step") %in% names(prj.rec))) 
        stop("prj.func does include columns stock, year, step, trial and recruitment")
    }
    else {
      stop("No projection function supplied")
    }
  }
  else {
    stop("Invalid projection method")
  }
}
else {
  require(EnvStats)
  prj.rec <-
    tmp %>%
    #dplyr::group_by(stock) %>%
    dplyr::group_by(stock,year) %>%
    #dplyr::summarise(recruitment = mean(recruitment)) %>%
    dplyr::summarise(recruitment = sum(recruitment)) %>% dplyr::ungroup() %>% 
    dplyr::group_by(stock) %>% dplyr::summarise(recruitment = geoMean(recruitment)) %>% 
    dplyr::left_join(expand.grid(stock = stocks %>%
                                   purrr::map(Rgadget:::getStockNames) %>%
                                   unlist,
                                 year = (sim.begin):(sim.begin+years-1),
                                 trial = 1:num.trials) %>%
                       dplyr::arrange(stock,year,trial)) %>% 
    dplyr::mutate(step = (rec$step[rec$year == min(ref.years)])[1]) %>% 
    dplyr::select(stock,year,trial,recruitment,step)
  
  
  #prj.rec <- tmp %>% dplyr::group_by(stock) %>% dplyr::summarise(recruitment = mean(recruitment)) %>% 
   # dplyr::left_join(expand.grid(stock = stocks %>% purrr::map(Rgadget:::getStockNames) %>% 
    #                               unlist, year = (sim.begin):(sim.begin + years - 
     #                                                            1), trial = 1:num.trials) %>% dplyr::arrange(stock, 
      #                                                                                                        year, trial))
}
if (num.trials == 1 & length(effort) == 1) {
  prj.rec %>% dplyr::mutate(switch = paste(stock, "rec", 
                                           year, sep = "."), lower = 0, upper = recruitment + 
                              1, optimise = 0) %>% dplyr::select(switch, value = recruitment, 
                                                                 lower, upper, optimise) %>% dplyr::bind_rows(params, 
                                                                                                              data.frame(switch = "rgadget.effort", value = effort, 
                                                                                                                         lower = 1e-04, upper = 100, optimise = 0, stringsAsFactors = FALSE)) %>% 
    write.gadget.parameters(file = sprintf("%s/params.forward", 
                                           pre))
} else {
  params %>% dplyr::select(switch, value) %>% tidyr::spread(switch, 
                                                            value) %>% dplyr::slice(rep(1, num.trials * length(effort))) %>% 
    dplyr::bind_cols(prj.rec %>% dplyr::mutate(switch = paste(stock, 
                                                              "rec", year, step, sep = ".")) %>% dplyr::select(trial, 
                                                                                                               switch, recruitment) %>% tidyr::spread(switch, 
                                                                                                                                                      recruitment) %>% dplyr::select(-trial) %>% dplyr::slice(rep(1:num.trials, 
                                                                                                                                                                                                                  each = length(effort))) %>% dplyr::mutate(rgadget.effort = rep(effort, 
                                                                                                                                                                                                                                                                                 num.trials))) %>% write.gadget.parameters(file = sprintf("%s/params.forward", 
                                                                                                                                                                                                                                                                                                                                          pre), columns = FALSE)
}
if (is.null(rec.scalar)) {
  prj.rec <- prj.rec %>% dplyr::mutate(rec.scalar = 1)
} else {
  if (sum(rec.scalar$stock %in% prj.rec$stock) == 0) 
    warning("No stocks found in rec.scalar")
  prj.rec <- prj.rec %>% dplyr::left_join(rec.scalar %>% 
                                            dplyr::select(stock, year, rec.scalar)) %>% dplyr::mutate(rec.scalar = ifelse(is.na(rec.scalar), 
                                                                                                                          1, rec.scalar))
}
print.txt <- paste("[component]", "type             stockprinter", 
                   "stocknames       %1$s", "areaaggfile      %2$s/Aggfiles/%1$s.area.agg", 
                   "ageaggfile       %2$s/Aggfiles/%1$s.allages.agg", "lenaggfile       %2$s/Aggfiles/%1$s.len.agg", 
                   "printfile        %2$s/out/%1$s.lw", "printatstart     0", 
                   "yearsandsteps    all 1", sep = "\n")
catch.print <- paste("[component]", "type\t\tpredatorpreyprinter", 
                     "predatornames\t\t%3$s", "preynames\t\t%1$s", "areaaggfile      %2$s/Aggfiles/%1$s.area.agg", 
                     "ageaggfile       %2$s/Aggfiles/%1$s.allages.agg", "lenaggfile       %2$s/Aggfiles/%1$s.alllen.agg", 
                     "printfile        %2$s/out/catch.%1$s.lw", "yearsandsteps    all all", 
                     sep = "\n")
if (!is.null(custom.print)) {
  custom.print <- readLines(custom.print) %>% gsub("printfile[ \t]+([A-Za-z0-9]+)", 
                                                   sprintf("printfile\t%s/%s/\\1", pre, "out"), .) %>% 
    paste(., collapse = "\n ")
} else {
  NULL
}
printfile <- paste(custom.print, ";", paste(sprintf(catch.print, 
                                                    unique(fleet$prey$stock), pre, paste(all.fleets, paste(fleet$fleet$fleet, 
                                                                                                           collapse = " "))), collapse = "\n"), paste(sprintf(print.txt, 
                                                                                                                                                              unique(fleet$prey$stock), pre), collapse = "\n"), ";", 
                   "[component]", "type\tlikelihoodsummaryprinter", "printfile\t.jnk", 
                   sep = "\n")
catch.files <- sprintf("catch.%s.lw", unique(fleet$prey$stock))
print.files <- sprintf("%s.lw", unique(fleet$prey$stock))
dir.create(sprintf("%s/out/", pre), showWarnings = FALSE, 
           recursive = TRUE)
main$printfiles <- sprintf("%s/printfile", pre)
Rgadget:::write.unix(printfile, f = sprintf("%s/printfile", pre))
main$likelihoodfiles <- ";"
plyr::llply(stocks, function(x) {
  tmp <- prj.rec %>% dplyr::filter(stock == x@stockname, 
                                   trial == 1)
  if (x@doesrenew == 1) {
    x@renewal.data <- x@renewal.data %>% dplyr::arrange(year, 
                                                        step) %>% dplyr::filter(year < sim.begin) %>% 
      dplyr::bind_rows(x@renewal.data %>% dplyr::filter(year == 
                                                          min(ref.years)) %>% dplyr::mutate(n = n()) %>% 
                         dplyr::slice(rep(1:n[1], length(unique(tmp$year)))) %>% 
                         dplyr::mutate(year = rep(as.character(tmp$year), 
                                                  each = n[1]), number = sprintf("(* (* 0.0001 #%s.rec.%s.%s ) %s)", 
                                                                                 x@stockname, year, step, tmp$rec.scalar)) %>% 
                         dplyr::select_(.dots = names(x@renewal.data))) %>% 
      as.data.frame()
  }
  Rgadget:::gadget_dir_write(gd, x)
})
main$stockfiles <- paste(sprintf("%s/%s", pre, plyr::laply(stocks, 
                                                           function(x) x@stockname)), collapse = " ")
Rgadget:::write.gadget.main(main, file = sprintf("%s/main.pre", pre))

#in cesga
#change manually fleet.pre file
#gd=list(dir='.',rel.dir='PRE_final')
#pre <- paste(gd$dir, gd$rel.dir, sep = "/")
callGadget(s = 1, i = sprintf("%s/params.forward", pre), 
           main = sprintf("%s/main.pre", pre))
time <- new("gadget-time", firstyear = time$firstyear, firststep = time$firststep, 
            lastyear = time$lastyear, laststep = time$laststep, notimesteps = time$notimesteps)
out <- list.files(paste(pre, "out", sep = "/")) %>% purrr::set_names(paste(paste(pre, 
                                                                                 "out", sep = "/"), ., sep = "/"), .) %>% purrr::map(readoutput) %>% 
  purrr::map(~.x %>% dplyr::left_join(dplyr::data_frame(trial = 0:(num.trials * 
                                                                     length(effort) - 1), effort = rep(effort, num.trials)), 
                                      by = "trial"))
catch <- out[catch.files] %>% bind_rows(.id = "stock") %>% 
  mutate(stock = gsub("catch.(.+).lw", "\\1", stock))
lw <- out[print.files] %>% bind_rows(.id = "stock") %>% mutate(stock = gsub("(^.+).lw", 
                                                                            "\\1", stock))
out <- out[!(names(out) %in% c(catch.files, print.files))]
out <- list(custom = out, catch = catch, lw = lw, recruitment = prj.rec %>% 
              tibble::as_tibble(), num.trials = num.trials, stochastic = stochastic, 
            sim.begin = sim.begin)
class(out) <- c("gadget.forward", class(out))
if (save.results) {
  save(out, file = sprintf("%s/out.Rdata", pre))
}

source_data("https://github.com/mmrinconh/gadgetanchovy/blob/master/Anchovybenchmark_allnumbers_59/WGTS.Rdata?raw=True")
fit<-out
load("/run/user/1000/gvfs/sftp:host=ft2.cesga.es,user=csmdpmrh/mnt/netapp1/Store_CSIC/home/csic/mdp/mrh/GADGET_backup/Anchovy2018_benchmark_allnumbers_59/PRE_final/out.Rdata")
hola<-plyr::ddply(out$catch %>% filter(year>1988), ~year + effort + trial, summarise, 
                  catch = sum(biomass_consumed)/1e+06)

REC<-rbind(fit$res.by.year %>% select(year,recruitment) %>% filter(year>1988) %>% mutate(recruitment=recruitment/1e06), out$recruitment %>% select(year,recruitment) %>% mutate(recruitment=recruitment/1e06))
#RECno<-rbind(fit$res.by.year %>% select(year,recruitment) %>% filter(year>1988) %>% mutate(recruitment=recruitment), out$recruitment %>% mutate(recruitment=recruitment))
#out$recruitment %>% mutate(recruitment=recruitment/1e06)

gadfor<-out
gadfor$lw <-gadfor$lw %>% mutate(biomass=number*mean_weight) %>% filter(year>1988)

g<-arrangeGrob(#ggplot(fit$res.by.year,aes(year,total.number))+geom_line()+xlim(c(1988,2015)) ,
  #ylim(c(0,62)),
  ggplot(hola, aes(year, catch))+ geom_line() + theme_bw() + 
    ylab("Catch (in '000 tons)") + xlab("Year") +
    geom_text(aes(label=ifelse(year>2016,as.character(signif(catch,2)),'')),hjust=0,vjust=1,angle=0),
  ggplot(plyr::ddply(gadfor$lw, ~year, summarise, bio = sum(biomass)/1e+06) , aes(year, bio)) + geom_line() + theme_bw() + ylab("Biomass (in '000 tons)") + 
    xlab("Year") +
    geom_text(aes(label=ifelse(year>2016,as.character(signif(bio/1e+06,2)),'')),hjust=0,vjust=1, angle=0),
  ggplot(REC, aes(year, recruitment)) + 
    geom_line() + theme_bw() + ylab("Recruitment (in millions)") + xlab("Year")+ 
    geom_text(aes(label=ifelse(year>2016,as.character(signif(recruitment,4)),'')),hjust=1,vjust=1,angle=0)
)
ggsave("Forecastplots2.pdf",g, width = 5.2, height = 8.4, units = c("in", "cm", "mm"))
setwd("~/Back up de MIPC/Documentos/TEXdocuments/Benchmark/Anchovy2017_benchmark_allnumbers_59")
ggsave("Forecastplots2.pdf",g, width = 5.2, height = 8.4, units = c("in", "cm", "mm"))
ggsave("Forecastplots2.jpg",g, width = 5.2, height = 8.4, units = c("in", "cm", "mm"))




