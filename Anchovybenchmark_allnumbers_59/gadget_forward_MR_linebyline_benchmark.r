rm(list = ls())
#checkpoint::scanForPackages()
#checkpoint("2018-02-26") 
library(Rgadget)
library (reshape)
#source_data("https://github.com/mmrinconh/gadgetanchovy/blob/master/Anchovybenchmark_allnumbers_59/WGTS.Rdata?raw=True")
load("WGTS/WGTS.Rdata")
fit<-out
#filter(fit$fleet.info,year>=2015)
pre.fleet <- filter(fit$fleet.info,year==2016) %>% 
  select(fleet, ratio = harv.rate) %>% filter(fleet=="seine")
pre.fleet[,c(4,5)]#data.frame(fle





years = 1; params.file = "WGTS/params.final"; main.file = "main"; 
    num.trials = 1; fleets = pre.fleet; 
    biomass = FALSE; effort = pre.fleet$ratio; spawnmodel = "none"; spawnvar = NULL; 
    selectedstocks = NULL; biomasslevel = NULL; check.previous = FALSE; 
    save.results = TRUE; stochastic = FALSE; rec.window = c(2011:2016); 
    compact = TRUE; mat.par = c(0, 0); gd = list(dir = ".", rel.dir = "PRE_FV") 

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
    rec <- Rgadget:::get.gadget.recruitment(stocks, params)
    rec <- arrange(rec, stock, year)
    plyr::l_ply(stocks, function(x) {
        Rgadget:::writeAggfiles(x, folder = sprintf("%s/Aggfiles", pre))
    })
    sim.begin <- time$lastyear + 1
    rec <- subset(rec, year < sim.begin)
    if (nrow(rec) == 0) 
        stop("No recruitment info found")
    time$lastyear <- sim.begin + years
    Rgadget:::write.gadget.time(time, file = sprintf("%s/time.pre", pre))
    main$timefile <- sprintf("%s/time.pre", pre)
    time.grid <- expand.grid(year = time$firstyear:time$lastyear, 
        step = 1:length(time$notimesteps), area = area$areas)
    area$temperature <- mutate(time.grid, temperature = 5)
    main$areafile <- sprintf("%s/area", pre)
    Rgadget:::write.gadget.area(area, file = sprintf("%s/area", pre))
    fleet <- plyr::llply(fleet, function(x) {
        tmp <- subset(x, fleet %in% fleets$fleet)
    })
    if (biomass) {
        fleet$fleet <- mutate(fleet$fleet, fleet = sprintf("%s.pre", 
            fleet), multiplicative = 1, quotafunction = "annualselect", 
            selectstocks = selectedstocks, biomasslevel = biomasslevel, 
            quotalevel = paste(effort, collapse = "\t"), amount = sprintf("%s/fleet.pre", 
                pre), type = "quotafleet")
    } else {
        fleet$fleet <- mutate(fleet$fleet, fleet = sprintf("%s.pre", 
            fleet), multiplicative = "#rgadget.effort", amount = sprintf("%s/fleet.pre", 
            pre), type = "linearfleet")
    }
    fleet$prey <- mutate(fleet$prey, fleet = sprintf("%s.pre", 
        fleet))
    fleet.predict <- plyr::ddply(fleets, "fleet", function(x) {
        tmp <- mutate(subset(time.grid, (year >= sim.begin | 
            (year == (sim.begin - 1) & step > time$laststep)) & 
            area %in% fleet$fleet$livesonareas), fleet = sprintf("%s.pre", 
            x$fleet), ratio = x$ratio)
        return(tmp)
    })
    Rgadget:::write.gadget.table(arrange(fleet.predict[c("year", "step", 
        "area", "fleet", "ratio")], year, step, area), file = sprintf("%s/fleet.pre", 
        pre), col.names = FALSE, row.names = FALSE, quote = FALSE)
    main$fleetfiles <- c(main$fleetfiles, sprintf("%s/fleet", 
        pre))
    Rgadget:::write.gadget.fleet(fleet, file = sprintf("%s/fleet", pre))
    if (!is.null(rec.window)) {
        if (length(rec.window) == 1) {
            tmp <- subset(rec, year < rec.window)
        } else {
            tmp <- subset(rec, year < max(rec.window) & year > 
                min(rec.window))
        }
    } else {
        tmp <- rec
    }
    #stop
    if (stochastic) {
        fitAR <- lm(tmp$recruitment[-1] ~ head(tmp$recruitment, 
            -1))
        coeffAR <- as.numeric(coefficients(fitAR))
        sdAR <- sd(resid(fitAR))
        x <- array(pmax(rnorm(years * num.trials, coeffAR[1], 
            sdAR), 0), c(num.trials, years))
    } else {
      require(EnvStats)
        x <- array(geoMean(tail(tmp$recruitment, 5)), c(num.trials, 
            years))
        coeffAR <- c(0, 0, 0)
    }
    rec.forward <- array(0, c(num.trials, years + 1), dimnames = list(trial = 1:num.trials, 
        year = sim.begin:(sim.begin + years)))
    if (num.trials == 1) {
        rec.forward[1] <- tail(rec$recruitment, 1)
        for (i in 1:years) {
            rec.forward[i + 1] <- coeffAR[2] * rec.forward[i] + 
                x[i]
        }
        rec.out <- data.frame(year = sim.begin:(sim.begin + years), 
            recruitment = as.numeric(tail(rec.forward, years)))
        tmp <- dplyr::mutate(rec.out, recuitment = recruitment, lower = 0, 
            upper = recruitment + 1, optimise = 0)
        tmp$year <- paste("rec", tmp$year, sep = "")
        names(tmp)[1:2] <- c("switch", "value")
        params <- subset(params, !(switch %in% tmp$switch))
        params.forward <- plyr::rbind.fill(params, data.frame(switch = "rgadget.effort", 
            value = effort, lower = 1e-04, upper = 100, optimise = 0, 
            stringsAsFactors = FALSE), tail(tmp[names(params)], 
            -1))
        write.gadget.parameters(params.forward, file = sprintf("%s/params.forward", 
            pre))
    } else {
        rec.forward[, 1] <- tail(rec$recruitment, 1)
        for (i in 1:years) {
            rec.forward[, i + 1] <- coeffAR[2] * rec.forward[, 
                i] + x[, i]
        }
        rec.out <- arrange(melt(rec.forward[, -1], value.name = "recruitment"), 
            trial, year)
        rec.forward <- as.data.frame(rec.forward[, -1])
        names(rec.forward) <- paste("rec", sim.begin:(sim.begin + 
            years - 1), sep = "")
        tmp <- as.data.frame(t(params$value))
        names(tmp) <- params$switch
        params.forward <- cbind(tmp, rec.forward)
        if (spawnmodel == "hockeystick") {
            params.forward$hockey.ssb <- spawnvar$ssb
        }
        params.forward <- plyr::ldply(effort, function(x) {
            params.forward$rgadget.effort <- x
            return(params.forward)
        })
        Rgadget:::write.gadget.parameters(params.forward, file = sprintf("%s/params.forward", 
            pre), columns = FALSE)
    }
    #stop
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
    printfile <- paste(paste(sprintf(catch.print, unique(fleet$prey$stock), 
        pre, paste(all.fleets, paste(fleet$fleet$fleet, collapse = " "))), 
        collapse = "\n"), paste(sprintf(print.txt, unique(fleet$prey$stock), 
        pre), collapse = "\n"), ";", "[component]", "type\tlikelihoodsummaryprinter", 
        "printfile\t.jnk", sep = "\n")
    dir.create(sprintf("%s/out/", pre), showWarnings = FALSE, 
        recursive = TRUE)
    main$printfiles <- sprintf("%s/printfile", pre)
    Rgadget:::write.unix(printfile, f = sprintf("%s/printfile", pre))
    main$likelihoodfiles <- ";"
    if (spawnmodel == "hockeystick") {
      plyr::llply(stocks, function(x) {
            if (x@doesrenew == 0) {
                sp.stock <- spawnvar$spawnratio$stock[1]
                x@doesspawn <- 1
                x@spawning = new("gadget-spawning", spawnsteps = 1, 
                  spawnareas = 1, firstspawnyear = sim.begin, 
                  lastspawnyear = sim.begin + years, spawnstocksandratio = spawnvar$spawnratio, 
                  proportionfunction = c(func = "constant", 1), 
                  weightlossfunction = c(func = "constant", 0), 
                  recruitment = c(func = "hockeystick", sprintf("%s/hockey.rec", 
                    pre), "(* 1e6 #hockey.ssb )"), stockparameters = data.frame(mean = stocks[[sp.stock]]@renewal.data$mean[1], 
                    std.dev = stocks[[sp.stock]]@renewal.data$stddev[1], 
                    alpha = stocks[[sp.stock]]@renewal.data$alpha[1], 
                    beta = stocks[[sp.stock]]@renewal.data$beta[1]))
                time.var <- data.frame(year = c(time$firstyear, 
                  sim.begin:(sim.begin + years)), step = 1, value = c(0, 
                  sprintf("(* 1e4 #rec%s)", sim.begin:(sim.begin + 
                    years))))
                write.unix("hockey.rec\ndata\n; year step value", 
                  f = sprintf("%s/hockey.rec", pre))
                write.gadget.table(time.var, col.names = FALSE, 
                  row.names = FALSE, append = TRUE, file = sprintf("%s/hockey.rec", 
                    pre), quote = FALSE)
            } else {
                x@renewal.data <- subset(x@renewal.data, year < 
                  sim.begin)
            }
            gadget_dir_write(gd, x)
        })
    } else {
      plyr::llply(stocks, function(x) {
            rec.years <- sim.begin:(sim.begin + years)
            if (x@doesrenew == 1) {
                x@renewal.data <- plyr::rbind.fill(subset(x@renewal.data, 
                  year < sim.begin), data.frame(year = rec.years, 
                  step = tail(x@renewal.data$step, 1), area = tail(x@renewal.data$area, 
                    1), age = tail(x@renewal.data$age, 1), number = sprintf("(* 0.0001 #rec%s )", 
                    rec.years), mean = tail(x@renewal.data$mean, 
                    1), stddev = tail(x@renewal.data$stddev, 
                    1), relcond = tail(x@renewal.data$relcond, 1), 
                  #beta = tail(x@renewal.data$beta, 1), 
		stringsAsFactors = FALSE))
            }
            Rgadget:::gadget_dir_write(gd, x)
        })
    }
#stop
######x@renewal.data<-slot(stocks$anch,"renewal.data") ==  stocks$anch@renewal.data
#####x@renewal.data <- rbind.fill(subset(x@renewal.data, 
#   #               year < sim.begin), data.frame(year = rec.years, 
#                  step = tail(stocks$anch@renewal.data$step, 1), area = tail(stocks$anch@renewal.data$area, 
#                    1), age = tail(stocks$anch@renewal.data$age, 1), number = sprintf("(* 0.0001 #rec%s )", 
#                  rec.years), mean = tail(stocks$anch@renewal.data$mean, 
#                    1), stddev = tail(stocks$anch@renewal.data$stddev, 
#                    1), alpha = tail(stocks$anch@renewal.data$alpha, 1), 
#                  beta = tail(stocks$anch@renewal.data$beta, 1), stringsAsFactors = FALS
    main$stockfiles <- paste(sprintf("%s/%s", pre, plyr::laply(stocks, 
        function(x) x@stockname)), collapse = " ")
    Rgadget:::write.gadget.main(main, file = sprintf("%s/main.pre", pre))
    callGadget(s = 1, i = sprintf("%s/params.forward", pre), 
        main = sprintf("%s/main.pre", pre))
    time <- new("gadget-time", firstyear = time$firstyear, firststep = time$firststep, 
        lastyear = time$lastyear, laststep = time$laststep, notimesteps = time$notimesteps)
    out <- list(lw = plyr::ldply(unique(fleet$prey$stock), function(x) {
        numsteps <- nrow(subset(Rgadget:::getTimeSteps(time), step == 1))
        tmp <- read.table(sprintf("%s/out/%s.lw", pre, x), comment.char = ";")
        file.remove(sprintf("%s/out/%s.lw", pre, x))
        names(tmp) <- c("year", "step", "area", "age", "length", 
            "number", "weight")
        tmp$stock <- x
        if (num.trials > 1) {
            tmp2 <- length(unique(tmp$area)) * numsteps * length(unique(tmp$length))
            tmp <- cbind(trial = as.factor(rep(1:num.trials, 
                each = tmp2)), effort = as.factor(rep(effort, 
                each = tmp2 * num.trials)), tmp)
        } else {
            tmp2 <- length(unique(tmp$area)) * numsteps * length(unique(tmp$length))
            tmp$trial <- as.factor(1)
            tmp$effort <- as.factor(rep(effort, each = tmp2))
        }
        tmp$length <- as.numeric(gsub("len", "", tmp$length))
        if (compact) {
            tmp <- plyr::ddply(tmp, ~year + step + trial + effort + 
                stock, summarise, total.bio = sum(number * weight), 
                ssb = sum(Rgadget:::logit(mat.par[1], mat.par[2], length) * 
                  number * weight))
        }
        return(tmp)
    }), catch = plyr::ldply(unique(fleet$prey$stock), function(x) {
        numsteps <- nrow(Rgadget:::getTimeSteps(time))
        tmp <- read.table(sprintf("%s/out/catch.%s.lw", pre, 
            x), comment.char = ";")
        file.remove(sprintf("%s/out/catch.%s.lw", pre, x))
        names(tmp) <- c("year", "step", "area", "age", "length", 
            "number.consumed", "biomass.consumed", "mortality")
        tmp$stock <- x
        if (num.trials > 1) {
            tmp2 <- length(unique(tmp$area)) * numsteps
            tmp <- cbind(trial = as.factor(rep(1:num.trials, 
                each = tmp2)), effort = as.factor(rep(effort, 
                each = tmp2 * num.trials)), tmp)
        } else {
            tmp$trial <- as.factor(1)
            tmp2 <- length(unique(tmp$area)) * numsteps
            tmp$effort <- as.factor(rep(effort, each = tmp2))
        }
        return(tmp)
    }), recruitment = rec.out, num.trials = num.trials, stochastic = stochastic, 
        sim.begin = sim.begin)
    class(out) <- c("gadget.forward", class(out))
    if (save.results) {
        save(out, file = sprintf("%s/out.Rdata", pre))
    }
 #   return(out)
#}
   # load("/run/user/1000/gvfs/sftp:host=ft2.cesga.es,user=csmdpmrh/mnt/netapp1/Store_CSIC/home/csic/mdp/mrh/GADGET_backup/Anchovy2018_benchmark_allnumbers_59/PRE_FV/out.Rdata")
    
    
    
    
    hola<-plyr::ddply(out$catch %>% filter(year>1988), ~year + effort + trial, summarise, 
                      catch = sum(biomass.consumed)/1e+06)
    
           
                                                               
    g<-arrangeGrob(#ggplot(fit$res.by.year,aes(year,total.number))+geom_line()+xlim(c(1988,2015)) ,
      #ylim(c(0,62)),
      ggplot(hola, aes(year, catch))+ geom_line() + theme_bw() + 
        ylab("Catch (in '000 tons)") + xlab("Year") +
        geom_text(aes(label=ifelse(year>2016,as.character(signif(catch,2)),'')),hjust=0,vjust=0),
     ggplot(out$lw %>% filter(year>1988) , aes(year, total.bio/1e+06)) + geom_line() + theme_bw() + ylab("Biomass (in '000 tons)") + 
        xlab("Year") +
       geom_text(aes(label=ifelse(year>2016,as.character(signif(total.bio/1e+06,2)),'')),hjust=0,vjust=0)
    )
    ggsave("Forecastplots.pdf",g, width = 5.2, height = 7.27, units = c("in", "cm", "mm"))
    setwd("~/Back up de MIPC/Documentos/TEXdocuments/Benchmark/Anchovy2017_benchmark_allnumbers_59")
    ggsave("Forecastplots.pdf",g, width = 5.2, height = 6, units = c("in", "cm", "mm"))
    ggsave("Forecastplots.jpg",g, width = 5.2, height = 6, units = c("in", "cm", "mm"))
    
    out$recruitment %>% mutate(recruitment=recruitment/1e06)
    
    forerec<-ggplot(out$recruitment %>% filter(year>1988), aes(year, recruitment/1e+06)) + 
      geom_point() + theme_bw() + ylab("Recruitment (in millions)") + xlab("Year")+ 
      geom_text(aes(label=ifelse(year>2016,as.character(signif(recruitment/1e+06,2)),'')),hjust=0,vjust=0)
    ggsave("Forecastrec.pdf",forerec, width = 3, height = 4, units = c("in", "cm", "mm"))
    ggsave("Forecastrec.jpg",forerec, width = 3, height = 4, units = c("in", "cm", "mm"))