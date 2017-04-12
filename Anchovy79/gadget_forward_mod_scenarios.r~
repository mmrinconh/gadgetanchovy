#Simulation starting in the first step of the next year.Initial recruitment to simulate goes from quarter 1 to quarter 4 of the last year considered
gadget.forward.scenarios<-function (years = 2, steps =8, params.file = "WGTS/params.final", main.file = "main", 
    num.trials = 10, fleets = data.frame(fleet = "seine", ratio = 1), 
    biomass = FALSE, effort = 0.2, spawnmodel = "none", spawnvar = NULL, 
    selectedstocks = NULL, biomasslevel = NULL, check.previous = FALSE, 
    save.results = TRUE, stochastic = TRUE, rec.window = 1996:2014, 
    compact = TRUE, mat.par = c(0, 0), gd = list(dir = ".", rel.dir = "BASE_fv"), Windvalues=c(20,20,20,20,20,20,20,20,20)) 
{ pre <- paste(gd$dir, gd$rel.dir, sep = "/")
    if (check.previous) {
        if (file.exists(sprintf("%s/out.Rdata", pre))) {
            load(sprintf("%s/out.Rdata", pre))
            return(out)
        }
    }
    dir.create(pre, showWarnings = FALSE, recursive = TRUE)
    dir.create(sprintf("%s/Aggfiles", pre), showWarnings = FALSE)
    main <- read.gadget.main(file = main.file)
    stocks <- Rgadget:::read.gadget.stockfiles(main$stockfiles)
    time <- Rgadget:::read.gadget.time(main$timefile)
	time$lastyear<-max(rec.window) #modified from the original to plot later starting in 2015
 area <- Rgadget:::read.gadget.area(main$areafile)
    fleet <- Rgadget:::read.gadget.fleet(main$fleetfiles)
    all.fleets <- paste(fleet$fleet$fleet, collapse = " ")
    params <- read.gadget.parameters(params.file)
#To put seasonal recruitment is only necessary to add collapse=FALSE in get.gadget.recruitment and add the step in arrange
    rec <- get.gadget.recruitment(stocks, params, collapse= FALSE)
    rec <- arrange(rec, stock, year, step)
    l_ply(stocks, function(x) {
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
    fleet <- llply(fleet, function(x) {
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
    fleet.predict <- ddply(fleets, "fleet", function(x) {
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
    if (!is.null(rec.window)) { #rec.window is not used if n.trials is bigger than 1 (tmp is only used below in that case)
        if (length(rec.window) == 1) {
            tmp <- subset(rec, year < rec.window)
        } else {
            tmp <- subset(rec, year < max(rec.window) & year > 
                min(rec.window))
        }
    } else {
        tmp <- rec
    }	
	library(repmis)
	source_data("https://github.com/mmrinconh/gadgetanchovy/blob/master/Anchovy79/froid.Rdata?raw=True")
	#load("VAR4ser.Rdata")
       rec.forward <- array(0, c(num.trials, years + 1), dimnames = list(trial = 1:num.trials,year = sim.begin:(sim.begin + years)))
	rec.forward.steps <-matrix(c(tail(rec$recruitment,4)/1e6,rep(0,steps)),nr=num.trials, nc=steps + 4, dimnames = list(trial = 1:num.trials,step = 1:(steps+4)), byrow=T)
	 rec.forward[, 1] <- tail(rec$recruitment, 1)
	datamatpred<-model.frame(froid)[dim(froid$model)[1],][-1]
	names(datamatpred)<-colnames(model.frame(froid))[-1] 
	object<-froid
	y <- array(0,c(num.trials, steps)) 
	        for (i in 1:steps){
		set.seed(42)
		y[,i]<-pmax(rnorm(num.trials, 0, sd(resid(object))),0) #With y[i] I just use the sd as the sd of residuals for model
		}
	for (i in 1:steps) {
		datamatpred[,c(2,3)]<-Windvalues[c(i,i+1)]
		datamatpred[,c(1)]<-rec.forward.steps[1,i]
	         rec.forward.steps[, i + 4] <- predict(froid,newdata=datamatpred)+y[,i]	
        }
	for (i in 1:years) {
            rec.forward[,i+1] <- rowSums(rec.forward.steps[,(i*4+1):(i*4+4)])
        }
	colMeans(rec.forward)
	colMeans(rec.forward.steps)
        rec.out <- arrange(melt(rec.forward[, -1], value.name = "recruitment"), 
            trial, year)
        rec.forward <- as.data.frame(rec.forward[, -1]*1e6)
        names(rec.forward) <- paste("rec", sim.begin:(sim.begin + 
            years - 1), sep = "")
        tmp <- as.data.frame(t(params$value))
        names(tmp) <- params$switch
        params.forward <- cbind(tmp, rec.forward)
        if (spawnmodel == "hockeystick") {
            params.forward$hockey.ssb <- spawnvar$ssb
        }
        params.forward <- ldply(effort, function(x) {
            params.forward$rgadget.effort <- x
            return(params.forward)
        })
        Rgadget:::write.gadget.parameters(params.forward, file = sprintf("%s/params.forward", 
            pre), columns = FALSE)
    #}
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
  	llply(stocks, function(x) {
            rec.years <- sim.begin:(sim.begin + years)
            if (x@doesrenew == 1) {
                x@renewal.data <- rbind.fill(subset(x@renewal.data, 
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
	main$stockfiles <- paste(sprintf("%s/%s", pre, laply(stocks, 
        function(x) x@stockname)), collapse = " ")
    write.gadget.main(main, file = sprintf("%s/main.pre", pre))
	save.image(file=sprintf("%s/preout.Rdata", pre))
}




