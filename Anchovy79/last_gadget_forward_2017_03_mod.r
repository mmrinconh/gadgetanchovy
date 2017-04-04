
forw<-gadget.forward(years = 2, params.file = "WGTS/params.final", main.file = "main", 
    num.trials = 10, fleets = data.frame(fleet = "seine", ratio = 1), 
    effort = 0.85, rec.scalar = NULL, check.previous = FALSE, 
    save.results = TRUE, stochastic = TRUE, rec.window = NULL, 
    compact = TRUE, mat.par = c(0, 0), gd = list(dir = ".", rel.dir = "PRE2"), 
    method = "AR1", ref.years = NULL, custom.print = NULL)

function (years = 2, params.file = "params.out", main.file = "main", 
    num.trials = 10, fleets = data.frame(fleet = "seine", ratio = 1), 
    effort = 0.85, rec.scalar = NULL, check.previous = FALSE, 
    save.results = TRUE, stochastic = TRUE, rec.window = NULL, 
    compact = TRUE, mat.par = c(0, 0), gd = list(dir = ".", rel.dir = "PRE"), 
    method = "AR1", ref.years = NULL, custom.print = NULL) 
#effort=1-exp(-1.94) 1.94 is the F of 2014=0.85
{

years = 2; steps =8; params.file = "WGTS/params.final"; main.file = "main"; 
    num.trials = 10; fleets = data.frame(fleet = "seine", ratio = 1); 
    biomass = FALSE; effort = 0.85; rec.scalar = NULL; check.previous = FALSE;
    save.results = TRUE; stochastic = TRUE; rec.window = 1996:2009; 
    compact = TRUE; mat.par = c(0, 0); gd = list(dir = ".", rel.dir = "PRE2");
 method = "AR1"; ref.years = NULL; custom.print = NULL




    pre <- paste(gd$dir, gd$rel.dir, sep = "/")
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
    area <- Rgadget:::read.gadget.area(main$areafile)
    fleet <- Rgadget:::read.gadget.fleet(main$fleetfiles)
    all.fleets <- paste(fleet$fleet$fleet, collapse = " ")
    params <- read.gadget.parameters(params.file)
    rec <- get.gadget.recruitment(stocks, params, collapse = FALSE) %>% 
        na.omit()
    if (is.null(ref.years)) {
        ref.years <- min(rec$year)
    }
    rec.step.ratio <- rec %>% dplyr::group_by(stock, step) %>% 
        dplyr::filter(year %in% ref.years) %>% dplyr::summarise(rec.ratio = sum(recruitment)) %>% 
        dplyr::mutate(rec.ratio = rec.ratio/sum(rec.ratio))
    rec <- rec %>% dplyr::group_by(stock, year) %>% dplyr::summarise(recruitment = sum(recruitment)) %>% 
        dplyr::arrange(stock, year)
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
    fleet <- llply(fleet, function(x) {
        tmp <- subset(x, fleet %in% fleets$fleet)
    })
    fleet$fleet <- mutate(fleet$fleet, fleet = sprintf("%s.pre", 
        fleet), multiplicative = "#rgadget.effort", amount = sprintf("%s/fleet.pre", 
        pre), type = "linearfleet")
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
    if (!is.null(rec.window)) {
        if (length(rec.window) == 1) {
            tmp <- rec %>% dplyr::filter(year < rec.window)
        }        else {
            tmp <- rec %>% dplyr::filter(year <= max(rec.window) & 
                year >= min(rec.window))
        }
    }    else {
        tmp <- rec
    }
    if (stochastic) {
        if (tolower(method) == "bootstrap") {
            prj.rec <- tmp %>% split(.$stock) %>% purrr::map(~dplyr::select(., 
                recruitment) %>% dplyr::slice(plyr::rlply(ceiling(num.trials * 
                years/nrow(tmp)), c(sample(rec.window[2]:rec.window[3] - 
                rec.window[1] + 1, replace = TRUE), sample(rec.window[1]:rec.window[2] - 
                rec.window[1] + 1, replace = TRUE))) %>% unlist())) %>% 
                purrr::map(~dplyr::slice(., 1:(num.trials * years))) %>% 
                purrr::map(~tibble::data_frame(year = rep((sim.begin):(sim.begin + 
                  years - 1), num.trials), trial = rep(1:num.trials, 
                  each = years), recruitment = .$recruitment)) %>% 
                dplyr::bind_rows(.id = "stock") %>% select(stock, 
                year, trial, recruitment)
        }        else {
            prj.rec <- tmp %>% split(.$stock) %>% purrr::map(~lm(.$recruitment[-1] ~ 
                head(.$recruitment, -1))) %>% purrr::map(~dplyr::bind_cols(broom::glance(.), 
                as.data.frame(t(broom::tidy(.)$estimate)))) %>% 
                purrr::map(~dplyr::rename(., a = V1, b = V2)) %>% 
                purrr::map(~data.frame(year = rep((sim.begin):(sim.begin + 
                  years - 1), num.trials), trial = rep(1:num.trials, 
                  each = years), x = pmax(rnorm(years * num.trials, 
                  .$a, .$sigma), 0), a = .$a, b = .$b, sigma = .$sigma)) %>% 
                dplyr::bind_rows(.id = "stock") %>% dplyr::mutate(rec = x + 
                b * dplyr::lag(x), rec = ifelse(is.na(rec), x, 
                rec)) %>% select(stock, year, trial, recruitment = rec)
        }
    }    else {
        prj.rec <- tmp %>% dplyr::filter(year > sim.begin - 4) %>% 
            dplyr::group_by(stock) %>% dplyr::summarise(recruitment = mean(recruitment)) %>% 
            dplyr::left_join(expand.grid(stock = stocks %>% purrr::mcallGadget(s = 1, i = sprintf("%s/params.forward", pre), 
        main = sprintf("%s/main.pre", pre))ap(Rgadget:::getStockNames) %>% 
                unlist, year = (sim.begin):(sim.begin + years - 
                1), trial = 1:num.trials) %>% dplyr::arrange(stock, 
                year, trial))
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
                "rec", year, sep = ".")) %>% dplyr::select(trial, 
                switch, recruitment) %>% tidyr::spread(switch, 
                recruitment) %>% dplyr::select(-trial) %>% dplyr::slice(rep(1:num.trials, 
                each = length(effort))) %>% dplyr::mutate(rgadget.effort = rep(effort, 
                num.trials))) %>% write.gadget.parameters(file = sprintf("%s/params.forward", 
            pre), columns = FALSE)
    }
    if (is.null(rec.scalar)) {
        prj.rec <- prj.rec %>% mutate(rec.scalar = 1)
    }    else {
        if (sum(rec.scalar$stock %in% prj.rec$stock) == 0) 
            warning("No stocks found in rec.scalar")
        prj.rec <- prj.rec %>% left_join(rec.scalar %>% select(stock, 
            year, rec.scalar)) %>% mutate(rec.scalar = ifelse(is.na(rec.scalar), 
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
        custom.print <- paste(readLines(custom.print), collapse = "\n ")
    }    else {
        NULL
    }
    printfile <- paste(custom.print, ";", paste(sprintf(catch.print, 
        unique(fleet$prey$stock), pre, paste(all.fleets, paste(fleet$fleet$fleet, 
            collapse = " "))), collapse = "\n"), paste(sprintf(print.txt, 
        unique(fleet$prey$stock), pre), collapse = "\n"), ";", 
        "[component]", "type\tlikelihoodsummaryprinter", "printfile\t.jnk", 
        sep = "\n")
    dir.create(sprintf("%s/out/", pre), showWarnings = FALSE, 
        recursive = TRUE)
    main$printfiles <- sprintf("%s/printfile", pre)
    Rgadget:::write.unix(printfile, f = sprintf("%s/printfile", pre))
    main$likelihoodfiles <- ";"
    llply(stocks, function(x) {
        tmp <- prj.rec %>% dplyr::filter(stock == x@stockname, 
            trial == 1) %>% dplyr::left_join(rec.step.ratio, 
            by = c("stock"))
        if (x@doesrenew == 1) {
            x@renewal.data <- x@renewal.data %>% dplyr::filter(year < 
                sim.begin) %>% dplyr::bind_rows(x@renewal.data %>% 
                dplyr::filter(year == min(ref.years)) %>% dplyr::slice(rep(1:n(), 
                nrow(tmp))) %>% dplyr::mutate(year = as.character(tmp$year), 
                number = sprintf("(* (* 0.0001 #%s.rec.%s ) %s)", 
                  x@stockname, year, tmp$rec.scalar * tmp$rec.ratio)) %>% 
                dplyr::select_(.dots = names(x@renewal.data))) %>% 
                as.data.frame()
        }
         Rgadget:::gadget_dir_write(gd, x)
    })
    main$stockfiles <- paste(sprintf("%s/%s", pre, laply(stocks, 
        function(x) x@stockname)), collapse = " ")
    write.gadget.main(main, file = sprintf("%s/main.pre", pre))
#here change manually normalcondfile by norm
    callGadget(s = 1, i = sprintf("%s/params.forward", pre), 
        main = sprintf("%s/main.pre", pre), log='tmp')
    time <- new("gadget-time", firstyear = time$firstyear, firststep = time$firststep, 
        lastyear = time$lastyear, laststep = time$laststep, notimesteps = time$notimesteps)
    printOut <- NA
    if (!is.null(custom.print)) {
        tmp <- read.gadget.file(".", custom.print, recursive = FALSE)
        printOut <- vector("list", length(tmp) - 1)
        names(printOut) <- lapply(tmp[-1], function(x) {
            x[["printfile"]]
        })
        for (i in 1:length(printOut)) {
            printOut[[i]] <- read.table(paste(".", names(printOut)[i], 
                sep = "/"), comment.char = ";")
            if (lapply(tmp[-1], function(x) {
                x[["type"]]
            })[[i]] == "stockstdprinter") {
                colnames(printOut[[i]]) <- c("year", "step", 
                  "area", "age", "number", "length", "weight", 
                  "stddev", "consumed", "biomass")
            }
            if (lapply(tmp[-1], function(x) {
                x[["type"]]
            })[[i]] == "stockfullprinter") {
                colnames(printOut[[i]]) <- c("year", "step", 
                  "area", "age", "length", "number", "weight")
            }
            if (lapply(tmp[-1], function(x) {
                x[["type"]]
            })[[i]] == "stockprinter") {
                colnames(printOut[[i]]) <- c("year", "step", 
                  "area", "age", "length", "number", "weight")
            }
            if (lapply(tmp[-1], function(x) {
                x[["type"]]
            })[[i]] == "predatorprinter") {
                colnames(printOut[[i]]) <- c("year", "step", 
                  "area", "pred", "prey", "amount")
            }
            if (lapply(tmp[-1], function(x) {
                x[["type"]]
            })[[i]] == "Predatoroverprinter") {
                colnames(printOut[[i]]) <- c("year", "step", 
                  "area", "length", "biomass")
            }
            if (lapply(tmp[-1], function(x) {
                x[["type"]]
            })[[i]] == "preyoverprinter") {
                colnames(printOut[[i]]) <- c("year", "step", 
                  "area", "length", "biomass")
            }
            if (lapply(tmp[-1], function(x) {
                x[["type"]]
            })[[i]] == "stockpreyfullprinter") {
                colnames(printOut[[i]]) <- c("year", "step", 
                  "area", "age", "length", "number", "biomass")
            }
            if (lapply(tmp[-1], function(x) {
                x[["type"]]
            })[[i]] == "stockpreyprinter") {
                colnames(printOut[[i]]) <- c("year", "step", 
                  "area", "age", "length", "number", "biomass")
            }
            if (lapply(tmp[-1], function(x) {
                x[["type"]]
            })[[i]] == "predatorpreyprinter") {
                colnames(printOut[[i]]) <- c("year", "step", 
                  "area", "age", "length", "number", "biomass", 
                  "mortality")
            }
            file.remove(paste(".", names(printOut)[i], sep = "/"))
        }
    }
    out <- list(lw = ldply(unique(fleet$prey$stock), function(x) {
        numsteps <- nrow(subset(Rgadget:::getTimeSteps(time), step == 1))
        tmp <- read.table(sprintf("%s/out/%s.lw", pre, x), comment.char = ";")
        file.remove(sprintf("%s/out/%s.lw", pre, x))
        names(tmp) <- c("year", "step", "area", "age", "length", 
            "number", "weight")
        tmp$stock <- x
        if (num.trials > 1) {
            tmp2 <- length(unique(tmp$area)) * numsteps * length(unique(tmp$length))
            tmp <- cbind(trial = as.factor(rep(1:num.trials, 
                each = length(effort) * tmp2)), effort = as.factor(rep(effort, 
                each = tmp2, num.trials)), tmp)
        } else {
            tmp2 <- length(unique(tmp$area)) * numsteps * length(unique(tmp$length))
            tmp$trial <- as.factor(1)
            tmp$effort <- as.factor(rep(effort, each = tmp2))
        }
        tmp$length <- as.numeric(gsub("len", "", tmp$length))
        if (compact) {
            tmp <- ddply(tmp, ~year + step + trial + effort + 
                stock, summarise, total.bio = sum(number * weight), 
                ssb = sum(Rgadget:::logit(mat.par[1], mat.par[2], length) * 
                  number * weight))
        }
        return(tmp)
    }), catch = ldply(unique(fleet$prey$stock), function(x) {
        numsteps <- nrow(Rgadget:::getTimeSteps(time))
        trials.tmp <- rep(1:num.trials, each = length(effort))
        tmp <- read.table(sprintf("%s/out/catch.%s.lw", pre, 
            x), comment.char = ";")
        file.remove(sprintf("%s/out/catch.%s.lw", pre, x))
        names(tmp) <- c("year", "step", "area", "age", "length", 
            "number.consumed", "biomass.consumed", "mortality")
        tmp$stock <- x
        if ((num.trials > 1) | (length(effort) > 1)) {
            tmp2 <- length(unique(tmp$area)) * numsteps
            tmp <- cbind(trial = as.factor(rep(1:num.trials, 
                each = length(effort) * tmp2)), effort = as.factor(rep(effort, 
                each = tmp2, num.trials)), tmp)
        } else {
            tmp$trial <- as.factor(1)
            tmp2 <- length(unique(tmp$area)) * numsteps
            tmp$effort <- as.factor(rep(effort, each = tmp2))
        }
        return(tmp)
    }), custom.print = printOut, recruitment = prj.rec, num.trials = num.trials, 
        stochastic = stochastic, sim.begin = sim.begin)
    class(out) <- c("gadget.forward", class(out))
    if (save.results) {
        save(out, file = sprintf("%s/out.Rdata", pre))
    }
    return(out)
}

