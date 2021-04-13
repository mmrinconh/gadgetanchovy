wgts="WGTS"
main.file=NULL
if(is.null(main.file)) {
  if(is.null(wgts)){
    main.file <- 'main'
  } else {
    main.file <- sprintf('%s/main.final',wgts)
  }
}
main <- Rgadget:::read.gadget.main(main.file)
out <- read.printfiles(sprintf('%s/out.fit',wgts))
SS <- read.gadget.lik.out(sprintf('%s/SS.print',wgts))
stocks <- Rgadget:::read.gadget.stockfiles(main$stockfiles)
fleet.predict = NULL
predator.prey <- 
  out[grepl('.+\\.prey\\..+',names(out))] %>% 
  purrr::set_names(.,names(.)) %>% 
  purrr::keep(~'number_consumed' %in% names(.)) %>% 
  dplyr::bind_rows(.id='stock') %>% 
  tidyr::separate(stock,c('prey','predator'),sep='\\.prey\\.') %>% 
  dplyr::as_data_frame() %>% 
  dplyr::group_by(year,step,prey,predator) %>% 
  dplyr::mutate(suit = mortality/max(mortality),
                suit = ifelse(is.finite(suit),suit,0),
                length = gsub('len', '', length) %>% 
                  as.numeric())



if(!is.null(fleet.predict)){
  d <- 
    predator.prey %>% 
    dplyr::filter(predator %in% fleet.predict$fleet)
} else {
  d <- predator.prey
}



harv.suit <- 
  d %>% 
  dplyr::group_by(year,step,prey,length) %>% 
  dplyr::filter(biomass_consumed > 0) %>% 
  dplyr::summarise(suit = sum(biomass_consumed*suit)/sum(biomass_consumed)) %>% 
  dplyr::rename(stock = prey)


stock.prey <- 
  out[sprintf('%s.prey',names(stocks))] %>% 
  purrr::set_names(.,names(stocks)) %>% 
  dplyr::bind_rows(.id='stock') %>% 
  dplyr::as_data_frame()

f.age.range <- 
  stock.prey %>% 
  dplyr::group_by(stock) %>% 
  dplyr::summarise(age.min = max(age),age.max=max(age))

f.by.year <- 
  stock.prey %>% 
  dplyr::left_join(f.age.range,by="stock") %>% 
  dplyr::group_by(stock,year,area) %>%
  dplyr::summarise(catch=sum(biomass_consumed),
                   num.catch=sum(number_consumed),
                   F=mean(mortality[age>=age.min&age<=age.max]))


params.file <- sprintf('%s/params.final',wgts)
params <- read.gadget.parameters(params.file)
stock.recruitment <- Rgadget:::get.gadget.recruitment(stocks,params)


mat.par=NULL
stock.full <-
  out[sprintf('%s.full',names(stocks))] %>% 
  purrr::set_names(.,names(stocks)) %>% 
  dplyr::bind_rows(.id='stock') %>% 
  dplyr::mutate(length=as.numeric(gsub('len','',length))) %>% 
  dplyr::as_data_frame()
Res.by.year <- 
  stock.full %>% 
  dplyr::filter(step == 2) %>%
  dplyr::left_join(harv.suit) %>% 
  dplyr::group_by(stock,year,area) %>%
  dplyr::summarise(total.number = sum(number),
                   total.biomass = sum(number*mean_weight),
                   harv.biomass = sum(number*suit*mean_weight),
                   ssb = sum(mean_weight*Rgadget:::logit(mat.par[1],
                                                         mat.par[2],
                                                         length)*
                               number)) %>% 
  dplyr::left_join(f.by.year %>%
                     dplyr::mutate(area = area),
                   by = c("stock","year","area")) %>% 
  dplyr::left_join(stock.recruitment %>% 
                     dplyr::mutate(stock = as.character(stock),
                                   area = paste0('area',area),
                                   year = as.numeric(year))) %>% 
  dplyr::ungroup()
save(Res.by.year, file="resbyyear.Rdata")
