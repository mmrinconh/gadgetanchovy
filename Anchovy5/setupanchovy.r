library(mfdb)
## Create a gadget directory, define some defaults to use with our queries below
gd <- gadget_directory("example-iceland-model")
defaults <- list(
  area = mfdb_group("101" = unique(reitmapping$SUBDIVISION)),
  timestep = mfdb_timestep_quarterly,
  year = 1984:2012,
  species = 'COD')
## Write out areafile and update mainfile with areafile location
gadget_dir_write(gd, gadget_areafile(
  size = mfdb_area_size(mdb, defaults)[[1]],
  temperature = mfdb_temperature(mdb, defaults)[[1]]))
## Write a penalty component to the likelihood file
gadget_dir_write(gd, gadget_likelihood_component("penalty",
                                                 name = "bounds",
                                                 weight = "0.5",
                                                 data = data.frame(
                                                   switch = c("default"),
                                                   power = c(2),
                                                   stringsAsFactors = FALSE)))
## Query length data to create IGFS catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
  sampling_type = 'IGFS',
  species = 'COD',
  length = mfdb_interval("len", seq(0, 150, by = 2))),
  defaults))

gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "ldist.igfs",
                                                 weight = 1,
                                                 data = aggdata[[1]],
                                                 fleetnames = c("igfs"),
                                                 stocknames = c("codimm", "codmat")))
rm(aggdata)
## Age IGFS
aggdata <-
  mfdb_sample_count(mdb, c('age', 'length'),
                    c(list(sampling_type = 'IGFS',
                           age = mfdb_step_interval('age',by=1,from=1,to=12),
                           species='COD',
                           length = mfdb_interval("len", seq(0, 150, by = 4))),
                      defaults))
gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.igfs",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("igfs"),
                                             stocknames = c("codimm", "codmat")))
rm(aggdata)

## Maturity @3 from IGFS
aggdata <- mfdb_sample_count(mdb, c('maturity_stage', 'length'),
                             append(defaults,
                                    list(sampling_type='IGFS',
                                         age=mfdb_group(age=3),
                                         length = mfdb_step_interval('len', by = 2, to = 100),
                                         maturity_stage = mfdb_group(codimm = 1, codmat = 2:5))))

gadget_dir_write(gd,
                 gadget_likelihood_component("stockdistribution",
                                             name = "matp.igfs",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("igfs"),
                                             stocknames = c("codimm", "codmat")))



## Query length data to create AUT catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
  sampling_type = 'AUT',
  species = 'COD',
  length = mfdb_interval("len", seq(0, 150, by = 2))),
  defaults))

gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "ldist.aut",
                                                 weight = 1,
                                                 data = aggdata[[1]],
                                                 fleetnames = c("aut"),
                                                 stocknames = c("codimm", "codmat")))
rm(aggdata)
## Age AUT
aggdata <-
  mfdb_sample_count(mdb, c('age', 'length'),
                    c(list(sampling_type = 'AUT',
                           age = mfdb_step_interval('age',by=1,from=1,to=12),
                           length = mfdb_interval("len", seq(0, 150, by = 4))),
                      defaults))
gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.aut",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("aut"),
                                             stocknames = c("codimm", "codmat")))
rm(aggdata)


## Query length data to create COMM catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
  sampling_type = 'SEA',
  species = 'COD',
  length = mfdb_interval("len", seq(0, 150, by = 2))),
  defaults))

gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "ldist.comm",
                                                 weight = 1,
                                                 data = aggdata[[1]],
                                                 fleetnames = c("comm"),
                                                 stocknames = c("codimm", "codmat")))
rm(aggdata)
## Age AUT
aggdata <-
  mfdb_sample_count(mdb, c('age', 'length'),
                    c(list(sampling_type = 'SEA',
                           age = mfdb_step_interval('age',by=1,from=1,to=12),
                           length = mfdb_interval("len", seq(0, 150, by = 4))),
                      defaults))
gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.comm",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("comm"),
                                             stocknames = c("codimm", "codmat")))
rm(aggdata)

## IGFS survey indices

igfs.SI1 <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(4,17))),
  defaults))

igfs.SI2 <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(17,33))),
  defaults))

igfs.SI3 <- mfdb_sample_count(mdb, c( 'length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(33,140))),
  defaults))


gadget_dir_write(gd, gadget_likelihood_component("surveyindicies",
                                                 name = "si.gp1",
                                                 weight = 1,
                                                 data = igfs.SI1[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("codimm")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindicies",
                                                 name = "si.gp2",
                                                 weight = 1,
                                                 data = igfs.SI2[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("codimm","codmat")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindicies",
                                                 name = "si.gp3",
                                                 weight = 1,
                                                 data = igfs.SI3[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("codimm","codmat")))
## AUT survey indices

igfs.SI1 <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(16,27))),
  defaults))

igfs.SI2 <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(27,39))),
  defaults))

igfs.SI3 <- mfdb_sample_count(mdb, c( 'length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(39,140))),
  defaults))


gadget_dir_write(gd, gadget_likelihood_component("surveyindicies",
                                                 name = "si.gp1a",
                                                 weight = 1,
                                                 data = igfs.SI1[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("codimm")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindicies",
                                                 name = "si.gp2a",
                                                 weight = 1,
                                                 data = igfs.SI2[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("codimm","codmat")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindicies",
                                                 name = "si.gp3a",
                                                 weight = 1,
                                                 data = igfs.SI3[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("codimm","codmat")))
