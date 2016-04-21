
source("RedwoodDataLoader.R")



ttm_testdata = read.csv("examples/example_ttm/TTM-Baruch-2016-04-15 13_54_53.101035 (1).csv")
ttm_testdata <- ttm_testdata %>%
  dplyr::filter(Period == 56)
ttm_testdata = rwp(data = ttm_testdata,
                             keys = c("rp.allocations","rp.assignedGroup","rp.configuration",
                                      "rp.confirm","rp.endowment","rp.price","rp.inTTM",
                                      "rp.last_limits","rp.next_round","rp.perform_allocation",
                                      "rp.result","rp.results"))