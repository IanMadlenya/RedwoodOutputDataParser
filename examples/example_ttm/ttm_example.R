
source("RedwoodDataLoader.R")



ttm_testdata = read.csv("examples/example_ttm/20160428-ttm-test-a/test-b/Revealed Preferences-2016-04-28 12-47-02.614648.csv")
View(ttm_testdata %>%
       filter(Period %in% c(56,57) & Sender == 1 & Key == "rp.round_started"))


ttm_testdata <- ttm_testdata %>%
  dplyr::filter(Period == 56)
ttm_testdata = rwp(data = ttm_testdata,
                   keys = c("rp.allocations","rp.assignedGroup","rp.configuration",
                            "rp.confirm","rp.endowment","rp.price","rp.inTTM",
                            "rp.last_limits","rp.next_round","rp.perform_allocation",
                            "rp.result","rp.results"))