
# Load Dependencies  -------
source("RedwoodDataLoader.R")

# Load data -----

# Load experiment data
ttm_data  = read.csv("examples/example_ttm/TTM-Baruch-2016-04-15 13_54_53.101035 (1).csv")

# load config file
ttm_config <- read.csv("examples/example_ttm/ttm_config.csv")


# Groups -------
#' there are two groups playing TTM, these are defined by the config file
#' 
# - Extract subject groupings, as defined by config file:
# - bulk of confusing code is just extracting subject-ids for each group
groups <- ttm_config$subject[grep("\\[",as.character(ttm_config$subject))]
groups <- unique(groups)
groups_tmp <- as.character(groups)
groups <- data.frame()
# - loop creates a clean data.frame
for (i in 1:length(groups_tmp)){
  groups_tmp2 <- strsplit(groups_tmp[i],c(",|\\[|\\]")) %>%
    unlist %>%
    as.integer
  groups_tmp2 <- groups_tmp2[!is.na(groups_tmp2)]
  
  groups <- rbind(
    groups, 
    data.frame(
      subject = groups_tmp2,
      group_id   = i 
    )
  )
} # Sorry, that was hacky...
# `groups` data.frame is a clean table with subjects and their group number. 
#    groups


# Focus on TTM Period ------------

# select single ttm period
ttm_P65 <- ttm_data %>%
  dplyr::filter(Period == 56)

# rw-parser
ttm_P65 <- rwp(ttm_P65,
               keys = c("rp.confirm","rp.round_started"))

ttm_P65_rp.confirm <- ttm_P65 %>% dplyr::filter(Key == "rp.confirm")
ttm_P65_rp.round_started <- ttm_P65 %>% dplyr::filter(Key == "rp.round_started")
ttm_P65 <- left_join(
  ttm_P65_rp.confirm %>% select(Period:Time, -Group, -Key, starts_with("rp.confirm"), -starts_with("rp.round_started")),
  ttm_P65_rp.round_started %>% select(-Time, -ClientTime, -Key, -datetime, -Group, starts_with("rp.round_started"), -starts_with("rp.confirm")),
  by = c("Period", "Sender", "rp.confirm.round" = "rp.round_started.round")
)


ttm_P65 <- left_join(
  ttm_P65 %>% mutate(Sender = as.integer(Sender)),
  groups,
  by = c("Sender" = "subject")
) %>%
  select(Period, Sender, group_id, rp.confirm.round, everything()) %>% #bring group_id to the fore
  arrange(Period, group_id, rp.confirm.round, Sender, Time)  #sort




write.csv(ttm_P65, 
          file = "examples/example_ttm/TTM_p56.CSV")



