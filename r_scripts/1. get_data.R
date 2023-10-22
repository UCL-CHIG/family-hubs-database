setwd("S:/ICH_PPP_CENB_CEBCH/Matthew/OHID/DATABASE/For GitHub/")

# to install fingertipsR
options(repos = c(
  ropensci = 'https://ropensci.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
install.packages("fingertipsR")

library(fingertipsR)
library(data.table)

# fingertips --------------------------------------------------------------

prof_name <- "child and maternal health"
domains <- c("child health profiles",
             "pregnancy and birth",
             "early years",
             "school-age children",
             "young people")

profs <- data.table(profiles())
profs[, ProfileName := tolower(ProfileName)]
profs[, DomainName := tolower(DomainName)]
profs <- profs[grepl(prof_name, ProfileName)]

domain_ids <- profs[DomainName %in% domains]$DomainID

spine <- fread("raw/spine.csv", stringsAsFactors = F)

area_codes <- c("E92000001", "E12000007", spine$LAD21C)

fingertips_data_london <- data.table()

for (i in 1:length(domain_ids)) {
  
  domain_name <- profs[DomainID == domain_ids[i]]$DomainName
  inds <- data.table(indicators(DomainID = domain_ids[i]))
  inds_vector <- inds$IndicatorID
  dt <- data.table(fingertips_data(IndicatorID = inds_vector, AreaTypeID = 402)) # Upper tier local authorities (post 4/21)
  dt <- dt[AreaCode %in% area_codes]
  dt[, domain_name := domain_name]
  
  fingertips_data_london <- rbind(fingertips_data_london, dt)
  
}

fingertips_data_london[grep("self-harm", tolower(IndicatorName))]

inds <- c(93700, # children in relative low income
          93701, # children in absolute low income
          93739, # homeless
          90812, # hosp adm MH
          90813, # self -harm 10-24
          90808, # hosp adm substance misuse
          92904) # hosp adm alcohol

fingertips_data_london <- fingertips_data_london[IndicatorID %in% inds]
rm(inds)

# drop england and london region
fingertips_data_london <- fingertips_data_london[!(AreaName %in% c("England", "London region"))]

# keep persons only
table(fingertips_data_london$IndicatorName, fingertips_data_london$Sex)
fingertips_data_london <- fingertips_data_london[!(Sex %in% c("Female", "Male"))]

# rationalise year variable and check for duplication
table(fingertips_data_london$Timeperiod)
fingertips_data_london[, year_end := as.integer(substr(Timeperiod, nchar(Timeperiod) - 1, nchar(Timeperiod)))]
table(fingertips_data_london$IndicatorName, fingertips_data_london$year_end)

# remove domain id and drop as some Indicators appear in 2 domains
fingertips_data_london <- fingertips_data_london[, -c("domain_name")]
fingertips_data_london <- fingertips_data_london[!duplicated(fingertips_data_london)]
table(fingertips_data_london$IndicatorName, fingertips_data_london$year_end)

# now only take max year per LA
fingertips_data_london[, year_end_max_la := max(year_end), by = .(AreaCode, IndicatorName)]
fingertips_data_london <- fingertips_data_london[year_end == year_end_max_la]
table(fingertips_data_london$IndicatorName, fingertips_data_london$year_end_max_la)

# clean up
setnames(fingertips_data_london, tolower(names(fingertips_data_london)))
fingertips_data_london <- fingertips_data_london[, c("areaname", "areacode", "indicatorname", "indicatorid",
                                                     "age", "timeperiod", "year_end",
                                                     "value", "lowerci95.0limit", "upperci95.0limit",
                                                     "count", "denominator",
                                                     "valuenote")]

fwrite(fingertips_data_london, file = "final_database/fingertips_data_london.csv")

rm(list = ls())

# spine -------------------------------------------------------------------

spine <- fread("raw/spine.csv")

# pop size
pop <- fread("raw/pop2021.csv")
pop <- pop[ladcode21 %in% spine$LAD21C]
pop[, child_age := age < 18]

pop[, population_2021_n := sum(population_2021), by = ladcode21]
pop[, age_sp_population_2021_n := sum(population_2021), by = .(ladcode21, child_age)]

pop <- pop[age < 18]

pop[, perc_child_population := round((age_sp_population_2021_n / population_2021_n) * 100, 1)]

pop <- pop[, c("ladcode21", "age_sp_population_2021_n", "population_2021_n", "perc_child_population")]
setnames(pop, c("LAD21C", "child_population_2021", "total_population_2021", "perc_child_population_2021"))
pop <- pop[!duplicated(pop)]

spine <- merge(spine,
               pop,
               by = "LAD21C",
               all.x = T)
rm(pop)

# pop change
pop <- fread("raw/MYEB1_detailed_population_estimates_series_UK_(2020_geog21).csv")
pop <- pop[ladcode21 %in% spine$LAD21C]
pop <- pop[age < 18]
pop <- pop[, c("ladcode21", "population_2011")]
pop[, child_population_2011 := sum(population_2011), by = ladcode21]
pop <- pop[, c("ladcode21", "child_population_2011")]
pop <- pop[!duplicated(pop)]

spine <- merge(spine,
               pop,
               by.x = "LAD21C",
               by.y = "ladcode21",
               all.x = T)

spine[, change_child_population := round(child_population_2021 / child_population_2011, 2)]

# % pop BAME
pop_eth <- fread("raw/pop_eth.csv")
pop_eth <- pop_eth[code %in% spine$LAD21C]
pop_eth <- pop_eth[age < 18]
pop_eth[, total_white := sum(white), by = code]
pop_eth[, total_other := sum(other), by = code]
pop_eth[, total_pop := total_white + total_other]
pop_eth[, perc_child_em_population_2021 := round((total_other / total_pop) * 100, 1)]

# recode CoL & Wmin to just Wmin
pop_eth[code == "E09000001", code := "E09000033"]

pop_eth <- pop_eth[, c("code", "perc_child_em_population_2021")]
pop_eth <- pop_eth[!duplicated(pop_eth)]
setnames(pop_eth, c("LAD21C", "perc_child_em_population_2021"))

spine <- merge(spine,
               pop_eth,
               by = "LAD21C",
               all.x = T)

rm(pop_eth)

# fsm
fsm <- fread("raw/fsm_time_series.csv")
fsm <- fsm[new_la_code %in% spine$LAD21C]
fsm <- fsm[phase == "Total" & time_period == 202021]
fsm <- fsm[, c("new_la_code", "fsm_eligible_percent")]
setnames(fsm, c("LAD21C", "fsm_eligible_percent_aut202021"))

spine <- merge(spine,
               fsm,
               by = "LAD21C",
               all.x = T)

rm(fsm)

# imd
imd <- fread("raw/imd2019.csv")
imd <- imd[, -1]

spine <- merge(spine,
               imd,
               by = "LAD21C",
               all.x = T)

rm(imd)

# ofsted
ofsted <- fread("raw/ofsted.csv")

ofsted <- ofsted[, c("LAD21C",
                     "ofsted_date_1", "ofsted_overall_1",
                     "ofsted_date_2", "ofsted_overall_2")]

ofsted[ofsted_overall_1 == "Outstanding", ofsted_overall_1 := 1]
ofsted[ofsted_overall_1 == "Good", ofsted_overall_1 := 2]
ofsted[ofsted_overall_1 == "Requires improvement", ofsted_overall_1 := 3]
ofsted[ofsted_overall_1 == "Inadequate", ofsted_overall_1 := 4]

ofsted[ofsted_overall_2 == "Outstanding", ofsted_overall_2 := 1]
ofsted[ofsted_overall_2 == "Good", ofsted_overall_2 := 2]
ofsted[ofsted_overall_2 == "Requires improvement", ofsted_overall_2 := 3]
ofsted[ofsted_overall_2 == "Inadequate", ofsted_overall_2 := 4]

ofsted[, ofsted_overall_1 := as.integer(ofsted_overall_1)]
ofsted[, ofsted_overall_2 := as.integer(ofsted_overall_2)]

spine <- merge(spine,
               ofsted,
               by = "LAD21C",
               all.x = T)

rm(ofsted)

spine[, ofsted_change := ofsted_overall_1 - ofsted_overall_2]

# SEND
send <- fread("raw/sen_phase_type.csv")
send <- send[new_la_code %in% spine$LAD21C]
send <- send[time_period == 202122]
send <- send[hospital_school == "Total"]
send <- send[type_of_establishment == "Total"]
send <- send[phase_type_grouping == "Total"]

send <- send[, c("new_la_code", "sen_support_percent", "ehc_plan_percent")]
setnames(send, c("LAD21C", "sen_support_percent202122", "ehc_plan_percent202122"))

spine <- merge(spine,
               send,
               by = "LAD21C",
               all.x = T)

rm(send)

# social care
# cin refs
cin_refs <- fread("raw/cin_referrals.csv")
cin_refs <- cin_refs[location_code %in% spine$LAD21C]
cin_refs[Referrals == "x", Referrals := "5031"] # replace Hackey's 2022 value with 2020 - latest available
cin_refs <- cin_refs[time_period == 2022]
cin_refs[, Referrals := as.integer(Referrals)]
cin_refs <- cin_refs[, c("location_code", "Referrals")]
setnames(cin_refs, c("LAD21C", "cin_refs_202122"))

spine <- merge(spine,
               cin_refs,
               by = "LAD21C",
               all.x = T)

rm(cin_refs)

spine[, cin_refs_202122_rate_10000 := round((cin_refs_202122 / child_population_2021) * 10000, 1)]

# cin need
cin_need <- fread("raw/cin_in_need_31.csv")
cin_need <- cin_need[location_code %in% spine$LAD21C]
cin_need[At31_episodes == "x", At31_episodes := "3094"] # replace Hackey's 2022 value with 2020 - latest available
cin_need <- cin_need[time_period == 2022]
cin_need[, At31_episodes := as.integer(At31_episodes)]
cin_need <- cin_need[, c("location_code", "At31_episodes")]
setnames(cin_need, c("LAD21C", "cin_at31_episodes_2022"))

spine <- merge(spine,
               cin_need,
               by = "LAD21C",
               all.x = T)

rm(cin_need)

spine[, cin_at31_episodes_2022_rate_10000 := round((cin_at31_episodes_2022 / child_population_2021) * 10000, 1)]

# cla
cla <- fread("raw/cla_data.csv")
cla <- cla[location_code %in% spine$LAD21C]
cla <- cla[time_period == 2022]
cla_31 <- cla[population_count == "Children looked after at 31 March each year", c("location_code", "number")]
setnames(cla_31, c("LAD21C", "cla_at31_2022"))
cla_start <- cla[population_count == "Children starting to be looked after each year", c("location_code", "number")]
setnames(cla_start, c("LAD21C", "cla_starts_202122"))

spine <- merge(spine,
               cla_31,
               by = "LAD21C",
               all.x = T)

spine <- merge(spine,
               cla_start,
               by = "LAD21C",
               all.x = T)

rm(cla, cla_31, cla_start)

spine[, cla_at31_2022_rate_10000 := round((cla_at31_2022 / child_population_2021) * 10000, 1)]
spine[, cla_starts_202122_rate_10000 := round((cla_starts_202122 / child_population_2021) * 10000, 1)]

# fh data
fh <- fread("raw/web_searches_data.csv")
fh <- fh[, c("LAD21C", "has_cc", "number_cc", "has_fh", "number_fh", "notes")]
setnames(fh, "notes", "fh_notes")

spine <- merge(spine,
               fh,
               by = "LAD21C",
               all.x = T)

# save
fwrite(spine, file = "final_database/contextual_database.csv")

rm(list = ls())
