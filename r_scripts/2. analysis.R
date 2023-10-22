setwd("S:/ICH_PPP_CENB_CEBCH/Matthew/OHID/DATABASE")
library(data.table)
library(ggplot2)

spine <- fread("spine_enhanced.csv", stringsAsFactors = F)
ftps <- fread("fingertips_data_london.csv")

ftps <- merge(ftps,
              spine[, c("LAD21C", "budget_2021_75")],
              by.x = "areacode", 
              by.y = "LAD21C",
              all.x = T)

# FH details --------------------------------------------------------------

sum_fun <- function(x, dp) {
  med <- round(quantile(x, 0.50), dp)
  lq <- round(quantile(x, 0.25), dp)
  uq <- round(quantile(x, 0.75), dp)
  minimum <- round(min(x), dp)
  maximum <- round(max(x), dp)
  mu <- round(mean(x), dp)
  sd <- round(sd(x), dp)
  return(
    paste0(med, " (", lq, ", ", uq, "); [", minimum, ", ", maximum, "]; ", mu, " (", sd, ")")
  )
}

# demography --------------------------------------------------------------

# child pop size
aggregate(spine$child_population_2021 ~ spine$budget_2021_75, FUN = sum_fun, dp = 2)

spine[, LAD21NM := factor(LAD21NM,
                          levels = spine$LAD21NM[order(spine$budget_2021_75, spine$child_population_2021, decreasing = T)])]
spine[, budget_2021_75_tmp := as.character(budget_2021_75)]
spine[LAD21NM == params$auth, budget_2021_75_tmp := params$auth]

namevals <- c("0", "1", params$auth)
colvalues <- c("blue", "red", "orange")
labvalues <- c("No", "Yes", params$auth)

names(colvalues) <- namevals
names(labvalues) <- namevals

ggplot(aes(x = LAD21NM,
           y = child_population_2021,
           fill = as.factor(budget_2021_75_tmp)),
       data = spine) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Child population 2021",
                     expand = c(0, 0)) +
  scale_x_discrete(name = "") +
  labs(fill = "TF2",
       title = "Child population size in 2021") +
  scale_fill_manual("TF2",
                    labels = labvalues,
                    values = colvalues) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(colour = "black")) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 1]$child_population_2021, probs = 0.50)), " (",
                                  round(quantile(spine[budget_2021_75 == 1]$child_population_2021, probs = 0.25)), ", ",
                                  round(quantile(spine[budget_2021_75 == 1]$child_population_2021, probs = 0.75)), ")"),
           x = 10, y = 75000, size = 3) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 0]$child_population_2021, probs = 0.50)), " (",
                                  round(quantile(spine[budget_2021_75 == 0]$child_population_2021, probs = 0.25)), ", ",
                                  round(quantile(spine[budget_2021_75 == 0]$child_population_2021, probs = 0.75)), ")"),
           x = 28, y = 75000, size = 3)

# % pop children
aggregate(spine$perc_child_population_2021 ~ spine$budget_2021_75, FUN = sum_fun, dp = 2)

spine[, LAD21NM := factor(LAD21NM,
                          levels = spine$LAD21NM[order(spine$budget_2021_75, spine$perc_child_population_2021, decreasing = T)])]

ggplot(aes(x = LAD21NM,
           y = perc_child_population_2021,
           fill = as.factor(budget_2021_75)),
       data = spine) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "% child population 2021",
                     expand = c(0, 0),
                     limits = c(0, 30)) +
  scale_x_discrete(name = "") +
  labs(fill = "TF2",
       title = "Percentage of population who are children 2021") +
  scale_fill_discrete(labels = c("Not TF2", "TF2")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(colour = "black")) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 1]$perc_child_population_2021, probs = 0.50)), " (",
                                  round(quantile(spine[budget_2021_75 == 1]$perc_child_population_2021, probs = 0.25)), ", ",
                                  round(quantile(spine[budget_2021_75 == 1]$perc_child_population_2021, probs = 0.75)), ")"),
           x = 10, y = 25, size = 3) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 0]$perc_child_population_2021, probs = 0.50)), " (",
                                  round(quantile(spine[budget_2021_75 == 0]$perc_child_population_2021, probs = 0.25)), ", ",
                                  round(quantile(spine[budget_2021_75 == 0]$perc_child_population_2021, probs = 0.75)), ")"),
           x = 28, y = 25, size = 3)

# change in size of child population over last 10  years
aggregate(spine$change_child_population ~ spine$budget_2021_75, FUN = sum_fun, dp = 2)
table(spine$budget_2021_75, spine$change_child_population > 1)

spine[, change_child_population_pc := (change_child_population - 1) * 100]

spine[, LAD21NM := factor(LAD21NM,
                          levels = spine$LAD21NM[order(spine$budget_2021_75, spine$change_child_population_pc, decreasing = T)])]

ggplot(aes(x = LAD21NM,
           y = change_child_population_pc,
           fill = as.factor(budget_2021_75)),
       data = spine) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Relative change in child population 2021 to 2021",
                     expand = c(0, 0),
                     limits = c(-20, 20)
                     ) +
  scale_x_discrete(name = "") +
  labs(fill = "TF2",
       title = "Percentage change in child population 2011 to 2021") +
  scale_fill_discrete(labels = c("Not TF2", "TF2")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(colour = "black")) +
  # annotate("text", label = paste0("Number increasing: ", 11, "\nNumber decreasing: ", 5),
  #          x = 10, y = 1.4, size = 3) +
  # annotate("text", label = paste0("Number increasing: ", 13, "\nNumber decreasing: ", 4),
  #          x = 28, y = 1.4, size = 3) +
  geom_hline(yintercept = 0, colour = "black")

# % children BAME
aggregate(spine$perc_child_em_population_2021 ~ spine$budget_2021_75, FUN = sum_fun, dp = 2)

spine[, LAD21NM := factor(LAD21NM,
                          levels = spine$LAD21NM[order(spine$budget_2021_75, spine$perc_child_em_population_2021, decreasing = T)])]

ggplot(aes(x = LAD21NM,
           y = perc_child_em_population_2021,
           fill = as.factor(budget_2021_75)),
       data = spine) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "% child population not white 2021",
                     expand = c(0, 0),
                     limits = c(0, 100)) +
  scale_x_discrete(name = "") +
  labs(fill = "TF2",
       title = "Percentage of child population who are not white 2021") +
  scale_fill_discrete(labels = c("Not TF2", "TF2")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(colour = "black")) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 1]$perc_child_em_population_2021, probs = 0.50,na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 1]$perc_child_em_population_2021, probs = 0.25,na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 1]$perc_child_em_population_2021, probs = 0.75,na.rm = T)), ")"),
           x = 10, y = 80, size = 3) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 0]$perc_child_em_population_2021, probs = 0.50,na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 0]$perc_child_em_population_2021, probs = 0.25,na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 0]$perc_child_em_population_2021, probs = 0.75,na.rm = T)), ")"),
           x = 28, y = 80, size = 3)


# deprivation -------------------------------------------------------------

# % fsm
aggregate(spine$fsm_eligible_percent_aut202021 ~ spine$budget_2021_75, FUN = sum_fun, dp = 2)

spine[, LAD21NM := factor(LAD21NM,
                          levels = spine$LAD21NM[order(spine$budget_2021_75, spine$fsm_eligible_percent_aut202021, decreasing = T)])]

ggplot(aes(x = LAD21NM,
           y = fsm_eligible_percent_aut202021,
           fill = as.factor(budget_2021_75)),
       data = spine) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "% children getting FSM 2020/21",
                     expand = c(0, 0),
                     limits = c(0, 40)) +
  scale_x_discrete(name = "") +
  labs(fill = "2021 Family Hub budget",
       title = "Percentage of children getting free school meals in 2020/21") +
  scale_fill_discrete(labels = c("Not eligible", "Eligible")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(colour = "black")) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 1]$fsm_eligible_percent_aut202021, probs = 0.50)), " (",
                                  round(quantile(spine[budget_2021_75 == 1]$fsm_eligible_percent_aut202021, probs = 0.25)), ", ",
                                  round(quantile(spine[budget_2021_75 == 1]$fsm_eligible_percent_aut202021, probs = 0.75)), ")"),
           x = 10, y = 30, size = 3) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 0]$fsm_eligible_percent_aut202021, probs = 0.50)), " (",
                                  round(quantile(spine[budget_2021_75 == 0]$fsm_eligible_percent_aut202021, probs = 0.25)), ", ",
                                  round(quantile(spine[budget_2021_75 == 0]$fsm_eligible_percent_aut202021, probs = 0.75)), ")"),
           x = 28, y = 30, size = 3)

# IMD
aggregate(spine$imd_rank_avg_rank_london ~ spine$budget_2021_75, FUN = sum_fun, dp = 2)
aggregate(spine$imd_10pc ~ spine$budget_2021_75, FUN = sum_fun, dp = 2)
aggregate(spine$imd_rank_10pc_london ~ spine$budget_2021_75, FUN = sum_fun, dp = 2)

spine[, LAD21NM := factor(LAD21NM,
                          levels = spine$LAD21NM[order(spine$budget_2021_75, spine$imd_10pc, decreasing = T)])]

ggplot(aes(x = LAD21NM,
           y = imd_10pc,
           fill = as.factor(budget_2021_75)),
       data = spine) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "% LSOAs in most deprived decile nationally",
                     expand = c(0, 0),
                     limits = c(0, 15)) +
  scale_x_discrete(name = "") +
  labs(fill = "2021 Family Hub budget",
       title = "IMD 2019: percentage of LSOAs in the most deprived decile nationally") +
  scale_fill_discrete(labels = c("Not eligible", "Eligible")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(colour = "black")) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 1]$imd_10pc, probs = 0.50)), " (",
                                  round(quantile(spine[budget_2021_75 == 1]$imd_10pc, probs = 0.25)), ", ",
                                  round(quantile(spine[budget_2021_75 == 1]$imd_10pc, probs = 0.75)), ")"),
           x = 10, y = 10, size = 3) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 0]$imd_10pc, probs = 0.50)), " (",
                                  round(quantile(spine[budget_2021_75 == 0]$imd_10pc, probs = 0.25)), ", ",
                                  round(quantile(spine[budget_2021_75 == 0]$imd_10pc, probs = 0.75)), ")"),
           x = 28, y = 10, size = 3)

table(spine$imd_10pc == 0, spine$budget_2021_75)

# homelessness
aggregate(ftps[indicatorid == 93739]$value ~ ftps[indicatorid == 93739]$budget_2021_75, FUN = sum_fun, dp = 2) # homelessness

spine <- merge(spine,
               ftps[indicatorid == 93739, c("areacode", "value")],
               by.x = "LAD21C",
               by.y = "areacode")

setnames(spine, "value", "homeless")

spine[, LAD21NM := factor(LAD21NM,
                          levels = spine$LAD21NM[order(spine$budget_2021_75, spine$homeless, decreasing = T)])]

ggplot(aes(x = LAD21NM,
           y = homeless,
           fill = as.factor(budget_2021_75)),
       data = spine) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Households owed duty rate per 1,000",
                     expand = c(0, 0),
                     limits = c(0, 40)) +
  scale_x_discrete(name = "") +
  labs(fill = "2021 Family Hub budget",
       title = "Households (applicant 16-24 yrs) owed homelessness duty, rate per 1,000") +
  scale_fill_discrete(labels = c("Not eligible", "Eligible")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(colour = "black")) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 1]$homeless, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 1]$homeless, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 1]$homeless, probs = 0.75, na.rm = T)), ") per 1,000"),
           x = 10, y = 30, size = 3) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 0]$homeless, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 0]$homeless, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 0]$homeless, probs = 0.75, na.rm = T)), ") per 1,000"),
           x = 28, y = 30, size = 3)

# children in rel/ low income fams
aggregate(ftps[indicatorid == 93701]$value ~ ftps[indicatorid == 93701]$budget_2021_75, FUN = sum_fun, dp = 2) # abs low income
aggregate(ftps[indicatorid == 93700]$value ~ ftps[indicatorid == 93700]$budget_2021_75, FUN = sum_fun, dp = 2) # rel low income

spine <- merge(spine,
               ftps[indicatorid == 93700, c("areacode", "value")],
               by.x = "LAD21C",
               by.y = "areacode")

setnames(spine, "value", "low_income")

spine[, LAD21NM := factor(LAD21NM,
                          levels = spine$LAD21NM[order(spine$budget_2021_75, spine$low_income, decreasing = T)])]

ggplot(aes(x = LAD21NM,
           y = low_income,
           fill = as.factor(budget_2021_75)),
       data = spine) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "% low income",
                     expand = c(0, 0),
                     limits = c(0, 30)) +
  scale_x_discrete(name = "") +
  labs(fill = "2021 Family Hub budget",
       title = "Percentage of children in relative low income families") +
  scale_fill_discrete(labels = c("Not eligible", "Eligible")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(colour = "black")) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 1]$low_income, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 1]$low_income, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 1]$low_income, probs = 0.75, na.rm = T)), ")"),
           x = 10, y = 25, size = 3) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 0]$low_income, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 0]$low_income, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 0]$low_income, probs = 0.75, na.rm = T)), ")"),
           x = 28, y = 25, size = 3)

# children's services -----------------------------------------------------

# CIN 31 Mar
aggregate(spine$cin_at31_episodes_2022_rate_10000 ~ spine$budget_2021_75, FUN = sum_fun, dp = 2)

spine[, LAD21NM := factor(LAD21NM,
                          levels = spine$LAD21NM[order(spine$budget_2021_75, spine$cin_at31_episodes_2022_rate_10000, decreasing = T)])]

ggplot(aes(x = LAD21NM,
           y = cin_at31_episodes_2022_rate_10000,
           fill = as.factor(budget_2021_75)),
       data = spine) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Rate per 10,000",
                     expand = c(0, 0),
                     limits = c(0, 1600)) +
  scale_x_discrete(name = "") +
  labs(fill = "2021 Family Hub budget",
       title = "Children in need 31 Mar 2022 per 10,000") +
  scale_fill_discrete(labels = c("Not eligible", "Eligible")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(colour = "black")) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 1]$cin_at31_episodes_2022_rate_10000, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 1]$cin_at31_episodes_2022_rate_10000, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 1]$cin_at31_episodes_2022_rate_10000, probs = 0.75, na.rm = T)), ")"),
           x = 10, y = 1100, size = 3) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 0]$cin_at31_episodes_2022_rate_10000, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 0]$cin_at31_episodes_2022_rate_10000, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 0]$cin_at31_episodes_2022_rate_10000, probs = 0.75, na.rm = T)), ")"),
           x = 28, y = 1100, size = 3)

# CIN ref
aggregate(spine$cin_refs_202122_rate_10000 ~ spine$budget_2021_75, FUN = sum_fun, dp = 2)

spine[, LAD21NM := factor(LAD21NM,
                          levels = spine$LAD21NM[order(spine$budget_2021_75, spine$cin_refs_202122_rate_10000, decreasing = T)])]

ggplot(aes(x = LAD21NM,
           y = cin_refs_202122_rate_10000,
           fill = as.factor(budget_2021_75)),
       data = spine) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Rate per 10,000",
                     expand = c(0, 0),
                     limits = c(0, 1000)) +
  scale_x_discrete(name = "") +
  labs(fill = "2021 Family Hub budget",
       title = "Children referred to CSC 2021/22 per 10,000") +
  scale_fill_discrete(labels = c("Not eligible", "Eligible")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(colour = "black")) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 1]$cin_refs_202122_rate_10000, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 1]$cin_refs_202122_rate_10000, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 1]$cin_refs_202122_rate_10000, probs = 0.75, na.rm = T)), ")"),
           x = 10, y = 800, size = 3) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 0]$cin_refs_202122_rate_10000, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 0]$cin_refs_202122_rate_10000, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 0]$cin_refs_202122_rate_10000, probs = 0.75, na.rm = T)), ")"),
           x = 28, y = 800, size = 3)

# CLA 31 Mar
aggregate(spine$cla_at31_2022_rate_10000 ~ spine$budget_2021_75, FUN = sum_fun, dp = 2)

spine[, LAD21NM := factor(LAD21NM,
                          levels = spine$LAD21NM[order(spine$budget_2021_75, spine$cla_at31_2022_rate_10000, decreasing = T)])]

ggplot(aes(x = LAD21NM,
           y = cla_at31_2022_rate_10000,
           fill = as.factor(budget_2021_75)),
       data = spine) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Rate per 10,000",
                     expand = c(0, 0),
                     limits = c(0, 200)) +
  scale_x_discrete(name = "") +
  labs(fill = "2021 Family Hub budget",
       title = "Children looked after 31 Mar 2022 per 10,000") +
  scale_fill_discrete(labels = c("Not eligible", "Eligible")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(colour = "black")) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 1]$cla_at31_2022_rate_10000, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 1]$cla_at31_2022_rate_10000, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 1]$cla_at31_2022_rate_10000, probs = 0.75, na.rm = T)), ")"),
           x = 10, y = 150, size = 3) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 0]$cla_at31_2022_rate_10000, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 0]$cla_at31_2022_rate_10000, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 0]$cla_at31_2022_rate_10000, probs = 0.75, na.rm = T)), ")"),
           x = 28, y = 150, size = 3)

# CLA starts
aggregate(spine$cla_starts_202122_rate_10000 ~ spine$budget_2021_75, FUN = sum_fun, dp = 2)

spine[, LAD21NM := factor(LAD21NM,
                          levels = spine$LAD21NM[order(spine$budget_2021_75, spine$cla_starts_202122_rate_10000, decreasing = T)])]

ggplot(aes(x = LAD21NM,
           y = cla_starts_202122_rate_10000,
           fill = as.factor(budget_2021_75)),
       data = spine) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Rate per 10,000",
                     expand = c(0, 0),
                     limits = c(0, 150)) +
  scale_x_discrete(name = "") +
  labs(fill = "2021 Family Hub budget",
       title = "Children starting to be looked after 2021/22 per 10,000") +
  scale_fill_discrete(labels = c("Not eligible", "Eligible")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(colour = "black")) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 1]$cla_starts_202122_rate_10000, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 1]$cla_starts_202122_rate_10000, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 1]$cla_starts_202122_rate_10000, probs = 0.75, na.rm = T)), ")"),
           x = 10, y = 100, size = 3) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 0]$cla_starts_202122_rate_10000, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 0]$cla_starts_202122_rate_10000, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 0]$cla_starts_202122_rate_10000, probs = 0.75, na.rm = T)), ")"),
           x = 28, y = 100, size = 3)

# ofsted inspection results (next two as table)
aggregate(spine$ofsted_overall_2 ~ spine$budget_2021_75, FUN = sum_fun, dp = 2)

# change in ofsted ratings
aggregate(spine$ofsted_change ~ spine$budget_2021_75, FUN = sum_fun, dp = 2)

# MH and SEN outcomes -----------------------------------------------------

# alcohol
aggregate(ftps[indicatorid == 92904]$value ~ ftps[indicatorid == 92904]$budget_2021_75, FUN = sum_fun, dp = 2) # alcohol adm

spine <- merge(spine,
               ftps[indicatorid == 92904, c("areacode", "value")],
               by.x = "LAD21C",
               by.y = "areacode")

setnames(spine, "value", "alcohol")

spine[, LAD21NM := factor(LAD21NM,
                          levels = spine$LAD21NM[order(spine$budget_2021_75, spine$alcohol, decreasing = T)])]

ggplot(aes(x = LAD21NM,
           y = alcohol,
           fill = as.factor(budget_2021_75)),
       data = spine) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Rate per 100,000",
                     expand = c(0, 0),
                     limits = c(0, 40)) +
  scale_x_discrete(name = "") +
  labs(fill = "2021 Family Hub budget",
       title = "Alcohol-specific condition admissions <18 yrs, per 100,000") +
  scale_fill_discrete(labels = c("Not eligible", "Eligible")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(colour = "black")) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 1]$alcohol, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 1]$alcohol, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 1]$alcohol, probs = 0.75, na.rm = T)), ")"),
           x = 10, y = 25, size = 3) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 0]$alcohol, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 0]$alcohol, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 0]$alcohol, probs = 0.75, na.rm = T)), ")"),
           x = 28, y = 25, size = 3)

# self-harm
aggregate(ftps[indicatorid == 90813]$value ~ ftps[indicatorid == 90813]$budget_2021_75, FUN = sum_fun, dp = 2) # self-harm adm

spine <- merge(spine,
               ftps[indicatorid == 90813, c("areacode", "value")],
               by.x = "LAD21C",
               by.y = "areacode")

setnames(spine, "value", "selfharm")

spine[, LAD21NM := factor(LAD21NM,
                          levels = spine$LAD21NM[order(spine$budget_2021_75, spine$selfharm, decreasing = T)])]

ggplot(aes(x = LAD21NM,
           y = selfharm,
           fill = as.factor(budget_2021_75)),
       data = spine) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Rate per 100,000",
                     expand = c(0, 0),
                     limits = c(0, 550)) +
  scale_x_discrete(name = "") +
  labs(fill = "2021 Family Hub budget",
       title = "Self-harm admissions ages 10-24 yrs, per 100,000") +
  scale_fill_discrete(labels = c("Not eligible", "Eligible")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(colour = "black")) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 1]$selfharm, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 1]$selfharm, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 1]$selfharm, probs = 0.75, na.rm = T)), ")"),
           x = 10, y = 400, size = 3) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 0]$selfharm, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 0]$selfharm, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 0]$selfharm, probs = 0.75, na.rm = T)), ")"),
           x = 28, y = 400, size = 3)

# substance
aggregate(ftps[indicatorid == 90808]$value ~ ftps[indicatorid == 90808]$budget_2021_75, FUN = sum_fun, dp = 2) # substance adm

spine <- merge(spine,
               ftps[indicatorid == 90808, c("areacode", "value")],
               by.x = "LAD21C",
               by.y = "areacode")

setnames(spine, "value", "substance")

spine[, LAD21NM := factor(LAD21NM,
                          levels = spine$LAD21NM[order(spine$budget_2021_75, spine$substance, decreasing = T)])]

ggplot(aes(x = LAD21NM,
           y = substance,
           fill = as.factor(budget_2021_75)),
       data = spine) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Rate per 100,000",
                     expand = c(0, 0),
                     limits = c(0, 150)) +
  scale_x_discrete(name = "") +
  labs(fill = "2021 Family Hub budget",
       title = "Substance misuse admissions 15-24 yrs, per 100,000") +
  scale_fill_discrete(labels = c("Not eligible", "Eligible")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(colour = "black")) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 1]$substance, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 1]$substance, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 1]$substance, probs = 0.75, na.rm = T)), ")"),
           x = 10, y = 100, size = 3) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 0]$substance, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 0]$substance, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 0]$substance, probs = 0.75, na.rm = T)), ")"),
           x = 28, y = 100, size = 3)

# mh
aggregate(ftps[indicatorid == 90812]$value ~ ftps[indicatorid == 90812]$budget_2021_75, FUN = sum_fun, dp = 2) # mh adm

spine <- merge(spine,
               ftps[indicatorid == 90812, c("areacode", "value")],
               by.x = "LAD21C",
               by.y = "areacode")

setnames(spine, "value", "mh")

spine[, LAD21NM := factor(LAD21NM,
                          levels = spine$LAD21NM[order(spine$budget_2021_75, spine$mh, decreasing = T)])]

ggplot(aes(x = LAD21NM,
           y = mh,
           fill = as.factor(budget_2021_75)),
       data = spine) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Rate per 100,000",
                     expand = c(0, 0),
                     limits = c(0, 150)) +
  scale_x_discrete(name = "") +
  labs(fill = "2021 Family Hub budget",
       title = "Mental health conditions admissions < 18 yrs, per 100,000") +
  scale_fill_discrete(labels = c("Not eligible", "Eligible")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(colour = "black")) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 1]$mh, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 1]$mh, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 1]$mh, probs = 0.75, na.rm = T)), ")"),
           x = 10, y = 100, size = 3) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 0]$mh, probs = 0.50, na.rm = T)), " (",
                                  round(quantile(spine[budget_2021_75 == 0]$mh, probs = 0.25, na.rm = T)), ", ",
                                  round(quantile(spine[budget_2021_75 == 0]$mh, probs = 0.75, na.rm = T)), ")"),
           x = 28, y = 100, size = 3)

# sen support
aggregate(spine$sen_support_percent202122 ~ spine$budget_2021_75, FUN = sum_fun, dp = 2)

spine[, LAD21NM := factor(LAD21NM,
                          levels = spine$LAD21NM[order(spine$budget_2021_75, spine$sen_support_percent202122, decreasing = T)])]

ggplot(aes(x = LAD21NM,
           y = sen_support_percent202122,
           fill = as.factor(budget_2021_75)),
       data = spine) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "% children getting SEN support 2020/21",
                     expand = c(0, 0),
                     limits = c(0, 20)) +
  scale_x_discrete(name = "") +
  labs(fill = "2021 Family Hub budget",
       title = "Percentage of children getting SEN support in 2020/21") +
  scale_fill_discrete(labels = c("Not eligible", "Eligible")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(colour = "black")) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 1]$sen_support_percent202122, probs = 0.50)), " (",
                                  round(quantile(spine[budget_2021_75 == 1]$sen_support_percent202122, probs = 0.25)), ", ",
                                  round(quantile(spine[budget_2021_75 == 1]$sen_support_percent202122, probs = 0.75)), ")"),
           x = 10, y = 17, size = 3) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 0]$sen_support_percent202122, probs = 0.50)), " (",
                                  round(quantile(spine[budget_2021_75 == 0]$sen_support_percent202122, probs = 0.25)), ", ",
                                  round(quantile(spine[budget_2021_75 == 0]$sen_support_percent202122, probs = 0.75)), ")"),
           x = 28, y = 17, size = 3)

# sen ehcp
aggregate(spine$ehc_plan_percent202122 ~ spine$budget_2021_75, FUN = sum_fun, dp = 2)

spine[, LAD21NM := factor(LAD21NM,
                          levels = spine$LAD21NM[order(spine$budget_2021_75, spine$ehc_plan_percent202122, decreasing = T)])]

ggplot(aes(x = LAD21NM,
           y = ehc_plan_percent202122,
           fill = as.factor(budget_2021_75)),
       data = spine) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "% children on EHCP 2020/21",
                     expand = c(0, 0),
                     limits = c(0, 10)) +
  scale_x_discrete(name = "") +
  labs(fill = "2021 Family Hub budget",
       title = "Percentage of children on EHCP in 2020/21") +
  scale_fill_discrete(labels = c("Not eligible", "Eligible")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(colour = "black",
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(colour = "black")) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 1]$ehc_plan_percent202122, probs = 0.50)), " (",
                                  round(quantile(spine[budget_2021_75 == 1]$ehc_plan_percent202122, probs = 0.25)), ", ",
                                  round(quantile(spine[budget_2021_75 == 1]$ehc_plan_percent202122, probs = 0.75)), ")"),
           x = 10, y = 7, size = 3) +
  annotate("text", label = paste0("Median (IQR):\n",
                                  round(quantile(spine[budget_2021_75 == 0]$ehc_plan_percent202122, probs = 0.50)), " (",
                                  round(quantile(spine[budget_2021_75 == 0]$ehc_plan_percent202122, probs = 0.25)), ", ",
                                  round(quantile(spine[budget_2021_75 == 0]$ehc_plan_percent202122, probs = 0.75)), ")"),
           x = 28, y = 7, size = 3)

# associaton with FH eligibility for self-harm counter-intuitive
# check correlation with actual deprivation
ftps <- merge(ftps,
              spine[, c("LAD21C", "imd_avg_rank")],
              by.x = "areacode",
              by.y = "LAD21C",
              all.x = T)

# self harm
plot(ftps[indicatorid == 90813]$value, ftps[indicatorid == 90813]$imd_avg_rank)
cor.test(ftps[indicatorid == 90813]$value, ftps[indicatorid == 90813]$imd_avg_rank)
cor.test(ftps[indicatorid == 90813 & value < 500]$value, ftps[indicatorid == 90813 & value < 500]$imd_avg_rank)

# alcohol
plot(ftps[indicatorid == 92904]$value, ftps[indicatorid == 92904]$imd_avg_rank)
cor.test(ftps[indicatorid == 92904]$value, ftps[indicatorid == 92904]$imd_avg_rank)
cor.test(ftps[indicatorid == 92904 & value < 35]$value, ftps[indicatorid == 92904 & value < 35]$imd_avg_rank)

# substance misuse
plot(ftps[indicatorid == 90808]$value, ftps[indicatorid == 90808]$imd_avg_rank)
cor.test(ftps[indicatorid == 90808]$value, ftps[indicatorid == 90808]$imd_avg_rank)

# mh adm
plot(ftps[indicatorid == 90812]$value, ftps[indicatorid == 90812]$imd_avg_rank)
cor.test(ftps[indicatorid == 90812]$value, ftps[indicatorid == 90812]$imd_avg_rank)

