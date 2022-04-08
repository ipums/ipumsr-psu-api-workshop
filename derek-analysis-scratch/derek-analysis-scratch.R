library(ipumsr)
library(tidyverse)
library(santoku)

dans_initial_extract_def <- define_extract_from_json(
  "derek-analysis-scratch/api_ex_1.json",
  "usa"
)

dans_initial_extract_def <- define_extract_from_json("api_ex.json", "usa")

revised_extract <- dans_initial_extract_def %>% 
  revise_extract_micro(
    vars_to_add = c(
      "RACE", "EDUC", "MIGRATE1", "HISPAN", "CPI99", "COSTELEC", "CINETHH", 
      "MOMLOC", "POPLOC", "MARRINYR", "DIVINYR", "WIDINYR", "FERTYR", 
      "SPEAKENG", "HHINCOME"
    )
  )

submitted_revised_extract <- revised_extract %>% 
  submit_extract() %>% 
  wait_for_extract()

ddi_path <- download_extract(
  submitted_revised_extract, 
  download_dir = "derek-analysis-scratch"
)

data <- read_ipums_micro(ddi_path)

# Outcome variables are: COSTELEC, CINETHH, MOMLOC/POPLOC, MARRINYR, 
# DIVINYR, WIDINYR, FERTYR, EMPSTAT, MIGRATE1


# Prep outcome vars -------------------------------------------------------

# > COSTELEC ----
ipums_val_labels(ddi, COSTELEC)

data %>% 
  filter(PERNUM == 1) %>% 
  group_by(YEAR) %>% 
  summarize(elec_special_code = mean(COSTELEC >= 9993))

# Good, only ~3% electricity special code, 

data %>% 
  filter(PERNUM == 1) %>% 
  filter(COSTELEC >= 9993) %>% 
  mutate(COSTELEC = as_factor(COSTELEC, levels = "both")) %>% 
  count(YEAR, COSTELEC) %>% 
  ggplot(aes(x = YEAR, y = n, color = COSTELEC)) + 
    geom_line()

# We do see a jump in 9993, No charge or no electricity used, but in absolute 
# terms this is a very small number of households I think (yes, 250 households 
# out of 12,000 at the peak in 2018)

# What about the distribution without special values
data %>% 
  filter(PERNUM == 1 & COSTELEC < 9993) %>% 
  mutate(costelec_grouped = chop_evenly(COSTELEC, 5)) %>% 
  ggplot(aes(x = costelec_grouped)) + 
    geom_bar() + 
    facet_wrap(~YEAR)

data %>% 
  filter(PERNUM == 1 & COSTELEC < 9993) %>% 
  mutate(
    COSTELEC99 = COSTELEC * CPI99,
    costelec_grouped = 
      chop_equally(COSTELEC99, groups = 5, labels = lbl_intervals())
  ) %>% 
  ggplot(aes(x = costelec_grouped)) + 
    geom_bar() + 
    facet_wrap(~YEAR)

data %>% 
  filter(PERNUM == 1 & COSTELEC < 9993) %>% 
  mutate(COSTELEC99 = COSTELEC * CPI99, YEAR = as.ordered(YEAR)) %>% 
  filter(COSTELEC99 < 2500) %>% 
  ggplot(aes(x = YEAR, y = COSTELEC99, weight = HHWT)) + 
    geom_boxplot()

# This preceding plot is probably the most even-handed way to look at the trend 
# in electricity costs over time. It shows that the distribution of costs went 
# down over time, and does not show a particular shock in 2017 or 2018.



# > CINETHH ----
ipums_val_labels(data$CINETHH)

data %>% 
  count(CINETHH)

data %>% 
  filter(PERNUM == 1 & CINETHH > 0) %>% 
  mutate(CINETHH = CINETHH %>% lbl_clean() %>% as_factor()) %>% 
  add_count(YEAR, wt = HHWT, name = "year_total") %>% 
  group_by(YEAR, CINETHH) %>% 
  summarize(pct = 100 * sum(HHWT) / year_total[[1]], .groups = "drop") %>% 
  ggplot(aes(x = YEAR, y = pct, fill = CINETHH)) + 
    geom_col()

# Plot shows steadily increasing access to Internet service


# > MOMLOC/POPLOC ----
data %>% 
  filter(AGE <= 10) %>% 
  mutate(at_least_one_parent = MOMLOC > 0 | POPLOC > 0) %>% 
  group_by(YEAR) %>% 
  summarize(
    pct = 100 * sum(PERWT[at_least_one_parent]) / sum(PERWT)
  ) %>% 
  ggplot(aes(x = YEAR, y = pct)) +
    geom_line()

# You actually do see a drop in 2017, but it's only a little over 1%, and could 
# easily be due to sampling error

data %>% 
  filter(AGE <= 10) %>% 
  mutate(both_parents = MOMLOC > 0 & POPLOC > 0) %>% 
  group_by(YEAR) %>% 
  summarize(pct = 100 * sum(PERWT[both_parents]) / sum(PERWT)) %>% 
  ggplot(aes(x = YEAR, y = pct)) +
  geom_line()

# For both parents, the trend is very different, a drop in 2016, then increasing 
# through 2018, then back down -- probably just sampling variability?


# > MARRINYR ----
ipums_val_labels(data$MARRINYR)
data %>% 
  filter(MARRINYR != 0) %>% 
  group_by(YEAR) %>% 
  summarize(pct = 100 * sum(PERWT[MARRINYR == 2]) / sum(PERWT)) %>% 
  ggplot(aes(x = YEAR, y = pct)) + 
  geom_line()
  

# > DIVINYR ----
ipums_val_labels(data$DIVINYR)
data %>% 
  filter(!DIVINYR %in% c(0, 8)) %>% 
  group_by(YEAR) %>% 
  summarize(pct = 100 * sum(PERWT[DIVINYR == 2]) / sum(PERWT)) %>% 
  ggplot(aes(x = YEAR, y = pct)) + 
  geom_line()


# > WIDINYR ----
ipums_val_labels(data$WIDINYR)
data %>% 
  filter(WIDINYR != 0) %>% 
  group_by(YEAR) %>% 
  summarize(pct = 100 * sum(PERWT[WIDINYR == 2]) / sum(PERWT)) %>% 
  ggplot(aes(x = YEAR, y = pct)) + 
  geom_line()


# > FERTYR ----
ipums_val_labels(data$FERTYR)
data %>% 
  filter(!FERTYR %in% c(0, 8)) %>% 
  group_by(YEAR) %>% 
  summarize(pct = 100 * sum(PERWT[FERTYR == 2]) / sum(PERWT)) %>% 
  ggplot(aes(x = YEAR, y = pct)) + 
  geom_line()


# > EMPSTAT ----
ipums_val_labels(data$EMPSTAT)

data %>% 
  count(EMPSTAT)

data %>% 
  filter(EMPSTAT != 0) %>% 
  mutate(EMPSTAT = EMPSTAT %>% lbl_clean() %>% as_factor()) %>% 
  add_count(YEAR, wt = PERWT, name = "year_total") %>% 
  group_by(YEAR, EMPSTAT) %>% 
  summarize(pct = 100 * sum(PERWT) / year_total[[1]], .groups = "drop") %>% 
  ggplot(aes(x = YEAR, y = pct, fill = EMPSTAT)) + 
  geom_col()


# > MIGRATE1 ----
ipums_val_labels(data$MIGRATE1)

data %>% 
  filter(!MIGRATE1 %in% c(0, 9)) %>% 
  add_count(YEAR, wt = PERWT, name = "year_total") %>% 
  filter(MIGRATE1 != 1) %>% 
  mutate(MIGRATE1 = MIGRATE1 %>% lbl_clean() %>% as_factor()) %>% 
  group_by(YEAR, MIGRATE1) %>% 
  summarize(pct = 100 * sum(PERWT) / year_total[[1]], .groups = "drop") %>% 
  ggplot(aes(x = YEAR, y = pct, fill = MIGRATE1)) + 
  geom_col()

# The graph above combines two things: total percent not in the same house as 
# last year, and within those, the shares that moved within state, across state, 
# and from abroad