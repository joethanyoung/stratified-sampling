# import libraries
library(readr)
library(dplyr)
library(readxl)
library(writexl)
library(sampling)

# read dataframe
df <- read_excel("L06101基础表保全清单.xlsx", skip = 1)

# glimpse dataset
df %>% glimpse()

# check uniquesness
unique(df$保全类型)

# set sample
sample_size <- c(
  退保 = 4,
  保单贷款 = 4,
  签名变更 = 4,
  联系方式变更 = 3
)

# set seed
set.seed(1234)

# recode & data manipulation
df <- df %>% filter(!is.na(保单保费) & !is.na(保全类型)) %>%
  mutate(保全类型 = recode(
    保全类型,
    '退保' = '退保',
    '保单贷款' = '保单贷款',
    '签名变更' = '签名变更',
    '联系方式变更' = '联系方式变更'
  ))

filtered_df <- df %>% filter()
  mutate(保单保费 = as.numeric(保单保费)) %>%
  filter(保单保费 >= 50000) %>%
  arrange(desc(保单保费))

# Check the size of each stratum
stratum_sizes <- filtered_df %>% 
  group_by(保全类型) %>% 
  summarise(n = n())
print(stratum_sizes)

# stratified
stratified_sample <- strata(
  data = filtered_df,
  stratanames = "保全类型",
  size = sample_size,
  method = "srswor"                 # non-replacement sampling
)

# retrieve results
sampled_indices <- stratified_sample

# retrieve samples
sampled_data <- filtered_df[sampled_indices,]
