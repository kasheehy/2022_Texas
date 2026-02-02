# this is the master script for loading, inspecting, and analyzing data collected
# in Texas, 2022. Includes both behavioral (collected by Kirsten Sheehy, Jon
# Aguiñaga, and Nishika Raghavan) and parasite data (collected by Jessica Stephenson et al.).


# Packages to Load --------------------------------------------------------
library(dplyr)
library(readr)
library(tidyr)
library(tibble)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(lme4)
library(modelr)
library(effects)
library(sjPlot)
library(ISLR)
library(likert)
library(lmerTest)

# Load Data ---------------------------------------------------------------
boris_data <- read.csv("data/copy_RAW_Texas_BORISdata_20240515.csv")
parasite_data <- read.csv("data/copy_RAW_parasite_data_20230428.csv")
length_data <- read.csv("data/copy_Fish Length Data.csv")
ID_data <- read.csv(("data/copy_RAW_trial_ID_data_completeonly_20240220.csv"))

# Tidy Data ---------------------------------------------------------------

### PARASITE DATA ----

# rename columns to be consistent across data sets
parasite_data <- parasite_data %>% rename(
  fish.ID = fish.id,
  site.ID = collection.site
)

# change collection.date and dissection.date to a date format (YYYY-MM-DD)
parasite_data$collection.date <- as.Date(parasite_data$collection.date,
  format = "%m/%d/%y"
)
parasite_data$dissection.date <- as.Date(parasite_data$dissection.date,
  format = "%m/%d/%y"
)

# change site names from abbreviation (WES, BR) to full (Weslaco, Brownsville)
parasite_data$site.ID <- gsub("WES", "Weslaco", parasite_data$site.ID)
parasite_data$site.ID <- gsub("BR-OP", "Brownsville", parasite_data$site.ID)

# note: the 'OP' in Brownsville stands for 'overpass'. We explored several sites
# in Brownsville, but only used the ones from the overpass for this study, so I
# simplified the name to just 'Brownsville'.

### LENGTH DATA ----
# rename columns to be consistent across data sets
length_data <- length_data %>% rename(
  file.name = file_name,
  date.image = date_image,
  site.ID = site_ID,
  fish.ID = fish_ID
)

### ID DATA ----

### BORIS DATA ----
# remove unnecessary columns (largely meta data, unused features in BORIS)
boris_data <- boris_data %>% select(
  -Observation.date, # this is just the day processed in BORIS
  -Description,
  -FPS,
  -Behavioral.category,
  -Modifiers,
  -Comment.start,
  -Comment.stop
)

# rename columns (to match up across data)
boris_data <- boris_data %>% rename(
  pool = Subject,
  trial.length = Total.length,
  start = Start..s.,
  stop = Stop..s.,
  duration = Duration..s.
)

# split Observation.id into new columns: pi, date, and observer
boris_data <- boris_data %>% separate_wider_delim(Observation.id,
  delim = "_",
  names = c(
    "site.ID",
    "trial.ID",
    "batch.ID",
    "trial.date"
  ),
  too_many = "drop"
)

# split Media.file into columns to extract file name (could also use
# Observation.id, but figured this would help avoid typos made in Boris)
boris_data <- boris_data %>% separate_wider_delim(Media.file,
  delim = "/",
  names = c(
    "file1",
    "file2",
    "file3",
    "file4",
    "file5",
    "video.ID"
  ),
  too_few = "align_end"
)

# remove the excess filepath columns
boris_data <- boris_data %>% select(
  -file1,
  -file2,
  -file3,
  -file4,
  -file5
)

# change trial.date from (YYYYMMDD) to a date (YYYY-MM-DD)
boris_data$trial.date <- as.Date(boris_data$trial.date, format = "%Y%m%d")

# remove 'trial' from the data entries for trial.ID
boris_data$trial.ID <- gsub("trial", "", boris_data$trial.ID)
boris_data$trial.ID <- gsub("trail", "", boris_data$trial.ID) # just a typo

# remove 'pool' from data in pool column
boris_data$pool <- gsub("Pool ", "", boris_data$pool)

# change site ID from abbreviations to full name
# note: doing them in this order is important
boris_data$site.ID <- gsub("Wes", "Weslaco", boris_data$site.ID)
boris_data$site.ID <- gsub("WES", "Weslaco", boris_data$site.ID)
boris_data$site.ID <- gsub("BR1", "Brownsville", boris_data$site.ID)
boris_data$site.ID <- gsub("BR2", "Brownsville", boris_data$site.ID)
boris_data$site.ID <- gsub("BR", "Brownsville", boris_data$site.ID)
# note: there are three entry types for Brownsville: BR, BR1, and BR2
# need to revisit lab notebook to confirm, but I believe BR1 and BR2
# are the two sides of the garage (i.e. the two cameras)


## Create columns for each 'behavior' (open, hiding, startle)
# start by duplicating the 'behavior' column for start
boris_data <- boris_data %>%
  mutate(behavior.start = Behavior)

boris_data <- boris_data %>%
  mutate(behavior.stop = Behavior)

# then pivot_wider with names from behavior_start and values from start
boris_data_wide <- boris_data %>%
  pivot_wider(
    names_from = behavior.start,
    values_from = start,
    names_prefix = "start_"
  )

boris_data_wide <- boris_data_wide %>%
  pivot_wider(
    names_from = behavior.stop,
    values_from = stop,
    names_prefix = "stop_"
  )

# now, I need to get the duration of each behavior
boris_data_wide <- boris_data_wide %>%
  pivot_wider(
    names_from = Behavior,
    values_from = duration,
    names_prefix = "duration_"
  )

# I'll remove the duration_Startle because these are 'points' not 'states'
# and do not have a duration
boris_data_wide <- boris_data_wide %>% select(-duration_Startle)

# ok. now I need to get this into a tidy format...
# I could just sum all the hiding/open data to get the total amount of
# time they spend doing each behavior in a trial, but they I lose info about how
# much they switch. I need to figure out how to get each start and stop
# (within a trial) into its own column...
#
# For now, I'm just going to work with durations and forget about switching.

# I need a column in both ID_data and boris_data_wide to join by
# I'll create a new column that merges the file name (which already includes
# site, trial, and batch) with pool # for both data sets

boris_data_wide$merge.ID <- paste(
  boris_data_wide$video.ID,
  boris_data_wide$pool
)
ID_data$merge.ID <- paste(
  ID_data$video.ID,
  ID_data$pool
)

boris_data_merge <- boris_data_wide %>%
  left_join(ID_data, by = "merge.ID")

# remove duplicate columns
boris_data_merge <- boris_data_merge %>% select(
  -merge.ID,
  -video.ID.y,
  -pool.y,
  -site.ID.y,
  -trial.ID.y,
  -batch.ID.y,
  -trial.date.y
)
# add column for species from fish.ID
boris_data_merge <- boris_data_merge %>%
  mutate(species = fish.ID)
boris_data_merge <- boris_data_merge %>%
  separate_wider_delim(species,
    delim = "-",
    names = c(
      "species",
      "junk.num"
    )
  )
boris_data_merge <- boris_data_merge %>% select(-junk.num)

# now, I'm going to create some columns for summary data (e.g. total duration
# hiding per fish per trial)

# poking around...
# creating a simpler dataframe

# time hiding per trial
total_hiding <- boris_data_merge %>%
  aggregate(
    duration_Hiding ~ fish.ID + trial.ID.x,
    sum
  )

total_hiding <- total_hiding %>%
  mutate(duplicate.fish.ID = fish.ID)

total_hiding <- total_hiding %>%
  separate_wider_delim(duplicate.fish.ID,
    delim = "-",
    names = c(
      "species",
      "junk.num"
    )
  )
total_hiding <- total_hiding %>%
  select(-junk.num)

# time in open per trial
total_open <- boris_data_merge %>%
  aggregate(
    duration_Open ~ fish.ID + trial.ID.x,
    sum
  )

total_open <- total_open %>%
  mutate(duplicate.fish.ID = fish.ID)

total_open <- total_open %>%
  separate_wider_delim(duplicate.fish.ID,
    delim = "-",
    names = c(
      "species",
      "junk.num"
    )
  )
total_open <- total_open %>%
  select(-junk.num)

# now, I want to merge total hiding and open per fish per trial
# I'm going to create a column with both fish ID and trial to create a unique
# row for each fish/trial combination. I'll then use this to join the hiding
# and open datasets
total_open <- total_open %>%
  unite(fish.ID_trial, c(fish.ID, trial.ID.x))

total_hiding <- total_hiding %>%
  unite(fish.ID_trial, c(fish.ID, trial.ID.x))

# merge
total_hide_open <- total_hiding %>%
  left_join(total_open, by = "fish.ID_trial")

# get rid of duplicate columns
total_hide_open <- total_hide_open %>%
  select(-species.y)

# separate fish ID and trial
total_hide_open <- total_hide_open %>%
  separate_wider_delim(fish.ID_trial,
    delim = "_",
    names = c(
      "fish.ID",
      "trial"
    )
  )
# and for my own sanity, renaming the species column
total_hide_open <- total_hide_open %>%
  rename(species = species.x)

# time hiding and open by trial
total_hide_open <- total_hide_open %>%
  mutate(site.ID = boris_data_merge$site.ID.x[match(fish.ID, boris_data_merge$fish.ID)])

# hiding and open by trial, joined with parasite and size data
total_hide_open <- total_hide_open %>%
  left_join(parasite_data, by = "fish.ID")

total_hide_open <- total_hide_open %>%
  left_join(length_data, by = "fish.ID")

total_hide_open <- total_hide_open %>%
  select(
    -site.ID.y,
    -species.y,
    -site.ID,
    -species
  )

# rename columns
total_hide_open <- total_hide_open %>% rename(
  species = species.x,
  site.ID = site.ID.x,
  total.parasites = totalpara
)

# Inspect Data ----

### fish.IDs ----
unique(ID_data$fish.ID) # 69 unique IDs
unique(boris_data_merge$fish.ID) # 65 unique IDs
unique(parasite_data$fish.ID)
# 121, but 2 are just 'P.formosa' and 'P.latipina' because they didn't receive trials(?)

### video.ID ----
unique(boris_data_wide$video.ID) # 31
unique(ID_data$video.ID) # 36

### length data ----
length_hist <- total_hide_open %>%
  ggplot(mapping = aes(total_length..mm.)) +
  geom_histogram()

### boris_data, distributions ----
hiding_hist <- total_hide_open %>%
  ggplot(mapping = aes(duration_Hiding)) +
  geom_histogram()

open_hist <- total_hide_open %>%
  ggplot(mapping = aes(duration_Open)) +
  geom_histogram()

# trying to plot proportion of time hiding vs open
proportion_behavior <- total_hide_open %>%
  group_by(fish.ID, trial.ID) %>%
  mutate(percent_open = duration_Open / sum(duration_Hiding + duration_Open) * 100)

### parasite data ----
parasite_hist <- total_hide_open %>%
  ggplot(mapping = aes(totalpara)) +
  geom_histogram()

sp_parasite_hist <- total_hide_open %>%
  ggplot(mapping = aes(
    fill = species,
    x = site.ID,
    y = total.parasites
  )) +
  geom_bar(position = "stack", stat = "identity")

### length data ----
length_hist <- total_hide_open %>%
  ggplot(mapping = aes(total_length..mm.)) +
  geom_histogram()

# diff in hiding between species
species_hiding <- total_hide_open %>%
  ggplot(mapping = aes(
    x = species,
    y = duration_Hiding,
    fill = species
  )) +
  geom_boxplot()

# diff in open between species
species_open <- total_hide_open %>%
  ggplot(mapping = aes(
    x = species,
    y = duration_Open,
    fill = species
  )) +
  geom_boxplot()

# change in hiding over trials by species
trial_hiding <- total_hide_open %>%
  ggplot(mapping = aes(
    x = trial.ID,
    y = duration_Hiding,
    fill = species,
    dodge = species
  )) +
  geom_boxplot()

# change in hiding over trials by species
trial_open <- total_hide_open %>%
  ggplot(mapping = aes(
    x = trial.ID,
    y = duration_Open,
    fill = species,
    dodge = species
  )) +
  geom_boxplot()

# diff in open between sites
site_open <- total_hide_open %>%
  ggplot(mapping = aes(
    x = site.ID.x,
    y = duration_Open
  )) +
  geom_boxplot()

# diff in hiding between sites
site_hiding <- total_hide_open %>%
  ggplot(mapping = aes(
    x = site.ID.x,
    y = duration_Hiding
  )) +
  geom_boxplot()
# diff in opn between sites by species
site_spp_open <- total_hide_open %>%
  ggplot(mapping = aes(
    x = site.ID.x,
    y = duration_Open,
    fill = species.x,
    dodge = species.x
  )) +
  geom_boxplot()

# diff in hiding between sites by species
site_spp_hiding <- total_hide_open %>%
  ggplot(mapping = aes(
    x = site.ID.x,
    y = duration_Hiding,
    fill = species.x,
    dodge = species.x
  )) +
  geom_boxplot()

# diff in total parasites by species
parasites_spp <- total_hide_open %>%
  ggplot(mapping = aes(
    x = species.x,
    y = totalpara,
    fill = species.x
  )) +
  geom_boxplot()

# diff in total parasites by site
parasites_site <- total_hide_open %>%
  ggplot(mapping = aes(
    x = site.ID.x,
    y = totalpara
  )) +
  geom_boxplot()

# diff in total parasites by site and spp
parasites_site_spp <- total_hide_open %>%
  ggplot(mapping = aes(
    x = site.ID.x,
    y = totalpara,
    fill = species.x,
    dodge = species.x
  )) +
  geom_boxplot()

# variation in parasites with open beh
parasites_open <- total_hide_open %>%
  ggplot(mapping = aes(
    x = totalpara,
    y = duration_Open,
    color = species.x
  )) +
  geom_point()

# variation in parasites with hiding beh
parasites_hiding <- total_hide_open %>%
  ggplot(mapping = aes(
    x = totalpara,
    y = duration_Hiding,
    fill = species.x
  )) +
  geom_smooth()

# avg length at each site by spp.
length_site <- total_hide_open %>%
  ggplot(mapping = aes(
    x = site.ID.x,
    y = total_length..mm.,
    fill = species.x,
    dodge = species.x
  )) +
  geom_boxplot()

# # box plot of all hiding values by site
# all_hiding_site <- boris_data_wide %>%
#   ggplot(mapping = aes(x = site.ID,
#                        y = duration_Hiding)) +
#   geom_boxplot()
#
# # box plot of all hiding values by site
# all_hiding_site <- boris_data_wide %>%
#   ggplot(mapping = aes(x = site.ID,
#                        y = duration_Hiding)) +
#   geom_boxplot()
#
# # box plot of all hiding values by species
# all_hiding_spp <- boris_data_merge %>%
#   ggplot(mapping = aes(x = species,
#                        y = duration_Hiding)) +
#   geom_boxplot()
#
# # box plot of all open values by species
# all_open_spp <- boris_data_merge %>%
#   ggplot(mapping = aes(x = species,
#                        y = duration_Open)) +
#   geom_boxplot()
# # box and whisker plot of duration hiding between sites
# mean_hiding_site <- boris_data_wide %>%
#   ggplot(mapping = aes(x = site.ID,
#                        y = duration_Hiding)) +
#   geom_boxplot()
#
# # box and whisker plot of duration hiding between sites
# mean_hiding_trial <- boris_data_wide %>%
#   ggplot(mapping = aes(x = trial.ID,
#                        y = duration_Hiding)) +
#   geom_boxplot()
#
# # looks like lots of zeros, I think this is because of the way the data
# # is, with lots of 'missing' values since it's not tidy
# # in the mean time, I'm just going to remove the very very low values?
# mean_hiding_site <- boris_data_wide %>%
#   filter(duration_Hiding > 1) %>%
#   ggplot(mapping = aes(x = site.ID,
#                        y = duration_Hiding)) +
#   geom_boxplot()
# # hmm, still looks super weird! gonn go back and try to tidy the data

# Models ----

# full model
mod1 <- lmer(duration_Hiding ~ total.parasites * species * trial.ID + (1 | fish.ID),
  data = total_hide_open
)
hist(resid(mod1))
plot(mod1)

# decompose to two way
mod2 <- lmer(duration_Hiding ~ total.parasites:species + total.parasites:trial.ID + species:trial.ID + (1 | fish.ID),
  data = total_hide_open
)
hist(resid(mod2))
plot(mod2)
anova(mod1, mod2)

# I think I'm done, but here are the other models I would have run
mod3 <- lmer(duration_Hiding ~ total.parasites:trial.ID + species:trial.ID + (1 | fish.ID),
  data = total_hide_open
)
anova(mod2, mod3)

mod4 <- lmer(duration_Hiding ~ total.parasites:species + species:trial.ID + (1 | fish.ID),
  data = total_hide_open
)
anova(mod2, mod4)

mod5 <- lm(duration_Hiding ~ total.parasites:species + total.parasites:trial.ID,
  data = total_hide_open
)
anova(mod2, mod5)

mod6 <- lmer(duration_Hiding ~ total.parasites + species + trial.ID + (1 | fish.ID),
  data = total_hide_open
)
anova(mod2, mod6)

# plot mod 2
mod2.updated <- update(mod2, na.action = na.exclude)
total_hide_open.x <- total_hide_open %>%
  mutate(
    fit.m = predict(mod2.updated, re.form = NA),
    fit.c = predict(mod2.updated, re.form = NULL),
    resid2 = resid(mod2.updated)
  )

total_hide_open.x %>%
  ggplot(aes(
    x = total.parasites,
    y = duration_Hiding,
    fill = species,
    dodge = species
  )) +
  geom_point(aes(
    col = species,
    alpha = 0.5
  )) +
  stat_smooth(aes(col = species))

# Plotting the interactions using ggplot2
# Parasite:species
total_hide_open %>%
  ggplot(aes(
    x = total.parasites,
    y = duration_Hiding,
    col = species
  )) +
  geom_point() +
  stat_smooth(method = "lm", se = T) +
  labs(
    title = "Hiding in response to parasites",
    x = "Total parasites",
    y = "Duration spent hiding"
  ) +
  theme_classic()

# duration_Hiding:site.ID
total_hide_open %>%
  ggplot(aes(
    x = site.ID,
    y = total.parasites,
    col = species
  )) +
  geom_jitter(aes(alpha = 0.5)) +
  geom_boxplot() +
  theme_classic()

# Plotting the interactions using ggplot2
ggplot(total_hide_open, aes(
  x = total.parasites,
  y = duration_Hiding,
  color = species
)) +
  geom_line(aes(group = species), size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Interaction Plot of mod2",
    x = "Total parasites",
    y = "Duration spent hiding"
  ) +
  theme_minimal()

## trying with just Brownsville
total_hide_open.b <- subset(total_hide_open, site.ID == "Brownsville")

mod1.b <- lmer(duration_Hiding ~ total.parasites * species * trial.ID + (1 | fish.ID),
  data = total_hide_open.b
)
hist(resid(mod1.b))
plot(mod1.b)

mod2.b <- lmer(duration_Hiding ~ total.parasites:species + total.parasites:trial.ID + species:trial.ID + (1 | fish.ID),
  data = total_hide_open.b
)
hist(resid(mod2.b))
plot(mod2.b)
anova(mod1.b, mod2.b)

mod3.b <- lmer(duration_Hiding ~ totalpara:trial.ID.x.x + species.x:trial.ID.x.x + (1 | fish.ID),
  data = brownsville_hide_open
)
anova(mod2.b, mod3.b)

mod4.b <- lmer(duration_Hiding ~ totalpara:species.x + species.x:trial.ID.x.x + (1 | fish.ID),
  data = brownsville_hide_open
)
anova(mod2.b, mod4.b)

mod5.b <- lmer(duration_Hiding ~ totalpara:species.x + totalpara:trial.ID.x.x + (1 | fish.ID),
  data = brownsville_hide_open
)
anova(mod2.b, mod5.b)

mod6.b <- lmer(duration_Hiding ~ totalpara + species.x + trial.ID.x.x + (1 | fish.ID),
  data = brownsville_hide_open
)
anova(mod2.b, mod6.b)

# plot mod2.b
brownsville_hide_open %>%
  ggplot(aes(
    x = totalpara,
    y = duration_Hiding,
    col = species.x
  )) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

## Models (post-discussion with Kate) ----

### Parasites by Sp ----

# was getting errors because of NA values in total.parasites, creating new dataset
# with na.omit
total_hide_open_na.omit <- total_hide_open %>%
  drop_na(total.parasites, total_length..mm.)
# NOTE there seem to be a lot of missing data for the lengths. Will need to go
# through and re-check Tommy's work. Proceeding with smaller dataset for now...

mod1 <- lm(total.parasites ~ species * site.ID + total_length..mm.,
  data = total_hide_open_na.omit
)
summary(mod1)

mod2 <- lm(total.parasites ~ species + site.ID,
  data = total_hide_open_na.omit
)
summary(mod2)

mod3 <- lm(total.parasites ~ site.ID,
  data = total_hide_open_na.omit
)
summary(mod3)

anova(mod1, mod2)
anova(mod2, mod3)

### Parasites and Beh ----

mod1.beh <- lmer(duration_Hiding ~ total.parasites * species * site.ID + (1 | fish.ID),
  data = total_hide_open_na.omit
)
summary(mod1.beh)

mod2.beh <- lmer(
  duration_Hiding ~ total.parasites * species +
    total.parasites * site.ID + species * site.ID + (1 | fish.ID),
  data = total_hide_open_na.omit
)
summary(mod2.beh)

# now I'm subsetting with data just from Brownsville

total_hide_open_na.omit_BV <- total_hide_open_na.omit %>%
  filter(site.ID == "Brownsville")

# oops, this model gives us an error because there is only one species (P. latipinna)
# in this filtered dataset. There ARE Amazons from Brownsville, but I'm guessing
# the length data is what is messing this up.
mod1.beh.BV <- lmer(duration_Hiding ~ total.parasites * species + (1 | fish.ID),
  data = total_hide_open_na.omit_BV
)
summary(mod1.beh.BV)

mod2.beh.BV <- lmer(duration_Hiding ~ total.parasites + species + (1 | fish.ID),
  data = total_hide_open_na.omit_BV
)
summary(mod2.beh.BV)

# now I'm subsetting with data just from Weslaco

total_hide_open_na.omit_WL <- total_hide_open_na.omit %>%
  filter(site.ID == "Weslaco")

mod1.beh.WL <- lmer(duration_Hiding ~ total.parasites * species + (1 | fish.ID),
  data = total_hide_open_na.omit_WL
)
summary(mod1.beh.WL)

mod2.beh.WL <- lmer(duration_Hiding ~ total.parasites + species + (1 | fish.ID),
  data = total_hide_open_na.omit_WL
)
summary(mod2.beh.WL)

mod3.beh.WL <- lmer(duration_Hiding ~ total.parasites + (1 | fish.ID),
  data = total_hide_open_na.omit_WL
)
summary(mod3.beh.WL)

mod4.beh.WL <- lmer(duration_Hiding ~ species + (1 | fish.ID),
  data = total_hide_open_na.omit_WL
)
summary(mod4.beh.WL)

# FIX THIS ----
# notes on things to fix:

## Fish Length ----
# Looks like there are a few errors in the fish length dataset. Many fish who went
# through behavioral trials don't have length data. It's possible that they were never
# photographed, but it's also possible that Tommy never got to these. Will need to check.
#
# Also, some fish have two different lengths recorded (e.g. PL13).

## Boris Data ----
# I think that the biggest concern here is being sure that Boris and Nishika managed
# to end the trials at a consistent time. I should probably figure out a way to truncate
# the data in R, so that I don't have to do it in Boris (I doubt there's an easy way to do
# that there).
# I would also (one day) like to look at the how often fish are switching between
# hiding and open. I need to figure out how to get all of that into a tidy format...
