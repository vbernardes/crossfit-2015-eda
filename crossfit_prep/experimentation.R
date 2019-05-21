library(data.world)
library(tidyverse)
library(GGally)

athletes <- read.csv('bgadoci-crossfit-data/data/athletes.csv')
leaderboard <- read.csv('bgadoci-crossfit-data/data/leaderboard_15.csv')

#Using an inner join, match on athlete ids that are in both dataframes
cf <- leaderboard %>%
  inner_join(athletes, by="athlete_id")

##########################################
filter(cf, stage == 1 & rank == 1)

str(cf)
###########
# Simple plots to get started

# Age histogram
ggplot(data = cf,
       aes(x = age)) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(xlim = c(15, 54)) +
  scale_x_continuous(breaks = seq(15, 54, 5))

# Male height histogram
ggplot(data = subset(cf, gender == 'Male'),
       aes(x = height)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(limits = c(55, quantile(cf$height, .99, na.rm = T)),
                     breaks = seq(55, 100, 5))

# Male weight histogram
ggplot(data = subset(cf, gender == 'Male'),
       aes(x = weight)) +
  geom_histogram(binwidth = 5) +
  scale_x_continuous(limits = c(100, 300))

# Male Rx score vs. height
ggplot(data = subset(cf, gender == 'Male' & scaled == 'false'),
       aes(x = height, y = score)) +
  geom_jitter(alpha = 1/25) +
  scale_x_continuous(limits = c(55, quantile(cf$height, .99, na.rm = T)))

# Score vs. rank (should be highly correlated)
ggplot(data = cf[cf$stage == 3,],
       aes(x = score, y = rank)) +
  geom_jitter(aes(color = gender))

# what the hell is up with the above plot?

# It's not scale vs. Rx, I've checked it:
# Score vs. rank 2
ggplot(data = cf[cf$stage == 4 & cf$scaled == 'false',],
       aes(x = score, y = rank)) +
  geom_point(aes(color = gender))
# zooming in:
ggplot(data = cf[cf$stage == 4 & cf$scaled == 'false',],
       aes(x = score, y = rank)) +
  geom_jitter(aes(color = gender)) +
  scale_y_continuous(limits = c(70000, 90000))

# Maybe it's age? Let's see:
ggplot(data = male.open15.4,
       aes(x = score, y = rank)) +
  geom_point(aes(color = age))
# Damn. It's not age either.

# Let's try another approach at scaled vs. Rx
ggplot(data = male.open15.4,
       aes(x = score, y = rank)) +
  geom_point(aes(color = scaled))
# GODAMMIT! I had tested it before!

# WHY?!:
ggplot(data = subset(cf, stage == 4 & scaled == 'false' & gender == 'Male'),
       aes(x = score, y = rank)) +
  geom_point(aes(color = gender))

cf <- cf
cf$rx <- cf$scaled == 'false'

ggplot(data = subset(cf, stage == 4 & gender == 'Male' & rx),
       aes(x = score, y = rank)) +
  geom_jitter(aes(color = rx), height = 5000, alpha = 1/10) +
  scale_x_continuous(limits = c(90, 110))


########################################
latin_america <- subset(cf, region == 'Latin America')
length(unique(latin_america$athlete_id))

#######################################
# ggpairs:
#pairs <- ggpairs(cf,
#                 columns = c('stage', 'rank', 'score', 'scaled', 'gender', 'age', 'height', 'weight'))


#######################################
male.open.15.4 <- subset(cf, division == 'Male' & stage == '4')

# Score vs. age:
ggplot(male.open.15.4[male.open.15.4$category == 'Rx',],
       aes(x = age, y = score)) +
  geom_jitter(alpha = 1/10)

# Rank vs. age (first 10000):
ggplot(male.open.15.4[male.open.15.4$category == 'Rx' & male.open.15.4$rank <= 10000,],
       aes(x = age, y = rank)) +
  geom_jitter()

# Score vs. age and let's add affiliate (top 100):
ggplot(male.open.15.4[male.open.15.4$category == 'Rx' & male.open.15.4$rank <= 100,],
       aes(x = age, y = score)) +
  geom_jitter(aes(color = affiliate))

# Score vs. age vs. region (top 1000):
ggplot(male.open.15.4[male.open.15.4$category == 'Rx' & male.open.15.4$rank <= 1000,],
       aes(x = age, y = score)) +
  geom_jitter(aes(color = region)) +
  scale_color_brewer() +
  theme_dark()

########################################
open.15.4.rx <- subset(cf, stage == '4' & category == 'Rx')

# Score vs. age vs. division:
ggplot(open.15.4.rx,
       aes(x = age, y = score)) +
  geom_jitter(aes(color = division), alpha = 1/50) +
  scale_y_continuous(limits = c(0, 170))

# other approach:
# Score vs. age vs. division:
ggplot(open.15.4.rx,
       aes(x = age, y = score)) +
  facet_wrap(~division) +
  geom_jitter(alpha = 1/20, aes(color = age_bucket)) +
  scale_y_continuous(limits = c(0, 170))

### Let's do age buckets:
cf$age_bucket <- cut(cf$age, c(seq(15, 55, 5)))

########################################
# Let's take a look at some body measurements

# Score vs. height (exclude outliers)
ggplot(subset(open.15.4.rx, division == 'Male'),
       aes(x = height, y = score)) +
  geom_jitter(alpha = 1/10) +
  geom_smooth() +
  scale_x_continuous(limits = c(quantile(open.15.4.rx$height, .05, na.rm = T),
                                quantile(open.15.4.rx$height, .98, na.rm = T)))


########################################
# Let's try to see the light at the end of the tunnel with benchmarks
male.open.15.5.rx <- subset(cf,
                            stage == '5' & division == 'Male' & category == 'Rx')

# Score vs. Fran
ggplot(male.open.15.5.rx,
       aes(x = fran, y = score)) +
  geom_jitter(alpha = .8, aes(color = age_bucket)) +
  scale_color_brewer() +
  scale_x_continuous(limits = c(quantile(male.open.15.5.rx$fran, .02, na.rm = T),
                                quantile(male.open.15.5.rx$fran, .98, na.rm = T))) +
  scale_y_continuous(limits = c(0, 1500))

cor.test(male.open.15.5.rx$fran, male.open.15.5.rx$score)

###
male.open.15.1.rx <- subset(cf,
                            stage == '1' & division == 'Male' & category == 'Rx')

# Score vs. snatch and deadlift
ggplot(male.open.15.1.rx,
       aes(x = deadlift, y = score)) +
  geom_jitter(alpha = 1/15) +
  scale_x_continuous(limits = c(quantile(male.open.15.1.rx$deadlift, .02, na.rm = T),
                                quantile(male.open.15.1.rx$deadlift, .98, na.rm = T)))

ggplot(male.open.15.1.rx,
       aes(x = snatch, y = score)) +
  geom_jitter(alpha = 1/15) +
  scale_x_continuous(limits = c(quantile(male.open.15.1.rx$snatch, .02, na.rm = T),
                                quantile(male.open.15.1.rx$snatch, .98, na.rm = T)))

cor.test(male.open.15.1.rx$snatch, male.open.15.1.rx$score)

### Let's subset only people that have a snatch benchmark
male.open.15.1a.rx <- subset(cf,
                            stage == '1.1' &
                              division == 'Male' &
                              category == 'Rx')

ggplot(male.open.15.1a.rx,
       aes(x = snatch, y = score)) +
  geom_jitter(alpha = 1/10) +
  scale_x_continuous(limits = c(quantile(male.open.15.1a.rx$snatch, .01, na.rm = T),
                                quantile(male.open.15.1a.rx$snatch, .99, na.rm = T)))

cor.test(male.open.15.1a.rx$snatch, male.open.15.1a.rx$score)

ggplot(male.open.15.1a.rx,
       aes(x = deadlift, y = score)) +
  geom_jitter(alpha = 1/15) +
  scale_x_continuous(limits = c(quantile(male.open.15.1a.rx$deadlift, .01, na.rm = T),
                                quantile(male.open.15.1a.rx$deadlift, .99, na.rm = T)))

ggplot(male.open.15.1a.rx,
       aes(x = grace, y = score)) +
  geom_jitter(alpha = 1/20) +
  scale_x_continuous(limits = c(quantile(male.open.15.1a.rx$grace, .01, na.rm = T),
                                quantile(male.open.15.1a.rx$grace, .99, na.rm = T)))

cor.test(male.open.15.1a.rx$grace, male.open.15.1a.rx$score)

### Top 10000 for 15.1a:
grace.15.1a <- subset(male.open.15.1a.rx,
                      !is.na(grace) & rank <= 10000)
ggplot(grace.15.1a,
       aes(x = grace, y = score)) +
  geom_jitter(aes(color = age_bucket)) +
  scale_color_brewer() +
  scale_x_continuous(limits = c(quantile(grace.15.1a$grace, .01, na.rm = T),
                                quantile(grace.15.1a$grace, .99, na.rm = T)))

#############
# 15.2:
male.open.15.2.rx <- subset(cf,
                            stage == '2' &
                              division == 'Male' &
                              category == 'Rx' &
                              !is.na(pullups))

ggplot(male.open.15.2.rx,
       aes(x = pullups, y = score)) +
  geom_jitter(alpha = 1/10) +
  scale_x_continuous(limits = c(quantile(male.open.15.2.rx$pullups, .01, na.rm = T),
                                quantile(male.open.15.2.rx$pullups, .99, na.rm = T)))

##############
# General pullups vs. score on 15.2
ggplot(subset(cf, !is.na(pullups) & stage == '2'),
       aes(x = pullups, y = score)) +
  geom_jitter(alpha = 1/10, aes(color = division)) +
  scale_x_continuous(limits = c(quantile(cf$pullups, .01, na.rm = T),
                                quantile(cf$pullups, .99, na.rm = T)))

############################
# Try to get overall rank
overallrank_male <- cf[cf$category == 'Rx' & cf$division == 'Male', ] %>%
  group_by(athlete_id) %>%
  summarise(name = first(name),
            point_total = sum(rank))
overallrank_male$overall_rank <- rank(overallrank_male$point_total)
overallrank_male$overall_rank <- as.integer(overallrank_male$overall_rank)

overallrank_female <- cf[cf$category == 'Rx' & cf$division == 'Female', ] %>%
  group_by(athlete_id) %>%
  summarise(name = first(name),
            point_total = sum(rank))
overallrank_female$overall_rank <- rank(overallrank_female$point_total)
overallrank_female$overall_rank <- as.integer(overallrank_female$overall_rank)

# Join rank on main table
cf_rank <- cf %>%
  left_join(overallrank_male[, c('athlete_id', 'overall_rank')], by='athlete_id')
cf_rank <- cf_rank %>%
  left_join(overallrank_female[, c('athlete_id', 'overall_rank')], by='athlete_id')
cf_rank$overall_rank.x <- ifelse(!is.na(cf_rank$overall_rank.y),
                                 cf_rank$overall_rank.y,
                                 cf_rank$overall_rank.x)
cf_rank$overall_rank.y <- NULL
names(cf_rank)[names(cf_rank) == 'overall_rank.x'] <- 'overall_rank'


###################################
unique_athletes <- cf[!duplicated(cf$athlete_id),]

# let's try to get something vs. overall rank
ggplot(subset(cf, category == 'Rx' & stage == 1),
       aes(x = score, y = overall_rank)) +
  geom_jitter(alpha = 1/20)

###################################
ggplot(male.open.15.2.rx,
       aes(x = age, y = score)) +
  geom_jitter(alpha = 1/10, color = 'orange') +
  geom_line(stat = 'summary', fun.y = mean, color = 'blue')
###################################
ggplot(male.open.15.5.rx,
       aes(x = 5*round(weight/5), y = score)) +
  #scale_color_gradient(low = "red", high = "blue") +
  scale_color_brewer() +
  scale_x_continuous(limits = c(quantile(unique_athletes$weight, .01, na.rm = T),
                                quantile(unique_athletes$weight, .99, na.rm = T))) +
  scale_y_continuous(limits = c(250, 1500)) +
  geom_jitter(alpha = 1/3, aes(color = age_bucket)) +
  geom_line(stat = 'summary', fun.y = mean, color = 'blue')

####### Elite athletes
elite <- subset(cf, rank <= 100 & category == 'Rx')

ggplot(subset(elite, stage == 2),
       aes(x = fran, y = score)) +
  geom_jitter(aes(color = division)) +
  scale_x_continuous(limits = c(100, 300))

######## Top teams
# ???


###### How long

# Freq. table of how_long
sort(table(athletes$howlong), decreasing = T)
ggplot(athletes[athletes$howlong %in% valid_how_long,], aes(x = howlong)) +
  geom_bar() +
  coord_flip()

valid_how_long <- c('1-2 years|', '2-4 years|', '6-12 months|', 'Less than 6 months|', '4+ years|')

unique_ath_temp <- unique_athletes %>%
  left_join(athletes[, c('athlete_id', 'howlong')], by = 'athlete_id')

ggplot(subset(unique_ath_temp, howlong %in% valid_how_long),
       aes(x = howlong, y = overall_rank)) +
  geom_jitter(alpha = 1/25)

cf_howlong <- cf %>%
  left_join(athletes[, c('athlete_id', 'howlong')], by = 'athlete_id')
cf_howlong[!(cf_howlong$howlong %in% valid_how_long), ]$howlong <- NA
cf_howlong$howlong <- factor(cf_howlong$howlong)
levels(cf_howlong$howlong) <- ordered(c('Less than 6 months|', '6-12 months|', '1-2 years|', '2-4 years|', '4+ years|'))

ggplot(subset(cf_howlong,
              category == 'Rx' &
                stage == 3 &
                division == 'Female' &
                !is.na(howlong)) %>%
         group_by(filthy50) %>%
         top_n(-10, score),
       aes(x = filthy50, y = score)) +
  geom_point(alpha = 1, aes(size = weight, color = age)) +
  #scale_y_continuous(limits = c(0, 55000)) +
  #scale_color_brewer(palette = 'Reds') +
  facet_wrap(~stage, scales = 'free') +
  scale_x_continuous(limits = c(50, 1000)) +
  scale_color_gradient(low = 'red', high = 'blue')

top10.15.3.byage <- subset(open.15.3, division == 'Female') %>%
  group_by(age) %>%
  top_n(10, score)

####################################
# Rx vs scaled
ggplot(cf,
       aes(x = score)) +
  geom_histogram(binwidth = 5,
                 aes(fill = category)) +
  facet_wrap(c('stage', 'division'),
             scales = 'free')

####################################
# 15.3: Rx vs scaled
ggplot(subset(cf, stage == 3),
       aes(x = score)) +
  geom_histogram(binwidth = 10, aes(fill = category)) +
  facet_wrap(~division, scales = 'free')

###################################
# Strength vs. results on workouts
cf_strength <- as.data.frame(cf[cf$stage == 1.1, c('score', 'athlete_id')])
names(cf_strength)[ names(cf_strength) == 'score'] <- 'score1.1'

cf_str <- cf
cf_str <- left_join(cf_str, cf_strength, by = 'athlete_id')

ggplot(subset(cf_str, category == 'Rx'),
       aes(x = score1.1, y = score)) +
  geom_jitter(alpha = 1/250, aes(color = division)) +
  geom_smooth() +
  facet_wrap(c('stage', 'division'), scales = 'free') +
  scale_x_continuous(limits = c(quantile(cf_str$score1.1, .05, na.rm = T),
                                quantile(cf_str$score1.1, .98, na.rm = T)))

# General correlation between strength and all workouts
cor.test(cf_str$score1.1, cf_str$score)

str.male.open.15.5.rx <- subset(cf_str, division == 'Male' &
                              stage == 5 &
                              category == 'Rx')
cor.test(str.male.open.15.5.rx$score1.1, str.male.open.15.5.rx$score)
cor.test(str.male.open.15.5.rx$weight, str.male.open.15.5.rx$score)
cor.test(str.male.open.15.5.rx$height, str.male.open.15.5.rx$score)

##### Overall Rank by Scores
ggplot(subset(cf_rank, category == 'Rx'),
       aes(x = score, y = overall_rank)) +
  geom_jitter(alpha = 1/150, aes(color = division)) +
  geom_smooth() +
  facet_wrap(c('division', 'stage'), scales = 'free')

##### Overall Rank by Strength
cf_rank_str <- subset(cf_rank, stage == 1.1 & category == 'Rx' & overall_rank < 75000)
ggplot(cf_rank_str,
       aes(x = score, y = overall_rank)) +
  geom_jitter(alpha = 1/150, aes(color = division)) +
  geom_smooth() +
  facet_wrap(~division)

male_cf_rank_str <- subset(cf_rank_str, division == 'Male')
cor.test(male_cf_rank_str$score, male_cf_rank_str$overall_rank)

####### How long
ggplot(subset(unique_athletes, !is.na(howlong)),
       aes(x=howlong)) +
  geom_bar()



######## 15.1a -> weight
ggplot(data = open.15.1a.rx,
       aes(x = 5*round(weight/5), y = score)) +
  geom_line(stat = 'summary',
            fun.y = mean,
            aes(color = division)) +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(limits = c(quantile(unique_athletes$weight, .01, na.rm = T),
                                quantile(unique_athletes$weight, .99, na.rm = T)))

cor.test(open.15.1a.rx$weight, open.15.1a.rx$score)

########## Age vs. overall rank
ggplot(subset(cf_rank, category == 'Rx'),
       aes(x = age, y = overall_rank)) +
  geom_jitter(alpha = 1/250, aes(color = division)) +
  facet_wrap(~division) +
  geom_smooth() +
  scale_x_continuous(limits = c(quantile(unique_athletes$age, .03, na.rm = T),
                                quantile(unique_athletes$age, .97, na.rm = T)))

######## 15.5 Top 20
open.15.5.rx <- subset(cf, stage == '5' & category == 'Rx')

ggplot(subset(cf, category == 'Rx' & rank <= 100),
       aes(x = age, y = score)) +
  geom_jitter(aes(size = weight, color = division)) +
  facet_wrap(~stage, scales = 'free')

######### Weight vs. deadlift
ggplot(unique_athletes,
       aes(x = weight, y = overall_rank)) +
  geom_point(alpha = 1/2, aes(color = howlong)) +
  scale_color_brewer() +
  scale_x_continuous(limits = c(0, 400)) +
  scale_y_continuous(limits = c(0, 700)) +
  geom_smooth(data = subset(unique_athletes, category == 'Rx'),
              color = 'red') +
  geom_smooth(data = subset(unique_athletes, category == 'Scaled'),
              color = 'blue') +
  facet_wrap(~division, scales = 'free')

cor.test(unique_athletes[unique_athletes$division == 'Male',]$weight,
         unique_athletes[unique_athletes$division == 'Male',]$deadlift)

######### 15.4: weight vs height vs score
ggplot(subset(open.15.4.rx,
              division == 'Male' &
              weight >= 100 &
              weight <= 400),
       aes(x = height, y = score)) +
  geom_jitter(alpha = 1/20, aes(color = weight)) +
  scale_color_gradient(low = 'red', high = 'green') +
  geom_smooth() +
  scale_x_continuous(limits = c(61, 80))

#### Athlete weight vs height
ggplot(subset(unique_athletes,
              category == 'Rx' &
                height >= 60 &
                height <= 80),
       aes(x = weight, y = height)) +
  geom_jitter() +
  geom_smooth() +
  facet_wrap(~division, scales = 'free') +
  scale_x_continuous(limits = c(quantile(unique_athletes$weight, .01, na.rm = T),
                                quantile(unique_athletes$weight, .99, na.rm = T)))

ggplot(subset(unique_athletes,
              category == 'Rx' &
                height >= 60 &
                height <= 80),
       aes(x = weight, y = height)) +
  geom_boxplot() +
  facet_wrap(~division, scales = 'free') +
  scale_x_continuous(limits = c(quantile(unique_athletes$weight, .01, na.rm = T),
                                quantile(unique_athletes$weight, .99, na.rm = T)))


########
ggplot(open.15.4.rx,
       aes(y = height, x = 10*round(score/10))) +
  geom_line(stat = 'summary',
            fun.y = mean,
            aes(color = division)) +
  scale_color_brewer(palette = 'Set1') +
  scale_y_continuous(limits = c(62, 82))
