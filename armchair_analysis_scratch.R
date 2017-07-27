rm(list = ls())

# load libraries
library(caret)
library(caretEnsemble)
library(doParallel)
library(dplyr)

# find the RData file
load("~/Downloads/Armchair Data_2016.RData")

# import list items as data.frames
for(i in names(imports)) {
  nam <- imports[[i]]
  assign(tolower(i), nam)
  rm(nam)
}

#### team table ####
team.aug <- team %>%
  mutate(off_plays = ra + pa,
         total_snps = snpo + snpd,
         pts_h1 = q1p + q2p,
         pts_h2 = q3p + q4p,
         total_yards = ry + py,
         total_td = tdr + tdp) %>%
  left_join(select(game, gid, seas, wk)) %>%
  group_by(tname, seas) %>%
  arrange(tname, seas, wk) %>%
  mutate_at(vars(c(-tid, -gid, -wk)), 
            funs(cumsum = cumsum, 
                 cummean = cummean))

# get all tid's associated with each gid for joins
tid.list <- team %>%
  group_by(gid) %>%
  summarise(tid.min = min(tid),
            tid.max = max(tid))

# join team to game, then add tid.list
team.game <- left_join(x = team.aug, y = game) %>%
  left_join(tid.list) %>%
  mutate(home_bool = tname == h,
         playoff_bool = wk > 17,
         # dome_bool = cond %in% c("Dome", "Closed Roof", "Covered Roof") | stad %in% c("University of Phoenix Stadium", "Texas Stadium", ),
         turf_bool = surf != "Grass")

# get opponent tid for each row
team.game$tid_opp <- ifelse(team.game$tid == team.game$tid.min, 
                            team.game$tid.max, 
                            team.game$tid.min)

# engineer some features
team.game.aug <- team.game %>%
  left_join(team.aug, by = c("gid" = "gid", "tid_opp" = "tid"), suffix = c("", "_allowed")) %>%
  select(-ends_with("cumsum_allowed"), -ends_with("cummean_allowed"), -ends_with("seas_allowed"), -ends_with("wk_allowed"), -tid.min, -tid.max) %>%
  mutate(team_pair_id = paste0("(", tm),
         opp = tname_allowed, tname_allowed = NULL,
         win_bool = pts > pts_allowed,
         tie_bool = pts == pts_allowed,
         margin = pts - pts_allowed) %>%
  mutate_at(vars(ends_with("allowed")),
            funs(cumsum = cumsum,
                 cummean = cummean)) %>%
  mutate_all(funs(lag1 = lag))

#### WEEK 1 ####
# I want to stitch together the relevant week 1 known data (location, dow, etc.) and the relevant season-ending data from the season before, for both teams.
# If they played each other last year, that'd be good to know, too!

# calculate regular season totals from previous season (max's of cumsums and cummeans)
reg.season.totals <- team.game.aug %>%
  select(-ends_with("lag1")) %>%
  filter(!playoff_bool) %>%
  group_by(tname, seas) %>%
  summarise_at(vars(ends_with("cumsum"), ends_with("cummean"), ends_with("cumsum_allowed"), ends_with("cummean_allowed")),
               funs(max = max)) %>%
  mutate(next_seas = seas + 1)

# calculate variability across games from previous season (sd of non-cumsum/cummean)
reg.season.sd <- team.game.aug %>%
  filter(!playoff_bool) %>%
  select(tid:seas) %>%
  group_by(tname, seas) %>%
  summarise_at(vars(pts:total_td),
               funs(var = var,
                    stdev = sd)) %>%
  mutate(next_seas = seas + 1)

# stitch it all together!
week1.df <- team.game.aug %>%
  filter(wk == 1) %>%
  select(tid, gid, seas, wk, tname, opp, home_bool, turf_bool, win_bool, tie_bool, pts, pts_allowed, margin) %>%
  # add previous season totals and variability for team
  left_join(reg.season.totals, by = c("tname" = "tname", "seas" = "next_seas")) %>%
  left_join(reg.season.sd, by = c("tname" = "tname", "seas" = "next_seas")) %>%
  # add previous season totals for opponent
  left_join(reg.season.totals, by = c("opp" = "tname", "seas" = "next_seas"), suffix = c("", "_opp")) %>%
  left_join(reg.season.sd, by = c("opp" = "tname", "seas" = "next_seas"), suffix = c("", "_opp")) %>%
  # clear out NAs
  na.omit()

week1.inputs <- week1.df %>%
  ungroup() %>%
  select(-tid, -gid, -seas, -wk, -tname, -opp, -seas_opp, -seas.y, -pts, -pts_allowed, -margin, -win_bool, -tie_bool)

nzv <- nearZeroVar(week1.inputs)
week1.nzv <- week1.inputs[-nzv]

cormat <- cor(week1.nzv)
highcorr <- findCorrelation(cormat, .9)
week1.nohc <- week1.nzv[-highcorr]

preProc <- preProcess(week1.nohc, method = c("center", "scale"))
week1.preProc <- predict(preProc, week1.nohc)

week1.model.data.margin <- data.frame(margin = week1.df$margin, week1.preProc)
week1.model.data.win <- data.frame(win = factor(week1.df$win_bool, labels = c("not_win", "win")), week1.preProc)

### MODELS ###

week1.split <- createDataPartition(week1.model.data.win$win, p = 0.8, list = FALSE)
week1.train <- week1.model.data.win[week1.split, ]
week1.test <- week1.model.data.win[-week1.split, ]

week1.rpart <- rpart(win ~ ., data = week1.train)
week1.earth <- earth(win ~ .*., data = week1.train, nk = 10, glm = list(family = binomial), nfold = 3, ncross = 100)
week1.ranger <- ranger(win ~ ., data = week1.train, num.trees = 100000, mtry = 1,
                       write.forest = TRUE, importance = "impurity", classification = TRUE)
week1.cvglmnet <- cv.glmnet(x = as.matrix(select(week1.train, -win)), y = week1.train$win, family = "binomial")

summary(week1.rpart)

cf <- as.data.frame(as.matrix(coef(week1.cvglmnet, s = "lambda.min")))
row.names(cf)[cf != 0]

week1pred.ranger <- predict(week1.ranger, dat = week1.test)
confusionMatrix(week1pred.ranger$predictions, week1.test$win)

week1pred.cvglmnet <- predict(week1.cvglmnet, newx = as.matrix(select(week1.test, -win)), type = "class")
confusionMatrix(week1pred.cvglmnet, week1.test$win)

#### SCRATCH ####

# # get season totals; useful for previous season comparisons
# team.game.seas <- team.game.agg2 %>%
#   filter(!playoff_bool) %>%
#   group_by(tname, seas) %>%
#   summarise_at(vars(pts:snpd, off_plays:total_td), funs(seas_sum = sum,
#                                                         seas_mean = mean)) %>%
#   left_join(select(team.game.agg, tname, seas, playoff_bool) %>% group_by(tname, seas) %>% summarise(playoff_wins = sum(playoff_bool)))

# bye_weeks
# bye.weeks <- team.game.agg %>%
#   group_by(tname, seas) %>%
#   summarise(bye_week = min(wk[wk > game_num], na.rm = TRUE))

# ### WEEK 1 ####
# # I want to tie aggregated previous year performance for both home and away teams to game 1 outcomes, and predict several things: points scored, points allowed, and win probability
# 
# week1.raw <- team.game.agg %>%
#   filter(wk == 1) %>%
#   mutate(prev_seas = seas - 1) %>%
#   left_join(team.game.seas, by = c("tname" = "tname", "prev_seas" = "seas"))
# names(week1.raw) <- gsub("seas_", "prev_seas_", names(week1.raw))
# week1.raw <- rename(week1.raw, prev_seas_playoff_wins = playoff_wins)
# 
# week1.opptm <- week1.raw %>%
#   left_join(week1.raw, by = c("gid" = "gid", "tid" = "tid_opp"), suffix = c("", "_opp")) %>%
#   filter(seas > 2000) %>%
#   na.omit()
#   
# week1.inputs <- week1.opptm %>%
#   select(home_bool, turf_bool, contains("_prev_"))
# 
# # cleanse
# nzv <- nearZeroVar(week1.inputs)
# temp <- week1.inputs[-nzv]
# numerics <- temp[, sapply(temp, is.numeric)]
# cormat <- cor(temp)
# highcorr <- findCorrelation(cormat, cutoff = .9)
# week1.inputs.clean <- temp[-highcorr]
# preProc <- preProcess(week1.inputs.clean, method = c("center", "scale", "YeoJohnson"))
# week1.preProc <- predict(preProc, week1.inputs.clean)
# week1.preProc$outcome <- week1.opptm$pts
# ranger.mod <- ranger(outcome ~ ., data = week1.preProc, write.forest = TRUE, importance = "impurity")
# rf.mod <- randomForest(pts ~ ., data = week1.clean)
# 
# mat <- scale(select(week1.clean, -pts))
# cv.glmnet.mod <- cv.glmnet(x = as.matrix(select(week1.preProc, -outcome)), y = week1.preProc$outcome)
# coef(cv.glmnet.mod)
