#### armchair_helpers ####
# TODO: get this documented with roxygen2

augment_team <- function(TEAM) {
  # purpose: augment the "team" table from ArmchairAnalysis NFL Data
  # args:
  #  TEAM: a data.frame of the "team" table From ArmchairAnalysis NFL Data
  team.aug <- TEAM %>%
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
  
  return(team.aug)
}

join_team_to_game <- function(TEAM_AUG, GAME) {
  # purpose: augment the "team" table from ArmchairAnalysis NFL Data
  # args:
  #  TEAM_AUG: an data.frame of the "team" table From ArmchairAnalysis NFL Data augmented by the augment_team function from the armchair_helpers
  
  # get all tid's associated with each gid for joins
  tid.list <- TEAM_AUG %>%
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
  
  
  
}