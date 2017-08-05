# What happens to WRs with > 24 targets rookie year?

# get WRs who were rookies in 2000 or later
player_wr <- filter(player, pos1 == "WR" & start >= 2000)
offense_wr <- offense %>%
  inner_join(player_wr, by = c("player" = "player")) %>%
  inner_join(select(game, gid, wk, season = seas), 
             by = c("gid" = "gid")) %>%
  filter(wk <= 17)

rookie_wr_season <- offense_wr %>%
  filter(season == start) %>%
  group_by(player, season) %>%
  summarise(player_name = first(pname),
            team = first(team),
            drafted = max(dpos),
            targets_sum = sum(trg)) %>%
  left_join(offense_wr %>% 
              filter(season == (start + 1)) %>% 
              group_by(player, season) %>% 
              summarise(targets_n_plus_1 = sum(trg),
                        start_year = max(start)) %>% 
              ungroup(),
            by = c("player" = "player", "season" = "start_year"))

ggplot(data = filter(rookie_wr_season, targets_sum >= 24), 
       aes(x = targets_sum, y = targets_n_plus_1)) +
  geom_point(alpha = .8) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1),
              color = "chartreuse4", alpha = .5,
              fill = "lightgrey") +
  scale_x_continuous(breaks = seq(0, 150, 50)) +
  xlab("Targets, Rookie Season") +
  # scale_y_continuous(breaks = seq(50, 150, 50)) +
  ylab("Targets, Rookie Season + 1") +
  theme_minimal()
  
