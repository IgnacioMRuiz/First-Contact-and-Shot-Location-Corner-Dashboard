# Load required libraries ------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(jsonlite)
library(stringr)

# ---- IMPORT DATA -------------------------------------------------------------

matches <- read.csv(".../Data/Row/matches.csv")
events <- read.csv(".../Data/Row/events.csv")
data_360 <- read.csv(".../Data/Row/data_360.csv")



# ---- CLEAN MATCH DATA --------------------------------------------------------

matches <- matches %>%
  select(-c(match_status, match_status_360, last_updated, last_updated_360,
            competition.competition_id, competition.country_name, season.season_id,
            home_team.home_team_gender, home_team.country.id, home_team.country.name,
            home_team.home_team_group, away_team.away_team_gender, away_team.country.name,
            away_team.country.id, away_team.away_team_group, metadata.data_version,
            metadata.shot_fidelity_version, metadata.xy_fidelity_version, competition_stage.id,
            competition_stage.name, stadium.id, stadium.country.id, stadium.country.name,
            referee.id, referee.country.id, referee.country.name, kick_off, stadium.name,
            referee.name)) %>%
  mutate(
    match_date = format(as.Date(match_date, format="%Y-%m-%d"), "%d/%m/%Y"),
    home_team_result = case_when(
      home_score > away_score ~ "Win",
      home_score < away_score ~ "Loss",
      home_score == away_score ~ "Draw"
    ),
    away_team_result = case_when(
      home_score < away_score ~ "Win",
      home_score > away_score ~ "Loss",
      home_score == away_score ~ "Draw"
    )
  ) %>%
  select(competition.competition_name, match_id, match_week, match_date,
         home_team.home_team_id, home_team.home_team_name, home_score, home_team_result,
         away_team.away_team_id, away_team.away_team_name, away_score, away_team_result) %>%
  rename(
    competition_name = competition.competition_name,
    match_week = match_week,
    match_date = match_date,
    home_team_id = home_team.home_team_id,
    home_team_name = home_team.home_team_name,
    home_score = home_score,
    home_team_result = home_team_result,
    away_team_id = away_team.away_team_id,
    away_team_name = away_team.away_team_name,
    away_score = away_score,
    away_team_result = away_team_result
  ) %>%
  arrange(match_week)

# Bayer Leverkusen Results
matches <- matches %>%
  mutate(
    blk_result = case_when(
      home_team_name == "Bayer Leverkusen" ~ home_team_result,
      away_team_name == "Bayer Leverkusen" ~ away_team_result,
      TRUE ~ NA_character_
    )
  )



# ---- CLEAN EVENTS DATA -------------------------------------------------------

events <- events %>%
  filter(play_pattern.name == "From Corner") %>%
  arrange(match_id, possession, timestamp) %>%
  mutate(id_possession = paste0(match_id, "_", possession))



# ---- TAKER DATA --------------------------------------------------------------

taker_data <- events %>%
  filter(pass.type.name == "Corner") %>%
  select(id, match_id, id_possession, period, minute, timestamp, type.name, possession_team.name, team.name,
         player.name, player.id, pass.length, pass.body_part.name, pass.technique.name,
         location.x, location.y, pass.end_location.x, pass.end_location.y) %>%
  mutate(
    short_long_taker = if_else(pass.length > 10, "Long", "Short"),
    side_taker = case_when(
      location.y < 40 ~ "Left",
      location.y >= 40 ~ "Right"
    )
  ) %>%
  rename(
    possession_team_name_taker = possession_team.name,
    team_name_taker = team.name,
    taker = player.name,
    player_id_taker = player.id,
    action_subtype_taker = type.name,
    foot_taker = pass.body_part.name,
    pass_technique_taker = pass.technique.name,
    start_x_taker = location.x,
    start_y_taker = location.y,
    end_x_taker = pass.end_location.x,
    end_y_taker = pass.end_location.y,
    id_taker = id
  )

# Regular Takers by Side
top_takers <- taker_data %>%
  filter(team_name_taker == "Bayer Leverkusen") %>%
  group_by(side_taker) %>%
  count(taker, name = "corner_count") %>%
  arrange(side_taker, desc(corner_count)) %>%
  group_by(side_taker) %>%
  slice_max(order_by = corner_count, n = 2) %>%
  ungroup() %>%
  mutate(is_top_taker = TRUE)

taker_data <- taker_data %>%
  left_join(top_takers, by = c("taker", "side_taker")) %>%
  mutate(regular_taker = if_else(is_top_taker == TRUE, "YES", "NO", missing = "NO")) %>%
  select(-is_top_taker, -corner_count)



# ---- FIRST CONTACT DATA ------------------------------------------------------

first_contact_outcome <- events %>%
  group_by(id_possession) %>%
  slice(1) %>%
  mutate(first_contact_outcome = case_when(
    pass.outcome.name %in% c("Incomplete", "Out") ~ "Lost",
    is.na(pass.outcome.name) ~ "Won",
    TRUE ~ NA_character_
  )) %>%
  select(match_id, id_possession, first_contact_outcome) %>%
  ungroup()

first_contact_players <- events %>%
  group_by(id_possession) %>%
  filter(n() >= 2) %>%
  mutate(row_index = case_when(
    type.name[2] == "Ball Receipt*" & type.name[3] == "Pressure" ~ 4,
    type.name[2] == "Ball Receipt*" ~ 3,
    type.name[2] == "Pressure" ~ 4,
    type.name[2] %in% c("Goal Keeper", "Duel", "Ball Recovery", "Foul Committed", "Pass", "Clearance") ~ 2,
    TRUE ~ NA_integer_
  )) %>%
  filter(row_number() == row_index) %>%
  select(id, match_id, id_possession, player.id, player.name, team.name, possession_team.name,
         type.name, shot.statsbomb_xg, location.x, location.y, carry.end_location.x, carry.end_location.y,
         pass.end_location.x, pass.end_location.y, shot.body_part.name) %>%
  ungroup()

first_contact <- first_contact_outcome %>%
  left_join(first_contact_players, by = c("match_id", "id_possession")) %>%
  mutate(
    end_x = coalesce(carry.end_location.x, pass.end_location.x),
    end_y = coalesce(carry.end_location.y, pass.end_location.y)
  ) %>%
  rename(
    possession_team_name_first = possession_team.name,
    team_name_first = team.name,
    player_first = player.name,
    player_id_first = player.id,
    action_subtype_first = type.name,
    start_x_first = location.x,
    start_y_first = location.y,
    shot_xg_first = shot.statsbomb_xg,
    id.first = id
  )



# ---- COMBINE TAKER AND FIRST CONTACT DATA ------------------------------------

contacts_df <- taker_data %>%
  inner_join(first_contact, by = c("match_id", "id_possession"))



# ---- ZONE AND MOMENT DEFINITIONS ---------------------------------------------

x_min <- 80
x_max <- 120
y_min <- 0
y_max <- 80

x_segments <- seq(x_min, x_max, length.out = 11)
y_segments <- seq(y_min, y_max, length.out = 16)

contacts_df <- contacts_df %>%
  mutate(
    zone_x = findInterval(end_x_taker, x_segments),
    zone_y = findInterval(end_y_taker, y_segments),
    b04_possession = if_else(possession_team_name_taker == "Bayer Leverkusen", "Offensive", "Defensive")
  )



# ---- POSSESSION OUTCOME ------------------------------------------------------

possession_ends <- events %>%
  filter(location.x > 80) %>%
  filter(!is.na(shot.outcome.name)) %>%
  group_by(id_possession) %>%
  arrange(desc(row_number())) %>%
  summarise(
    shot_outcome_possession = first(shot.outcome.name),
    shot_outcome_x = first(location.x),
    shot_outcome_y = first(location.y),
    id_final_shot = first(id)
  ) %>%
  mutate(
    shot_outcome_possession = case_when(
      shot_outcome_possession %in% c("Saved", "Post") ~ "On Target",
      shot_outcome_possession == "Goal" ~ "Goal",
      shot_outcome_possession %in% c("Off T", "Wayward", "Saved Off Target") ~ "Off Target",
      shot_outcome_possession == "Blocked" ~ "Blocked"
    )
  )

# Small Calculations
contacts_df <- contacts_df %>%
  left_join(possession_ends, by = "id_possession") %>%
  mutate(
    possession_outcome = if_else(is.na(shot_outcome_possession), "No shot", "Shot"),
    first_contact_shot = if_else(id_final_shot == id.first, "YES", "NO"),
    first_contact_goal = if_else(id_final_shot == id.first & shot_outcome_possession == "Goal", "YES", "NO"))

contacts_df <- contacts_df %>%
  mutate(minutes_period = case_when(
    period == 1 & minute <= 15 ~ "0 - 15",
    period == 1 & minute <= 30 ~ "15 - 30",
    period == 1 & minute <= 45 ~ "30 - 45",
    period == 1 & minute > 45 ~ "+45",
    period == 2 & minute <= 60 ~ "45 - 60",
    period == 2 & minute <= 75 ~ "60 - 75",
    period == 2 & minute <= 90 ~ "75 - 90",
    period == 2 & minute > 90 ~ "+90",
    TRUE ~ NA_character_
  ))



# ---- FINALIZE DATA FRAME -----------------------------------------------------

contacts_df <- contacts_df %>%
  select(-c(timestamp, action_subtype_taker, player_id_taker, 
            pass.length, id.first, player_id_first, end_x, end_y, id_final_shot))

contacts_df <- contacts_df %>%
  rename(
    Id.Taker = id_taker,
    Match.Id = match_id,
    Id.Possession = id_possession,
    Period = period,
    Minute = minute,
    Minutes.Period = minutes_period,
    Possession.Team.Taker = possession_team_name_taker,
    Team.Name.Taker = team_name_taker,
    Taker = taker,
    Foot.Taker = foot_taker,
    Pass.Technique.Taker = pass_technique_taker,
    Start.X.Taker = start_x_taker,
    Start.Y.Taker = start_y_taker,
    End.X.Taker = end_x_taker,
    End.Y.Taker = end_y_taker,
    Short.Long.Taker = short_long_taker,
    Side.Taker = side_taker,
    Regular.Taker = regular_taker,
    First.Contact.Outcome = first_contact_outcome,
    Possession.Team.First.Contact = possession_team_name_first,
    Team.Name.First.Contact = team_name_first,
    Action.Type.First.Contact = action_subtype_first,
    Shot.xG.First.Contact = shot_xg_first,
    Start.X.First.Contact = start_x_first,
    Start.Y.First.Contact = start_y_first,
    Carry.End.X.First.Contact = carry.end_location.x,
    Carry.End.Y.First.Contact = carry.end_location.y,
    Pass.End.X.First.Contact = pass.end_location.x,
    Pass.End.Y.First.Contact = pass.end_location.y,
    Zone.X.Aimed = zone_x,
    Zone.Y.Aimed = zone_y,
    B04.Possession = b04_possession,
    Possession.Shot.Outcome = shot_outcome_possession,
    Shot.End.X.First.Contact = shot_outcome_x,
    Shot.End.Y.First.Contact = shot_outcome_y,
    Possession.Outcome = possession_outcome,
    Goal.First.Contact = first_contact_goal,
    Shot.Body.Part = shot.body_part.name)

contacts_df <- contacts_df %>%
  mutate(Corner = "Corner")

# ---- IMPORT TRACKING DATA ----------------------------------------------------

data_360 <- read.csv(".../Data/Row/data_360.csv")



# ---- FILTER EVENTS CORNERS --------------------------------------------------------

data_360 <- data_360 %>% 
  semi_join(contacts_df, by = c("event_uuid" = "Id.Taker"))


data_360 <- data_360 %>%
  left_join(contacts_df %>% select(Id.Taker, Possession.Team.Taker), 
            by = c("event_uuid" = "Id.Taker"))


# Substitute values with B04 or OPP
data_360 <- data_360 %>%
  mutate(teammate = case_when(
    Possession.Team.Taker == "Bayer Leverkusen" & teammate == TRUE  ~ "B04",
    Possession.Team.Taker == "Bayer Leverkusen" & teammate == FALSE ~ "OPP",
    Possession.Team.Taker != "Bayer Leverkusen" & teammate == TRUE  ~ "OPP",
    Possession.Team.Taker != "Bayer Leverkusen" & teammate == FALSE ~ "B04"
  ))

# Substitude Possession.Team.Taker with 
data_360 <- data_360 %>%
  mutate(Possession.Team.Taker = case_when(
    Possession.Team.Taker == "Bayer Leverkusen" ~ "B04",
    TRUE ~ "OPP"
  ))

# Count of players in the box
area_counts <- data_360 %>%
  filter(player.x.location > 102, 
         player.y.location >= 18, player.y.location <= 62, 
         keeper == FALSE) %>%  # Excluir filas con keeper == TRUE
  group_by(event_uuid, teammate) %>%
  summarise(count = n()) %>%
  spread(teammate, count, fill = 0) %>%
  rename(B04.Players.Box = B04, OPP.Players.Box = OPP) %>%
  mutate(Box.Balance = case_when(
    B04.Players.Box > OPP.Players.Box ~ "Superiority",
    B04.Players.Box == OPP.Players.Box ~ "Balance",
    B04.Players.Box < OPP.Players.Box ~ "Inferiority"
  ))

# Join with data in contacts
contacts_df <- contacts_df %>%
  left_join(area_counts %>% select(B04.Players.Box, OPP.Players.Box, Box.Balance), 
            by = c("Id.Taker" = "event_uuid"))

# Substitute Possession.Team.Taker with 
data_360 <- data_360 %>%
  mutate(actor = case_when(
    keeper == "TRUE" ~ "Keeper",
    actor == "TRUE" ~ "Taker",
    actor == "FALSE" ~ "Player"
  )) %>%
  select(-c(keeper))


# ---- OPPONENT CALIBER --------------------------------------------------------
opponent_caliber <- read.csv(".../Data/Row/Opponent_caliber.csv")

opponent_caliber <- opponent_caliber %>%
  mutate(Team.Name = str_replace(Team.Name, "Borussia M�nchengladbach", "Borussia Mönchengladbach"),
         Team.Name = str_replace(Team.Name, "FC K�ln", "FC Köln"))

opponent_caliber <- opponent_caliber %>%
  mutate(Opp.Caliver = case_when(
    Points.Per.Match >= 0 & Points.Per.Match <= 1 ~ "Tier 3",
    Points.Per.Match > 1 & Points.Per.Match <= 2 ~ "Tier 2",
    Points.Per.Match > 2 & Points.Per.Match <= 3 ~ "Tier 1"
  ))

matches <- matches %>%
  mutate(Opposition = ifelse(home_team_name == "Bayer Leverkusen", 
                             away_team_name, 
                             home_team_name))


matches <- matches %>%
  left_join(opponent_caliber, by = c("Opposition" = "Team.Name"))

matches <- matches %>%
  select(-c(Games, Points))

# ---- SHOT IN POSSESSION ------------------------------------------------------

possession_ends <- events %>%
  filter(location.x > 80, type.name == "Shot") %>%
  group_by(id_possession) %>%
  slice_tail(n = 1) %>%
  select(id, match_id, id_possession, player.id, player.name, team.name,
         possession_team.name, type.name, shot.statsbomb_xg, location.x, 
         location.y,shot.end_location.x, shot.end_location.y, shot.end_location.z,
         shot.body_part.name, shot.statsbomb_xg) %>%
  ungroup()

possessions.w.shot <- possession_ends %>%
  left_join(contacts_df %>% 
              select(Id.Possession, first_contact_shot, Possession.Shot.Outcome, B04.Possession), 
            by = c("id_possession" = "Id.Possession"))


# ---- EXPORT DATA -------------------------------------------------------------

write.csv(matches, file = ".../Data/Processed/matches_processed.csv", row.names = FALSE, na = "")
write.csv(contacts_df, file = ".../Data/Processed/contacts_processed.csv", row.names = FALSE, na = "") 
write.csv(data_360, file = ".../Data/Processed/data_360.csv", row.names = FALSE, na = "") 
write.csv(possessions.w.shot, file = ".../Data/Processed/possessions_w_shot.csv", row.names = FALSE, na = "") 


# Clean up environment ---------------------------------------------------------
rm(list = setdiff(ls(), c("matches", "contacts_df", "data_360", "possessions.w.shot")))

