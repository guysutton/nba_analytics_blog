# Write function to download and clean NBA play-by-play
# data with lineups 

get_pbp_lineups <- function(teams, years){

# Load required packages 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, 
               zoo,
               janitor,
               nbastatR,
               future)

# Extract all games for the 2020 season (excluding pre-season and all-star weekend)
future::plan(multiprocess)
game_logs <- nbastatR::game_logs(seasons = {{ years }})

# Process the game logs 
future::plan(multiprocess)
games <- game_logs %>%
  # Keep only these columns 
  dplyr::select(idGame, slugTeam, slugOpponent, locationGame) %>%
  # Use a conditional statement to process the team home/away data 
  dplyr::mutate(slugTeamHome = dplyr::if_else(locationGame == "H", 
                                              slugTeam, 
                                              slugOpponent),
                slugTeamAway = dplyr::if_else(locationGame == "A", 
                                              slugTeam, 
                                              slugOpponent)) %>%
  # Drop these columns - no longer required
  dplyr::select(-c(slugTeam, slugOpponent, locationGame)) %>%
  dplyr::distinct(idGame, .keep_all = TRUE) %>%
  # Here, we filter for the teams we want data for 
  # We have to use team codes. E.g. MIL = Milwaukee Bucks
  # - Filter the games where either (denoted by the '|' bar) the home or 
  #   or away team is the Bucks. 
  dplyr::filter(slugTeamHome %in% {{ teams }} | 
                  slugTeamAway %in% {{ teams }})

# Extract the play-by-play for each game 
future::plan(multiprocess)
play_logs_all <- nbastatR::play_by_play_v2(game_ids = unique(games$idGame))


############################################################
# - Pre-processing data cleaning
############################################################

# Lots, and lots, and lots of data cleaning 
new_pbp <- play_logs_all %>%
  dplyr::distinct(idGame, numberEvent, .keep_all = TRUE) %>%
  dplyr::group_by(idGame) %>%
  dplyr::mutate(numberEvent = row_number()) %>%  
  dplyr::ungroup() %>%
  dplyr::select(idGame, numberEventMessageType, numberEventActionType, 
                namePlayer1, namePlayer2, namePlayer3,  
                slugTeamPlayer1, slugTeamPlayer2,  slugTeamPlayer3, 
                numberPeriod, timeQuarter, minuteRemainingQuarter,  
                secondsRemainingQuarter, descriptionPlayHome, numberEvent, 
                descriptionPlayVisitor, scoreHome, scoreAway) %>%
  dplyr::mutate(shotPtsHome = dplyr::case_when(
    numberEventMessageType == 3 & !stringr::str_detect(descriptionPlayHome, "MISS") ~ 1,         
    numberEventMessageType == 1 & stringr::str_detect(descriptionPlayHome, "3PT") ~ 3,          
    numberEventMessageType == 1 & !stringr::str_detect(descriptionPlayHome, "3PT") ~ 2,
    TRUE ~ 0)) %>%
  dplyr::mutate(shotPtsAway = dplyr::case_when(
    numberEventMessageType == 3 & !stringr::str_detect(descriptionPlayVisitor, "MISS") ~ 1,
    numberEventMessageType == 1 & stringr::str_detect(descriptionPlayVisitor, "3PT") ~ 3,
    numberEventMessageType == 1 & !stringr::str_detect(descriptionPlayVisitor, "3PT") ~ 2,
    TRUE ~ 0)) %>%
  dplyr::group_by(idGame) %>%
  dplyr::mutate(ptsHome = cumsum(shotPtsHome),
                ptsAway = cumsum(shotPtsAway)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(secsLeftQuarter = (minuteRemainingQuarter * 60) + secondsRemainingQuarter) %>%
  dplyr::mutate(secsStartQuarter = dplyr::case_when(
    numberPeriod %in% c(1:5) ~ (numberPeriod - 1) * 720,
    TRUE ~ 2880 + (numberPeriod - 5) * 300)) %>%
  dplyr::mutate(secsPassedQuarter = ifelse(numberPeriod %in% c(1:4), 
                                           720 - secsLeftQuarter, 
                                           300 - secsLeftQuarter),
                secsPassedGame = secsPassedQuarter + secsStartQuarter) %>%
  dplyr::left_join(games %>%
                     dplyr::select(idGame, slugTeamHome, slugTeamAway)) %>%
  dplyr::select(idGame, numberEventMessageType, numberEventActionType, 
                slugTeamHome, slugTeamAway, slugTeamPlayer1, slugTeamPlayer2, 
                slugTeamPlayer3, numberPeriod, timeQuarter, secsPassedGame, numberEvent, 
                namePlayer1, namePlayer2, namePlayer3, descriptionPlayHome,
                descriptionPlayVisitor, ptsHome, ptsAway, shotPtsHome, shotPtsAway) %>%
  dplyr::mutate(marginBeforeHome = ptsHome - ptsAway - shotPtsHome + shotPtsAway,
                marginBeforeAway = ptsAway - ptsHome - shotPtsAway + shotPtsHome,
                timeQuarter = stringr::str_pad(timeQuarter, width = 5, pad = 0))

# Define where substitutes where made 
subs_made <- new_pbp %>%
  dplyr::filter(numberEventMessageType == 8) %>%
  dplyr::mutate(slugTeamLocation = ifelse(slugTeamPlayer1 == slugTeamHome, "Home", "Away")) %>%
  dplyr::select(idGame, numberPeriod, timeQuarter, secsPassedGame, 
                slugTeamPlayer = slugTeamPlayer1,
                slugTeamLocation, 
                playerOut = namePlayer1, 
                playerIn = namePlayer2) %>%
  tidyr::pivot_longer(cols = starts_with("player"), 
                      names_to = "inOut",
                      names_prefix = "player",
                      values_to = "namePlayer") %>%
  dplyr::group_by(idGame, numberPeriod, slugTeamPlayer, namePlayer) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::ungroup()

others_qtr <- new_pbp %>%
  dplyr::filter(numberEventMessageType != 8) %>%                             
  dplyr::filter(!(numberEventMessageType == 6 & 
                    numberEventActionType %in% c(10, 11, 16, 18, 25))) %>%
  tidyr::pivot_longer(cols = starts_with("namePlayer"),
                      names_to = "playerNumber",
                      names_prefix = "namePlayer",
                      values_to = "namePlayer") %>%
  dplyr::mutate(slugTeamPlayer = dplyr::case_when(playerNumber == 1 ~ slugTeamPlayer1,
                                                  playerNumber == 2 ~ slugTeamPlayer2,
                                                  playerNumber == 3 ~ slugTeamPlayer3,
                                                  TRUE ~ "None")) %>%
  dplyr::mutate(slugTeamLocation = ifelse(slugTeamPlayer == slugTeamHome, 
                                          "Home", 
                                          "Away")) %>%
  dplyr::filter(!is.na(namePlayer),
                !is.na(slugTeamPlayer)) %>%
  dplyr::anti_join(subs_made %>%
                     dplyr::select(idGame, numberPeriod, slugTeamPlayer, namePlayer)) %>%  
  dplyr::distinct(idGame, numberPeriod, namePlayer, slugTeamPlayer, slugTeamLocation)

lineups_quarters <- subs_made %>%
  dplyr::filter(inOut == "Out") %>%
  dplyr::select(idGame, numberPeriod, slugTeamPlayer, namePlayer, slugTeamLocation) %>%
  dplyr::bind_rows(others_qtr) %>%
  dplyr::arrange(idGame, numberPeriod, slugTeamPlayer)

lineup_subs <- new_pbp %>%
  dplyr::filter(numberEventMessageType == 8) %>%
  dplyr::select(idGame, numberPeriod, timeQuarter, secsPassedGame, 
                slugTeamPlayer = slugTeamPlayer1, 
                playerOut = namePlayer1, 
                playerIn = namePlayer2, numberEvent) %>%
  dplyr::arrange(idGame, numberEvent) %>%
  dplyr::group_by(idGame, numberPeriod, slugTeamPlayer) %>%
  dplyr::mutate(row1 = row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(lineups_quarters %>%
                     dplyr::group_by(idGame, numberPeriod, slugTeamPlayer) %>%
                     dplyr::summarise(lineupBefore = paste(sort(unique(namePlayer)), 
                                                           collapse = ", ")) %>%
                     dplyr::ungroup() %>%
                     dplyr::mutate(row1 = 1)) %>%
  dplyr::select(-row1)

lineup_subs <- lineup_subs %>%
  dplyr::mutate(lineupBefore = stringr::str_split(lineupBefore, ", ")) %>% 
  dplyr::arrange(idGame, numberEvent) %>%
  dplyr::group_by(idGame, numberPeriod, slugTeamPlayer) %>%
  dplyr::mutate(lineupAfter = accumulate2(playerIn, 
                                          playerOut, 
                                          ~setdiff(c(..1, ..2), ..3), 
                                          .init = lineupBefore[[1]])[-1],
                lineupBefore = dplyr::coalesce(lineupBefore, lag(lineupAfter))) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate_all( ~ purrr::map_chr(., ~paste(.x, collapse = ", "))) %>%
  dplyr::mutate_at(vars("numberEvent", "numberPeriod", "idGame"), 
                   ~ as.integer(.)) %>%
  dplyr::mutate(secsPassedGame = as.numeric(secsPassedGame)) %>%
  dplyr::arrange(idGame, numberEvent) %>%
  dplyr::left_join(lineups_quarters %>%
                     dplyr::distinct(idGame, slugTeamPlayer, slugTeamLocation)) %>%
  dplyr::filter(!is.na(slugTeamLocation))

lineup_game <- new_pbp %>%
  dplyr::group_by(idGame, numberPeriod) %>%
  dplyr::mutate(row1 = row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(lineups_quarters %>%
                     dplyr::group_by(idGame, numberPeriod, slugTeamLocation) %>%
                     dplyr::summarise(lineupBefore = paste(sort(unique(namePlayer)), 
                                                           collapse = ", ")) %>%
                     dplyr::ungroup() %>%
                     tidyr::pivot_wider(names_from = slugTeamLocation,
                                        names_prefix = "lineupInitial",
                                        values_from = lineupBefore) %>%
                     dplyr::mutate(row1 = 1)) %>%
  dplyr::select(-row1) %>%
  dplyr::left_join(lineup_subs %>%
                     mutate(lineupBeforeHome = ifelse(slugTeamLocation == "Home", lineupBefore, NA),
                            lineupAfterHome = ifelse(slugTeamLocation == "Home", lineupAfter, NA),
                            lineupBeforeAway = ifelse(slugTeamLocation == "Away", lineupBefore, NA),
                            lineupAfterAway = ifelse(slugTeamLocation == "Away", lineupAfter, NA)) %>%
                     select(idGame, numberPeriod, timeQuarter, secsPassedGame, 
                            numberEvent, slugTeamPlayer1 = slugTeamPlayer,
                            contains("Home"), 
                            contains("Away"))) %>%
  dplyr::mutate_at(vars(c(lineupBeforeHome, lineupAfterHome)), 
                   ~ ifelse(!is.na(lineupInitialHome), 
                            lineupInitialHome, 
                            .)) %>%
  dplyr::mutate_at(vars(c(lineupBeforeAway, lineupAfterAway)), 
                   ~ ifelse(!is.na(lineupInitialAway), 
                            lineupInitialAway, 
                            .)) %>%
  dplyr::select(-starts_with("lineupInitial"))

lineup_game <- lineup_game %>%
  dplyr::group_by(idGame, numberPeriod) %>%
  dplyr::mutate(lineupHome = na.locf(lineupAfterHome, na.rm = FALSE),
                lineupAway = na.locf(lineupAfterAway, na.rm = FALSE),
                lineupHome = ifelse(is.na(lineupHome), 
                                    na.locf(lineupBeforeHome, 
                                            fromLast = TRUE, 
                                            na.rm = FALSE), 
                                    lineupHome),
                lineupAway = ifelse(is.na(lineupAway), 
                                    na.locf(lineupBeforeAway, 
                                            fromLast = TRUE, 
                                            na.rm = FALSE), 
                                    lineupAway),
                lineupHome = stringr::str_split(lineupHome, ", "),
                lineupAway = stringr::str_split(lineupAway, ", "),
                lineupHome = purrr::map_chr(lineupHome, 
                                            ~ paste(sort(.), 
                                                    collapse = ", ")),
                lineupAway = purrr::map_chr(lineupAway, 
                                            ~ paste(sort(.), 
                                                    collapse = ", "))) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(starts_with("lineupBefore"), 
                   starts_with("lineupAfter")))

lineup_game_stats <- lineup_game %>%
  dplyr::mutate(canSub = case_when(numberEventMessageType == 5 & 
                                     !numberEventActionType %in% c(1, 2) ~ 1,
                                   numberEventMessageType == 6 & 
                                     numberEventActionType != 16 ~ 1,
                                   numberEventMessageType == 11 & 
                                     numberEventActionType != 4 ~ 1,
                                   numberEventMessageType == 7 & 
                                     numberEventActionType == 5 ~ 1, 
                                   numberEventMessageType == 4 & 
                                     numberEventActionType == 0 & 
                                     !stringr::str_detect(stringr::str_to_upper(descriptionPlayHome),
                                                          "OFF:") ~ 1,
                                   numberEventMessageType == 4 & 
                                     numberEventActionType == 0 & 
                                     !str_detect(stringr::str_to_upper(descriptionPlayVisitor), 
                                                 "OFF:") ~ 1,
                                   TRUE ~ 0)) %>%
  dplyr::mutate(secsPassedGame2 = ifelse(timeQuarter == "12:00" &
                                           (str_detect(str_to_lower(descriptionPlayHome), 
                                                       "technical") |
                                              str_detect(str_to_lower(descriptionPlayVisitor),
                                                         "technical")), 
                                         secsPassedGame + 0.5, secsPassedGame)) %>% 
  dplyr::group_by(idGame, numberPeriod, secsPassedGame) %>%
  dplyr::mutate(numberNew = ifelse(numberEventMessageType == 3 & 
                                     numberEventActionType == 12, 
                                   paste(numberEvent[numberEventMessageType == 3 &
                                                       numberEventActionType == 11], 
                                         collapse = ", "), 
                                   as.character(numberEvent)),
                numberNew = ifelse(numberEventMessageType == 3 & 
                                     numberEventActionType %in% c(14, 15), 
                                   paste(numberEvent[numberEventMessageType == 3 & 
                                                       numberEventActionType == 13], 
                                         collapse = ", "),
                                   numberNew)) %>%
  dplyr::mutate(numberNew = stringr::str_split(numberNew, ", "),
                numberNew = purrr::map(numberNew, ~as.numeric(.)),
                numberNew = purrr::map2_dbl(numberNew, 
                                            numberEvent, 
                                            ~ max(.x[.x <= .y]))) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(idGame, numberNew, numberEvent) %>%
  dplyr::group_by(idGame) %>%
  dplyr::mutate(newptsHome = cumsum(shotPtsHome),
                newptsAway = cumsum(shotPtsAway)) %>%
  dplyr::group_by(idGame, numberPeriod, secsPassedGame2) %>%
  dplyr::mutate(subOpp = cumsum(canSub)) %>%
  dplyr::group_by(idGame = as.character(idGame),
                  numberPeriod = as.character(numberPeriod), 
                  subOpp, 
                  secsPassedGame2 = as.character(secsPassedGame2)) %>%
  dplyr::mutate(hasFouls = sum(numberEventMessageType == 3)) %>%
  dplyr::mutate(newptsHome = ifelse(hasFouls > 0,
                                    newptsHome[row_number() == 
                                                 max(row_number()[numberEventMessageType == 3])],
                                    newptsHome),
                newptsAway = ifelse(hasFouls > 0,
                                    newptsAway[row_number() == 
                                                 max(row_number()[numberEventMessageType == 3])],
                                    newptsAway)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-hasFouls) %>%
  dplyr::select(-c(numberNew, secsPassedGame2)) %>%
  dplyr::mutate_all(~ as.character(.)) %>%
  dplyr::mutate(secsPassedGame = as.numeric(secsPassedGame),
                numberEvent = as.numeric(numberEvent))

# For each event, create a distinct row for each player from each team 
game_test <- lineup_game_stats %>%
  # Drop the events that are not plays (e.g. technical fouls)
  tidyr::drop_na(namePlayer1) %>%
  # Give a unique identified to each play per game 
  dplyr::group_by(idGame, numberEvent) %>%
  dplyr::mutate(numberEvent = dplyr::cur_group_id()) %>%
  # Duplicate the full team line-up. We need it for later. 
  dplyr::mutate(homeTeamLineup = lineupHome,
                awayTeamLineup = lineupAway) %>%
  # Combine team line-ups into a single column
  tidyr::unite(homeTeamLineup:awayTeamLineup, col = "lineups", sep = ", ") %>%
  tidyr::separate_rows(., 
                       lineupHome, 
                       lineupAway,
                       sep = ",", 
                       convert = TRUE) %>%
  # Remove whitespace from start of player names, if present
  dplyr::mutate(lineupHome = stringr::str_trim(lineupHome, side = c("left")),
                lineupAway = stringr::str_trim(lineupAway, side = c("left"))) %>%
  # Convert players names into long-format so each player now has their own row 
  # for each event in the game
  tidyr::pivot_longer(
    cols = lineupHome:lineupAway,
    names_to = "teamHomeAway",
    values_to = "playerNames") %>%
  dplyr::ungroup() %>%
  # For each event, is the lineup the same as the previous event? 
  dplyr::group_by(idGame) %>%
  dplyr::mutate(lineupChange = lineups != lag(lineups),
                lineupChange = coalesce(lineupChange, FALSE)) %>%
  dplyr::mutate(lineupStint = cumsum(lineupChange)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(idGame, lineupStint, numberEvent) %>%
  # Has that player been subbed? 
  dplyr::group_by(idGame, playerNames) %>%
  dplyr::arrange(idGame, playerNames, numberEvent) %>%
  # Add column which states whether player was subbed or not? 
  dplyr::mutate(player_subbed = dplyr::case_when(
    lag(numberEvent) != numberEvent - 1 ~ "Yes", 
    TRUE ~ "No"))

# Define each stint each player has on court 
define_stint <- game_test %>%
  # Define stints as the cumulative sum of times the player is subbed
  # cumsum starts at 0, so we need to + 1 to each value 
  dplyr::mutate(stints_on_court = cumsum(player_subbed == "Yes"),
                stints_on_court = stints_on_court + 1) %>%
  dplyr::ungroup() %>%
  # Define start and end time of each stint with a min/max statement
  dplyr::group_by(idGame, playerNames, stints_on_court) %>%
  dplyr::mutate(time_at_start = min(secsPassedGame),
                time_at_end   = max(secsPassedGame)) %>%
  # Calculate total seconds of each stint
  dplyr::mutate(time_stint_secs = time_at_end - time_at_start) %>%
  # Convert seconds into minutes and seconds 
  dplyr::mutate(time_stint_mins = time_stint_secs / 60) %>%
  # Calculate time since start of each stint 
  dplyr::mutate(time_since_start_stint = secsPassedGame - time_at_start) %>%
  # Manually define 60 second intervals - 
  # I couldn't force ggplot2::cut_width to start @ 0... Help?  
  dplyr::mutate(minute_cats = dplyr::case_when(
    between(time_since_start_stint, 0, 60) ~ "1",
    between(time_since_start_stint, 61, 120) ~ "2",
    between(time_since_start_stint, 121, 180) ~ "3",
    between(time_since_start_stint, 181, 240) ~ "4",
    between(time_since_start_stint, 241, 300) ~ "5",
    between(time_since_start_stint, 301, 360) ~ "6",
    between(time_since_start_stint, 361, 420) ~ "7",
    between(time_since_start_stint, 421, 480) ~ "8",
    between(time_since_start_stint, 481, 540) ~ "9",
    between(time_since_start_stint, 541, 600) ~ "10",
    between(time_since_start_stint, 601, 660) ~ "11",
    between(time_since_start_stint, 661, 720) ~ "12",
    between(time_since_start_stint, 721, 780) ~ "13",
    between(time_since_start_stint, 781, 840) ~ "14",
    between(time_since_start_stint, 841, 900) ~ "15",
    between(time_since_start_stint, 901, 960) ~ "16",
    between(time_since_start_stint, 961, 1020) ~ "17",
    between(time_since_start_stint, 1021, 1080) ~ "18",
    between(time_since_start_stint, 1081, 1140) ~ "19",
    between(time_since_start_stint, 1141, 1200) ~ "20",
    between(time_since_start_stint, 1221, 1280) ~ "21",
    between(time_since_start_stint, 1281, 1340) ~ "22",
    between(time_since_start_stint, 1341, 1400) ~ "23",
    between(time_since_start_stint, 1401, 1460) ~ "24")) %>%
  # For each play, does the score change or not?
  # - We will need this to classify whether 2pt/3pt/free throw attempts
  #   were successful or not. 
  dplyr::mutate(score_change = dplyr::if_else(shotPtsHome > 0 | 
                                                shotPtsAway > 0, 1, 0))

description_play <- define_stint %>%
  dplyr::mutate(descriptionPlay = dplyr::coalesce(descriptionPlayHome, descriptionPlayVisitor))

# Define each event as a particular play
events_defined <- description_play %>%
  # Was 3pt fg attempt made?
  dplyr::mutate(is_3pt_made = dplyr::case_when(
    stringr::str_detect(descriptionPlay, "3PT") &
      score_change == 1 ~ 1,
    stringr::str_detect(descriptionPlay, "3PT") &
      score_change == 0 ~ 0)) %>%
  # Was 2pt fg attempt made?
  dplyr::mutate(is_2pt_made = dplyr::case_when(
    numberEventMessageType == 1 & 
      !stringr::str_detect(descriptionPlay, "3PT") ~ 1,
    numberEventMessageType == 2 & 
      !stringr::str_detect(descriptionPlay, "3PT") ~ 0)) %>%
  # Was event a rebound? 
  dplyr::mutate(is_rebound = dplyr::case_when(
    numberEventMessageType == 4 & 
      stringr::str_detect(descriptionPlay, "REBOUND") ~ 1,
    TRUE ~ 0)) %>%
  # Was event a made free throw?
  dplyr::mutate(is_ft_made = dplyr::case_when(
    numberEventMessageType == 3 & 
      score_change == 1 ~ 1,
    numberEventMessageType == 3 & 
      score_change == 0 ~ 0))

}


##########################################################################
##########################################################################
##########################################################################

