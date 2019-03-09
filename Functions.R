library(tidyverse) # install.packages("tidyverse")
library(rvest) # install.packages("rvest")
library(progress) # install.packages("progress")

## 2 functions here: get_schedule() and get_box_score()
## You should probably use get_schedule() to then use get_box_score()

## Example:
## games <- get_schedule(2019)
## first_game_data <- games %>% slice(1) %>% get_box_score()
## all_season_data <- games %>% get_box_score()

get_schedule <- function(season, ..., progress = TRUE) {
  
  if (any(nchar(season) > 4) | any(!stringr::str_detect(season, "[0-9]{4,4}"))) {
    
    cat("\n")
    
    stop('\n\nMake sure your seasons are all 4-digit numbers like 1994
          \r(for 1993-94) and 2017 (for 2016-17)\n\n')
    
  }
  
  
  if (any(season < 1988) | any(season > 2019) | any(season == 2005)) {
    
    cat("\n")
    
    stop('\n\nOnly include seasons from (and including) 1988 to 2004 and
          \rfrom (and including) 2006 to whatever current season it is\n\n')

    
  }
  
  if (!is.logical(progress)) {
    
    cat("\n")
    
    stop('\n\nMake sure you set "progress" to either TRUE or FALSE 
          \r(it defaults to TRUE)\n\n')
    
  }
  
  season <- as.numeric(season)
  
  league <- "NHL"
  
  leagues <- league %>%
    tibble::enframe(name = NULL) %>%
    purrr::set_names("league") %>%
    mutate_all(toupper) %>%
    distinct()
  
  seasons <- season %>%
    tibble::enframe(name = NULL) %>%
    purrr::set_names("season") %>%
    distinct()
  
  mydata <- tidyr::crossing(leagues, seasons)
  
  
  if (progress) {
    
    pb <- progress::progress_bar$new(format = "get_schedule() [:bar] :percent ETA: :eta", clear = FALSE, total = nrow(mydata), show_after = 0) 
    
    cat("\n")
    
    pb$tick(0)
    
  }
  
  .get_schedule <- function(league, season, ...) {

    seq(2.5, 3.5, by = 0.001) %>%
      sample(1) %>%
      Sys.sleep()
    
    url <- stringr::str_c("https://www.hockey-reference.com/leagues/NHL_", season, "_games.html")
    
    page <- xml2::read_html(url)
    
    url_prefix <- case_when(league == "NHL" ~ "http://www.hockey-reference.com")
    
    game_urls <- page %>%
      rvest::html_nodes("#games th a") %>%
      rvest::html_attr("href") %>%
      stringr::str_c(url_prefix, .) %>%
      tibble::enframe(name = NULL) %>%
      purrr::set_names("url")
    
    date <- page %>%
      rvest::html_nodes("#games tbody th") %>%
      rvest::html_text() %>%
      tibble::enframe(name = NULL) %>%
      mutate_all(as.character) %>%
      purrr::set_names("date")
    
    away_team <- page %>%
      rvest::html_nodes("#games .left+ .left a") %>%
      rvest::html_text() %>%
      tibble::enframe(name = NULL) %>%
      purrr::set_names("away_team")
    
    home_team <- page %>%
      rvest::html_nodes("#games td~ .left a") %>%
      rvest::html_text() %>%
      tibble::enframe(name = NULL) %>%
      purrr::set_names("home_team")
    
    away_goals <- page %>%
      rvest::html_nodes("#games .right:nth-child(3)") %>%
      rvest::html_text() %>%
      tibble::enframe(name = NULL) %>%
      purrr::set_names("away_goals")
    
    home_goals <- page %>%
      rvest::html_nodes("#games .right~ .left+ .right") %>%
      rvest::html_text() %>%
      tibble::enframe(name = NULL) %>%
      purrr::set_names("home_goals")
    
    overtime <- page %>%
      rvest::html_nodes("#games td.center") %>%
      rvest::html_text() %>%
      tibble::enframe(name = NULL) %>%
      purrr::set_names("overtime")
    
    attendance <- page %>%
      rvest::html_nodes("#games .right:nth-child(7)") %>%
      rvest::html_text() %>%
      tibble::enframe(name = NULL) %>%
      purrr::set_names("attendance") %>%
      mutate(attendance = stringr::str_replace_all(attendance, c("," = "")))
    
    game_length <- page %>%
      rvest::html_nodes("#games .right+ .right") %>%
      rvest::html_text() %>%
      tibble::enframe(name = NULL) %>%
      purrr::set_names("messy_game_length") %>%
      mutate(hours = stringr::str_split(messy_game_length, "\\:", simplify = TRUE, n = 2)[,1]) %>%
      mutate(minutes = stringr::str_split(messy_game_length, "\\:", simplify = TRUE, n = 2)[,2]) %>%
      mutate(game_length = ifelse(messy_game_length != "", as.numeric(hours) * 60 + as.numeric(minutes), as.numeric(NA))) %>%
      select(game_length)
    
    game_notes <- page %>%
      rvest::html_nodes("#games .left") %>%
      rvest::html_text() %>%
      tibble::enframe(name = NULL) %>%
      purrr::set_names("game_notes") %>%
      filter(row_number() %% 4 == 0) %>%
      filter(game_notes != "Notes")
    
    schedule <- date %>%
      bind_cols(away_team) %>%
      bind_cols(away_goals) %>%
      bind_cols(home_team) %>%
      bind_cols(home_goals) %>%
      bind_cols(overtime) %>%
      bind_cols(attendance) %>%
      bind_cols(game_length) %>%
      bind_cols(game_notes) %>%
      mutate(season = season) %>%
      mutate(league = league) %>%
      filter(away_goals != "" | home_goals != "") %>%
      bind_cols(game_urls) %>%
      mutate(season = stringr::str_c(season - 1, stringr::str_sub(season, 3, 4), sep = "-")) %>%
      mutate_all(stringr::str_squish) %>%
      mutate_all(~na_if(., "")) %>%
      mutate_at(vars(away_goals, home_goals, attendance, game_length), as.numeric) %>%
      distinct() %>%
      select(league, season, date, away_team, away_goals, home_team, home_goals, overtime, attendance, game_length, game_notes, url)
        
    if (progress) {pb$tick()}
    
    return(schedule)
    
  }
  
  schedule_data <- purrr::map2_dfr(mydata[["league"]], mydata[["season"]], .get_schedule)
  
  cat("\n")
  
  return(schedule_data)
  
}

games <- get_schedule(c(1988:2004, 2006:2018))


get_box_score <- function(..., progress = TRUE, combine_home_and_away = TRUE) {
  
  if (!is.logical(progress)) {
    
    cat("\n")
    
    stop('\n\nMake sure you set "progress" to either
          \rTRUE or FALSE (it defaults to TRUE)\n\n')
    
  }
  
  if (!is.logical(combine_home_and_away)) {
    
    cat("\n")
    
    stop('\n\nMake sure you set "combine_home_and_away" to 
          \reither TRUE or FALSE (it defaults to TRUE)\n\n')
    
  }
  
  if (progress) {
    
    pb <- progress::progress_bar$new(format = "get_box_score() [:bar] :percent ETA: :eta", clear = FALSE, total = nrow(...), show_after = 0) 
    
    cat("\n")
    
    pb$tick(0)}
  
  .get_box_score <- function(date, away_team, home_team, season, league, url, ...) {
    
    seq(0.1, 0.3, by = 0.001) %>%
      sample(1) %>%
      Sys.sleep()
    
    game_info <- tibble(date, away_team, home_team, season, league, url)
    
    page <- xml2::read_html(url)
    
    away_team <- page %>%
      rvest::html_nodes(".filter div:nth-child(4) a") %>%
      rvest::html_text() %>%
      stringr::str_replace_all("Goalies", "")
    
    home_team <- page %>%
      rvest::html_nodes(".filter div:nth-child(6) a") %>%
      rvest::html_text() %>%
      stringr::str_replace_all("Goalies", "")
    
    scoring_table <- page %>%
      rvest::html_node("#scoring") %>%
      rvest::html_text()
    
    penalty_table <- page %>%
      rvest::html_node("#penalty") %>%
      rvest::html_text()
    
    away_skater_ids <- page %>%
      rvest::html_nodes('[data-stat="player"]') %>%
      as.character() %>%
      tibble::enframe(name = NULL) %>%
      filter(row_number() > which(stringr::str_detect(value, 'aria-label=\"Player\"'))[1]) %>%
      filter(row_number() < which(stringr::str_detect(value, 'TOTAL'))[1]) %>%
      mutate(player_id = stringr::str_split(value, 'data-append-csv=\"', simplify = TRUE, n = 2)[,2]) %>%
      mutate(player_id = stringr::str_split(player_id, '"', simplify = TRUE, n = 2)[,1]) %>%
      select(player_id)
    
    away_skaters <- page %>%
      rvest::html_nodes('[class="sortable stats_table"]') %>%
      purrr::pluck(1) %>%
      rvest::html_table()
    
    if (ncol(away_skaters) == 18) {
      
      away_skaters <- away_skaters %>%
        purrr::set_names("rank", "name", "goals", "assists", "points", "plus_minus", "penalty_minutes", "even_strength_goals", "power_play_goals", "shorthanded_goals", "game_winning_goals", "even_strength_assists", "power_play_assists", "shorthanded_assists", "shots", "shooting_percentage", "shifts", "time_on_ice")
      
    }
    
    else {
      
      away_skaters <- away_skaters %>%
        purrr::set_names("rank", "name", "goals", "assists", "points", "plus_minus", "penalty_minutes", "even_strength_goals", "power_play_goals", "shorthanded_goals", "game_winning_goals", "shots", "shooting_percentage", "shifts", "time_on_ice") %>%
        mutate(even_strength_assists = as.numeric(NA)) %>%
        mutate(power_play_assists = as.numeric(NA)) %>%
        mutate(shorthanded_assists = as.numeric(NA))
      
    }
      
    away_skaters <- away_skaters %>%
      as_tibble() %>%
      filter(row_number() > 1) %>%
      filter(name != "TOTAL") %>%
      bind_cols(away_skater_ids) %>%
      mutate(games_played = 1) %>%
      mutate(team = away_team) %>%
      mutate(season = season) %>%
      mutate(date = date) %>%
      mutate(away_team = away_team) %>%
      mutate(home_team = home_team) %>%
      mutate(league = league) %>%
      mutate(game_url = url) %>%
      mutate_at(vars(games_played, goals, assists, points, penalty_minutes, even_strength_goals, power_play_goals, shorthanded_goals, game_winning_goals, even_strength_assists, power_play_assists, shorthanded_assists, shots, shifts), ~ifelse(stringr::str_detect(., "^[0-9]+$"), as.numeric(.), as.numeric(NA))) %>%
      mutate(plus_minus = ifelse(stringr::str_detect(plus_minus, "^[-]?[0-9]+$"), as.numeric(plus_minus), as.numeric(NA))) %>%
      mutate(shooting_percentage = ifelse(stringr::str_detect(shooting_percentage, "^[0-9]{1,3}[.]{0,1}[0-9]{0,}+$"), as.numeric(shooting_percentage), as.numeric(NA))) %>%
      mutate(mins = stringr::str_split(time_on_ice, "\\:", simplify = TRUE, n = 2)[,1]) %>%
      mutate(secs = stringr::str_split(time_on_ice, "\\:", simplify = TRUE, n = 2)[,2]) %>%
      mutate(time_on_ice_secs = (as.numeric(mins) * 60) + as.numeric(secs)) %>%
      mutate(time_on_ice = round(time_on_ice_secs/60, 2)) %>%
      select(name, player_id, team, games_played, goals, assists, points, plus_minus, penalty_minutes, even_strength_goals, power_play_goals, shorthanded_goals, game_winning_goals, even_strength_assists, power_play_assists, shorthanded_assists, shots, shooting_percentage, shifts, time_on_ice, time_on_ice_secs, season, date, away_team, home_team, league, game_url)
    
    home_skater_ids <- page %>%
      rvest::html_nodes('[data-stat="player"]') %>%
      as.character() %>%
      tibble::enframe(name = NULL) %>%
      filter(row_number() > which(stringr::str_detect(value, 'aria-label=\"Player\"'))[3]) %>%
      filter(row_number() < which(stringr::str_detect(value, 'TOTAL'))[1]) %>%
      mutate(player_id = stringr::str_split(value, 'data-append-csv=\"', simplify = TRUE, n = 2)[,2]) %>%
      mutate(player_id = stringr::str_split(player_id, '"', simplify = TRUE, n = 2)[,1]) %>%
      select(player_id)
    
    home_skaters <- page %>%
      rvest::html_nodes('[class="sortable stats_table"]') %>%
      purrr::pluck(3) %>%
      rvest::html_table()
    
    if (ncol(home_skaters) == 18) {
      
      home_skaters <- home_skaters %>%
        purrr::set_names("rank", "name", "goals", "assists", "points", "plus_minus", "penalty_minutes", "even_strength_goals", "power_play_goals", "shorthanded_goals", "game_winning_goals", "even_strength_assists", "power_play_assists", "shorthanded_assists", "shots", "shooting_percentage", "shifts", "time_on_ice")
      
    }
    
    else {
      
      home_skaters <- home_skaters %>%
        purrr::set_names("rank", "name", "goals", "assists", "points", "plus_minus", "penalty_minutes", "even_strength_goals", "power_play_goals", "shorthanded_goals", "game_winning_goals", "shots", "shooting_percentage", "shifts", "time_on_ice") %>%
        mutate(even_strength_assists = as.numeric(NA)) %>%
        mutate(power_play_assists = as.numeric(NA)) %>%
        mutate(shorthanded_assists = as.numeric(NA))
      
    }
      
    home_skaters <- home_skaters %>%
      as_tibble() %>%
      filter(row_number() > 1) %>%
      filter(name != "TOTAL") %>%
      bind_cols(home_skater_ids) %>%
      mutate(games_played = 1) %>%
      mutate(team = home_team) %>%
      mutate(season = season) %>%
      mutate(date = date) %>%
      mutate(away_team = away_team) %>%
      mutate(home_team = home_team) %>%
      mutate(league = league) %>%
      mutate(game_url = url) %>%
      mutate_at(vars(games_played, goals, assists, points, penalty_minutes, even_strength_goals, power_play_goals, shorthanded_goals, game_winning_goals, even_strength_assists, power_play_assists, shorthanded_assists, shots, shifts), ~ifelse(stringr::str_detect(., "^[0-9]+$"), as.numeric(.), as.numeric(NA))) %>%
      mutate(plus_minus = ifelse(stringr::str_detect(plus_minus, "^[-]?[0-9]+$"), as.numeric(plus_minus), as.numeric(NA))) %>%
      mutate(shooting_percentage = ifelse(stringr::str_detect(shooting_percentage, "^[0-9]{1,3}[\\.]{0,1}[0-9]{0,}+$"), as.numeric(shooting_percentage), as.numeric(NA))) %>%
      mutate(mins = stringr::str_split(time_on_ice, "\\:", simplify = TRUE, n = 2)[,1]) %>%
      mutate(secs = stringr::str_split(time_on_ice, "\\:", simplify = TRUE, n = 2)[,2]) %>%
      mutate(time_on_ice_secs = (as.numeric(mins) * 60) + as.numeric(secs)) %>%
      mutate(time_on_ice = round(time_on_ice_secs/60, 2)) %>%
      select(name, player_id, team, games_played, goals, assists, points, plus_minus, penalty_minutes, even_strength_goals, power_play_goals, shorthanded_goals, game_winning_goals, even_strength_assists, power_play_assists, shorthanded_assists, shots, shooting_percentage, shifts, time_on_ice, time_on_ice_secs, season, date, away_team, home_team, league, game_url)
    
    away_goalie_ids <- page %>%
      rvest::html_nodes('[data-stat="player"]') %>%
      as.character() %>%
      tibble::enframe(name = NULL) %>%
      filter(row_number() > which(stringr::str_detect(value, 'aria-label=\"Player\"'))[2]) %>%
      filter(row_number() < which(stringr::str_detect(value, 'aria-label=\"Player\"'))[1]) %>%
      mutate(player_id = stringr::str_split(value, 'data-append-csv=\"', simplify = TRUE, n = 2)[,2]) %>%
      mutate(player_id = stringr::str_split(player_id, '"', simplify = TRUE, n = 2)[,1]) %>%
      select(player_id)
    
    away_goalies <- page %>%
      rvest::html_nodes('[class="sortable stats_table"]') %>%
      purrr::pluck(2) %>%
      rvest::html_table() %>%
      purrr::set_names("rank", "name", "decision", "goals_against", "shots_against", "saves", "save_percentage", "shutouts", "penalty_minutes", "time_on_ice") %>%
      as_tibble() %>%
      filter(row_number() > 1) %>%
      filter(name != "TOTAL") %>%
      bind_cols(away_goalie_ids) %>%
      mutate(games_played = 1) %>%
      mutate(team = away_team) %>%
      mutate(wins = ifelse(decision == "W", 1, 0)) %>%
      mutate(losses = ifelse(decision == "L", 1, 0)) %>%
      mutate(season = season) %>%
      mutate(date = date) %>%
      mutate(away_team = away_team) %>%
      mutate(home_team = home_team) %>%
      mutate(league = league) %>%
      mutate(game_url = url) %>%
      mutate_at(vars(games_played, goals_against, shots_against, saves, shutouts, penalty_minutes), ~ifelse(stringr::str_detect(., "^[0-9]+$"), as.numeric(.), as.numeric(NA))) %>%
      mutate(save_percentage = ifelse(stringr::str_detect(save_percentage, "^[1]{0,1}[\\.][0-9]{3,3}+$"), as.numeric(save_percentage), as.numeric(NA))) %>%
      mutate(mins = stringr::str_split(time_on_ice, "\\:", simplify = TRUE, n = 2)[,1]) %>%
      mutate(secs = stringr::str_split(time_on_ice, "\\:", simplify = TRUE, n = 2)[,2]) %>%
      mutate(time_on_ice_secs = (as.numeric(mins) * 60) + as.numeric(secs)) %>%
      mutate(time_on_ice = round(time_on_ice_secs/60, 2)) %>%
      select(name, player_id, team, games_played, wins, losses, goals_against, shots_against, saves, save_percentage, shutouts, penalty_minutes, time_on_ice, time_on_ice_secs, season, date, away_team, home_team, league, game_url)
    
    home_goalie_ids <- page %>%
      rvest::html_nodes('[data-stat="player"]') %>%
      as.character() %>%
      tibble::enframe(name = NULL) %>%
      filter(row_number() > which(stringr::str_detect(value, 'aria-label=\"Player\"'))[4]) %>%
      mutate(player_id = stringr::str_split(value, 'data-append-csv=\"', simplify = TRUE, n = 2)[,2]) %>%
      mutate(player_id = stringr::str_split(player_id, '"', simplify = TRUE, n = 2)[,1]) %>%
      select(player_id)
    
    home_goalies <- page %>%
      rvest::html_nodes('[class="sortable stats_table"]') %>%
      purrr::pluck(4) %>%
      rvest::html_table() %>%
      purrr::set_names("rank", "name", "decision", "goals_against", "shots_against", "saves", "save_percentage", "shutouts", "penalty_minutes", "time_on_ice") %>%
      as_tibble() %>%
      filter(row_number() > 1) %>%
      filter(name != "TOTAL") %>%
      bind_cols(home_goalie_ids) %>%
      mutate(games_played = 1) %>%
      mutate(team = home_team) %>%
      mutate(wins = ifelse(decision == "W", 1, 0)) %>%
      mutate(losses = ifelse(decision == "L", 1, 0)) %>%
      mutate(season = season) %>%
      mutate(date = date) %>%
      mutate(away_team = away_team) %>%
      mutate(home_team = home_team) %>%
      mutate(league = league) %>%
      mutate(game_url = url) %>%
      mutate_at(vars(games_played, goals_against, shots_against, saves, shutouts, penalty_minutes), ~ifelse(stringr::str_detect(., "^[0-9]+$"), as.numeric(.), as.numeric(NA))) %>%
      mutate(save_percentage = ifelse(stringr::str_detect(save_percentage, "^[1]{0,1}[\\.][0-9]{3,3}+$"), as.numeric(save_percentage), as.numeric(NA))) %>%
      mutate(mins = stringr::str_split(time_on_ice, "\\:", simplify = TRUE, n = 2)[,1]) %>%
      mutate(secs = stringr::str_split(time_on_ice, "\\:", simplify = TRUE, n = 2)[,2]) %>%
      mutate(time_on_ice_secs = (as.numeric(mins) * 60) + as.numeric(secs)) %>%
      mutate(time_on_ice = round(time_on_ice_secs/60, 2)) %>%
      select(name, player_id, team, games_played, wins, losses, goals_against, shots_against, saves, save_percentage, shutouts, penalty_minutes, time_on_ice, time_on_ice_secs, season, date, away_team, home_team, league, game_url)
    
    if (scoring_table != "Scoring Summary Table\n") {
      
      goal_info <- page %>%
        rvest::html_node("#scoring") %>% 
        rvest::html_table() %>% 
        purrr::set_names("time", "team", "messy_data") %>%
        as_tibble() %>%
        mutate(messy_data = stringr::str_replace_all(messy_data, "[\r\n\t]", "")) %>% 
        filter(!stringr::str_detect(time, "Period"))
      
      scoring_ids <- page %>%
        rvest::html_node("#scoring") %>% 
        as.character() %>% 
        stringr::str_split("\n<tr>", simplify = TRUE) %>% 
        t() %>% 
        as_tibble() %>% 
        purrr::set_names("messy_data") %>% 
        filter(row_number() > 1) %>%
        filter(!stringr::str_detect(messy_data, "successful")) %>%
        mutate(messy_data = stringr::str_split(messy_data, "href", simplify = TRUE, n = 2)[,2]) %>% 
        mutate(goal_id = stringr::str_split(messy_data, "\\/[a-z]\\/", simplify = TRUE, n = 2)[,2]) %>% 
        mutate(primary_assist_id = stringr::str_split(goal_id, "href", simplify = TRUE, n = 2)[,2]) %>% 
        mutate(primary_assist_id = stringr::str_split(primary_assist_id, "\\/[a-z]\\/", simplify = TRUE, n = 2)[,2]) %>% 
        mutate(secondary_assist_id = stringr::str_split(primary_assist_id, "href", simplify = TRUE, n = 2)[,2]) %>% 
        mutate(secondary_assist_id = stringr::str_split(secondary_assist_id, "\\/[a-z]\\/", simplify = TRUE, n = 2)[,2]) %>% 
        mutate_at(vars(goal_id, primary_assist_id, secondary_assist_id), ~stringr::str_split(., "\\.html", simplify = TRUE, n = 2)[,1]) %>% 
        select(goal_id, primary_assist_id, secondary_assist_id)
      
      if (any(stringr::str_detect(pull(goal_info, messy_data), "successful attempt\\b"))) {
        
        shootout_ids <- page %>%
          rvest::html_node("#scoring") %>% 
          as.character() %>% 
          stringr::str_split("\n<tr>", simplify = TRUE) %>% 
          t() %>% 
          as_tibble(.name_repair = NULL) %>% 
          purrr::set_names("messy_data") %>% 
          filter(row_number() > 1) %>%
          filter(stringr::str_detect(messy_data, "successful")) %>%
          mutate(messy_data = stringr::str_split(messy_data, "href", simplify = TRUE, n = 2)[,2]) %>% 
          mutate(shooter_id = stringr::str_split(messy_data, "\\/[a-z]\\/", simplify = TRUE, n = 2)[,2]) %>% 
          mutate(goalie_id = stringr::str_split(shooter_id, "href", simplify = TRUE, n = 2)[,2]) %>% 
          mutate(goalie_id = stringr::str_split(goalie_id, "\\/[a-z]\\/", simplify = TRUE, n = 2)[,2]) %>%
          mutate_at(vars(shooter_id, goalie_id), ~stringr::str_split(., "\\.html", simplify = TRUE, n = 2)[,1]) %>% 
          select(shooter_id, goalie_id)
        
        shootout_data <- goal_info %>%
          filter(stringr::str_detect(messy_data, "successful attempt\\b")) %>%
          bind_cols(shootout_ids) %>%
          mutate(shooter = stringr::str_split(messy_data, "\\ssuccessful|\\sunsuccessful", simplify = TRUE, n = 2)[,1]) %>% 
          mutate(goalie = stringr::str_split(messy_data, "versus\\s", simplify = TRUE, n = 2)[,2]) %>% 
          mutate(goal = ifelse(stringr::str_detect(messy_data, "\\bsuccessful\\b"), 1, 0)) %>% 
          mutate(season = season) %>%
          mutate(date = date) %>%
          mutate(away_team = away_team) %>%
          mutate(home_team = home_team) %>%
          mutate(league = league) %>%
          mutate(game_url = url) %>%
          mutate_all(stringr::str_squish) %>%
          mutate_all(~na_if(., "")) %>%
          mutate_at(vars(time, goal), as.numeric) %>%
          select(shooter_number = time, team, shooter, shooter_id, goalie, goalie_id, goal, season, date, away_team, home_team, league, game_url)
        
        box_score_data <- goal_info %>%
          filter(!stringr::str_detect(time, "Shootout")) %>%
          filter(!stringr::str_detect(messy_data, "successful attempt\\b"))
        
        if (nrow(box_score_data) != 0) {
        
          box_score_data <- box_score_data %>%
            bind_cols(scoring_ids) %>%
            mutate(game_strength = stringr::str_split(messy_data, "\U2014", simplify = TRUE, n = 2)[,1]) %>%
            mutate(game_strength = ifelse(game_strength == messy_data, "EV", game_strength)) %>%
            mutate(players = stringr::str_split(messy_data, "\U2014", simplify = TRUE, n = 2)[,2]) %>% 
            mutate(players = ifelse(players == "", messy_data, players)) %>%
            mutate(goal = stringr::str_split(players, "\\(", simplify = TRUE, n = 2)[,1]) %>%
            mutate(primary_assist = stringr::str_split(players, "\\)", simplify = TRUE)[,2]) %>%
            mutate(primary_assist = stringr::str_split(primary_assist, "\\sand", simplify = TRUE, n = 2)[,1]) %>%
            mutate(secondary_assist = stringr::str_split(players, "\\sand", simplify = TRUE, n = 2)[,2]) %>%
            mutate_all(stringr::str_squish) %>%
            mutate(season = season) %>%
            mutate(date = date) %>%
            mutate(away_team = away_team) %>%
            mutate(home_team = home_team) %>%
            mutate(league = league) %>%
            mutate(game_url = url) %>%
            mutate_all(stringr::str_squish) %>%
            mutate_all(~na_if(., "")) %>%
            select(time, game_strength, team, goal, primary_assist, secondary_assist, goal_id, primary_assist_id, secondary_assist_id, season, date, away_team, home_team, league, game_url)
          
        }
        
        else {
          
          box_score_data <- tibble()
          
        }

      }
      
      else {
        
        goal_info <- goal_info %>%
          bind_cols(scoring_ids) %>%
          mutate(game_strength = stringr::str_split(messy_data, "\U2014", simplify = TRUE, n = 2)[,1]) %>%
          mutate(game_strength = ifelse(game_strength == messy_data, "EV", game_strength)) %>%
          mutate(players = stringr::str_split(messy_data, "\U2014", simplify = TRUE, n = 2)[,2]) %>% 
          mutate(players = ifelse(players == "", messy_data, players)) %>%
          mutate(goal = stringr::str_split(players, "\\(", simplify = TRUE, n = 2)[,1]) %>%
          mutate(primary_assist = stringr::str_split(players, "\\)", simplify = TRUE)[,2]) %>%
          mutate(primary_assist = stringr::str_split(primary_assist, "\\sand", simplify = TRUE, n = 2)[,1]) %>%
          mutate(secondary_assist = stringr::str_split(players, "\\sand", simplify = TRUE, n = 2)[,2]) %>%
          mutate_all(stringr::str_squish) %>%
          select(time, team, game_strength, goal, primary_assist, secondary_assist, goal_id, primary_assist_id, secondary_assist_id)
        
        box_score_data <- goal_info %>%
          mutate(season = season) %>%
          mutate(date = date) %>%
          mutate(away_team = away_team) %>%
          mutate(home_team = home_team) %>%
          mutate(league = league) %>%
          mutate(game_url = url) %>%
          mutate_all(stringr::str_squish) %>%
          mutate_all(~na_if(., "")) %>%
          select(time, game_strength, team, goal, primary_assist, secondary_assist, goal_id, primary_assist_id, secondary_assist_id, season, date, away_team, home_team, league, game_url)
        
        
        shootout_data <- tibble()
        
      }
      
    }
    
    else {
      
      box_score_data <- tibble()
      
      shootout_data <- tibble()
      
    }
    
    if (penalty_table != "Penalty Summary Table\n") {
      
      penalty_info <- page %>%
        rvest::html_node("#penalty") %>%
        rvest::html_table() %>%
        purrr::set_names("time", "team", "messy_data") %>%
        as_tibble() %>%
        filter(!stringr::str_detect(time, "Period")) %>%
        mutate(penalty_taker = ifelse(stringr::str_detect(messy_data, "\\:"), stringr::str_split(messy_data, "\\:", simplify = TRUE, n = 2)[,1], NA)) %>%
        mutate(penalty_data = ifelse(stringr::str_detect(messy_data, "\\:"), stringr::str_split(messy_data, "\\:", simplify = TRUE, n = 2)[,2], messy_data)) %>%
        mutate(penalty_type = stringr::str_split(penalty_data, "\U2014", simplify = TRUE, n = 2)[,1]) %>%
        mutate(penalty_shooter = stringr::str_split(penalty_type, "penalty shot taken by", simplify = TRUE, n = 2)[,2]) %>%
        mutate(penalty_type = stringr::str_split(penalty_type, "penalty shot taken by", simplify = TRUE, n = 2)[,1]) %>%
        mutate(penalty_shot_goal = case_when(stringr::str_detect(penalty_shooter, "GOAL") & penalty_shooter != "" ~ 1, !stringr::str_detect(penalty_shooter, "GOAL") & penalty_shooter != "" ~ 0, TRUE ~ as.numeric(NA))) %>%
        mutate(penalty_shot_miss = case_when(stringr::str_detect(penalty_shooter, "MISS") & penalty_shooter != "" ~ 1, !stringr::str_detect(penalty_shooter, "MISS") & penalty_shooter != "" ~ 0, TRUE ~ as.numeric(NA))) %>%
        mutate(penalty_shot_save = case_when(stringr::str_detect(penalty_shooter, "SAVED") & penalty_shooter != "" ~ 1, !stringr::str_detect(penalty_shooter, "SAVED") & penalty_shooter != "" ~ 0, TRUE ~ as.numeric(NA))) %>%
        mutate(penalty_shooter = stringr::str_split(penalty_shooter, "\\-", simplify = TRUE, n = 2)[,1]) %>%
        mutate(penalty_mins = stringr::str_split(penalty_data, "\U2014", simplify = TRUE, n = 2)[,2]) %>%
        mutate(penalty_mins = stringr::str_replace_all(penalty_mins, c("min" = "")))
      
      penalty_ids <- page %>%
        rvest::html_node("#penalty") %>% 
        as.character() %>% 
        stringr::str_split("\n<tr>", simplify = TRUE) %>% 
        t() %>% 
        as_tibble(.name_repair = "minimal") %>% 
        purrr::set_names("messy_data") %>% 
        filter(row_number() > 1) %>%
        mutate(messy_data = stringr::str_split(messy_data, "href", simplify = TRUE, n = 2)[,2]) %>% 
        mutate(penalty_taker_id = stringr::str_split(messy_data, "\\/[a-z]\\/", simplify = TRUE, n = 2)[,2]) %>% 
        mutate(penalty_shooter_id = stringr::str_split(penalty_taker_id, "href", simplify = TRUE, n = 2)[,2]) %>% 
        mutate(penalty_shooter_id = stringr::str_split(penalty_shooter_id, "\\/[a-z]\\/", simplify = TRUE, n = 2)[,2]) %>%
        mutate_at(vars(penalty_taker_id, penalty_shooter_id), ~stringr::str_split(., "\\.html", simplify = TRUE, n = 2)[,1]) %>% 
        select(penalty_taker_id, penalty_shooter_id)
      
      penalty_data <- penalty_info %>%
        bind_cols(penalty_ids) %>%
        mutate(season = season) %>%
        mutate(date = date) %>%
        mutate(away_team = away_team) %>%
        mutate(home_team = home_team) %>%
        mutate(league = league) %>%
        mutate(game_url = url) %>%
        mutate_all(stringr::str_squish) %>%
        mutate_all(~na_if(., "")) %>%
        mutate(penalty_mins = ifelse(stringr::str_detect(penalty_mins, "^[0-9]+$"), as.numeric(penalty_mins), as.numeric(NA))) %>%
        mutate_at(vars(penalty_shot_goal, penalty_shot_miss, penalty_shot_save), as.numeric) %>%
        select(time, team, penalty_taker, penalty_taker_id, penalty_type, penalty_mins, penalty_shooter, penalty_shooter_id, penalty_shot_goal, penalty_shot_miss, penalty_shot_save, season, date, away_team, home_team, league, game_url)
      
    }
    
    else {
      
      penalty_data <- tibble()
      
    }
    
    skaters <- away_skaters %>% 
      bind_rows(home_skaters)
    
    goalies <- away_goalies %>% 
      bind_rows(home_goalies)
    
    game_info <- game_info %>%
      rename(home_team_long = home_team, away_team_long = away_team) %>%
      mutate(home_team = home_team) %>%
      mutate(away_team = away_team) %>%
      mutate(away_skater_names = stringr::str_c(pull(away_skaters, name), collapse = "|")) %>%
      mutate(away_skater_ids = stringr::str_c(pull(away_skaters, player_id), collapse = "|")) %>%
      mutate(home_skater_names = stringr::str_c(pull(home_skaters, name), collapse = "|")) %>%
      mutate(home_skater_ids = stringr::str_c(pull(home_skaters, player_id), collapse = "|")) %>%
      bind_cols(tidyr::nest(box_score_data)) %>%
      bind_cols(tidyr::nest(penalty_data)) %>%
      bind_cols(tidyr::nest(skaters)) %>%
      bind_cols(tidyr::nest(goalies)) %>%
      bind_cols(tidyr::nest(shootout_data)) %>%
      rename(goals = data, penalties = data1, skaters = data2, goalies = data3, shootout = data4)
    
    if (progress) {pb$tick()}
    
    if (combine_home_and_away) {
      
      game_data <- list(goals = box_score_data, 
                        penalties = penalty_data, 
                        skaters = skaters,
                        goalies = goalies,
                        shootout = shootout_data,
                        game_info = game_info)
    }
    
    else {
      
      game_data <- list(goals = box_score_data, 
                        penalties = penalty_data, 
                        away_skaters = away_skaters, 
                        home_skaters = home_skaters, 
                        away_goalies = away_goalies, 
                        home_goalies = home_goalies, 
                        shootout = shootout_data,
                        game_info = game_info)
    }
    
    return(game_data)
    
  }
  
  persistently_get_box_score <- elite::persistently(.get_box_score, max_attempts = 10, wait_seconds = 0.0001)
  
  try_get_box_score <- function(date, away_team, home_team, season, league, url, ...) {
    
    tryCatch(persistently_get_box_score(date, away_team, home_team, season, league, url, ...), 
             
             error = function(e) {
               cat("\n\n")
               print(e) 
               cat("\n")
               print(url)
               tibble()},
             
             warning = function(w) {
               cat("\n\n")
               print(w) 
               cat("\n")
               print(url)
               tibble()})
  }
  
  all_data <- purrr::pmap(..., try_get_box_score) %>% 
    purrr::transpose() %>% 
    purrr::map(bind_rows)
  
  cat("\n")
  
  return(all_data)
  
}
