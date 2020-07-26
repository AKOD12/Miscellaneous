require(readr)
require(dplyr)
require(xml2)
require(magrittr)
get_ncaa_baseball_roster <- function(teamid = NA,
                                     year = 2019) {
  
  id <- subset(ncaa_season_id_lu, season == year, select = id)
  
  school_info <- subset(master_ncaa_team_lu, school_id == teamid &
                          year == year) %>%
    dplyr::select(-year) %>%
    dplyr::distinct()
  
  url <- paste0("https://stats.ncaa.org/team/", teamid, "/roster/", id)
  
  payload <- xml2::read_html(url)
  
  payload <- payload %>%
    rvest::html_nodes("table") %>%
    .[1] %>%
    rvest::html_nodes("tr")
  
  parse_roster_table <- function(trs) {
    
    number <- trs %>%
      rvest::html_nodes("td") %>%
      .[1] %>%
      rvest::html_text()
    
    name <- trs %>%
      rvest::html_nodes("td") %>%
      .[2] %>%
      rvest::html_text()
    
    position <- trs %>%
      rvest::html_nodes("td") %>%
      .[3] %>%
      rvest::html_text()
    
    class <- trs %>%
      rvest::html_nodes("td") %>%
      .[4] %>%
      rvest::html_text()
    
    url_slug <- trs %>%
      rvest::html_nodes("td") %>%
      .[2] %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href")
    
    url_slug <- ifelse(url_slug %>%
                         as.data.frame() %>%
                         dplyr::rename(var = '.') %>%
                         nrow() == 0, NA, url_slug)
    
    player_id <- gsub(".*stats_player_seq=\\s*", "", url_slug)
    
    payload <- tibble::tibble(name = name,
                              class = class,
                              player_id = player_id,
                              number = number,
                              position = position,
                              url_slug = url_slug)
    
    payload
  }}
ncaa_scrape <- function(teamid, year, type = 'batting') {
  
  if (year < 2013) {
    stop('you must provide a year that is equal to or greater than 2013')
  }
  
  else
    if (type == "batting") {
      id <- subset(ncaa_season_id_lu, season == year, select = id)
      url <- paste0("http://stats.ncaa.org/team/",teamid,"/stats?game_sport_year_ctl_id=", id, "&id=", id)
      data_read <- xml2::read_html(url)
      data <- data_read %>%
        rvest::html_nodes("table") %>%
        .[[3]] %>%
        rvest::html_table(fill = TRUE)
      df <- as.data.frame(data)
      df$year <- year
      df$teamid <- teamid
      df <- df %>%
        dplyr::left_join(master_ncaa_team_lu,
                         by = c("teamid" = "school_id", "year" = "year"))
      df <- dplyr::select(df, year, school, conference, division, everything())
      df$Player <- gsub("x ", "", df$Player)
      if (!"RBI2out" %in% names(df)) {
        df$RBI2out <- NA
      }
      
      if('OPP DP' %in% colnames(df) == TRUE) {
        
        df <- df %>%
          dplyr::rename(DP = `OPP DP`)
      }
      
      df <- dplyr::select(df,year,school,conference,division,Jersey,Player,
                          Yr,Pos,GP,GS,BA,OBPct,SlgPct,R,AB,H,`2B`,`3B`,TB,
                          HR,RBI,BB,HBP,SF,SH,K,DP,CS,Picked,SB,RBI2out,teamid,conference_id)
    }
  
  else {
    year_id <- subset(ncaa_season_id_lu, season == year, select = id)
    type_id <- subset(ncaa_season_id_lu, season == year, select = pitching_id)
    url <- paste0("http://stats.ncaa.org/team/", teamid, "/stats?id=", year_id, "&year_stat_category_id=", type_id)
    data_read <- xml2::read_html(url)
    data <- data_read %>%
      rvest::html_nodes("table") %>%
      .[[3]] %>%
      rvest::html_table(fill = TRUE)
    df <- as.data.frame(data)
    df <- df[,-6]
    df$year <- year
    df$teamid <- teamid
    df <- df %>%
      dplyr::left_join(master_ncaa_team_lu, by = c("teamid" = "school_id", "year" = "year"))
    df <- dplyr::select(df, year, school, conference, division, everything())
    df$Player <- gsub("x ", "", df$Player)
    df <- dplyr::select(df, year,school,conference,division,Jersey,Player,
                        Yr,Pos,GP,App,GS,ERA,IP,H,R,ER,BB,SO,SHO,BF,`P-OAB`,
                        `2B-A`,`3B-A`,Bk,`HR-A`,WP,HB,IBB,`Inh Run`,`Inh Run Score`,
                        SHA,SFA,Pitches,GO,FO,W,L,SV,KL,teamid,conference_id)
  }
  
  player_url <- data_read %>%
    html_nodes('#stat_grid a') %>%
    html_attr('href') %>%
    as.data.frame() %>%
    rename(player_url = '.') %>%
    mutate(player_url = paste0('http://stats.ncaa.org', player_url))
  
  player_names_join <- data_read %>%
    html_nodes('#stat_grid a') %>%
    html_text() %>%
    as.data.frame() %>%
    rename(player_names_join = '.')
  
  player_id <-
    stringr::str_split(pattern = '&stats_player_seq=',  string = player_url$player_url,simplify = T)[,2] %>%
    as.data.frame() %>%
    rename(player_id = '.')
  
  player_url_comb <- bind_cols(player_names_join, player_id, player_url)
  
  df <- left_join(df,
                  player_url_comb, by = c('Player' = 'player_names_join'))
  
  return(df)
  
}
princeton_batting<-ncaa_scrape(554,2017,type = "batting")
view(princeton_batting)
