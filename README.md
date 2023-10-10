Project 2
================
Michael Dolan

# Required Packages

``` r
library(httr)
library(jsonlite)
library(tidyverse)
```

``` r
Player_Search <- GET(url="https://www.balldontlie.io/api/v1/teams/?search=jayson+tatum")
str(Players, max.level=1)
```

    ## List of 10
    ##  $ url        : chr "https://www.balldontlie.io/api/v1/players/?search=Kevin+Durant&?season=2012"
    ##  $ status_code: int 200
    ##  $ headers    :List of 17
    ##   ..- attr(*, "class")= chr [1:2] "insensitive" "list"
    ##  $ all_headers:List of 1
    ##  $ cookies    :'data.frame': 0 obs. of  7 variables:
    ##  $ content    : raw [1:358] 7b 22 64 61 ...
    ##  $ date       : POSIXct[1:1], format: "2023-10-08 20:42:51"
    ##  $ times      : Named num [1:6] 0 0.409 0.449 0.521 0.573 ...
    ##   ..- attr(*, "names")= chr [1:6] "redirect" "namelookup" "connect" "pretransfer" ...
    ##  $ request    :List of 7
    ##   ..- attr(*, "class")= chr "request"
    ##  $ handle     :Class 'curl_handle' <externalptr> 
    ##  - attr(*, "class")= chr "response"

``` r
Player <- fromJSON(rawToChar(Player_Search$content))
str(parsed, max.level=1)
```

    ## List of 2
    ##  $ data:'data.frame':    1 obs. of  8 variables:
    ##  $ meta:List of 5

``` r
Player$data$team
```

    ## NULL

``` r
PlayerID <- Player$data[1,1]

Stats_Query <- GET(url =
               "https://www.balldontlie.io/api/v1/stats/?per_page=100&page=2&seasons[]=2022&player_ids[]=434")
Stats <- fromJSON(rawToChar(Stats_Query$content))
Stats$data$game
```

    ##        id                     date home_team_id home_team_score period postseason
    ## 1 1007826 2023-05-27T00:00:00.000Z           16             103      4       TRUE
    ## 2 1007828 2023-05-29T00:00:00.000Z            2              84      4       TRUE
    ##   season status  time visitor_team_id visitor_team_score
    ## 1   2022  Final Final               2                104
    ## 2   2022  Final Final              16                103

``` r
player_game_stats <- function(first_name=NULL, last_name=NULL,
                              season=NULL, postseason=NULL,
                              stats=everything(), ...){
  
              # Return Error message if First and Last name not specified
                      if (is.null(first_name) | is.null(last_name)) {message("Error: First and last name of an NBA player must be specified.")}
                      else {
                        
                      # Find the player's PlayerID to be used later
                      player_search_url <- paste("https://www.balldontlie.io/api/v1/players/?search", paste(first_name, last_name, sep = "+"), sep = "=")
                      player_search <- GET(url=player_search_url)
                      player_info <- fromJSON(rawToChar(player_search$content))
                      playerID <- player_info$data[1,1]
                      
                      # Use the PlayerID to return the players stats for
                      # every game in a season or several seasons
                      stats_url <- paste0("https://www.balldontlie.io/api/v1/stats/?per_page=100",
                                if_else(is.null(season), "",
                                       paste0("&seasons[]=",
                                         paste(season, collapse="&seasons[]=")
                                        )),
                                paste0("&player_ids[]=", playerID),
                                ifelse(is.null(postseason), "",
                                       paste0("&postseason=", postseason))
                                ) 
                      player_stats_api <- GET(url = stats_url)
                      player_content <-
                        fromJSON(rawToChar(player_stats_api$content))
                      player_stats1 <- player_content$data
                      
                  # This API only allows 100 rows to be returned at a time,
                  # so if there are more than 100, a loop is initialized.
                  # The loop pulls the data for each page after the first,
                  # and stores all pages into a list.
                      total_pages <- player_content$meta$total_pages
                      if (total_pages > 1) {
                        page_list <- list()
                        page_list[[1]] <- player_stats1
                        for (i in 2:total_pages) {
                          stats_url <- paste0("https://www.balldontlie.io/api/v1/stats/?per_page=100",
                                paste0("&page=", i),
                                if_else(is.null(season), "",
                                       paste0("&seasons[]=",
                                         paste(season, collapse="&seasons[]=")
                                        )),
                                paste0("&player_ids[]=", playerID),
                                ifelse(is.null(postseason), "",
                                       paste0("&postseason=", postseason))
                                ) 
                      player_stats_api <- GET(url = stats_url)
                      player_content <-
                        fromJSON(rawToChar(player_stats_api$content))
                        page_list[[i]] <- player_content$data
                        }
                        
                        # Each dataset in the list is combine vertically
                        player_stats <- bind_rows(page_list)
                      }
                      
                        # If there is only one page,
                        # that page is stored as player_stats
                      else{player_stats <- player_stats1}
                      
                      # Create a cleaner date variable
                      player_stats$date <-
                        substr(player_stats$game$date, 1, 10)
                      
                      # Create a Home/Away game variable
                      player_stats$home_away <-
                      if_else(player_stats$team$id == player_stats$game$home_team_id, "Home", "Away")
                      
                      # Create a Win/Loss game variable
                      player_stats$win_loss <-
                        if_else((player_stats$home_away == "Home" &
                                player_stats$game$home_team_score > 
                                player_stats$game$visitor_team_score) | 
                                (player_stats$home_away == "Away" &
                                player_stats$game$visitor_team_score > 
                                player_stats$game$home_team_score),
                                "Win", "Loss")
                      
                      # Create visible score variables
                      player_stats$team_score <- if_else(
                                player_stats$home_away == "Home",
                                player_stats$game$home_team_score,
                                player_stats$game$visitor_team_score)
                      player_stats$opponent_score <- if_else(
                                player_stats$home_away == "Home",
                                player_stats$game$visitor_team_score,
                                player_stats$game$home_team_score)
                      
                      # Create a visible Post-Season variable
                      player_stats$post_season <- player_stats$game$postseason
                      
                      # Create a visible player_team variable
                      player_stats$player_team <- player_stats$team$full_name
                      
                      # Create opponent_team_id variable
                      player_stats$opponent_team_id <-
                        if_else(player_stats$home_away=="Home",
                                player_stats$game$visitor_team_id,
                                player_stats$game$home_team_id)
                      
                      # Create an opponent_team variable
                      # Note: The original dataset only provides team ID
                      # for the opposing team.
                      # Instead of pulling a team dataset with another
                      # API URL and merging the two datasets, which would wear
                      # on the API, I decided to create a codebook of teams
                      # since there are only 30 that need hardcoding.
                      player_stats$opponent_team <-
                      if_else(player_stats$opponent_team_id==1,
                              "Atlanta Hawks",
                      if_else(player_stats$opponent_team_id==2,
                              "Boston Celtics",
                      if_else(player_stats$opponent_team_id==3,
                              "Brooklyn Nets",
                      if_else(player_stats$opponent_team_id==4,
                              "Charlotte Hornets",
                      if_else(player_stats$opponent_team_id==5,
                              "Chicago Bulls",
                      if_else(player_stats$opponent_team_id==6,
                              "Cleveland Cavaliers",
                      if_else(player_stats$opponent_team_id==7,
                              "Dallas Mavericks",
                      if_else(player_stats$opponent_team_id==8,
                              "Denver Nuggets",
                      if_else(player_stats$opponent_team_id==9,
                              "Detroit Pistons",
                      if_else(player_stats$opponent_team_id==10,
                              "Golden State Warriors",
                      if_else(player_stats$opponent_team_id==11,
                              "Houston Rockets",
                      if_else(player_stats$opponent_team_id==12,
                              "Indiana Pacers",
                      if_else(player_stats$opponent_team_id==13,
                              "LA Clippers",
                      if_else(player_stats$opponent_team_id==14,
                              "Los Angeles Lakers",
                      if_else(player_stats$opponent_team_id==15,
                              "Memphis Grizzlies",
                      if_else(player_stats$opponent_team_id==16,
                              "Miami Heat",
                      if_else(player_stats$opponent_team_id==17,
                              "Milwaukee Bucks",
                      if_else(player_stats$opponent_team_id==18,
                              "Minnesota Timberwolves",
                      if_else(player_stats$opponent_team_id==19,
                              "New Orleans Pelicans",
                      if_else(player_stats$opponent_team_id==20,
                              "New York Knicks",
                      if_else(player_stats$opponent_team_id==21,
                              "Oklahoma City Thunder",
                      if_else(player_stats$opponent_team_id==22,
                              "Orlando Magic",
                      if_else(player_stats$opponent_team_id==23,
                              "Philadelphia 76ers",
                      if_else(player_stats$opponent_team_id==24,
                              "Phoenix Suns",
                      if_else(player_stats$opponent_team_id==25,
                              "Portland Trail Blazers",
                      if_else(player_stats$opponent_team_id==26,
                              "Sacramento Kings",
                      if_else(player_stats$opponent_team_id==27,
                              "San Antonio Spurs",
                      if_else(player_stats$opponent_team_id==28,
                              "Toronto Raptors",
                      if_else(player_stats$opponent_team_id==29,
                              "Utah Jazz",
                      if_else(player_stats$opponent_team_id==30,
                              "Washington Wizards", "ERROR"
                      ))))))))))))))))))))))))))))))
                      
                      # Sort games by date
                      player_stats_sorted <- arrange(player_stats, date)
                      
                      # Select important stats and reorder columns
                      player_stats_clean <- player_stats_sorted %>%
                        select(date, min, pts, reb, ast, stl, blk, fgm, fga,
                               fg_pct, fg3m, fg3a, fg3_pct, ftm, fta, ft_pct,
                               oreb, dreb, turnover, pf, home_away, win_loss,
                               player_team, team_score, opponent_team,
                               opponent_score, post_season)
                      
                      # Allow user to select variables and order,
                      # but always keeping the date for reference
                      player_stats_final <- player_stats_clean %>%
                        select(date, stats)
                      
                      # Print final player stats
                      player_stats_final
                      }
}
```

``` r
tatum_2022 <- player_game_stats(first_name = "jayson", last_name = "tatum", stats = "pts")
tatum_2022
```

    ##           date pts
    ## 1   2017-10-17  14
    ## 2   2017-10-18   8
    ## 3   2017-10-20  15
    ## 4   2017-10-24  22
    ## 5   2017-10-26  12
    ## 6   2017-10-28  20
    ## 7   2017-10-30   7
    ## 8   2017-11-01  12
    ## 9   2017-11-03  13
    ## 10  2017-11-05  13
    ## 11  2017-11-06  21
    ## 12  2017-11-08   5
    ## 13  2017-11-10  16
    ## 14  2017-11-12  13
    ## 15  2017-11-14  19
    ## 16  2017-11-16  12
    ## 17  2017-11-18  14
    ## 18  2017-11-20  15
    ## 19  2017-11-22  18
    ## 20  2017-11-24  11
    ## 21  2017-11-25  11
    ## 22  2017-11-27  10
    ## 23  2017-11-30  15
    ## 24  2017-12-02  15
    ## 25  2017-12-04  17
    ## 26  2017-12-06  17
    ## 27  2017-12-08  20
    ## 28  2017-12-10  11
    ## 29  2017-12-11   4
    ## 30  2017-12-13  15
    ## 31  2017-12-15   7
    ## 32  2017-12-16  19
    ## 33  2017-12-18  16
    ## 34  2017-12-20  11
    ## 35  2017-12-21  17
    ## 36  2017-12-23  13
    ## 37  2017-12-25  20
    ## 38  2017-12-27  18
    ## 39  2017-12-28  19
    ## 40  2017-12-31   9
    ## 41  2018-01-03  15
    ## 42  2018-01-05   4
    ## 43  2018-01-06  14
    ## 44  2018-01-11  16
    ## 45  2018-01-16  10
    ## 46  2018-01-18  11
    ## 47  2018-01-21   9
    ## 48  2018-01-23   4
    ## 49  2018-01-24  18
    ## 50  2018-01-27   4
    ## 51  2018-01-29  20
    ## 52  2018-01-31  15
    ## 53  2018-02-02  27
    ## 54  2018-02-04  17
    ## 55  2018-02-06   4
    ## 56  2018-02-08  11
    ## 57  2018-02-09  15
    ## 58  2018-02-11   9
    ## 59  2018-02-14  10
    ## 60  2018-02-23  15
    ## 61  2018-02-24  11
    ## 62  2018-02-26   5
    ## 63  2018-02-28   7
    ## 64  2018-03-03  12
    ## 65  2018-03-05  14
    ## 66  2018-03-08  12
    ## 67  2018-03-11  19
    ## 68  2018-03-14  19
    ## 69  2018-03-16   8
    ## 70  2018-03-18  23
    ## 71  2018-03-20  23
    ## 72  2018-03-23  13
    ## 73  2018-03-25  12
    ## 74  2018-03-26  23
    ## 75  2018-03-28  16
    ## 76  2018-03-31  24
    ## 77  2018-04-03  20
    ## 78  2018-04-04   8
    ## 79  2018-04-08  19
    ## 80  2018-04-10  12
    ## 81  2018-04-11  NA
    ## 82  2018-04-15  19
    ## 83  2018-04-17   4
    ## 84  2018-04-20  14
    ## 85  2018-04-22  21
    ## 86  2018-04-24   8
    ## 87  2018-04-26  22
    ## 88  2018-04-28  20
    ## 89  2018-04-30  28
    ## 90  2018-05-03  21
    ## 91  2018-05-05  24
    ## 92  2018-05-07  20
    ## 93  2018-05-09  25
    ## 94  2018-05-13  16
    ## 95  2018-05-15  11
    ## 96  2018-05-19  18
    ## 97  2018-05-21  17
    ## 98  2018-05-23  24
    ## 99  2018-05-25  15
    ## 100 2018-05-27  24
    ## 101 2018-10-16  23
    ## 102 2018-10-19  16
    ## 103 2018-10-20  24
    ## 104 2018-10-22   7
    ## 105 2018-10-25  24
    ## 106 2018-10-27   6
    ## 107 2018-10-30  16
    ## 108 2018-11-01  12
    ## 109 2018-11-03  14
    ## 110 2018-11-05  15
    ## 111 2018-11-08   4
    ## 112 2018-11-09  21
    ## 113 2018-11-11  27
    ## 114 2018-11-14  14
    ## 115 2018-11-16  21
    ## 116 2018-11-17  10
    ## 117 2018-11-19  20
    ## 118 2018-11-21  15
    ## 119 2018-11-23  14
    ## 120 2018-11-24  21
    ## 121 2018-11-26  20
    ## 122 2018-11-30  13
    ## 123 2018-12-01  19
    ## 124 2018-12-06  17
    ## 125 2018-12-08  18
    ## 126 2018-12-10  21
    ## 127 2018-12-12  12
    ## 128 2018-12-14  22
    ## 129 2018-12-15  17
    ## 130 2018-12-19  18
    ## 131 2018-12-21  20
    ## 132 2018-12-23  17
    ## 133 2018-12-25  23
    ## 134 2018-12-27   4
    ## 135 2018-12-29   7
    ## 136 2018-12-31  12
    ## 137 2019-01-02   8
    ## 138 2019-01-04  18
    ## 139 2019-01-07  16
    ## 140 2019-01-09  20
    ## 141 2019-01-10  17
    ## 142 2019-01-12  16
    ## 143 2019-01-14  34
    ## 144 2019-01-16  16
    ## 145 2019-01-18   2
    ## 146 2019-01-19  19
    ## 147 2019-01-21  19
    ## 148 2019-01-23  15
    ## 149 2019-01-26  20
    ## 150 2019-01-28   6
    ## 151 2019-01-30  20
    ## 152 2019-02-01  13
    ## 153 2019-02-03  11
    ## 154 2019-02-05  25
    ## 155 2019-02-07  22
    ## 156 2019-02-09  16
    ## 157 2019-02-12  20
    ## 158 2019-02-13  19
    ## 159 2019-02-21  17
    ## 160 2019-02-23  12
    ## 161 2019-02-26  11
    ## 162 2019-02-27  14
    ## 163 2019-03-01  10
    ## 164 2019-03-03  12
    ## 165 2019-03-05  17
    ## 166 2019-03-06  24
    ## 167 2019-03-09   6
    ## 168 2019-03-14  15
    ## 169 2019-03-16  18
    ## 170 2019-03-18   8
    ## 171 2019-03-20  13
    ## 172 2019-03-23  12
    ## 173 2019-03-26  21
    ## 174 2019-03-29  11
    ## 175 2019-03-30   9
    ## 176 2019-04-01  19
    ## 177 2019-04-03  16
    ## 178 2019-04-05  22
    ## 179 2019-04-07   0
    ## 180 2019-04-09   0
    ## 181 2019-04-14  15
    ## 182 2019-04-17  26
    ## 183 2019-04-19  18
    ## 184 2019-04-21  18
    ## 185 2019-04-28   4
    ## 186 2019-04-30   5
    ## 187 2019-05-03  20
    ## 188 2019-05-06  17
    ## 189 2019-05-08  14
    ## 190 2019-10-23  21
    ## 191 2019-10-25  25
    ## 192 2019-10-26  15
    ## 193 2019-10-30  25
    ## 194 2019-11-01  24
    ## 195 2019-11-05  18
    ## 196 2019-11-07  23
    ## 197 2019-11-09  19
    ## 198 2019-11-11   5
    ## 199 2019-11-13  23
    ## 200 2019-11-15  24
    ## 201 2019-11-17  14
    ## 202 2019-11-18  26
    ## 203 2019-11-20  30
    ## 204 2019-11-22  16
    ## 205 2019-11-25  20
    ## 206 2019-11-27  16
    ## 207 2019-11-29  26
    ## 208 2019-12-01  30
    ## 209 2019-12-04  19
    ## 210 2019-12-06  26
    ## 211 2019-12-09  19
    ## 212 2019-12-11  16
    ## 213 2019-12-12  15
    ## 214 2019-12-18  24
    ## 215 2019-12-20  26
    ## 216 2019-12-22  39
    ## 217 2019-12-25  11
    ## 218 2019-12-27  30
    ## 219 2019-12-28  12
    ## 220 2019-12-31  24
    ## 221 2020-01-03  13
    ## 222 2020-01-04  28
    ## 223 2020-01-06  17
    ## 224 2020-01-08  14
    ## 225 2020-01-09  15
    ## 226 2020-01-11  41
    ## 227 2020-01-13  21
    ## 228 2020-01-16  17
    ## 229 2020-01-18  26
    ## 230 2020-01-20  27
    ## 231 2020-01-22  23
    ## 232 2020-01-24   0
    ## 233 2020-01-26   0
    ## 234 2020-01-28   0
    ## 235 2020-01-30  20
    ## 236 2020-02-01  25
    ## 237 2020-02-03  28
    ## 238 2020-02-05  33
    ## 239 2020-02-07  32
    ## 240 2020-02-09  26
    ## 241 2020-02-11  15
    ## 242 2020-02-13  39
    ## 243 2020-02-21  28
    ## 244 2020-02-23  41
    ## 245 2020-02-25  36
    ## 246 2020-02-26  33
    ## 247 2020-02-29  32
    ## 248 2020-03-04  32
    ## 249 2020-03-06  18
    ## 250 2020-03-08  19
    ## 251 2020-03-10  30
    ## 252 2020-07-31   5
    ## 253 2020-08-02  34
    ## 254 2020-08-04  23
    ## 255 2020-08-05  19
    ## 256 2020-08-07  18
    ## 257 2020-08-09  29
    ## 258 2020-08-11  29
    ## 259 2020-08-17  32
    ## 260 2020-08-19  33
    ## 261 2020-08-21  15
    ## 262 2020-08-23  28
    ## 263 2020-08-30  21
    ## 264 2020-09-01  34
    ## 265 2020-09-03  15
    ## 266 2020-09-05  24
    ## 267 2020-09-07  18
    ## 268 2020-09-09  29
    ## 269 2020-09-11  29
    ## 270 2020-09-15  30
    ## 271 2020-09-17  21
    ## 272 2020-09-19  25
    ## 273 2020-09-23  28
    ## 274 2020-09-25  31
    ## 275 2020-09-27  24
    ## 276 2020-12-23  30
    ## 277 2020-12-25  20
    ## 278 2020-12-27  25
    ## 279 2020-12-29  27
    ## 280 2020-12-30  16
    ## 281 2021-01-01  28
    ## 282 2021-01-03  24
    ## 283 2021-01-04  40
    ## 284 2021-01-06  27
    ## 285 2021-01-08  32
    ## 286 2021-01-12   9
    ## 287 2021-01-13  23
    ## 288 2021-01-25  24
    ## 289 2021-01-27  25
    ## 290 2021-01-30  30
    ## 291 2021-02-02  27
    ## 292 2021-02-03  27
    ## 293 2021-02-05  34
    ## 294 2021-02-07  23
    ## 295 2021-02-09  23
    ## 296 2021-02-12  17
    ## 297 2021-02-14  33
    ## 298 2021-02-14   6
    ## 299 2021-02-16  21
    ## 300 2021-02-17  35
    ## 301 2021-02-19  25
    ## 302 2021-02-21  32
    ## 303 2021-02-23  28
    ## 304 2021-02-24  13
    ## 305 2021-02-26   9
    ## 306 2021-02-28  31
    ## 307 2021-03-02  14
    ## 308 2021-03-04  27
    ## 309 2021-03-11  31
    ## 310 2021-03-14  23
    ## 311 2021-03-16  29
    ## 312 2021-03-17  29
    ## 313 2021-03-19  15
    ## 314 2021-03-22   0
    ## 315 2021-03-24  18
    ## 316 2021-03-26  34
    ## 317 2021-03-27  27
    ## 318 2021-03-29  34
    ## 319 2021-03-31  25
    ## 320 2021-04-02  26
    ## 321 2021-04-04  22
    ## 322 2021-04-06  20
    ## 323 2021-04-07  25
    ## 324 2021-04-09  53
    ## 325 2021-04-11  28
    ## 326 2021-04-13  32
    ## 327 2021-04-15  14
    ## 328 2021-04-17  44
    ## 329 2021-04-19  14
    ## 330 2021-04-22  15
    ## 331 2021-04-23  38
    ## 332 2021-04-25  19
    ## 333 2021-04-28  35
    ## 334 2021-04-30  60
    ## 335 2021-05-02  33
    ## 336 2021-05-05  27
    ## 337 2021-05-09  29
    ## 338 2021-05-11  33
    ## 339 2021-05-12  29
    ## 340 2021-05-15  26
    ## 341 2021-05-16   0
    ## 342 2021-05-18  50
    ## 343 2021-05-22  22
    ## 344 2021-05-25   9
    ## 345 2021-05-28  50
    ## 346 2021-05-30  40
    ## 347 2021-06-01  32
    ## 348 2021-10-04  18
    ## 349 2021-10-20  20
    ## 350 2021-10-22  18
    ## 351 2021-10-24  31
    ## 352 2021-10-25  41
    ## 353 2021-10-27  23
    ## 354 2021-10-30  27
    ## 355 2021-11-01  20
    ## 356 2021-11-03  14
    ## 357 2021-11-04  10
    ## 358 2021-11-06  32
    ## 359 2021-11-10  22
    ## 360 2021-11-12  27
    ## 361 2021-11-13  21
    ## 362 2021-11-15  23
    ## 363 2021-11-17  34
    ## 364 2021-11-19  37
    ## 365 2021-11-20  33
    ## 366 2021-11-22  30
    ## 367 2021-11-24  15
    ## 368 2021-11-26  24
    ## 369 2021-11-28   8
    ## 370 2021-12-01  26
    ## 371 2021-12-03  37
    ## 372 2021-12-04  31
    ## 373 2021-12-07  34
    ## 374 2021-12-08  29
    ## 375 2021-12-10  24
    ## 376 2021-12-13  42
    ## 377 2021-12-17  27
    ## 378 2021-12-18  25
    ## 379 2021-12-20  17
    ## 380 2021-12-22  18
    ## 381 2021-12-25  25
    ## 382 2022-01-05  19
    ## 383 2022-01-06  36
    ## 384 2022-01-08  19
    ## 385 2022-01-10  24
    ## 386 2022-01-12  33
    ## 387 2022-01-14  20
    ## 388 2022-01-15  23
    ## 389 2022-01-17  27
    ## 390 2022-01-19  12
    ## 391 2022-01-21  27
    ## 392 2022-01-23  51
    ## 393 2022-01-25  36
    ## 394 2022-01-28  20
    ## 395 2022-01-29  38
    ## 396 2022-01-31  20
    ## 397 2022-02-02  19
    ## 398 2022-02-04  24
    ## 399 2022-02-06  15
    ## 400 2022-02-08  19
    ## 401 2022-02-11  24
    ## 402 2022-02-13  38
    ## 403 2022-02-15  28
    ## 404 2022-02-16  22
    ## 405 2022-02-24  30
    ## 406 2022-02-26  26
    ## 407 2022-02-27  24
    ## 408 2022-03-01  33
    ## 409 2022-03-03  37
    ## 410 2022-03-06  54
    ## 411 2022-03-09  44
    ## 412 2022-03-11  31
    ## 413 2022-03-13  21
    ## 414 2022-03-16  26
    ## 415 2022-03-18  32
    ## 416 2022-03-20  30
    ## 417 2022-03-21  36
    ## 418 2022-03-23  26
    ## 419 2022-03-27  34
    ## 420 2022-03-30  23
    ## 421 2022-04-01  31
    ## 422 2022-04-03  22
    ## 423 2022-04-06  16
    ## 424 2022-04-10  31
    ## 425 2022-04-17  31
    ## 426 2022-04-20  19
    ## 427 2022-04-23  39
    ## 428 2022-04-25  29
    ## 429 2022-05-01  21
    ## 430 2022-05-03  29
    ## 431 2022-05-07  10
    ## 432 2022-05-09  30
    ## 433 2022-05-11  34
    ## 434 2022-05-13  46
    ## 435 2022-05-15  23
    ## 436 2022-05-17  29
    ## 437 2022-05-19  27
    ## 438 2022-05-21  10
    ## 439 2022-05-23  31
    ## 440 2022-05-25  22
    ## 441 2022-05-27  30
    ## 442 2022-05-29  26
    ## 443 2022-06-02  12
    ## 444 2022-06-05  28
    ## 445 2022-06-08  26
    ## 446 2022-06-10  23
    ## 447 2022-06-13  27
    ## 448 2022-06-16  13
    ## 449 2022-10-18  35
    ## 450 2022-10-21  29
    ## 451 2022-10-22  40
    ## 452 2022-10-24  26
    ## 453 2022-10-28  32
    ## 454 2022-10-30  23
    ## 455 2022-11-02  26
    ## 456 2022-11-04  36
    ## 457 2022-11-05  26
    ## 458 2022-11-07  39
    ## 459 2022-11-09  31
    ## 460 2022-11-11  34
    ## 461 2022-11-12  43
    ## 462 2022-11-14  27
    ## 463 2022-11-16  19
    ## 464 2022-11-18  19
    ## 465 2022-11-21  28
    ## 466 2022-11-23  37
    ## 467 2022-11-25  30
    ## 468 2022-11-27   0
    ## 469 2022-11-28  35
    ## 470 2022-11-30  49
    ## 471 2022-12-02  14
    ## 472 2022-12-04  29
    ## 473 2022-12-05  31
    ## 474 2022-12-07  25
    ## 475 2022-12-10  18
    ## 476 2022-12-12  20
    ## 477 2022-12-13  44
    ## 478 2022-12-16  31
    ## 479 2022-12-18   0
    ## 480 2022-12-21  41
    ## 481 2022-12-23  30
    ## 482 2022-12-25  41
    ## 483 2022-12-27  38
    ## 484 2022-12-29  29
    ## 485 2023-01-01  25
    ## 486 2023-01-03  27
    ## 487 2023-01-05  29
    ## 488 2023-01-07  34
    ## 489 2023-01-09  32
    ## 490 2023-01-11  31
    ## 491 2023-01-12  20
    ## 492 2023-01-14  33
    ## 493 2023-01-16  51
    ## 494 2023-01-19  34
    ## 495 2023-01-21   0
    ## 496 2023-01-23  26
    ## 497 2023-01-24  31
    ## 498 2023-01-26  35
    ## 499 2023-01-28  30
    ## 500 2023-02-01  31
    ##  [ reached 'max' / getOption("max.print") -- omitted 50 rows ]

``` r
player_season_stats <- function(first_name=NULL, last_name=NULL,
                              season=NULL, stats=everything(), ...){
  
              # Return Error message if First and Last name not specified
                      if (is.null(first_name) | is.null(last_name)) {message("Error: First and last name of an NBA player must be specified.")}
                      else {
                        
                      # Find the player's PlayerID to be used later
                      player_search_url <- paste("https://www.balldontlie.io/api/v1/players/?search", paste(first_name, last_name, sep = "+"), sep = "=")
                      player_search <- GET(url=player_search_url)
                      player_info <- fromJSON(rawToChar(player_search$content))
                      playerID <- player_info$data[1,1]
                      
                      # Use the PlayerID to return the players average stats
                      # for a particular season or several seasons.
                      # If no season is specified, it returns the current season.
                      if (is.null(season)) {
                        stats_url <- paste0("https://www.balldontlie.io/api/v1/season_averages?",
                                       paste0("&player_ids[]=", playerID)) 
                      player_stats_api <- GET(url = stats_url)
                      player_content <-
                        fromJSON(rawToChar(player_stats_api$content))
                      player_stats <- player_content$data
                      } 
                      
                      
                      else{
                        season_list <- list()
                        for (i in season) {
                      stats_url <- paste0("https://www.balldontlie.io/api/v1/season_averages?",
                                       paste0("&season=", i),
                                paste0("&player_ids[]=", playerID)) 
                      player_stats_api <- GET(url = stats_url)
                      player_content <-
                        fromJSON(rawToChar(player_stats_api$content))
                      season_list[[i]] <- player_content$data
                        }
                        
                      # Each dataset in the list is combine vertically
                      player_stats <- bind_rows(season_list)
                      }
                      
                      # Sort games by season from most recent down
                      player_stats_sorted <- arrange(player_stats, desc(season))
                      
                      # Select important stats and reorder columns
                      player_stats_clean <- player_stats_sorted %>%
                        select(season, games_played, min, pts, reb, ast, stl,
                               blk, fgm, fga, fg_pct, fg3m, fg3a, fg3_pct,
                               ftm, fta, ft_pct, oreb, dreb, turnover, pf)
                      
                      # Allow user to select variables and order,
                      # but always keeping the season for reference
                      player_stats_final <- player_stats_clean %>%
                        select(season, stats)
                      
                      # Print final player stats
                      player_stats_final
                      }
}
```

``` r
tatum <- player_season_stats(first_name = "jayson", last_name = "tatum", season=c(2000:2023), stats = "games_played")
tatum
```

    ##   season games_played
    ## 1   2022           74
    ## 2   2021           77
    ## 3   2020           65
    ## 4   2019           66
    ## 5   2018           79
    ## 6   2017           80

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Including Code

You can include R code in the document as follows:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](Figs/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
