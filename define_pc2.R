files <- 
  tibble(files = list.files()) %>%
  filter(str_detect(files, "^week\\d{1,2}\\.csv")) %>%
  filter(files!= "week1.csv")

tracking_data <- 
  map_dfr(files$files, read.csv) %>%
  mutate(x = if_else(playDirection == "left", 120 - x, x),
         y = if_else(playDirection == "left", 160/3 - y, y),
         dir = if_else(playDirection == "left", (180 + dir) %% 360, dir))

df_targetedReceiver <- read_csv("targetedReceiver.csv")

ball_data <- 
  tracking_data %>%
  filter(team == "football") %>%
  select(frameId, gameId, playId, ball_x = x, ball_y = y)

model_data <- 
  inner_join(df_targetedReceiver,
             tracking_data, 
             by = c("targetNflId" = "nflId", "gameId", "playId")) %>%
  filter(event %in% c("pass_forward", "pass_arrived")) %>%
  group_by(gameId, playId, targetNflId) %>%
  filter(n() == 2) %>% 
  filter(!(playId == 435 & gameId == 2018123001)) %>% 
  left_join(ball_data, by = c("frameId", "gameId", "playId")) %>%
  mutate(ball_dist_base = sqrt((x - ball_x)^2 + (y - ball_y)^2),
         ball_dist_x = x - ball_x,
         ball_dist_y = y - ball_y,
         speed_x = s * sin(dir * pi/180),
         speed_y = s * cos(dir * pi/180)) %>%
  select(playDirection ,x, y, speed_x, speed_y, event, ball_dist_x, ball_dist_y) %>% 
  distinct() %>%
  pivot_wider(names_from = event, values_from = c(x, y, speed_x, speed_y, contains("ball_dist"))) %>%
  mutate(dist_covered__base = sqrt((x_pass_forward - x_pass_arrived)^2 + (y_pass_forward - y_pass_arrived)^2),
         dist_covered_x = x_pass_arrived - x_pass_forward,
         dist_covered_y = y_pass_arrived - y_pass_forward) %>%
  select(playDirection ,gameId, playId, dist_covered_x, dist_covered_y,
         ball_dist_x = ball_dist_x_pass_forward, ball_dist_y = ball_dist_y_pass_forward, 
         speed_x = speed_x_pass_forward, speed_y = speed_y_pass_forward) %>%
  drop_na(speed_y)

calc_PC2 <- function(time, next_time, ball_x, ball_y, ball_dist, x, y, s, theta, team, nflId, pitch_area) {
  
  
  
  speed_x_p <- get_speed_x(s, theta)
  speed_y_p <- get_speed_y(s, theta)
  
  
  
  sim_data <-
    model_data %>%
    filter(abs(speed_x - speed_x_p) < 3 &
             abs(speed_y - speed_y_p) < 3,
           abs(ball_dist_x - (x - ball_x)) < 4 &
             abs(ball_dist_y - (y - ball_y)) < 4) %>%
    ungroup()
  
  sim_data  <-
    sim_data %>%
    select(dist_covered_x, dist_covered_y)%>%
    summarise(cov = list(matrix(c(dist_covered_x, dist_covered_y), ncol = 2) %>% cov()),
              mean = list(c(mean(dist_covered_x), mean(dist_covered_y))))
  
  
  mean <-
    sim_data$mean[[1]]
  
  cov <-
    sim_data$cov[[1]]
  
  # sim_data <-
  #   model_data %>%
  #   filter(abs(speed_x - speed_x_p) < 6 &
  #            abs(speed_y - speed_y_p) < 6,
  #          abs(ball_dist_x - (x - ball_x)) < 6 &
  #            abs(ball_dist_y - (y - ball_y)) < 6) %>%
  #   ungroup()
  # 
  # sysfit <- 
  #   systemfit::systemfit(list(dist_covered_x ~
  #                               spline_speed_x + ball_dist_x,
  #                             dist_covered_y ~
  #                               spline_speed_y + ball_dist_y),
  #                        method = "SUR",
  #                        data = sim_data)
  # 
  # mean <- 
  #   predict(sysfit, 
  #         tibble(ball_dist_x = x - ball_x,
  #                ball_dist_y = y - ball_y,
  #                speed_x = speed_x_p,
  #                speed_u = speed_y_p
  #         ) %>%
  #           mutate(spline_speed_x = predict(x_spline, speed_x),
  #                  spline_speed_y = predict(y_spline, speed_y)))
  # 
  # cov <- sysfit$residCovEst
  
  
  mu_x <- as.numeric(x + mean[1])
  mu_y <- as.numeric(y + mean[2])
  
  Sigma <- cov
  
  pitch_area$I <- calc_I(as.matrix(pitch), x, y, mu_x, mu_y, Sigma)
  pitch_area$team <- team
  pitch_area$time <- time
  pitch_area$nflId <- nflId
  pitch_area$mu_x <- mu_x
  pitch_area$mu_y <- mu_y
  return(pitch_area)
}
