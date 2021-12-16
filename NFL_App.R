library(patchwork)
library(tidyverse, warn.conflicts = FALSE)
library(ggrepel, warn.conflicts = FALSE)
library(ggimage, warn.conflicts = FALSE)
library(nflfastR, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(nflreadr, warn.conflicts = FALSE)
library(nflscrapR, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(shiny)

data1 <- load_pbp(2010:2020) %>% 
  filter(season_type == "REG")

fd_dropbacks1 <- data1 %>%
  filter(down==1 & ydstogo==10 & yardline_100<=80 & yardline_100>=20 & qb_dropback==1 & wp>= .20 & wp<= .80 & half_seconds_remaining>120 & !is.na(epa)) %>%
  select(posteam, season, week, play_id, down, ydstogo, epa, wpa, series_success, yards_gained)

fd_runs1 <- data1 %>%
  filter(down==1 & ydstogo==10 & yardline_100<=80 & yardline_100>=20 & 
           qb_dropback==0 & play_type == "run" & wp>= .20 & wp<= .80 & 
           half_seconds_remaining>120 & !is.na(epa)) %>%
  select(posteam, season, week, play_id, down, ydstogo, epa, wpa, series_success, yards_gained)

### Data for QB drop backs on 1st and 10, grouped by season
fd_dropbacks_by_year <- fd_dropbacks1 %>%
  group_by(posteam, season)%>% 
  summarize(
    epa_fd_dropback = mean(epa),
    wpa_fd_dropback = mean(wpa),
    success_rate_fd_dropback = mean(series_success),
    yards_per_fd_dropback_play = mean(yards_gained), 
    fd_dropback_plays = n()
  ) %>%
  ungroup()
fd_dropbacks_by_year

### Data for runs on 1st and 10, grouped by season
fd_runs_by_year <- fd_runs1 %>%
  group_by(posteam, season)%>% 
  summarize(
    epa_fd_run = mean(epa),
    wpa_fd_run = mean(wpa),
    success_rate_fd_run = mean(series_success),
    yards_per_fd_run_play = mean(yards_gained),
    fd_run_plays = n()
  ) %>%
  ungroup()
fd_runs_by_year

### Data for both QB dropbacks and run plays on 1st and 10, grouped by season
data3 <- fd_runs_by_year%>%
  left_join(fd_dropbacks_by_year , by = c('posteam' = 'posteam','season' = 'season')) %>%
  mutate(tot_fd_plays = fd_run_plays + fd_dropback_plays,
         prop_fd_run_plays = fd_run_plays/tot_fd_plays,
         prop_fd_QBD_plays = fd_dropback_plays/tot_fd_plays)

ui <- fluidPage(
  titlePanel("NFL 1st Down Data"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create plots with information on series success"),
      
      selectInput("var", 
                  label = "Choose a team to look at",
                  choices = data3$posteam,
                  selected = "Percent White"),
      
      sliderInput("years", 
                  label = "Years",
                  min = 2010, max = 2020, value = c(2010,2020))
    ),
    
    mainPanel(  # here is the output!
      plotOutput("pass_plot"),
      plotOutput("run_plot")
    )
  )
)


server <- function(input, output) {
  
  output$pass_plot <- renderPlot({
    data3%>%filter(posteam == input$var)%>%filter(season %in% input$years[1]:input$years[2])%>%
      left_join(teams_colors_logos , by = c('posteam' = 'team_abbr'))%>%
      ggplot(aes(x=prop_fd_QBD_plays, y = success_rate_fd_dropback))+
      geom_image(aes(image = team_logo_espn), size = .05, asp = 16 / 9)+ 
      geom_text_repel(aes(label=season))+
      geom_hline(yintercept = mean(data3$success_rate_fd_dropback),
                 color = "red", linetype = "dashed", alpha=0.5) +
      #vertical line with mean proportion of play calls being a QBD
      geom_vline(xintercept = mean(data3$prop_fd_QBD_plays),
                 color = "red", linetype = "dashed", alpha=0.5)+
      xlab("Proportion of QBD Playcalls")+
      ylab("Percentage of Series Resulting in First Down or Touchdown")+
      ggtitle("QBD Success Rate Based on First Down Play Calling")
  })
  output$run_plot <- renderPlot({
    data3%>%filter(posteam == input$var)%>%filter(season %in% input$years[1]:input$years[2])%>%
      left_join(teams_colors_logos , by = c('posteam' = 'team_abbr'))%>%
      ggplot(aes(x=prop_fd_run_plays, y = success_rate_fd_run))+
      geom_image(aes(image = team_logo_espn), size = .05, asp = 16 / 9)+ 
      geom_text_repel(aes(label=season))+
      geom_hline(yintercept = mean(data3$success_rate_fd_run),
                 color = "red", linetype = "dashed", alpha=0.5) +
      #vertical line with mean proportion of play calls being a QBD
      geom_vline(xintercept = mean(data3$prop_fd_run_plays),
                 color = "red", linetype = "dashed", alpha=0.5)+
      xlab("Proportion of Run Playcalls")+
      ylab("Percentage of Series Resulting in First Down or Touchdown")+
      ggtitle("Run Success Rate Based on First Down Play Calling")
  })
  
}

shinyApp(ui = ui, server = server)