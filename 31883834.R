library(shiny)
library(tidyverse)
library(dplyr)
library(fmsb)
library(plotly)
library(shinythemes)
library(viridis)
library(lubridate)
##Q1 first part

matches=read.csv("Matches.csv")
ballbyball=read.csv("ballbyball.csv")

matches_updated=select(matches,-c(X,eliminator,umpire1,umpire2))
matches_numerical=select(matches_updated,-c(city,venue,player_of_match,team1,team2,toss_winner,toss_decision,winner,result))
ballbyball_updated=select(ballbyball,-c(X,fielder,extras_type,fielder,dismissal_kind,player_dismissed))
ballbyball_numerical=select(ballbyball_updated,-c(batsman,non_striker,bowler,batting_team,bowling_team))


total_matchdata=merge(matches_updated,ballbyball_updated)

total_matchdata = total_matchdata %>%
    dplyr::mutate(day = lubridate::year(date), 
                  month = lubridate::month(date), 
                  year = lubridate::day(date))

run_powerplay=total_matchdata%>%
  select(batting_team,total_runs,over)%>%
  filter(total_matchdata$over %in%  c("0", "1","2","3","4","5"))%>%
  group_by(batting_team,over)%>%
  summarise(Totalruns=sum(total_runs))%>%
  pivot_wider(names_from = over, values_from = Totalruns)

max <- c("max", "1700", "1700", "1700", "1700", "1700", "1700")
min <- c("min", "0", "0", "0", "0", "0", "0")

x=rbind(max,min)
df=data.frame(x)
colnames(df)=c("batting_team","0","1","2","3","4","5")
df$`0`=as.integer(df$`0`)
df$`1`=as.integer(df$`1`)
df$`2`=as.integer(df$`2`)
df$`3`=as.integer(df$`3`)
df$`4`=as.integer(df$`4`)
df$`5`=as.integer(df$`5`)
transform_run=rbind(df,run_powerplay)

#transform_run=read.csv("total_over.csv")
#transform_wicket=read.csv("total_wicket_over.csv")
colnames(transform_run)=c("Teams","1","2","3","4","5","6")

rownames(transform_run) <- c("max", "min", "Chennai Super Kings", "Deccan Chargers", 
                             "Delhi Capitals", "Gujarat Lions","Kings XI Punjab",
                             "Kochi Tuskers Kerala","Kolkata Knight Riders","Mumbai Indians",
                             "Rajasthan Royals","Rising Pune Supergiant","Royal Challengers Bangalore","Sunrisers Hyderabad")

wicket_powerplay=total_matchdata%>%
  select(bowling_team,is_wicket,over)%>%
  filter(total_matchdata$over %in%  c("0", "1","2","3","4","5"))%>%
  group_by(bowling_team,over)%>%
  summarise(Totalwickets=sum(is_wicket))%>%
  pivot_wider(names_from = over, values_from = Totalwickets)


wicket_powerplay=wicket_powerplay[-1,]
max1 <- c("max", "60", "60", "60", "60", "60", "60")
min1 <- c("min", "0", "0", "0", "0", "0", "0")


x1=rbind(max1,min1)
df1=data.frame(x1)
colnames(df1)=c("bowling_team","0","1","2","3","4","5")
df1$`0`=as.integer(df1$`0`)
df1$`1`=as.integer(df1$`1`)
df1$`2`=as.integer(df1$`2`)
df1$`3`=as.integer(df1$`3`)
df1$`4`=as.integer(df1$`4`)
df1$`5`=as.integer(df1$`5`)
transform_wicket=rbind(df1,wicket_powerplay)


rownames(transform_wicket) <- c("max", "min", "Chennai Super Kings", "Deccan Chargers", 
                             "Delhi Capitals", "Gujarat Lions","Kings XI Punjab",
                             "Kochi Tuskers Kerala","Kolkata Knight Riders","Mumbai Indians",
                             "Rajasthan Royals","Rising Pune Supergiant","Royal Challengers Bangalore","Sunrisers Hyderabad")

colnames(transform_wicket)=c("Teams","1","2","3","4","5","6")
##Question 1 part 2

season_run = total_matchdata %>%
    select(batting_team,total_runs,year,over)%>%
    filter(total_matchdata$over %in%  c("0", "1","2","3","4","5"))

season_agg=aggregate(cbind(season_run$total_runs ) ~ season_run$batting_team+season_run$year, FUN=sum)
colnames(season_agg)=c("Teams","Year","Runs")


season_wicket = total_matchdata %>%
    select(bowling_team,is_wicket,year,over)%>%
    filter(total_matchdata$over %in%  c("0", "1","2","3","4","5"))%>%
    group_by(bowling_team,year)%>%
    summarise(TotalWicket=sum(is_wicket))
season_wicket=season_wicket[-c(1,2),]
colnames(season_wicket)=c("Teams","Year","TotalWicket")


##Weather part 1

weather_final=read.csv("weather_final.csv")
weather_final=weather_final %>% filter(time=="18:00:00")

weather=merge(weather_final,matches)

#weather=read.csv("delhi_time_18.csv")

aggregate(cbind(weather$id ) ~ weather$X_conds, FUN=sum)

table(weather$X_conds)

match_count=weather%>%
    count(X_conds)

## part 2

winner_tosswinner=weather%>%
    select(toss_winner,winner,toss_decision,X_conds)

winner_tosswinner$matchcount=as.integer('1')

winner_tosswinner=aggregate(cbind(winner_tosswinner$matchcount) ~ winner_tosswinner$winner+winner_tosswinner$toss_winner+winner_tosswinner$X_conds, FUN=sum)

colnames(winner_tosswinner)=c("Winner","Tosswinner","weather","No_of_wins")

winner_tosswinner <- winner_tosswinner %>%
    mutate(text = paste0("Winner: ", Winner, "\n", "TossWinner: ", Tosswinner, "\n", "Wins: ",round(No_of_wins,2), "\n"))


##Plot Run graph
bowler_csk=total_matchdata%>%
    filter(year %in% c("14","15","18","19"),bowling_team=="Chennai Super Kings")%>%
    select(team1,bowler,total_runs,is_wicket,batting_team,year)

run_bowler=bowler_csk%>%
    select(bowler,total_runs,year,batting_team)%>%
    group_by(bowler,year)%>%
    summarise(TotalRuns=sum(total_runs))

run_bowler$year[run_bowler$year=="18"]="2018"
run_bowler$year[run_bowler$year=="14"]="2014"
run_bowler$year[run_bowler$year=="15"]="2015"
run_bowler$year[run_bowler$year=="19"]="2019"

run_bowler1=bowler_csk%>%
    select(bowler,total_runs,year,batting_team)%>%
    group_by(bowler,year,batting_team)%>%
    summarise(TotalRuns=sum(total_runs))

run_bowler1$year[run_bowler1$year=="18"]="2018"
run_bowler1$year[run_bowler1$year=="14"]="2014"
run_bowler1$year[run_bowler1$year=="15"]="2015"
run_bowler1$year[run_bowler1$year=="19"]="2019"
run_bowler1 <- run_bowler1 %>%
    mutate(text = paste0("Team: ", batting_team, "\n", "Runs Given: ",TotalRuns, "\n"))

wicket_bowler=bowler_csk%>%
    select(bowler,is_wicket,year)%>%
    group_by(bowler,year)%>%
    summarise(Totalwickets=sum(is_wicket))

wicket_bowler1=bowler_csk%>%
    select(bowler,is_wicket,year,batting_team)%>%
    group_by(bowler,year,batting_team)%>%
    summarise(Totalwickets=sum(is_wicket))

#run_bowler1 <- run_bowler1 %>%
#    mutate(text = paste0("Team: ", batting_team, "\n", "Runs Given: ",Totalwickets, "\n"))

wicket_bowler$year[wicket_bowler$year=="18"]="2018"
wicket_bowler$year[wicket_bowler$year=="14"]="2014"
wicket_bowler$year[wicket_bowler$year=="15"]="2015"
wicket_bowler$year[wicket_bowler$year=="19"]="2019"

wicket_bowler1$year[wicket_bowler1$year=="18"]="2018"
wicket_bowler1$year[wicket_bowler1$year=="14"]="2014"
wicket_bowler1$year[wicket_bowler1$year=="15"]="2015"
wicket_bowler1$year[wicket_bowler1$year=="19"]="2019"

team_names=unique(season_agg$Teams)
team_names=append(team_names,"Select All")


###userinterface for shiny dashboard
ui=fluidPage(
    theme = shinytheme("sandstone"),
    navbarPage(
        title = "Data Analysis on IPL",
        id="nav", 
        tabPanel("Overview", value="Data",
                 navlistPanel(tabPanel("INTRODUCTION",wellPanel(uiOutput("introduction"))),tabPanel("QUESTIONS",wellPanel(uiOutput("questions"))),tabPanel("CONCLUSION",wellPanel(uiOutput("conclusion"))),tabPanel("REFERENCES",wellPanel(uiOutput("reference"))))),
        tabPanel( "Power play Analysis", value="Data",
        fixedRow(width=20,column(width = 4,wellPanel(uiOutput("run_transform")),selectInput("inning_dashboard", label = "Innings", choices = c("Runs Scored","Wickets taken"),
                selected = "Runs Scored", multiple = FALSE),selectInput("teams_dashboard", "Select Team for Spider Radar", choices = team_names, multiple = TRUE, selected = "Chennai Super Kings" )),column(8,wellPanel(plotOutput("plot1")))),
                
        fixedRow(width=15,column(width = 4,wellPanel(uiOutput("season_run")),selectInput("Teams_Name", "Select Team for Runs Scored", choices = team_names, multiple = TRUE, selected = "Chennai Super Kings" )),
                 column(width = 8,plotOutput("plot2"))),
        
        
        fixedRow(width=15,column(width = 4,wellPanel(uiOutput("season_wicket")),selectInput("Teams_Name_wicket", "Select Team for Wickets taken", choices = team_names, multiple = TRUE, selected = "Chennai Super Kings" )),
                column(width = 8,plotOutput("plot3")))),
  
    
 
    tabPanel("Weather Analysis", value="Data",
    fixedRow(width=20,column(width = 4,wellPanel(uiOutput("weather_conditions"))), column(width = 7,plotlyOutput("plot4"))),
                      
    wellPanel(fixedRow(width=20, column(width = 8,uiOutput("tosswinner_winner")),column(width = 4,selectInput(
        "heatmap_dashboard", label = "Weather Conditions", choices = c(unique(winner_tosswinner$weather)),selected = "Haze", multiple = FALSE)))),
    
    fixedRow(width=20, plotlyOutput("plot5"))),
    
    
    
    tabPanel("Bowling Analysis", value="Data",
             
    wellPanel( fixedRow(width=10, uiOutput("top_bowlingperformance"))),
    
    wellPanel( fixedRow(width=10, 
            column(width = 9, uiOutput("circular_bar_run")),
        column(width = 3,sliderInput("range","Runs Given",min = min(run_bowler$TotalRuns),max = max(run_bowler$TotalRuns),value=range(run_bowler$TotalRuns),step = 10)))),
    
    fixedRow(width=50,plotOutput("plot6")),
    
    wellPanel(fixedRow(width=15,column(width=9,uiOutput("run_perteam")),column(width = 2,selectInput("csk_runs", label = "Select Year", choices = c("2014","2015","2018","2019"),selected = "19", multiple = FALSE)))),
    
    fixedRow(width=40,plotlyOutput("plot7")),
    
    wellPanel(fixedRow(width=20,
        column(width = 8, uiOutput("circular_wicket")),
                                column(width= 4,sliderInput("csk_wickets","Wickets taken",min = min(wicket_bowler$Totalwickets),max = max(wicket_bowler$Totalwickets),value=range(wicket_bowler$Totalwickets),step = 2)))),
    
     fixedRow(width=40,plotOutput("plot8")),
    
    wellPanel(fixedRow(width=15,column(width=9,uiOutput("wicket_perteam")),column(width = 2,selectInput("csk_wicket", label = "Select Year", choices = c("2014","2015","2018","2019"),selected = "19", multiple = FALSE)))),
    
    fixedRow(width=40,plotlyOutput("plot9")))
                                
             )

    )
    
 
 
 
 
#Server for shiny dashboard

server<-function(input,output,session){

   output$introduction <- renderUI({ 
    HTML(str_glue(
      '<div>
          <h3><center><b>Analysis on Indian Premier League</b></center></h3><br>
          <p>The Indian Premier League (IPL) is a T20 cricket competition that began in 2008. This tournament attracts players from all over the world and form teams with names of different states in India. The increasing volume of cricket enthusiasts across the world are increasing day by day. Finally, the game has the potential to establish itself on the sphere. Every new season of the IPL brings new selections; therefore team selectors must choose players strategically, Data visualizations help with the analysis of the players as well as the teams to explore, who is the most valuable player and how is one team performs in comparison to another. </p> 
          </div>'
    ) )
    
  })
   
   output$questions <- renderUI({ 
     HTML(str_glue(
       "<div>
          <h3><b>The visualisation will be answering the following questions.</b></h3><br>
          <p>1. Assess the teams over the seasons during the power play overs. Based on this information, calculate which team has benefitted the most/least.</p>
        <p>2. For the matches that took place in India's capital, Delhi. Examine if weather conditions influenced the outcome of the toss and how the teams have performed as per the outcome of the toss</p>
        <p>3. Choosing Chennai Super Kings as the team and then analyzing their bowlers' performances before and after the ban for two consecutive years in the middle and death overs.</p>
       </div>"
     ) )
     
   })
   
   output$conclusion <- renderUI({ 
     HTML(str_glue(
       "<div>
          <h3><center><b>Conclusion Summary</b></center></h3><br>
          <p>• The total number of runs scored by Mumbai Indians is highest for all the 6 overs of the power play when compared overwise and least runs scored is Rajasthan Royals. These are the two teams that have played all the seasons of IPL.</p>
<p>•	Although Sunrisers Hyderabad has not played all the seasons, but has a consistent increase in their performance in total runs scored in power play overs. Chennai Super kings’ performance has been consistently declining since season 14 in terms of runs scored.</p>
<p>•	Mumbai Indians have been the team that is taking most number of wickets as compared to other teams for majority of the years. Kolkata Knight Riders performance has been consistently declining from 2016 onwards.</p>
<p>•	42 matches have been played in “Haza” conditions, where as only a single match has been played in other weather conditions in delhi for all seasons.</p>
<p>•	Delhi Capitals have been the team which has won most number of toss, and was winner in maximum number of matches.</p>
<p>•	Bowlers performance have increased after than ban that is for the year 2018 and 2019. We can see that runs given/wickets taken to opponent teams have significantly declined.</p>
<p>•	Batting teams of Sunrisers Hyderabad and Mumbai Indians have scored more runs to the bowlers of CSK as compared to other teams whereas Royal Challengers Bangalore have scored very less runs to the same bowlers.</p>
<p>•	Many other results can be observed by interacting with the visualisations. 
</p>
       </div>"
     ) )
     
   })
   output$reference <- renderUI({ 
     HTML(str_glue(
       "<div>
          <h3><center><b>References</b></center></h3><br>
          <p>1.	41. Law 41 – Fielder (2021, September 2)<br>
https://www.iplt20.com/about/match-playing-conditions/law-41-fielder</p>
<p>2.	Indian Premier league (2021, September 4) Wikipedia https://en.wikipedia.org/wiki/Indian_Premier_League.</p>
<p>3.	Circular bar plot (2021, October 20)<br>
https://www.r-graph-gallery.com/297-circular-barplot-with-groups.html</p>
<p>4.	The R Graph Gallery (2021, October 20)<br>
 https://www.r-graph-gallery.com/index.html</p>
<p>5.	Spider Radar chart (2021, October 20)<br>
https://www.r-graph-gallery.com/142-basic-radar-chart.html</p>
<p>6.	Stacked bar chart (2021, October 22)<br>
https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html</p>
<p>7.	Five design sheet methodology (2021, October 25)<br>
https://towardsdatascience.com/five-design-sheet-methodology-approach-to-data-visualisation-603d760f2418</p>
<p>8.	Plotly graphs (2021, October 25)<br>
https://plotly.com/r/
</p>
       </div>"
     ) )
     
   })
    teams_global_filter = reactive({
        if (input$inning_dashboard == "Runs Scored"){
                transform_filter <- transform_run%>% filter(Teams %in% c("max","min",input$teams_dashboard))
                dashboard_dataset<-transform_filter[,2:7]
                output$run_transform <- renderUI({ 
                    HTML(str_glue(
                        '<div>
          <h4><center>Runs Scored per over</center></h4><br>
          <p>The spider radar depicts the total runs scored per over by the teams in all seasons. The nodes of the spider chart are 
          the overs (i.e 6 Overs) and inside segment are the runs scored by teams. By the help of the radar we can 
          compare teams performance with one another. Mumbai Indians has outperformed all other teams in respect to scoring maximum 
          runs in overs (4-6). where as Kolkata Knight riders is the team scoring maximum runs in overs(1-3).</p> 
          </div>'
                    ) )
                    
                })
            
        }
        if (input$inning_dashboard =="Wickets taken"){
                transform_filter <- transform_wicket%>% filter(Teams %in%  c("max","min",input$teams_dashboard))
                dashboard_dataset<-transform_filter[,2:7]
                output$run_transform <- renderUI({ 
                    HTML(str_glue(
                        '<div>
              <h4><center>Wickets lost per over</center></h4><br>
          <p>The spider radar depicts the total wickets fall per over by the teams in all seasons. The nodes of the spider chart are 
          the overs (i.e 6 Overs) and inside segment are the wicket falls by teams. By the help of the radar we can 
          compare teams performance with one another. Delhi Capital has the highest wicket fall in comparison to all other teams in 
           all overs. where as Chennai Super kings is the team having lowest wicket fall in overs.</p> 
          </div>'
                    ) )
                    
                })
            
        }
        
        return (dashboard_dataset)
    })

    output$plot1 <- renderPlot({
        colors_fill <- c(scales::alpha("red", 0.1),
                         scales::alpha("orange", 0.1),
                         scales::alpha("yellow", 0.1),
                         scales::alpha("lightgreen", 0.1),
                         scales::alpha("blue", 0.1),
                         scales::alpha("purple", 0.1),
                         scales::alpha("gray", 0.1),
                         scales::alpha("cyan", 0.1))
        
        colors_line <-  c(scales::alpha("darkred", 0.9),
                          scales::alpha("darkorange", 0.9),
                          scales::alpha("gold", 0.9),
                          scales::alpha("darkgreen", 0.9),
                          scales::alpha("darkblue", 0.9),
                          scales::alpha("purple", 0.9),
                          scales::alpha("darkgray", 0.9),
                          scales::alpha("cyan", 0.9))
        powerplay_graph<-radarchart(teams_global_filter(),axistype=1, 
                   title = "Performance of Team per over in Power play overs",
                   pcol = colors_line,lwd=2,lty=1,
                   pfcol = colors_fill,plwd = 2,
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,max(teams_global_filter()),50), cglwd=2)
        
        legend(x=1.1, 
               y=1.3, 
               legend = input$teams_dashboard, 
               bty = "n", pch=10 , col = colors_line, cex = 1.3, pt.cex = 2 )
        
    })
    
    run_team_powerplay<-reactive({
        if("Select All" %in% input$Teams_Name){
            run_team_df<-season_agg
        }
        else{
            run_team_df<-season_agg%>% filter(Teams %in% input$Teams_Name)
        }
    })
    

    output$season_run <- renderUI({ 
        HTML(str_glue(
            '<div>
          <h4><center>Batting Performance</center></h4>
          <p>The Line plot shows us  which team has scored the most amount of runs in the power play overs through the seasons(2008-2020). Multiple teams can be selected 
          through selector option and their performances can be compared.Overall, we can visualise that <b>Mumbai Indians</b> has been the one to have beniffted the most out of it. </p> 
          </div>'
        ) )
        
    })
    
    output$plot2<-renderPlot({
        
        run_agg_graph<-run_team_powerplay() %>%
            ggplot( aes(x=as.factor(Year), y=Runs, group=Teams, color=Teams)) +
            geom_line(lwd=1) +
            ggtitle("Batting Performance of Teams in Power play overs") +
            ylab("Number of Runs made")+xlab("Seasons")+
            geom_text(aes(label =Runs), vjust = -0.6,colour="black")+
            theme_bw()
        print(run_agg_graph)
    })
    
    wicket_team_powerplay<-reactive({
        if("Select All" %in% input$Teams_Name_wicket){
            run_team_df<-season_wicket
        }
        else{
            run_team_df<-season_wicket%>% filter(Teams %in% input$Teams_Name_wicket)
        }
    })
    
    output$season_wicket <- renderUI({ 
        HTML(str_glue(
            '<div>
          <h4><center>Bowling Performance</center></h4>
          <p>The Line plot shows us  hows the overall number of wickets the team has taken in the power play overs through the seasons(2008-2020). 
          Multiple teams can be selected through selector option and their performances can be compared.
          Overall, we can say that Mumbai Indians has been the one to have beniffted the most out of it. There has been a significant drop in number of wickets taken by Chennai super kings and Rajasthan Royals. </p> 
          </div>'
        ) )
        
    })
    
    
    output$plot3<-renderPlot({
        
        wicket_agg_graph<-wicket_team_powerplay() %>%
            ggplot( aes(x=as.factor(Year), y=TotalWicket, group=Teams, color=Teams)) +
            geom_line(lwd=1) +
            ggtitle("Bowling Performance of Teams in Power play overs") +
            ylab("Number of Wickets Taken")+xlab("Seasons")+
            geom_text(aes(label =TotalWicket), vjust = -0.6,colour="black")+
            theme_bw()
        print(wicket_agg_graph)
    })
    
    

    output$plot4<-renderPlotly({
        weather_plot=plot_ly(match_count,labels=~X_conds ,values=~n,type='pie',textposition='inside',textinfo='label',
                             title = 'Weather Conditions in Delhi',  hoverinfo = 'text',
                             text = ~paste('No of Matches played:', n),
                             marker=list(colors=colors,line=list(color='#FFFFFF',width=1)),
                                         showlegend=FALSE)
        print(weather_plot)
    })
    
    output$weather_conditions <- renderUI({ 
        HTML(str_glue(
            '<div>
          <h3>Weather Conditions</h3><br>
          <p> The Pie plot shows the different weather conditions in which matches were played in all seasons of IPL.</p>
          
          <p>The weather condition <b>Haze</b> is the condition, in which maximum number of matches i.e.<b>42</b> are played.
          Followed by <b>Mist</b> in which only 2 matches were played, and only one match was played in all other weather
          coniditions.</p> 
          <p><small><small> Observe data by hovering over pie chart</small></small></p>
          </div>'
        ) )
        
    })
    
    heatmap_filter<-reactive({
       heatmap_df<-winner_tosswinner%>%filter(weather==input$heatmap_dashboard)
    })
    
    
    output$plot5<-renderPlotly({
        p=heatmap_filter()%>%
            ggplot( aes(Tosswinner, Winner, fill= No_of_wins,text=text)) + 
            geom_tile() +
            scale_fill_gradient(low="white", high="red")+
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
        
        ggplotly(p, tooltip="text")
    })
    
    
    output$tosswinner_winner <- renderUI({ 
        HTML(str_glue(
            '<div>
          <h3>Winner Vs Toss winner</h3><br>
          <p> The heatmap helps us visualise the toss winner and in respect to that who is the winner in a specific weather conditions in Delhi.</p>
          
          <p> The weather conditions can be changed using the dropdown menu. We can see that Delhi Daredevils have been the match winner and toss winner in most of the 
          matches that have taken place in <b<Haze</b> weather condition. For all other weather condition we can only visualise only one match that has taken place in delhi in all seasons.</p> 
          <p><small><small> Observe data by hovering over heatmap</small></small></p>
          </div>'
        ) )
        
    })
    
    output$top_bowlingperformance <- renderUI({ 
        HTML(str_glue(
            "<div>
          <h4><center><b>Analyze bowling performance of Chennai Super Kings<b></center></h4><br>
          <p>There was a ban on Chennai Super Kings for 2 years. Hence analysing bowler's performance for 2 consecutive years before and after the ban.
          Bowler's performance is analysed in various aspects such as runs given by them, or wickets taken as whole and based on
          per team.</p> 
          </div>"
        ) )
        
    })
    
    runs_csk<- reactive({
        run_bowler[run_bowler$TotalRuns >=input$range[1] & 
                       run_bowler$TotalRuns <= input$range[2], ]
    })
    
    output$circular_bar_run <- renderUI({ 
        HTML(str_glue(
            "<div>
          <h4><b>Total runs given for each year</b></h4>
          <p> The circular plot, gives us the information about the number of runs given by the bowlers to other teams.
          Player's performance can assessed for 4 years which is 2 years before and after the ban. 
          To get better insight as to which player has improved their bowling performance and how much benificial he will
          be for upcoming season.</p> 
          </div>"
        ) )
        
    })
    
    output$plot6<-renderPlot({
        
        empty_bar <- 3
        to_add <- data.frame( matrix(NA, empty_bar*nlevels(run_bowler$year), ncol(run_bowler)) )
        colnames(to_add) <- colnames(run_bowler)
        to_add$year <- (rep(levels(run_bowler$year), each=empty_bar))
        #run_bowler <- rbind(run_bowler, to_add)
        run_bowler <- runs_csk() %>% arrange(year)
        run_bowler$id <- seq(1, nrow(run_bowler))
        
        label_data1=run_bowler
        
        number_of_bar <- nrow(label_data1)
        angle <-  90 - 360 * (label_data1$id-0.5) /number_of_bar 
        
        label_data1$hjust<-ifelse( angle < -90, 1, 0)
        label_data1$angle<-ifelse(angle < -90, angle+180, angle)
        
        base_data1 <- run_bowler%>% 
            group_by(year) %>% 
            summarize(start=min(id), end=max(id)) %>% 
            rowwise() %>% 
            mutate(title=mean(c(start, end)))
        
        grid_data1 <- base_data1
        grid_data1$end <- grid_data1$end[ c( nrow(grid_data1), 1:nrow(grid_data1)-1)] + 1
        grid_data1$start <- grid_data1$start - 1
        grid_data1 <- grid_data1[-1,]
        
        #######################
        
        p=ggplot(run_bowler, aes(x=as.factor(id), y=TotalRuns, fill=as.factor(year))) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
            
            geom_bar(aes(x=as.factor(id), y=TotalRuns, fill=as.factor(year)), stat="identity", alpha=0.5) +
            
            geom_segment(data=grid_data1, aes(x = end, y = 600, xend = start, yend =600), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
            geom_segment(data=grid_data1, aes(x = end, y = 400, xend = start, yend = 400), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
            geom_segment(data=grid_data1, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
            geom_segment(data=grid_data1, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
            
            
            # Add text showing the value of each 100/75/50/25 lines
            annotate("text", x = rep(max(run_bowler$id),4), y = c(100, 200, 400, 600), label = c("100", "200", "400", "600") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
            
            geom_bar( aes(x=as.factor(id), y=TotalRuns, fill=as.factor(year)), stat="identity", alpha=0.5) +
            ylim(-700,700) +
            theme_minimal() +
            theme(
                legend.position = "none",
                axis.text = element_blank(),
                axis.title = element_blank(),
                panel.grid = element_blank(),
                plot.margin = unit(rep(-1,4), "cm") 
            ) +
            coord_polar() +
            geom_text(data=label_data1, aes(x=id, y=TotalRuns, label=bowler, hjust=hjust), color="black", fontface="bold",alpha=1.6, size=3.0, angle= label_data1$angle, nudge_y=150,inherit.aes = FALSE ) +
            geom_text(data=label_data1, aes(x=id, y=TotalRuns, label=TotalRuns, hjust=hjust), color="black", fontface="bold",alpha=1.6, size=3.0, angle= label_data1$angle,inherit.aes = FALSE ) +
            geom_segment(data=base_data1, aes(x = start, y = -40, xend = end, yend = -40), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
            geom_text(data=base_data1, aes(x = title, y = -55, label=year), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)+
            scale_fill_brewer(palette = "Set2") 
        print(p)
    })
    
    output$run_perteam <- renderUI({ 
        HTML(str_glue(
            "<div>
          <p> The horizontal stacked bar plot, gives us the information about the number of runs given by the bowlers per team in four seasons.
          We can visualise from the bar plot that most runs have been given to Mumbai Indians, This could mean that CSK bowlers
          can not perform well infront of them. Many bowlers performances have increased for the season 2018 and 2019 in comparison to previous seasons.
          The runs given by the bowlers decreased by a significant amount.</p> 
          </div>"
        ) )
        
    })
    
    run_csk_filter<-reactive({
        run_csk_df<-run_bowler1%>%filter(year==input$csk_runs)
    })
    
    
    output$plot7=renderPlotly({
        
        r_csk=ggplot(run_csk_filter(), aes(fill=batting_team, y=TotalRuns, x=bowler)) + 
            geom_bar(position="stack", stat="identity")+
            coord_flip()+
            ggtitle("Number of runs given per team in a year")
        
        print(r_csk)
    })
    
    wicket_csk_filter<-reactive({
        wicket_csk_df<-wicket_bowler1%>%filter(year==input$csk_wicket)
    })
    
    output$wicket_perteam <- renderUI({ 
      HTML(str_glue(
        "<div>
          <p> The horizontal stacked bar plot, gives us the information about the number of wickets taken by the bowlers per team in four seasons.
          We can visualise from the bar plot that most runs have been given to Mumbai Indians. Many bowlers performances have increased for the season 2018 and 2019 in comparison to previous seasons.
          The runs given by the bowlers decreased by a significant amount.</p> 
          </div>"
      ) )
      
    })
    
    output$plot9=renderPlotly({
        
        w_csk=ggplot(wicket_csk_filter(), aes(fill=batting_team, y=Totalwickets, x=bowler)) + 
            geom_bar(position="stack", stat="identity")+
            coord_flip()+
          ggtitle("Number of wickets taken per team in a year")
        
        print(w_csk)
    })
    
    wicket_csk<- reactive({
        wicket_bowler[wicket_bowler$Totalwickets >=input$csk_wickets[1] & 
                       wicket_bowler$Totalwickets <= input$csk_wickets[2], ]
    })
    
    output$circular_wicket <- renderUI({ 
        HTML(str_glue(
            "<div>
          <h4>Total wickets taken for each year</h4>
          <p> The circular plot, gives us the information about the total number of wickets taken given by the bowlers in each year..
          Player's performance can assessed for 4 years which is 2 years before and after the ban. 
            To get better insight as to which player has improved their bowling performance and how much benificial he will
            be for upcoming season. </p> 
          </div>"
        ) )
        
    })
    output$plot8<-renderPlot({
        
        empty_bar_1 <- 3
        to_add <- data.frame( matrix(NA, empty_bar_1*nlevels(wicket_bowler$year), ncol(wicket_bowler)) )
        colnames(to_add) <- colnames(wicket_bowler)
        to_add$year <- (rep(levels(wicket_bowler$year), each=empty_bar_1))
        #run_bowler <- rbind(wicket_bowler, to_add)
        wicket_bowler <- wicket_csk() %>% arrange(year)
        wicket_bowler$id <- seq(1, nrow(wicket_bowler))
        
        label_data2=wicket_bowler
        
        number_of_bar <- nrow(label_data2)
        angle <-  90 - 360 * (label_data2$id-0.5) /number_of_bar 
        
        label_data2$hjust<-ifelse( angle < -90, 1, 0)
        label_data2$angle<-ifelse(angle < -90, angle+180, angle)
        
        base_data2 <- wicket_bowler %>% 
            group_by(year) %>% 
            summarize(start=min(id), end=max(id)) %>% 
            rowwise() %>% 
            mutate(title=mean(c(start, end)))
        
        grid_data2 <- base_data2
        grid_data2$end <- grid_data2$end[ c( nrow(grid_data2), 1:nrow(grid_data2)-1)] + 1
        grid_data2$start <- grid_data2$start - 1
        grid_data2 <- grid_data2[-1,]
        
        p1=ggplot(wicket_bowler, aes(x=as.factor(id), y=Totalwickets, fill=as.factor(year))) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
            
            geom_bar(aes(x=as.factor(id), y=Totalwickets, fill=as.factor(year)), stat="identity", alpha=0.5) +
            
            geom_segment(data=grid_data2, aes(x = end, y = 40, xend = start, yend =40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
            geom_segment(data=grid_data2, aes(x = end, y = 30, xend = start, yend = 30), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
            geom_segment(data=grid_data2, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
            geom_segment(data=grid_data2, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
            
            
            # Add text showing the value of each 100/75/50/25 lines
            annotate("text", x = rep(max(wicket_bowler$id),4), y = c(10, 20, 30, 40), label = c("10", "20", "30", "40") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
            
            geom_bar( aes(x=as.factor(id), y=Totalwickets, fill=as.factor(year)), stat="identity", alpha=0.5) +
            ylim(-50,40) +
            theme_minimal() +
            theme(
                legend.position = "none",
                axis.text = element_blank(),
                axis.title = element_blank(),
                panel.grid = element_blank(),
                plot.margin = unit(rep(-1,4), "cm") 
            ) +
            coord_polar() +
            geom_text(data=label_data2, aes(x=id, y=Totalwickets, label=bowler, hjust=hjust), color="black", fontface="bold",alpha=1.6, size=3.0, angle= label_data2$angle, nudge_y=8,inherit.aes = FALSE ) +
            geom_text(data=label_data2, aes(x=id, y=Totalwickets, label=Totalwickets, hjust=hjust), color="black", fontface="bold",alpha=1.6, size=3.0, angle= label_data2$angle,inherit.aes = FALSE ) +
            geom_segment(data=base_data2, aes(x = start, y = -3, xend = end, yend = -3), colour = "black", alpha=0.8, size=0.8 , inherit.aes = FALSE )  +
            geom_text(data=base_data2, aes(x = title, y = -5, label=year), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) +
            scale_fill_brewer(palette = "Set1") 

        print(p1)
    })
    
    
}





#Run shiny dashboard
shinyApp(ui,server)

