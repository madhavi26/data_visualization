#
# Madhavi Kadam
# IST 719
# Final Poster

library(dplyr)
library(ggplot2)
library(treemap)
library(NLP)
library(tm)
library(RColorBrewer)
library(tidyverse)

#loading Data
deliveries <- read.csv("deliveries.csv", sep=",", header=TRUE)
matches <- read.csv("matches.csv", sep=",", header=TRUE)

colnames(deliveries)[colnames(deliveries)=="match_id"] <- "id"
#head(matches)

p <- sort(table(matches$team1), decreasing = F)

names(p)
par(mar = c(2,13,13,2))

barplot(p, horiz = T, names.arg = names(p), las = 2, col = "coral", ylab = "Team", xlab = "Number of Matches Played", main = "Number of Matches Playes by Each Team")

w <- sort(table(matches$winner), decreasing = F)
names(w)
barplot(w, horiz = T, names.arg = names(w), las = 2, col = "pink", ylab = "Team", xlab = "Number of Matches Won", main = "Number of Matches Won by Each Team")


df_bat<- deliveries %>% group_by(batsman)%>% summarise(runs=sum(batsman_runs)) %>% arrange(desc(runs)) %>%filter(runs > 4000) 

df_bat %>% ggplot(aes(reorder(batsman,-runs),runs,fill=batsman)) +geom_bar(stat = "identity") +xlab("Batsman")+ ylab("Runs")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Player")+ ggtitle("Top Batsmen")+ guides(fill=F)

df_bowler<-deliveries %>% group_by(bowler) %>% filter(player_dismissed!="") %>% summarise(wickets= length(player_dismissed)) %>% top_n(n=10,wt=wickets) 

df_bowler %>% ggplot(aes(reorder(bowler,-wickets),wickets,fill=bowler))+geom_bar(stat = "identity") + ylab("Wickets")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Player")+ ggtitle("Top Bowlers")+ guides(fill=F)

table(matches$season)

deliveries %>% 
  left_join(matches) %>% 
  filter(batsman=="V Kohli"| batsman=="SK Raina" |batsman=="RG Sharma") %>% 
  group_by(batsman,season) %>% 
  summarise(runs = sum(batsman_runs)) %>%
  ggplot(aes(season,runs, col=batsman)) +geom_line(size= 2) + ggtitle("Season wise comparision(Runs)") + scale_x_continuous(breaks = 2008:2019)

Kohli<- deliveries %>% group_by(batsman,bowler) %>% filter(batsman=="V Kohli") %>% summarise(runs=sum(batsman_runs)) %>% top_n(n=7,wt=runs)

treemap(Kohli, 
        index=c("batsman","bowler"),  
        vSize = "runs",  
        type="index", 
        palette =brewer.pal(7,"Reds"), 
        title = "Runs by Virat Kohli against different bowlers",
        border.col="#FFFFFF",
        fontsize.legend = 0,bg.labels = "black",fontcolor.labels= "#FFFFFF",
        aspRatio= 1.1
)

Raina<- deliveries %>% group_by(batsman,bowler) %>% filter(batsman=="SK Raina") %>% summarise(runs=sum(batsman_runs)) %>% top_n(n=7,wt=runs)

treemap(Raina,
        index=c("batsman","bowler"),  
        vSize = "runs",  
        type="index", 
        palette = brewer.pal(7,"Reds"),  
        title = "Runs by SK Raina against different bowlers",
        border.col="#FFFFFF",
        fontsize.legend = 0,bg.labels = "black",fontcolor.labels= "#FFFFFF",
        aspRatio= 1.1
)

Rohit<- deliveries %>% group_by(batsman,bowler) %>% filter(batsman=="RG Sharma") %>% summarise(runs=sum(batsman_runs)) %>% top_n(n=7,wt=runs)

treemap(Rohit,
        index=c("batsman","bowler"), 
        vSize = "runs",  
        type="index", 
        palette = brewer.pal(7,"Blues"), 
        title = "Runs by Rohit against different bowlers",
        border.col="#FFFFFF",
        fontsize.legend = 0,bg.labels = "black",fontcolor.labels= "#FFFFFF",
        aspRatio= 1.1
)


scatter_df <- deliveries[!(deliveries$batsman_runs == 0),]
scatter_df <- scatter_df[!(scatter_df$batsman_runs == 2),]
#scatter_df <- scatter_df[!(scatter_df$batsman_runs == 3),]
#scatter_df <- scatter_df[!(scatter_df$batsman_runs == 5),]
scatter_df <- scatter_df[!(scatter_df$batsman_runs == 1),]
#scatter_df <- scatter_df[!(scatter_df$batsman_runs == 7),]

scatter_df %>% filter(batsman=="SK Raina") %>%
  group_by(id) %>% 
  mutate(cum_run= cumsum(batsman_runs),cum_ball=1:length(id)) %>%
  ggplot(aes(cum_ball,cum_run,col= as.factor(batsman_runs))) + scale_color_manual(values = c("rosybrown4","gold2","coral","cadetblue4","pink","firebrick4"))+
  geom_point()  + labs(col="Type of Runs") + xlab("Balls") + ylab("Runs")+
  ggtitle("Innings Progression SK Raina") +  coord_cartesian(ylim = c(0, 120)) 


