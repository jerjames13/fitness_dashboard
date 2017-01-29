library(httr)
library(XML)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(cowplot)

#login info for jefit and mfp
inputs4 <- list(username='jerjames13@gmail.com', password='testpw', submit='Log In')
inputs5 <- list(vb_login_username='bubbatully', vb_login_password='testpw', 
                do='login', cookieuser='1', s="06fbe588935b264db32b50e58b415eb3", securitytoken="1483402115-c53383d1087c2ad5fbf1cab6d447e55bca1b83bd",
                vb_login_md5password="", vb_login_md5password_utf="")
setwd("/Users/jeremy/Google Drive/MFP")

#get last 10 weight data points from mfp
POST('https://www.myfitnesspal.com/account/login', body=inputs4,encode='form', verbose())
chart <- GET('https://www.myfitnesspal.com/measurements/edit')
weight <- as.data.frame(readHTMLTable(rawToChar(chart$content),
                                      stringsAsFactors = FALSE))
weight <- weight[1:10,2:3]


#restructure new weight data, read csv of old weight data, merge the two into new master file
colnames(weight) <- c('Date', 'Weight')
weight$Weight <- as.numeric(substr(weight$Weight, 1, nchar(weight$Weight)-4))
weight$Date <- as.Date(weight$Date, format = '%m/%d/%Y')

weight2 <- read.csv("weight.csv")[,2:3]
new <- weight[as.Date(as.character(weight$Date)) > max(as.Date(weight2$Date)),]
weight <- rbind(new,weight2)
write.csv(weight, "weight.csv")
weight$Date <- as.Date(weight$Date)
weight17 <- weight[weight$Date > '2016-12-31',]

# create line chart of weight
w <- ggplot(weight17, aes(Date, Weight, group = 1)) + geom_line() + geom_point() +
  coord_fixed(ratio = .6) + theme_minimal() + coord_cartesian(ylim = c(165, 175)) + 
  theme(axis.title.x=element_blank(),axis.title.y  = element_text(size=17), panel.grid = element_blank()) +
  scale_y_continuous(breaks=c(165,170,175))+ scale_x_date()
w



#####


POST('https://www.jefit.com/login/sitelogin.php?do=login', body=inputs5, encode='form', verbose())

old_workout <- read.csv("workout.csv")[,2:8]
#old_workout$both <- c(0,1)
old_workout$date <- as.Date(old_workout$date)
#old_workout$ohp_max <- 92.5
#old_workout$bench_max <- 145.66
#old_workout$deadlift_max <- 166.5
workout <- data.frame(date = as.Date(character(0)), strength = numeric(0), 
                      cardio = numeric(0), both = numeric(0), bench_max = numeric(0),
                      squat_max=numeric(0), deadlift_max=numeric(0))
dates <- seq(max(old_workout$date),Sys.Date(), by='day')

#loop through jefit logs to pull workout counts and max lifts
for (i in 1:length(dates)) {
  chart2 <- GET(paste('https://www.jefit.com/my-jefit/my-logs/log/?dd=', dates[i],sep=''))
  
  #if log contains "treadmill" or "rowing", count as cardio. if it contains "Set 1", count it as strength. Will need
  #to add additional lines here if I plan to do other cardio exercises or do strength workouts that don't have sets
  cardio <- ifelse(length(grep('Treadmill', rawToChar(chart2$content))) + length(grep('Rowing', rawToChar(chart2$content))) > 0, 1, 0)
  strength <- length(grep('Set 1', rawToChar(chart2$content)))
  
  #find most recent 1RM from XML, reformat it to be numeric
  squat_search <- regexpr('=\"eid\" value=\"12\"/>\n                <input type=\"hidden\" name=\"myrecord\" value=\"', rawToChar(chart2$content))
  bench_search <- regexpr('eid\" value=\"2\"/>\n                <input type=\"hidden\" name=\"myrecord\" value=\"', rawToChar(chart2$content))
  deadlift_search <- regexpr('eid\" value=\"93\"/>\n                <input type=\"hidden\" name=\"myrecord\" value=\"', rawToChar(chart2$content))
  squat_max <- as.numeric(paste(strsplit(gsub("[^0-9.]", "", unlist(substr(rawToChar(chart2$content), 
                                                                  squat_search[[1]]+attr(squat_search,"match.length"), squat_search[[1]]+attr(squat_search,"match.length")+5))),
                                                                  "")[[1]], collapse = ""))
  bench_max <- as.numeric(paste(strsplit(gsub("[^0-9.]", "", unlist(substr(rawToChar(chart2$content), 
                                                            bench_search[[1]]+attr(bench_search,"match.length"), bench_search[[1]]+attr(bench_search,"match.length")+5))),
                                                            "")[[1]], collapse = ""))
  deadlift_max <- as.numeric(paste(strsplit(gsub("[^0-9.]", "", unlist(substr(rawToChar(chart2$content), 
                                                                       deadlift_search[[1]]+attr(deadlift_search,"match.length"), deadlift_search[[1]]+attr(deadlift_search,"match.length")+5))), 
                                                                       "")[[1]], collapse = ""))
  #save results for each day on a different row
  workout[i,1] <- dates[i]
  workout[i,2] <- strength
  workout[i,3] <- cardio
  workout[i,4] <- ifelse((strength+cardio) == 2, 1, 0)
  workout[i,5] <- ifelse(is.na(bench_max) & (length(workout[i-1,5])==0), old_workout[nrow(old_workout),5],
                         ifelse(is.na(bench_max),workout[i-1,5], bench_max))
  workout[i,6] <- ifelse(is.na(squat_max) & (length(workout[i-1,6])==0), old_workout[nrow(old_workout),6],
                         ifelse(is.na(squat_max),workout[i-1,6], squat_max))
  workout[i,7] <- ifelse(is.na(deadlift_max) & (length(workout[i-1,7])==0), old_workout[nrow(old_workout),7],
                         ifelse(is.na(deadlift_max),workout[i-1,7], deadlift_max))
  print(i)
}

#combine old workout data with new workout data, create rollup field to contain running sum of workouts
rollup <- rbind(old_workout[old_workout$date < min(workout$date),], workout)
rollup$both_run <- cumsum(rollup$both)
rollup$strength_run <- cumsum(ifelse(rollup$both == 0, rollup$strength, 0))
rollup$cardio_run <- cumsum(ifelse(rollup$both == 0, rollup$cardio, 0))
rollup$type <- ifelse(rollup$both == 1, 'both', ifelse(rollup$strength == 1, 'strength', ifelse(rollup$cardio == 1, 'cardio', 0)))

#reformat exercise count and max lift datasets to a form more easily plotted
rollup_e <- melt(rollup[,c(1,8:11)], id.vars=c("date", "type"))
colnames(rollup_e)[4] <- 'Workouts'
rollup_m <- melt(rollup[,c(1,5:7)], id.vars=c("date"))
colnames(rollup_m)[3] <- 'Max'

#get current counts and current maxes
cardio_workouts <-  max(rollup_e$Workouts[rollup_e$variable == 'cardio_run'])
both_workouts <- max(rollup_e$Workouts[rollup_e$variable == 'both_run'])
strength_workouts <-  max(rollup_e$Workouts[rollup_e$variable == 'strength_run'])
e_current <- cardio_workouts + both_workouts + strength_workouts

squat_current <-  max(rollup_m$Max[rollup_m$variable == 'squat_max'])
bench_current <-  max(rollup_m$Max[rollup_m$variable == 'bench_max'])
deadlift_current <-  max(rollup_m$Max[rollup_m$variable == 'deadlift_max'])
m_current <- squat_current + bench_current + deadlift_current


#labels for use in charts
labels_e <- data.frame(variable = c('both', 'strength', 'cardio'), Workouts = c(e_current-.5, cardio_workouts + (strength_workouts/2), cardio_workouts/2))
labels_m <- data.frame(variable = c(paste('bench: ', bench_current, sep=""), paste('squat: ', squat_current, sep=""),
                                    paste('deadlift: ', deadlift_current, sep="")), Workouts = c(m_current-75,  deadlift_current + (squat_current/2), deadlift_current/2))


#create exercise count chart
e <- ggplot(rollup_e, aes(date, Workouts)) + geom_area(aes(fill=variable)) +
  scale_fill_manual(values = alpha(c("#CC6666", "#9999CC", "#66CC99"), .65)) + guides(fill=FALSE) +
  theme_minimal() + coord_cartesian(ylim = c(0, 20)) + theme(axis.title.x=element_blank(),axis.title.y  = element_text(size=17))+
  geom_label(data=labels_e, aes(x=Sys.Date(), y=Workouts, label=variable), size=3, fontface='bold', hjust=1) 


#create 1RM max chart
m <- ggplot(rollup_m, aes(date, Max)) + geom_area(aes(fill=variable))+
  scale_fill_manual(values = alpha(c("#CC6666", "#9999CC", "#66CC99"), .65)) + guides(fill=FALSE) +
  theme_minimal() + coord_cartesian(ylim = c(0, 750)) + theme(axis.title.x=element_blank(),axis.title.y  = element_text(size=17))+
  geom_label(data=labels_m, aes(x=Sys.Date(), y=Workouts, label=variable), size=3, fontface='bold', hjust=1) 



#create top frame of chart
df <- data.frame()
w_current <- weight[1,2]
e_current <- cardio_workouts + both_workouts + strength_workouts
b_current <- 4
w_projected <- predict(lm(Weight ~ Date, data=weight17), data.frame(Date = as.Date('2017-03-01')))[[1]]
#labels <- data.frame(variable = c('both', 'strength', 'cardio'), Workouts = c(4.5, 3, 1.2))

b <- ggplot(df) + xlim(0, 10) + ylim(0, 10) + geom_text(data=df, aes(x=1, y=8, label='Books Read'), fontface='bold', size=5.5) + 
      geom_text(data=df, aes(x=1, y=1.3, label=b_current), size=5.5)+
      geom_text(data=df, aes(x=3.67, y=8, label='Weight'), fontface='bold', size=5.5)+
      geom_text(data=df, aes(x=3.67, y=1.3, label=w_current), size=5.5)+
      geom_text(data=df, aes(x=6.34, y=8, label='Workouts'), fontface='bold', size=5.5)+
      geom_text(data=df, aes(x=6.34, y=1.3, label=e_current), size=5.5) +
      geom_text(data=df, aes(x=9, y=8, label='Max'), fontface='bold', size=5.5)+
      geom_text(data=df, aes(x=9, y=1.3, label=m_current), size=5.5)+
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
      axis.text.y=element_blank(),axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank())



grid.arrange(b,w,e,m, heights=c(.6,1,3,3))

ggsave("file = plot.jpeg", plot=grid.arrange(b,w,e,m, heights=c(.6,1,3,3)),
       scale =1, bg = 'transparent', width=6.3, height=9.5, units='in')

write.csv(rollup[1:(nrow(rollup)-5), 1:7], "workout.csv")


setwd("/Users/jeremy")


