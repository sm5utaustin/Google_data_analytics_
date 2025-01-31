install.packages("tidyverse")
library("tidyverse")
install.packages("readr")
read.csv()

heart_rate_seconds <- read.csv("heartrate_seconds_merged.csv"", header = TRUE, sep = ",")



View(dailyActivity_merged)
clean_name
clean_names(dailyIntensities_merged)
is.null(dailyIntensities_merged)

pacman

install.packages("dplyr")
install.packages("tidyverse")
library("dplyr")
library("tidyverse")

#一番上が最新
Daily_fitness_cmp <-inner_join(Daily_fitness,weightLogInfo_merged)
Daily_fitness <- inner_join(Daily_fitness_ver2,sleepDay_merged)
Daily_fitness_active <-inner_join(Daily_fitness_ver5,dailySteps_merged)
Daily_finness_ver1 <-inner_join(Daily_fitness_ver5,dailyActivity_merged2)
Daily_fitness_ver5 <-inner_join(dailyCalories_merged,dailyIntensities_merged)

dailyActivity_merged2 <-rename(dailyActivity_merged,ActivityDay = ActivityDate)
print(dailyActivity_merged)

install.packages("janitor")
library("janitor")
＃フィルター
filter_cmp <-filter(Daily_fitness_cmp,(SleepDay == "5/11/2016 12:00:00 AM"| SleepDay == "4/12/2016 12:00:00 AM"))

#正式に処理
filter_cmp<-filter(Daily_fitness_cmp,IsManualReport == "TRUE")


＃重複確認
filter_dupes <-filter_cmp %>%
get_dupes()

rm(test)
#書き換え値　分から時間
test<- mutate(filter_cmp,TotalHourAsleep = TotalMinutesAsleep/60)

#データを算出

＃各運動を割り出し不足している運動を算出
install.packages("ggplot2")
library("ggplot2")

ggplot(data = Daily_fitness_cmp)+ geom_point(mapping = aes(x=ActivityDay,y = WeightKg)) 

flter_cmp %<%
install.packages("skimr")
library("skimr")
skim(Daily_fitness_cmp)

clean_list_fitness <-Daily_fitness_cmp %>%
clean_names(Daily_fitness_cmp) 

distinct()

＃グループ化したものを書き換える
selected_data <- filter_cmp %>%
select(Id,ActivityDay,WeightKg,BMI,Calories,TotalMinutesAsleep)
group_by(Id)
summarise(filter_cmp,mean(TotalMinutesAsleep/60))

clean_names(selected_data)

selected_data <- selected_data %>%
distinct()

ggplot(data= Group_ID) + geom_line(mapping = aes(x=ActivityDay,y=WeightKg))
＃グループ化
Group_ID <- filter_cmp %>%

Group_ID %>%
group_by(Id)


#平均値をだす
Average_Sleep <- Group_ID
summarise(Group_ID,mean(TotalMinutesAsleep/60))
select(Id,TotalMinutesAsleep)

#列の書き換え
Avg_data <-group_by(selected_data,Id)
Avg_sleep< - summarise(Avg_data,mean(TotalMinutesAsleep/60))

Avg_data<- 
  selected_data %>%
  group_by(Id) %>%
  summarise(avg_BMI=mean(BMI),
            avg_Sleep=mean(TotalMinutesAsleep/60),
            avg_Cal=mean(Calories))

Avg_try<-
  filter_cmp2 %>%
  group_by(Id) %>%
  summarise(Avg_BMI=mean(avg_BMI),
            Avg_Sleep=mean(avg_Sleep),
            Avg_Cal=mean(avg_Cal),
            Max_heartbeat=max(Value),
            Min_heartbeat=min(Value))
#Ave_cleanが正解
#推測：Avg_perfectとtryがそのように分けられてしまうのはValueの中での一番大きい値と小さい値で分けられてしまうため
#各6個の最大値と最低値を支持する必要がある

filter_cmp2 %>%
  group_by(Id) %>%
  summarise(Max_heartbeat=max(Id=="4558609924",Value))

Analyze_data <- 
  filter_cmp2 %>%
  group_by(Id)
  rename(filter_cmp, Avg_heartbeat="Value")


#先に平均値のデータをVisualizationさせる
ggplot(data=Avg_clean)+geom_bar(mapping=aes(x=Id,fill=BMI))
ggplot(data=Avg_clean)+geom_smooth(mapping=aes(x=Id,y=BMI))
ggplot(data=Avg_clean)+geom_smooth(mapping=aes(x=BMI,y=Cal),linetype=Id)


＃データの書き換え
library("stringr")

Avg_clean <- 
Avg_data2 %>%
str_replace_all(Avg_data2,c(	
"1503960366"= "A","4319703577"= "B","4558609924" = "C", "6962181067"="D"))

Avg_clean <- 
Avg_data2 %>%
Avg_clean <- str_replace_all(Avg_data2,c(	
1503960366,4319703577,4558609924, 6962181067),c("A","B","C","D"))
Avg_clean <- replace(Avg_clean,pattern =1503960366,replacement=A)

Avg_clean < - replace(Avg_clean,c(1503960366,4319703577,4558609924,6962181067),c("A","B","C","D"))
＃余分なスペースを消す
str_trim(filter_cmp2)


#各平均がでた            


Avg_best<- filter_cmp2 %>%
group_by(Id) %>%
summarise(BMI=round(avg_BMI,0),
          Sleep=round(avg_Sleep,0),
          Cal=round(avg_Cal,0),
          Avg_Heartbeat=mean(Value))

Avg_perfect <- Avg_best %>%
group_by(Id) %>%
summarise(Avg_BMI=mean(BMI),
          Avg_Sleep=mean(Sleep),
          Avg_Cal=mean(Cal),
          Avg_Heartbeat=mean(Avg_Heartbeat))
          
Avg_perfect <- Avg_best %>%
group_by(Id) %>%
summarise(Avg_BMI=mean(BMI),
          Avg_Sleep=mean(Sleep),
          Avg_Cal=mean(Cal),
          Avg_Heartbeat=round(Avg_Heartbeat,0))
#Avg_cleanが最終的にしっかり処理のされたデータ         
Avg_heart<- Avg_beat%>%
group_by(Id) %>%          
summarise(heartbeat=round(avg_heart,0))
＃平均値グラフ

ggplot(data=Avg_new)+geom_point(mapping=aes(x=Id,y=BMI))

#データが全部飛んだので書く
#Falseのデータを削除
#BMI.Calories, Sleepの時間を算出
#平均値を出しグループ分け
IDの書き換えの前に保存と書き換え
install.packages("dplyr")
library("dplyr")
install.packages("tidyverse")
library("tidyverse")
install.packages("janitor")
library("janitor")
filter_cmp<-filter(Daily_fitness_cmp,IsManualReport== "TRUE")
#Maxの量なためむり
Overall_data <- inner_join(filter_cmp,heartrate_seconds_merged) 
selected_data <- filter_cmp %>%
  select(Id,Calories,BMI,TotalMinutesAsleep/60)

Avg_data <- selected_data %>%
group_by(Id) %>%
summarise(avg_BMI=mean(BMI),
            avg_Sleep=mean(TotalMinutesAsleep/60),
            avg_Cal=mean(Calories))
            
Avg_data2 <- Avg_data %>%
group_by(Id) %>%
summarise(BMI=round(avg_BMI,0),
          Sleep=round(avg_Sleep,0),
          Cal=round(avg_Cal,0))
          
          
＃別のデータを算出（運動量）
selected_data2 <- filter_cmp %>%
  select(Id,LightlyActiveMinutes,FairlyActiveMinutes,VeryActiveMinutes)
  
Avg_data_workout <- selected_data2 %>%
group_by(Id) %>%
summarise(Light=mean(LightlyActiveMinutes),
            Normal=mean(FairlyActiveMinutes),
            Active=mean(VeryActiveMinutes))
Avg_workout <- Avg_data_workout %>%
group_by(Id) %>%
summarise(Light=round(Light,0),
          Normal=round(Normal,0),
          Active=round(Active,0))            
ggplot(data=Avg_clean)+geom_smooth(mapping=aes(x=Id,y=BMI))
ggplot(data=Avg_clean)+geom_smooth(mapping=aes(x=BMI,y=Cal),linetype=Id)

#含まれているものを確認する

str(Avg_data2)

#睡眠のほかのデータを表示
Avg_data2
options(digits=22)
Avg_data2
ggplot(Avg_data2,aes(x=Id, y=Sleep,fill=Sleep)) +geom_bar(stat="identity") + 
labs(title= "Sleep Quality",x="Id", y="Sleep Hour")+theme_minimal()

＃運動量の差を表示

ggplot(Avg_workout,aes(x=Id, y=Active,fill=Active)) +geom_bar(stat="identity") + 
labs(title= "Workout Minutes",x="Id", y="Active")+theme_minimal()