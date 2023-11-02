#Data preparation script

##01 - Assign hydrological indices 
# Matrix including 28 hyd metrics 

#setup

hydro<-read_csv("data/hydrological_indices_completed.csv")%>%
  filter(Year %in% c(1991:2011))%>% #selecting the correspond timescale with fishing data 
  dplyr::select(Year, 'Area','River','January','February','March','April', #selecting hydrological indices based on literature review
         'May','June','July','August','September','October','November','December',
         'day30_min', 'day90_min','day30_max','day90_max','Max_level_days','High_flows',
         'Min_level_days','Very_highflows','Low_flows','Very_lowflows',
         'Maximum_level','Minimum_level','Date_min', 'Date_max','Rise_rate','Fall_rate','Reversals')


#Scaling data to keep the same magnitude 
data_norm <- scale(na.omit(hydro[,-c(1,2,3)]),scale = T);head(data_norm)

#checking multicollinearity of hydrological indices
corr <- cor(na.omit(data_norm));
ggcorrplot(corr)

#calculate matrix of distance between my hydrological indices based on correlation values
dist = dist(corr)

#visualing the dendrogram 
hclust = hclust(dist)
plot(hclust)

#selecting n of clusters
paran(corr, iterations=5000, centile = 95)
fviz_nbclust(corr, kmeans, method = "wss")  

#cluster analysis --------
k2 <- kmeans(corr, centers = 2, nstart = 25)#using 2 clusters 
#based on the means the day90_max, day90_min and Min_level_days, Max_level_days were selected.

#plotting clusters 
fviz_cluster(k2, data = corr, geom = "text")

#creating a new file including lag of 1,2 and 3 yrs. This is related to the time that fish takes to respond to
#fluctuations in river water level
  
hyd <- hydro %>% 
  mutate( day30max_1 = as.numeric(lag(day30_max, 1)),
          day30max_2 = as.numeric(lag(day30_max, 2)),
          day30max_3 = as.numeric(lag(day30_max, 3)),
          day30min_1 = as.numeric(lag(day30_min, 1)),
          day30min_2 = as.numeric(lag(day30_min, 2)),
          day30min_3 = as.numeric(lag(day30_min, 3)),
          day90max_1 = as.numeric(lag(day90_max, 1)),
          day90max_2 = as.numeric(lag(day90_max, 2)),
          day90max_3 = as.numeric(lag(day90_max, 3)),
          day90min_1 = as.numeric(lag(day90_min, 1)),
          day90min_2= as.numeric(lag(day90_min, 2)),
          day90min_3 = as.numeric(lag(day90_min, 3)),
          fall1 = as.numeric(lag(Fall_rate, 1)),
          fall2 = as.numeric(lag(Fall_rate, 2)),
          fall3 = as.numeric(lag(Fall_rate, 3)),
          rise1 = as.numeric(lag(Rise_rate, 1)),
          rise2 = as.numeric(lag(Rise_rate, 2)),
          rise3 = as.numeric(lag(Rise_rate, 3)),
          datemin1 = as.numeric(lag(Date_min, 1)),
          datemin2 = as.numeric(lag(Date_min, 2)),
          datemin3 = as.numeric(lag(Date_min, 3)),
          datemax1 = as.numeric(lag(Date_max, 1)),
          datemax2 = as.numeric(lag(Date_max, 2)),
          datemax3 = as.numeric(lag(Date_max, 3)),
          max_days1=as.numeric(lag(Max_level_days,1)),
          max_days2=as.numeric(lag(Max_level_days,2)),
          max_days3=as.numeric(lag(Max_level_days,3)),
          min_days1=as.numeric(lag(Min_level_days,1)),
          min_days2=as.numeric(lag(Min_level_days,2)),
          min_days3=as.numeric(lag(Min_level_days,3)),
          high1=as.numeric(lag(High_flows,1)),
          high2=as.numeric(lag(High_flows,2)),
          high3=as.numeric(lag(High_flows,3)),
          very_high1=as.numeric(lag(Very_highflows,1)),
          very_high2=as.numeric(lag(Very_highflows,2)),
          very_high3=as.numeric(lag(Very_highflows,3)),
          low1=as.numeric(lag(Low_flows,1)),
          low2=as.numeric(lag(Low_flows,2)),
          low3=as.numeric(lag(Low_flows,3)),
          very_low1=as.numeric(lag(Very_lowflows,1)),
          very_low2=as.numeric(lag(Very_lowflows,2)),
          very_low3=as.numeric(lag(Very_lowflows,3)),
          max1=as.numeric(lag(Maximum_level,1)),
          max2=as.numeric(lag(Maximum_level,2)),
          max3=as.numeric(lag(Maximum_level,3)),
          min1=as.numeric(lag(Minimum_level,1)),
          min2=as.numeric(lag(Minimum_level,2)),
          min3=as.numeric(lag(Minimum_level,3)))

#filtering just the most weighted hydrological indices
hydrology_data_prepped<- 
  hyd[, c('Year', 'Area','River', 'day90max_1', 'day90max_2', 
          'day90max_3', 'day90min_1','day90min_2', 'day90min_3', 'max_days1', 
          'max_days2', 'max_days3', 'min_days1','min_days2','min_days3')]

#filtering any NA on the data frame (those ones from the lag effect)
final_hydrology<- 
  hydrology_data_prepped %>% 
  filter(!is.na(day90max_1),
         !is.na(day90max_2),
         !is.na(day90max_3),
         !is.na(day90min_1),
         !is.na(day90min_2),
         !is.na(day90min_3),
         !is.na(max_days1),
         !is.na(max_days2),
         !is.na(max_days3),
         !is.na(min_days1),
         !is.na(min_days2),
         !is.na(min_days3))


#restore my hydrological file
saveRDS(final_hydrology, file = "Advanced_R_codereview/scripts.rds")




