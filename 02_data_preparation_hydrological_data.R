#Data preparation script

##01 - Assign hydrological indices 
# Matrix including 28 hyd metrics 

#setup

setwd("/home/amp29/taxonomy_of_turnover_revised/uploaded_code")

hydro<-read_csv("data_cleaning/hydrology/data/clean/hyd_all.csv")%>%
  filter(Year %in% c(1991:2011))%>% #selecting the correspond timescale with fishing data 
  select(Year, 'Area','River','January','February','March','April', #selecting hydrological indices based on literature review
         'May','June','July','August','September','October','November','December',
         'day30_min', 'day90_min','day30_max','day90_max','Max_level_days','High_flows',
         'Min_level_days','Very_highflows','Low_flows','Very_lowflows',
         'Maximum_level','Minimum_level','Date_min', 'Date_max','Rise_rate','Fall_rate','Reversals')


#Scaling data to keep the same magnitude 
data_norm <- scale(na.omit(hydro[,-c(1,2,3)]),scale = T);head(data_norm)

#checking multicollinearity of hydrological indices
corr <- cor(na.omit(data_norm));
ggcorrplot(corr)

#cluster analysis --------
dist = dist(corr)

#vizualing the dendrogram 
hclust = hclust(dist)
plot(hclust)


fviz_nbclust(corr, kmeans, method = "wss") #selecting n of clusters 
k2 <- kmeans(corr, centers = 2, nstart = 25)#using 2 clusters 
fviz_cluster(k2, data = corr)#plotting clusters 

library(paran) #identify the PC axes to retain
paran(corr, iterations=5000, centile = 95)

saveRDS(hyd, file = "statistical_models/scripts/data_prep/data_prep_hydind.rds")
