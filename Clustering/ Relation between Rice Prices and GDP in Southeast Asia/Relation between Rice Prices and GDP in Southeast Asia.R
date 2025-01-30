library(readxl)
Data <- read_excel("C:/Producer Prices and GDP.xlsx")
View(Data)


#EDA
str(Data)
head(Data)
summary(Data)
dim(Data)
names(Data)
unique(Data$Area)
any(is.na(Data))

mean(Data$`rice_price`)
median(Data$`rice_price`)
sd(Data$`rice_price`)
range(Data$`rice_price`)
dataA <- table(Data$Area)
prop.table(table(Data$Area))


##Outliers
# IQR outliers
rice_Q1 <- quantile(Data$`rice_price`,0.25,na.rm = TRUE)
rice_Q3 <- quantile(Data$`rice_price`,0.75,na.rm = TRUE)
rice_IQR <- rice_Q3 - rice_Q1
rice_lower_bound <- rice_Q1 - 1.5 * rice_IQR
rice_upper_bound <- rice_Q3 + 1.5 * rice_IQR
rice_outliers <- Data$`rice_price`[Data$`rice_price`<rice_lower_bound | Data$`rice_price`> rice_upper_bound]
print(rice_outliers)

GDP_Q1 <- quantile(Data$`GDP`,0.25,na.rm = TRUE)
GDP_Q3 <- quantile(Data$`GDP`,0.75,na.rm = TRUE)
GDP_IQR <- GDP_Q3 - GDP_Q1
GDP_lower_bound <- GDP_Q1 - 1.5 * GDP_IQR
GDP_upper_bound <- GDP_Q3 + 1.5 * GDP_IQR
GDP_outliers <- Data$`GDP`[Data$`GDP`<GDP_lower_bound | Data$`GDP`> GDP_upper_bound]
print(GDP_outliers)

# Z outliers
rice_Z_scores <- (Data$`rice_price` - mean(Data$`rice_price`,na.rm = TRUE)) / sd(Data$`rice_price`,na.rm = TRUE)
rice_outliers_Z <- Data$`rice_price`[abs(rice_Z_scores) > 3]
print(rice_outliers_Z)

GDP_Z_scores <- (Data$`GDP` - mean(Data$`GDP`,na.rm = TRUE)) / sd(Data$`GDP`,na.rm = TRUE)
GDP_outliers_Z <- Data$`GDP`[abs(GDP_Z_scores) > 3]
print(GDP_outliers_Z)

## Historgram & Box & Scatter Plot 
library(ggplot2)
# Historgram Plot 
ggplot(Data,aes(x = `rice_price`)) + 
  geom_histogram(fill = "orange", color = "red") +
  labs(title = "Histogram", x = "ราคาข้าว")
ggplot(Data,aes(x = `GDP`)) + 
  geom_histogram(fill = "darkgreen", color = "green") +
  labs(title = "Histogram", x = "GDP")

# Box Plot 
ggplot(Data,aes(x = Area, y = `rice_price`, fill = Area)) +
  geom_boxplot(outlier.color = 'orange', outlier.size = 2) +
  labs(title = "rice price and Area",
       x = "Area",
       y = "rice price (US$ / ton)")

ggplot(Data,aes(x = Area, y = `GDP`, fill = Area)) +
  geom_boxplot(outlier.color = 'darkgreen', outlier.size = 2) +
  labs(title = "GDP and Area",
       x = "Area",
       y = "GDP (US$ )")

# Scatter Plot 
ggplot(Data,aes(y = GDP, x = `rice_price`)) + geom_point()
ggplot(Data,aes(y = GDP, x = logX)) + geom_point()


library(tidyverse)
library(cluster)
library(factoextra)
library(dplyr)

set.seed(123)
clustering_data <- Data[,c("rice_price","GDP")]
clustering_data <- na.omit(clustering_data)
clustering_data = data.frame(clustering_data)
scaled_data = scale(clustering_data)

# Elbow method
fviz_nbclust(clustering_data, kmeans,method = "wss") +
  labs(title = "Elbow Method")

fviz_nbclust(scaled_data, kmeans,method = "wss") +
  labs(title = "Elbow Method")

## Silhouette method
#non
fviz_nbclust(clustering_data,kmeans,method = "silhouette") +
  labs(title = "silhouette Method")

# scale data 
fviz_nbclust(scaled_data,kmeans,method = "silhouette") +
  labs(title = "silhouette Method")

#เลือก k = 3
k3_result <- kmeans(scaled_data, centers = 3, nstart = 25)
k3_result <- kmeans(clustering_data, centers = 3, nstart = 25)

fviz_cluster(k3_result,data = clustering_data,
             geom = "point",
             ellipse.type = "convex",
             main = "K-mean Clustering Results")
print(k3_result)

#เลือก k = 4
k4_result <- kmeans(scaled_data, centers = 4, nstart = 25)
fviz_cluster(k4_result,data = clustering_data,
             geom = "point",
             ellipse.type = "convex",
             main = "K-mean Clustering Results")
print(k4_result)

#เลือกข้อมูล
set.seed(123)
clustering_data <- Data[,c("rice_price","GDP")]
clustering_data = data.frame(clustering_data)
scaled_data = scale(clustering_data)

a = cbind(scaled_data[,c("rice_price","GDP")],Data[,c("Area","Year")])
a <- na.omit(a)

#ทำ K-mean
k3_result <- kmeans(a[,c("rice_price","GDP")],centers = 3, nstart = 25)
a$Cluster <- k3_result$cluster
fviz_cluster(k3_result,data = scaled_data,
             geom = "point",
             ellipse.type = "convex",
             main = "K-mean Clustering Results")

k4_result <- kmeans(clustering_data[,c("rice_price","GDP")],centers = 4, nstart = 25)
clustering_data$Cluster <- k4_result$cluster

#ดูจำนวนข้อมูลในแต่ละกลุ่ม
cluster_count <- table(a$Cluster)
print("จำนวนข้อมูลในแต่ละกลุ่ม")
print(cluster_count)
#ดูค่าเฉลี่ยของแต่ละกลุ่ม
cluster_means <- aggregate(cbind(GDP,rice_price)~ Cluster,
                            data = a,mean)
print("\nค่าเฉลี่ยขอองแต่ละกลุ่ม:")
print(cluster_means)

#ดูประเทศในแต่ละกลุ่ม
for(i in 1:3){
  cat("\nประเทศในกลุ่ม",i,":\n")
  print(unique(a$Area[a$Cluster == i]))
  }


#ดูข้อมูลประเททศและปีในแต่ละกลุ่ม
for(i in 1:3){
  cat("\nกลุ่มที่",i,":\n")
  #กรองข้อมูลเฉพาะกลุ่มที่ i
  cluster_data <- a[a$Cluster == i,]
  #สรุปข้อมูลแต่ละประเทศ
  countries <- unique(cluster_data$Area)
  
  for (Area in countries){
    country_data <- cluster_data[cluster_data$Area == Area,]
    cat("\nประเทศ", Area)
    cat("\nปี:",min(country_data$Year),"ถึง",max(country_data$Year))
    cat("\nค่าเฉลี่ย GDP:",mean(country_data$GDP))
    cat("\nค่าเฉลrice price:",mean(country_data$rice_price))
    cat("\n---------")
  }
}

############3
clustering_data <- Data[,c("rice_price","GDP","Area","Year")]
k3_result <- kmeans(clustering_data[,c("rice_price","GDP")],centers = 3, nstart = 25)
clustering_data$Cluster <- k3_result$cluster
#ดูข้อมูลประเททศและปีในแต่ละกลุ่ม
for(i in 1:3){
  cat("\nกลุ่มที่",i,":\n")
  #กรองข้อมูลเฉพาะกลุ่มที่ i
  cluster_data <- clustering_data[clustering_data$Cluster == i,]
  #สรุปข้อมูลแต่ละประเทศ
  countries <- unique(cluster_data$Area)
  
  for (Area in countries){
    country_data <- cluster_data[cluster_data$Area == Area,]
    cat("\nประเทศ", Area)
    cat("\nปี:",min(country_data$Year),"ถึง",max(country_data$Year))
    cat("\nค่าเฉลี่ย GDP:",mean(country_data$GDP))
    cat("\nค่าเฉลrice price:",mean(country_data$rice_price))
    cat("\n---------")
  }
}

##ทดสอบประสิทธิภาพ
install.packages("clValid")
install.packages("fpc")
library(clValid)
library(fpc)

#คำนวณ metrics
metrics <- cluster.stats( 
  d = dist(a[,c("rice_price","GDP")]),
  clustering = k3_result$cluster)

cat("Dunn Index:",metrics$dunn, "\n")
cat("Sillhouette Score:",metrics$avg.silwidth,"\n")
cat("Calinski-Harabasz Index:", metrics$ch,"\n")
