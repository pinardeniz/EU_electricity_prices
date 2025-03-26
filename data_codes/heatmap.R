# Load in Data
library(readxl)
dataR <- read_excel("C:/Users/user/Google Drive/KONFERANS 2023-2024 CALISMALAR/GUELPH_RESEARCH2023/ENERGY PRICES/REGIONAL/GLOBAL DATA/RESULTS/eu22variables_regression_kink/DATA.xlsx", sheet = "SWE", range = "A1:G118") # FOR FIN : range = "A26:G118", FOR HUN: range = "A35:G118", FOR ITA: range = "A47:G118", FOR LUX: range = "A38:G118", FOR SVN: range = "A14:G118"
dataR <- data.frame(dataR)

dataR <- read.delim("C:/Users/user/Google Drive/KONFERANS 2023-2024 CALISMALAR/GUELPH_RESEARCH2023/ENERGY PRICES/REGIONAL/GLOBAL DATA/RESULTS/el.txt")


cormat <- round(cor(dataR),2)
head(cormat)

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)


library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()



# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}


upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "grey", high = "red", mid = "yellow", 
                       midpoint = 0, limit = c(0.75,1),  
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme()+
  coord_fixed()



ggplot(data = melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "grey", mid = "yellow", high = "red", 
                       midpoint = 0.70,  # Orta nokta olarak 0.85 seçildi, bunu değiştirebilirsin
                       limits = c(0.50, 1),  # Renk skalasının sınırları
                       breaks = c(0.50, 0.70, 1),  # Legend'da gri, sarı ve kırmızı görünmesi için
                       labels = c("Low (0.50)", "Mid (0.70)", "High (1)"),
                       name = "Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  coord_fixed()




################

library(ggplot2)
library(reshape2)

# Define correlation categories
melted_cormat$category <- cut(melted_cormat$value,
                              breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1),
                              labels = c("0.50-0.60", "0.60-0.70", "0.70-0.80", "0.80-0.90", "0.90-1.00"),
                              include.lowest = TRUE)

# Define colors for each category
colors <- c("0.90-1.00" = "red",
            "0.80-0.90" = "orange",
            "0.70-0.80" = "yellow",
            "0.60-0.70" = "cyan", 
            "0.50-0.60" = "lightblue")

# Heatmap plot
ggplot(data = melted_cormat, aes(Var2, Var1, fill = category)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = colors, name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  coord_fixed()



###################

library(ggplot2)
library(reshape2)

# Define correlation categories (changing every 0.05)
melted_cormat$category <- cut(melted_cormat$value,
                              breaks = seq(0.5, 1, by = 0.05),  # Bins every 0.05
                              include.lowest = TRUE)

# Define a color gradient (from blue to red)
colors <- colorRampPalette(c("lightblue", "cyan", "yellow", "orange", "red"))(length(unique(melted_cormat$category)))

# Heatmap plot
ggplot(data = melted_cormat, aes(Var2, Var1, fill = category)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = colors, name = " ") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  coord_fixed()
