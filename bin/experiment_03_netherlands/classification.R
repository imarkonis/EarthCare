require(kernlab) 
require(data.table)
require(ggplot2)

library(e1071) 

source("./source/spatial_tools.R")
load("./data/experiment_3.rdata")  #Created in data_import

my_date <- as.Date("2016-9-16")
test_rdr <- rdr_prcp[time == my_date]
test_rdr <- merge(test_rdr, rdr_cells)

test_gpm <- gpm_d_prcp[time == my_date]
test_gpm <- merge(test_gpm, gpm_d_cells)

test_knmi <- knmi_prcp[time == my_date]
test_knmi <- merge(test_knmi, knmi_stations)
test_knmi <- test_knmi[complete.cases(test_knmi)]

ggplot(test_rdr, aes(y = lat, x = lon, col = prcp)) + 
  geom_point(data = test_gpm[id %in% knmi_stations$nearest_cell], aes(y = lat, x = lon, size = prcp), col = "orange", alpha = 0.5) +
  geom_point(data = test_knmi, aes(y = lat, x = lon, size = prcp), col = "red", alpha = 0.5) +
  labs(x = NULL, y = NULL) +
  theme_bw()

svmfit <- svm(y~., data = dat, kernel = "linear", scale = FALSE)


# Plot data
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")

svmfit <- svm(y~., data = dat, kernel = "linear", scale = FALSE)
# Plot Results
plot(svmfit, dat)

dat <- test_knmi[, .(lat, lon, prcp)]
dat[prcp > 5, prcp := 1]
dat[prcp > 1, prcp := 2]

dat[prcp < 1, prcp := 0]
dat[, prcp := as.factor(prcp)]

svmfit <- svm(prcp~., data = dat, type = "C-classification", kernel = "radial", scale = FALSE)
plot(svmfit, dat)

dat <- test_gpm[id %in% knmi_stations$nearest_cell, .(lat, lon, prcp)]
dat[prcp > 5, prcp := 1]
dat[prcp > 1, prcp := 2]

dat[prcp < 1, prcp := 0]
dat[, prcp := as.factor(prcp)]

svmfit <- svm(prcp~., data = dat, kernel = "radial", scale = FALSE)
plot(svmfit, dat)







dat <- test_knmi[, .(lat, lon, prcp)]
dat <- test_gpm[id %in% knmi_stations$nearest_cell, .(lat, lon, prcp)]
#dat[prcp > 5, prcp := 1]
dat[prcp > 1, prcp := 2]
dat[prcp < 1, prcp := 0]
dat[, prcp := as.factor(prcp)]


library(RColorBrewer)
kernfit <- ksvm(as.matrix(dat[,2:1]), dat$prcp, type = "C-svc", kernel = 'rbfdot', 
                C = 100, scaled = c())

# Create a fine grid of the feature space
x.1 <- seq(from = min(dat$lat), to = max(dat$lat), length = 100)
x.2 <- seq(from = min(dat$lon), to = max(dat$lon), length = 100)
x.grid <- expand.grid(x.2, x.1)

# Get class predictions over grid
pred <- predict(kernfit, newdata = x.grid)

# Plot the results
cols <- brewer.pal(3, "Set1")
plot(x.grid, pch = 19, col = adjustcolor(cols[pred], alpha.f = 0.05))

classes <- matrix(pred, nrow = 100, ncol = 100)
contour(x = x.2, y = x.1, z = classes, levels = 1:3, labels = "", add = TRUE)

points(dat[, 2:1], pch = 19, col = cols[predict(kernfit)])












library("mclust")
data("diabetes")
dat <- test_gpm[id %in% knmi_stations$nearest_cell, .(lat, lon, prcp)]
#dat[prcp > 1, prcp := 2]
#dat[prcp < 1, prcp := 0]
df <- scale(dat) # Standardize the data
mc <- Mclust(df)            # Model-based-clustering
summary(mc)    

mc$modelName                # Optimal selected model ==> "VVV"
mc$G                        # Optimal number of cluster => 3
head(mc$z, 30)              # Probality to belong to a given cluster
head(mc$classification, 30) # Cluster assignement of each observation

library(factoextra)
# BIC values used for choosing the number of clusters
fviz_mclust(mc, "BIC", palette = "jco")
# Classification: plot showing the clustering
fviz_mclust(mc, "classification", geom = "point", 
            pointsize = 1.5, palette = "jco")
# Classification uncertainty
fviz_mclust(mc, "uncertainty", palette = "jco")
