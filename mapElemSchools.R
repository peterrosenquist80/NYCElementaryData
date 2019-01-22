if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)
library(dplyr)
library(cowplot)
library(gridExtra)
#cool
#bring in the data
schools <- read.csv(file = '2016 school explorer.csv', stringsAsFactors = F)
#get coordinates for mapping
schools_coor <- schools[schools$Grade.High=='05' & schools$Grade.5.Math...All.Students.Tested > 0,
                        c("Longitude", "Latitude")]
schools_coor <- cbind(schools_coor, 1:591)
scores <- readRDS("scores_cat.rds") %>%
  cbind(1:591)
#merge coordinates with scores/classes
schools_merge <- merge(scores, schools_coor)[,-1]
#fix categories' levels 
schools_merge[schools_merge$cat3==1,"cat3"] <- "Low Wealth/Performance"
schools_merge[schools_merge$cat3==2,"cat3"] <- "High Wealth/Performance"
schools_merge[schools_merge$cat3==3,"cat3"] <- "Medium Wealth/Performance"
schools_merge[schools_merge$cat4==1,"cat4"] <- "Med-Low Wealth/Performance"
schools_merge[schools_merge$cat4==2,"cat4"] <- "Med-High Wealth/Performance"
schools_merge[schools_merge$cat4==3,"cat4"] <- "Low Wealth/Performance"
schools_merge[schools_merge$cat4==4,"cat4"] <- "High Wealth/Performance"
schools_merge[,'cat3'] <- factor(schools_merge$cat3, 
                                  levels = c("Low Wealth/Performance",
                                             "Medium Wealth/Performance",
                                             "High Wealth/Performance" ))
schools_merge[,'cat4'] <- factor(schools_merge$cat4, 
                                 levels = c("Low Wealth/Performance",
                                            "Med-Low Wealth/Performance",
                                            "Med-High Wealth/Performance",
                                            "High Wealth/Performance" ))
#register google maps api key
register_google(key = "AIzaSyBjNwBkk5_bAYgCXQKHlBh9yV5VjZ51y3s")
#ensure key is registered
has_google_key()
#generate google map of nyc, using coordinates and zoom
nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71),
                   maptype = "roadmap", zoom = 11)
#plot schools over nyc map
map <-ggmap(nyc_map, 
        base_layer = ggplot(data = schools_merge, aes(x = Longitude, y = Latitude))) +
        geom_point(aes(shape = cat4, color = cat4)) +
        labs(color = "Classes", shape = "Classes") +
        theme_void() +
        ggtitle("Elementary Schools of NY") +
        theme(plot.title = element_text(hjust = .5, face = "bold"),
              plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "in"))
p <- ggplot(aes(Economic.Need.Index, Average.Math.Proficiency), data = schools_merge) + 
        geom_point(aes(shape=cat4, color=cat4)) +
        theme(legend.position = "none", 
              axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10)) +
        coord_fixed(ratio = .2) +
        ggtitle("Economic Need vs Math Proficiency") +
        xlab("Scaled Economic Need Index") +
        ylab("Scaled Math Proficiency")
#save image as .tiff
tiff("elemSchoolMap.tiff", units="in", width=10, height=8, res=250)
grid.arrange(map, p, nrow = 2, 
             layout_matrix = as.matrix(nrow = 3, c(1, 1, 1, 1, 1, 1, 2, 2, NULL)))
dev.off()
