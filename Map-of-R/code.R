
library(readxl)
library(ggplot2)
library(dplyr)
library(stars)
library(ggspatial)

# point_data ---------------------------------------------------------------
point_site <- read_xlsx("city_site.xlsx")
point_data <- data.frame(id = 1:201,
                        value = runif(201, 1, 10),
                        group1 = rep(c('A-index','B-index','C-index'),
                                    each = 67),
                        group2 = c(rep('group-A', each = 100),rep('group-B', times = 101)))
point_data <- merge(point_site, point_data)
df_st_as_sf <- st_as_sf(point_data, coords = c("lon", "lat"),crs = 4326)

# province_data ----------------------------------------------------------------
pro_data <- read_xlsx("pro_data.xlsx")
china_map <- sf::st_read("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json")[c("adcode", "name", "geometry")]
china_map <- merge(china_map, pro_data, by.x = "name", by.y = "province", all.x = TRUE)
china_map <- mutate_all(china_map, ~replace(., is.na(.), 0))
nat.earth <- raster::brick("HYP_50M_SR_W.tif")

# data_range --------------------------------------------------------------
myfun <- function(value) {
  dis = "排名"
  if(value == 0)
    dis = "0"
  if(value>0 & value <= 5)
    dis = "1-5"
  if(value > 5 & value <= 10)
    dis = "6-10"
  if(value > 10 & value <=15)
    dis = "11-15"
  if(value > 15 & value <= 20)
    dis = "16-20"
  if(value > 20 & value <= 25)
    dis = "21-25"
  if(value > 25)
    dis = ">25"
  dis
}
china_map$dis <- sapply(china_map$index, function(x) myfun(x))
china_map$dis <- factor(china_map$dis,
                           levels = c("0","1-5","6-10","11-15","16-20",
                                      "21-25",">25"))

c("#f7fbfe","#08306c","#07519c","#4292c7","#6badd7","#9fcbe2","#a1afc9")

# Plot --------------------------------------------------------------------

library(ggrepel)

ggplot(china_map) +
  theme_bw() +
  geom_sf(color = 'black', size = 0.8, aes(fill = dis)) +
  scale_fill_manual(values = c("#f7fbfe", "#ffa400", "#07519c", "#4292c7", "#6badd7", "#9fcbe2", "#a1afc9")) +
  geom_text_repel(aes(label = name, geometry = geometry),
                  stat = "sf_coordinates",
                  size = 3,
                  color = "black",
                  min.segment.length = 0,  # 确保线段始终绘制
                  segment.color = "gray50",
                  box.padding = 0.5,  # 标签与点的距离
                  point.padding = 0.3) +
  annotation_north_arrow(location = "tl", which_north = F,
                         pad_x = unit(0.05, "in"),
                         pad_y = unit(0.05, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.line = element_blank(),
        panel.background = element_rect("#f5f6f1")) + # 地图底层颜色设置
  coord_sf(ylim = c(-3687082, 1654989),
           xlim = c(-3000000, 2700000),
           crs = "+proj=laea +lat_0=40 +lon_0=104") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  labs(x = '', y = '', color = NULL)

ggsave("Map_plot_with_labels.pdf", width = 7, height = 4.5)


