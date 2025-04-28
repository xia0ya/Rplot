
# 加载必要的包 ---------------------------------------------------------------
library(readxl)
library(ggplot2)
library(dplyr)
library(stars)
library(ggspatial)
library(sf)
library(raster)
library(classInt)
library(ggrepel)
library(RColorBrewer)

# 读取点位数据 ----------------------------------------------------------------
point_site <- read_xlsx("city_site.xlsx")

set.seed(123)  # 保证可复现
point_data <- data.frame(
  id = 1:201,
  value = runif(201, 1, 10),
  group1 = rep(c('A-index', 'B-index', 'C-index'), each = 67),
  group2 = c(rep('group-A', 100), rep('group-B', 101))
)

# 合并坐标信息
point_data <- merge(point_site, point_data)

# 转换为sf对象
df_st_as_sf <- st_as_sf(point_data, coords = c("lon", "lat"), crs = 4326)

# 读取省级数据 ----------------------------------------------------------------
pro_data <- read_xlsx("pro_data.xlsx", sheet = '2023')

# 读取中国地图
china_map <- st_read("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json")[c("adcode", "name", "geometry")]

# 合并省级指标数据
china_map <- merge(china_map, pro_data, by.x = "name", by.y = "province", all.x = TRUE)

# NA值处理
china_map <- mutate_all(china_map, ~ replace(., is.na(.), 0))

# 读取自然地形图 ----------------------------------------------------------------
nat.earth <- raster::brick("HYP_50M_SR_W.tif")

# 自然断点分级 -----------------------------------------------------------------
# 使用自然断点法 (Jenks) 分组
# 修改配色（更适合层次展示的颜色）
colors <- brewer.pal(7, "Pastel1")  # 使用RColorBrewer的调色板

breaks <- classIntervals(china_map$index, n = 6, style = "jenks")$brks
china_map$dis <- cut(china_map$index, breaks = breaks, include.lowest = TRUE, 
                     labels = c("高水平区", "中上水平区", "中水平区", "中下水平区", 
                                "次低水平区", "低水平区"))
# Plot --------------------------------------------------------------------
p1 <- ggplot(china_map) +
  theme_bw() +
  geom_sf(color = 'black', size = 0.8, aes(fill = dis)) +
  scale_fill_manual(values = colors, 
                    name = "区域等级", 
                    labels = c("高水平区", "中上水平区", "中水平区", 
                               "中下水平区", "次低水平区", "低水平区")) +  # 设置图例标题
  geom_text_repel(aes(label = name, geometry = geometry),
                  stat = "sf_coordinates",
                  size = 3,
                  color = "black",
                  min.segment.length = 0,  # 确保线段始终绘制
                  segment.color = "gray50",
                  box.padding = 0.0,  # 标签与点的距离
                  point.padding = 0.3,
                  max.overlaps = 50) +  # 增加最大重叠次数
  annotation_north_arrow(location = "tl", which_north = F,
                         pad_x = unit(0.05, "in"),
                         pad_y = unit(0.05, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.line = element_blank(),
        panel.background = element_rect("#f5f6f1"),  # 地图底层颜色设置
        legend.position = c(0.15, 0.25),  # 将图例放置在地图内部
        legend.background = element_rect(fill = "#f5f6f1", color = "black"),  # 设置图例背景
        legend.title = element_text(size = 10, face = "bold"),  # 设置图例标题样式
        legend.text = element_text(size = 10),  # 设置图例标签字体大小
        legend.key.size = unit(0.7, "lines"),  # 缩小图例项的大小
        legend.spacing = unit(0.3, "lines"),  # 缩小图例项之间的间距
        legend.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")) +  # 缩小图例外部边距
  coord_sf(ylim = c(-3687082, 1654989),
           xlim = c(-3000000, 2700000),
           crs = "+proj=laea +lat_0=40 +lon_0=104") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  labs(x = '', y = '', color = NULL)

print(p1)
# 保存输出 ---------------------------------------------------------------------
ggsave("Map_plot_with_labels.pdf", plot = p1, width = 7, height = 4.5)



