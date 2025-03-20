## 绘制甘肃省地图，人口密度与医院密度图
# https://zhuanlan.zhihu.com/p/3385910310，可参考此网站
# 阿里云DataV可视化网站

# 创建数据框
df <- data.frame(
  市县名称 = c("萧山区", "余杭区", "富阳区", "临安区", "桐庐县", "淳安县", "建德市", 
           "海曙区", "江北区", "北仑区", "镇海区", "鄞州区", "奉化区", "象山县", 
           "宁海县", "余姚市", "慈溪市", "鹿城区", "龙湾区", "瓯海区", "洞头区", 
           "永嘉县", "平阳县", "苍南县", "文成县", "泰顺县", "瑞安市", "乐清市", 
           "龙港市", "南湖区", "秀洲区", "嘉善县", "海盐县", "海宁市", "平湖市", 
           "桐乡市", "吴兴区", "南浔区", "德清县", "长兴县", "安吉县", "越城区", 
           "柯桥区", "上虞区", "新昌县", "诸暨市", "嵊州市", "婺城区", "金东区", 
           "武义县", "浦江县", "磐安县", "兰溪市", "义乌市", "东阳市", "永康市", 
           "柯城区", "衢江区", "常山县", "开化县", "龙游县", "江山市", "定海区", 
           "普陀区", "岱山县", "嵊泗县", "椒江区", "黄岩区", "路桥区", "玉环市", 
           "三门县", "天台县", "仙居县", "温岭市", "临海市", "莲都区", "青田县", 
           "缙云县", "遂昌县", "松阳县", "云和县", "庆元县", "景宁县", "龙泉市"),
  第一产业 = c(57.90, 52.60, 49.50, 46.00, 25.40, 38.40, 37.20, 16.18, 8.57, 8.46, 6.52, 
           27.15, 32.73, 81.69, 48.20, 50.19, 58.38, 2.04, 2.96, 6.66, 6.52, 17.23, 
           20.59, 28.01, 9.71, 10.10, 25.82, 21.23, 9.02, 13.40, 14.17, 22.10, 17.83, 
           18.61, 14.16, 24.17, 24.00, 27.00, 25.00, 37.00, 28.00, 13.09, 38.69, 52.14, 
           22.41, 50.91, 42.02, 21.64, 14.90, 16.56, 11.40, 12.85, 28.30, 23.76, 18.75, 
           8.99, 8.30, 21.78, 8.33, 14.15, 15.23, 23.98, 12.77, 68.93, 36.27, 35.24, 
           22.87, 19.26, 14.20, 38.60, 34.91, 16.63, 16.72, 81.39, 49.70, 21.23, 10.01, 
           12.04, 12.18, 13.40, 4.91, 7.56, 6.80, 16.18),
  第二产业 = c(693.40, 676.90, 345.00, 280.00, 160.80, 62.40, 182.00, 361.08, 204.62, 989.85, 
           657.62, 608.95, 412.28, 234.64, 354.75, 701.46, 1168.38, 279.89, 383.09, 
           286.26, 47.44, 195.58, 247.74, 127.92, 28.34, 41.60, 464.59, 591.85, 144.61, 
           350.47, 338.91, 347.98, 313.74, 574.81, 452.98, 481.42, 447.00, 271.00, 
           305.00, 347.00, 216.00, 360.91, 733.94, 514.58, 221.20, 617.62, 259.37, 
           199.55, 112.61, 130.35, 97.04, 46.03, 198.23, 422.03, 272.85, 335.31, 
           45.17, 75.84, 67.69, 52.85, 104.17, 136.43, 202.85, 110.28, 256.05, 19.57, 
           276.58, 253.70, 266.92, 334.84, 114.23, 120.18, 108.83, 502.45, 323.38, 
           119.28, 97.80, 106.71, 46.84, 45.17, 43.99, 28.26, 16.86, 51.13),
  第三产业 = c(1077.20, 2322.10, 417.50, 274.40, 190.10, 139.80, 172.80, 823.92, 453.51, 
           1022.18, 365.92, 1630.04, 240.76, 246.67, 319.60, 469.07, 781.54, 890.39, 
           331.35, 408.32, 60.45, 249.08, 266.18, 207.15, 73.97, 70.29, 546.69, 
           649.93, 162.78, 433.98, 351.45, 285.69, 212.95, 437.36, 311.86, 497.39, 
           530.00, 170.00, 214.00, 318.00, 243.00, 634.53, 744.10, 477.10, 217.85, 
           693.83, 299.88, 433.30, 131.75, 124.42, 126.02, 61.82, 173.63, 1039.81, 
           346.55, 295.49, 183.01, 111.73, 84.13, 83.50, 128.21, 152.23, 353.94, 
           260.65, 92.48, 61.68, 392.46, 284.42, 388.27, 259.13, 124.25, 164.88, 
           134.96, 553.03, 365.40, 266.45, 141.32, 124.69, 71.78, 60.92, 38.09, 
           43.11, 51.10, 79.84)
)

# 查看数据
head(df)




plot_gansu_map <- function(map_file, data) {
  library(dplyr)
  library(sf)
  library(ggplot2)
  library(ggspatial)
  library(ggrepel)
  library(tidyr)
  
  # 读取地图数据
  map_data <- st_read(map_file, quiet = TRUE)
  
  # 仅选择数据列
  people_data <- data[, c(1, 2)]
  
  # 确保名称匹配（去除空格）
  map_data$name <- trimws(map_data$name)
  people_data$市县名称 <- trimws(people_data$市县名称)
  
  # 合并数据
  map_data <- merge(map_data, people_data, by.x = "name", by.y = "市县名称", all.x = TRUE)
  
  # 替换 NA 为 0
  map_data <- map_data %>% mutate(across(everything(), ~replace_na(., 0)))
  
  # 获取用于填充的列名（动态选择）
  fill_column <- colnames(data)[2]
  
  # 绘制地图
  p <- ggplot(map_data) +
    theme_bw() +
    geom_sf(color = 'black', size = 0.4, aes(fill = .data[[fill_column]])) +  # 用动态列填充
    scale_fill_gradient(low = "#FFDAB9", high = "#07519c", name = fill_column) +  # 颜色梯度填充
    geom_text_repel(aes(label = name, geometry = geometry),
                    stat = "sf_coordinates",
                    size = 3,
                    color = "black",
                    min.segment.length = 0,  # 确保线段始终绘制
                    segment.color = "gray50",
                    box.padding = 0.1,  # 标签与点的距离
                    point.padding = 0.3) +
    annotation_north_arrow(location = "tl", which_north = FALSE,
                           pad_x = unit(0.05, "in"),
                           pad_y = unit(0.05, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", width_hint = 0.3) +
    theme(axis.text = element_text(size = 12, color = "black"),
          axis.line = element_blank(),
          panel.background = element_rect(fill = "#f5f6f1"),  # 地图底层颜色
          legend.background = element_rect(fill = alpha("#ffffff", 0.6), color = "black"),  # 半透明背景
          legend.title = element_text(size = 10, face = "bold"),
          legend.text = element_text(size = 10),
          legend.key.size = unit(0.6, "lines"),
          legend.spacing = unit(0.2, "lines"),
          legend.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"),
          legend.position = c(0.88, 0.17)) +  # 图例在右下角
    coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=104") +  # 投影
    labs(x = '', y = '', color = NULL)
  
  return(p)
}

# 使用函数绘图
data1 <- df[, c(1, 2)]  # 第一产业
p1 <- plot_gansu_map("浙江省.json", data1)
p1

data2 <- df[, c(1, 3)]  # 第二产业
p2 <- plot_gansu_map("浙江省.json", data2)
p2

data3 <- df[, c(1, 4)]  # 第二产业
p3 <- plot_gansu_map("浙江省.json", data3)
p3

library(patchwork)

p4 <- p1 + ggtitle("a.第一产业") +
  theme(plot.title = element_text(hjust = 0, vjust = -1, size = 12, face = "bold"))  # 调整标题位置和样式

p5 <- p2 + ggtitle("b.第二产业") +
  theme(plot.title = element_text(hjust = 0, vjust = -1, size = 12, face = "bold"))  # 调整标题位置和样式

p6 <- p2 + ggtitle("c.第三产业") +
  theme(plot.title = element_text(hjust = 0, vjust = -1, size = 12, face = "bold"))  # 调整标题位置和样式

combined_plot <- p4+ p5+p6 + plot_layout(ncol = 3)  # 按列排列，每行 3列

print(combined_plot)




