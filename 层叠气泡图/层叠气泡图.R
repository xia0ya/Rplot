# 加载必要的包
library(ggplot2)
library(dplyr)
library(ggforce)

# 创建数据框
data_gender <- data.frame(
  Gender = c("Men", "Women"),
  SpendingCelebrating = c(27, 27),
  Candy = c(52, 59),
  Flowers = c(56, 19),
  Jewelry = c(30, 14),
  GreetingCards = c(37, 43),
  EveningOut = c(33, 29),
  Clothing = c(20, 24),
  GiftCards = c(18, 24)
)

# 转换为长格式
data_long <- data_gender %>%
  tidyr::pivot_longer(
    cols = -Gender,
    names_to = "Category",
    values_to = "Value"
  )

# 为左右对称布局添加 x0 坐标和半径
data_long <- data_long %>%
  mutate(
    x0 = ifelse(Gender == "Men", -1, 1), # 左右分开
    y0 = as.numeric(factor(Category, levels = unique(Category))), # 按类别排序
    r = Value / 80 # 半径按值比例缩放
  )

# 绘图
ggplot(data_long) +
  # 绘制气泡
  geom_circle(
    aes(x0 = x0, y0 = y0, r = r, fill = Category), 
    alpha = 0.8, 
    color = "black", 
    size = 0.3
  ) +
  # 添加类别和数值标签
  geom_text(
    aes(x = x0, y = y0, label = paste(Category, "\n", Value, sep = "")),
    size = 4,
    fontface = "bold",
    color = "black"
  ) +
  # 添加性别标签
  geom_text(
    data = distinct(data_long, Gender, x0),
    aes(x = x0, y = max(data_long$y0) + 1, label = Gender),
    size = 6,
    fontface = "bold",
    color = "black"
  ) +
  # 调整填充颜色
  scale_fill_brewer(palette = "Set3") +
  # 调整背景和主题
  theme_void() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    #plot.caption = element_text(size = 12, color = "gray40", hjust = 0, margin = margin(20, 0, 0, 0)),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  # 添加标题
  labs(title = "Gender Spending Comparison Across Categories",
       caption = "Source: NRF Data | Analysis and Visualization: Liu Xiaoliang")
