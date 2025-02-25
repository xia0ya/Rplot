# 加载必要的包
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# 创建数据框
data <- data.frame(
  Age = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  SpendingCelebrating = c(51, 40, 31, 19, 18, 13),
  Candy = c(70, 62, 58, 60, 50, 42),
  Flowers = c(50, 44, 41, 37, 32, 25),
  Jewelry = c(33, 34, 29, 20, 13, 8),
  GreetingCards = c(33, 33, 42, 42, 43, 44),
  EveningOut = c(41, 37, 30, 31, 29, 24),
  Clothing = c(33, 27, 26, 20, 19, 12),
  GiftCards = c(23, 19, 22, 23, 20, 20)
)

# 转换为长格式
data_long <- data %>%
  pivot_longer(
    cols = -Age,
    names_to = "Category",
    values_to = "Value"
  )

# 保持 Category 的原始顺序
data_long <- data_long %>%
  mutate(
    Category = factor(Category, levels = names(data)[-1]), # 按列名顺序设置因子
    x = as.numeric(factor(Age)), # 将 Age 转为数字
    size = rescale(Value, to = c(3, 15)), # 根据 Value 缩放气泡大小
    y = as.numeric(Category) # 保持 y 值与 Category 一致
  )

# 获取调色板
palette <- brewer.pal(n = length(unique(data_long$Category)), name = "Set2")

# 绘制气泡图
ggplot(data_long, aes(x = x, y = y)) +
  # 绘制气泡图
  geom_point(aes(size = size, fill = Category), shape = 21, color = "black", alpha = 0.8) +
  # 添加数据标签
  geom_text(aes(label = Value), vjust = -1, size = 3, color = "black") +
  # 调整气泡大小
  scale_size(range = c(3, 30), name = "Value Size") +
  # 使用调色板
  scale_fill_manual(values = palette) +
  # 调整坐标轴
  scale_x_continuous(
    breaks = 1:6,
    labels = data$Age,
    name = "Age Group"
  ) +
  scale_y_continuous(
    breaks = 1:length(unique(data_long$Category)),
    labels = levels(data_long$Category), # 确保 y 轴标签和因子顺序一致
    name = "Spending Category"
  ) +
  # 美化主题
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5), # 标题居中
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(10, 0, 20, 0)),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 0, margin = margin(20, 0, 0, 0)), # 注释放置最左边
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  # 图表标题和注释
  labs(
    title = "Age vs Spending Categories",
    subtitle = "Bubble size represents spending percentage",
    caption = "Source: NRF Data | Analysis and Visualization: Liu Xiaoliang",
    fill = "Category"
  )
