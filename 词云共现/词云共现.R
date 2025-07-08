


# -------------------------------饼图-----------------------


######### 不算比例###################

# 数据准备
library(ggplot2)
library(ggrepel)
library(tidyverse)

# 设置数据
df <- data.frame(
  value = c(10, 20, 50, 20, 10, 20, 10, 15, 30, 10, 15, 20),
  group = c(
    "学生素质提升",
    "师资力量调配",
    "资源汇集",
    "提供就业发展方向性引领",
    "项目的管理",
    "培养方案修订",
    "协同学院学科核心课程建设",
    "校企合作",
    "项目校内外资源对接",
    "加强实践教学",
    "个性化指导与支持",
    "项目主任统筹学生学情摸排"
  )
)

# 计算标签位置
df2 <- df %>%
  mutate(csum = rev(cumsum(rev(value))),
         pos = value / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), value / 2, pos))

# 绘图
ggplot(df, aes(x = "", y = value, fill = fct_inorder(group))) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set3") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(group, "\n", value)),
                   size = 3.8, nudge_x = 1.2, show.legend = FALSE, segment.color = "grey40") +
  guides(fill = guide_legend(title = "任务类型")) +
  theme_void() +
  theme(legend.position = "none")







######## 算比例##################



# 数据准备
library(ggplot2)
library(ggrepel)
library(tidyverse)

# 设置数据
df <- data.frame(
  value = c(10, 20, 50, 20, 10, 20, 10, 15, 30, 10, 15, 20),
  group = c(
    "学生素质提升",
    "师资力量调配",
    "资源汇集",
    "提供就业发展方向性引领",
    "项目的管理",
    "培养方案修订",
    "协同学院学科核心课程建设",
    "校企合作",
    "项目校内外资源对接",
    "加强实践教学",
    "个性化指导与支持",
    "项目主任统筹学生学情摸排"
  )
)

# 计算百分比
df <- df %>%
  mutate(percentage = value / sum(value) * 100)

# 计算标签位置
df2 <- df %>%
  mutate(csum = rev(cumsum(rev(percentage))),
         pos = percentage / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), percentage / 2, pos))

# 绘图
# 不显示图例的饼图
ggplot(df, aes(x = "", y = percentage, fill = fct_inorder(group))) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set3") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(group, "\n", round(percentage, 1), "%")),
                   size = 3.8, nudge_x = 1.2, show.legend = FALSE, segment.color = "grey40") +
  theme_void() +
  theme(legend.position = "none")












######################################################################
#-------------------------------------------------------------------
####################################################
# ------------------ 安装并加载必要包 ------------------
# 安装并加载必要的R包（如果没有安装，请先运行安装命令）

# install.packages(c("jiebaR", "jiebaRD", "wordcloud2", "tm", "RColorBrewer", "showtext"))
library(jiebaR)
library(jiebaRD)
library(wordcloud2)
library(tm)
library(RColorBrewer)
library(showtext)
library(ggraph)
library(igraph)
library(tidygraph)
library(dplyr)
library(htmlwidgets)
library(webshot2)

# ------------------ 字体设置（支持中文显示） ------------------
showtext_auto() # 自动开启中文显示支持
windowsFonts(myfont = windowsFont("微软雅黑")) # 设置字体为微软雅黑

# ------------------ 停用词表 ------------------
# 停用词（可根据需求拓展）
stopwords <- c("的", "是", "了", "在", "和", "与", "及", "中", "对", "也", "要",
               "于", "并", "就", "等", "而", "可", "由", "不", "一个", "项目", "我们",
               "他们", "作为", "进行", "通过", "中")

# ------------------ 共现网络图 ------------------
# 1. 读取中文文本（你也可以用 readLines("1.txt")）
text <- "
引领组织整个铁三角项目的关键人物，在各项决策中起到团队核心人物角色。
项目主任在铁三角项目中的核心作用是统筹规划、资源协调，总体设计及学术专业能力的培养，教学组织与管理等。
我认为在“铁三角”项目中，项目主任核心作用在于统筹专业项目建设与管理，做好思政引领，确保人才培养目标的实现。
在专业建设方面，我们需要立足于学院的学科优势和行业发展趋势，制定具有前瞻性和适应性的项目培养方案。
项目主任还需充分协调各方教学资源，为项目的顺利实施提供有力保障。
这不仅包括校内的师资力量调配，还涉及与校外企业，专业机构的沟通协作，通过引入企业真实项目和行业前沿动态，丰富教学内容，拓宽学生的视野。
资源的整合与汇集，调动学院内外的人脉经费项目用于铁三角项目建设。
项目主任应当了解专业人才培养的社会需求、能力需求，为项目提供核心支持，为项目的运作提供行动方向和资源支持。
项目主任用于构建整个项目的培养方案，整体建构，引导整个项目的良性发展。
项目主任要出自优秀教师，组建专业团队，不仅关注专业建设，还重视学生的教育与管理。
把握学科专业方向。明确人才培养的目标定位，争取外部资源，整合内部力量。
做的整体规划，对接校内外资源，合理安排项目中班主任、辅导员职责分工，做好课程定位和师资选配。
总体规划项目人才培养定位，培养方案设计，协同班主任与辅导员做好学生的工作，对接综合导师、课程教师和社会实践导师对学生的指导和教学工作。
"

# 2. 分句
sentences <- unlist(strsplit(text, "[，。！？、\n]"))

# 3. 分词
cutter <- worker()
seg_list <- lapply(sentences, function(s) segment(s, cutter))

# 4. 提取共现词对函数
get_edges <- function(words) {
  words <- words[nchar(words) >= 2 & !(words %in% stopwords)] # 去除短词和停用词
  words <- unique(words)
  if (length(words) < 2) return(NULL)
  t(combn(words, 2)) # 生成词对
}

# 5. 生成边列表
edges_list <- lapply(seg_list, function(x) {
  if (is.character(x) && length(x) >= 2) get_edges(x) else NULL
})
edges_list <- edges_list[!sapply(edges_list, is.null)]
edges <- do.call(rbind, edges_list)
edges_df <- as.data.frame(edges, stringsAsFactors = FALSE)
colnames(edges_df) <- c("from", "to")

# 6. 汇总边频次
edge_count <- edges_df %>%
  count(from, to, sort = TRUE) %>%
  filter(n >= 1)  # 可调节阈值控制图复杂度

# 7. 构建图
graph <- tbl_graph(edges = edge_count, directed = FALSE)

# 8. 绘制网络图（正式简洁风）
ggraph(graph, layout = "fr") +
  geom_edge_link(aes(width = n), edge_colour = "#999999", alpha = 0.6) +
  geom_node_point(color = "#2F4F4F", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, family = "微软雅黑", size = 5) +
  theme_void() +
  ggtitle("词语共现网络图") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# ------------------ 词云生成 ------------------
generate_wordcloud <- function(text, output_file = "wordcloud.png") {
  
  # 中文分词
  cutter <- worker()
  words <- segment(text, cutter)
  
  # 去停用词和短词
  stopwords <- c("项目", "主任", "我们", "需要", "方面", "通过", "包括", "的是", "在", "与", "还", "的", "中", "对",
                 "其", "一定", "但", "不能", "虽然", "有时", "情况", "存在", "较大", "主要", "较难", "较为",
                 "不同", "多为", "还是", "首先", "其次", "再者", "一定", "较", "一")
  cleaned_words <- words[nchar(words) >= 2 & !(words %in% stopwords)]
  
  # 词频统计
  word_freq <- as.data.frame(table(cleaned_words), stringsAsFactors = FALSE)
  word_freq <- word_freq[order(word_freq$Freq, decreasing = TRUE), ]
  
  # 生成词云对象
  wc <- wordcloud2(word_freq,
                   size = 0.9,
                   fontFamily = "微软雅黑",
                   color = rep("black", nrow(word_freq)),
                   backgroundColor = "white",
                   shape = "circle",
                   rotateRatio = 0)
  
  # 保存为 HTML 临时文件
  temp_html <- tempfile(fileext = ".html")
  saveWidget(wc, temp_html, selfcontained = TRUE)
  
  # 转换为 PNG 图片
  webshot(temp_html, output_file, vwidth = 800, vheight = 600)
  
  message("词云已保存为图片：", output_file)
}

# 示例使用
text <- "引领组织整个铁三角项目的关键人物，在各项决策中起到团队核心人物角色
项目主任在铁三角项目中的核心作用是统筹规划、资源协调，总体设计及学术专业能力的培养，教学组织与管理等。"
generate_wordcloud(text, "wordcloud_example.png")
