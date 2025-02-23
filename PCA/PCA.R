# 加载所需的 R 包
library(tidyverse)
library(ggrepel)
library(FactoMineR)
library(magrittr)
library(factoextra)
library(RColorBrewer)
library(readxl)
library(ggExtra)
library(patchwork)
library(mice)  # 用于处理缺失值

# 自动化 PCA 分析函数（修正版）
automated_pca_analysis <- function(df, category_col = NULL, numeric_cols = NULL, auto_select = TRUE, 
                                   pca_components = c(1, 2), missing_method = "pmm", missing_m = 5, scale_data = TRUE,
                                   title_p1 = "PCA 双标图", title_p2 = "PCA 散点图", title_combined = "PCA 结果") {
  #' @param df 数据框，包含数值型和分类变量
  #' @param category_col 选择分类变量的列名（用于着色）
  #' @param numeric_cols 选择要分析的数值列（若 `auto_select = FALSE`，用户可手动指定）
  #' @param auto_select 是否自动选择数值列（默认 TRUE）
  #' @param pca_components 选择主成分（默认 PC1 和 PC2）
  #' @param missing_method 缺失值插补方法（默认 pmm）
  #' @param missing_m 插补次数（默认 5）
  #' @param scale_data 是否标准化数据（默认 TRUE）
  #' @param title_p1 PCA 双标图的标题
  #' @param title_p2 PCA 散点图的标题
  #' @param title_combined 组合图的标题
  
  if (is.null(df) || nrow(df) == 0) {
    stop("数据框为空，请提供有效数据")
  }
  
  # 1️⃣ 选择分类列 & 数值列
  all_numeric_cols <- names(df)[sapply(df, is.numeric)]
  factor_cols <- names(df)[sapply(df, is.factor) | sapply(df, is.character)]
  
  if (!is.null(category_col) && category_col %in% names(df)) {
    df$Category <- as.factor(df[[category_col]])  # 确保分类变量为因子型
  } else {
    stop("错误: 选择的分类列不存在！")
  }
  
  if (auto_select) {
    numeric_cols <- all_numeric_cols  # 自动选择所有数值列
  } else {
    if (is.null(numeric_cols) || !all(numeric_cols %in% names(df))) {
      stop("错误: 用户选择的数值列无效！")
    }
  }
  
  message("使用的分类列：", category_col)
  message("使用的数值列：", paste(numeric_cols, collapse = ", "))
  
  # 2️⃣ 处理缺失值
  message("处理缺失值，方法：", missing_method, "，插补次数：", missing_m)
  imputed_data <- mice(df[numeric_cols], method = missing_method, m = missing_m, seed = 123)
  df_imputed <- complete(imputed_data)
  
  # 3️⃣ 计算 PCA
  pca <- prcomp(df_imputed, scale. = scale_data)
  var_explained <- pca$sdev^2 / sum(pca$sdev^2)
  
  # 4️⃣ 绘制第一张图（PCA 双标图）
  p1 <- fviz_pca_biplot(pca, axes = pca_components,  
                        geom.ind = c("point"), geom.var = c("arrow", "text"),
                        pointshape = 20, pointsize = 4,
                        label = "var", repel = TRUE, col.var = "black",
                        labelsize = 3,
                        addEllipses = TRUE, ellipse.level = 0.95,
                        col.ind = df$Category
  ) +
    scale_color_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(df$Category)))) +  
    labs(title = title_p1,  
         x = paste0("PC1 (", round(var_explained[1] * 100, 2), "%)"),
         y = paste0("PC2 (", round(var_explained[2] * 100, 2), "%)")) +
    theme(panel.background = element_rect(fill = 'white', colour = 'black'),
          axis.title.x = element_text(colour = "black", size = 12, margin = margin(t = 12)),  
          axis.title.y = element_text(colour = "black", size = 12, margin = margin(r = 12)),  
          axis.text = element_text(color = "black"),  
          legend.title = element_blank(),
          legend.position = c(1, 0), legend.justification = c(1, 0))
  
  # 5️⃣ 绘制第二张图（PCA 散点图）
  p2 <- ggplot(as.data.frame(pca$x), aes(PC1, PC2)) +
    geom_point(size = 2, aes(color = df$Category), show.legend = FALSE) +
    scale_color_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(df$Category)))) +
    stat_ellipse(aes(color = df$Category), fill = "white", geom = "polygon", 
                 level = 0.95, alpha = 0.01, show.legend = FALSE) +
    labs(title = title_p2,
         x = paste0("PC1 (", round(var_explained[1] * 100, 2), "%)"),  
         y = paste0("PC2 (", round(var_explained[2] * 100, 2), "%)")) +
    theme_classic() +
    theme(axis.line = element_line(colour = "black"),
          axis.title = element_text(color = "black", face = "bold"),
          axis.text = element_text(color = "black", size = 10, face = "bold"))
  
  # 6️⃣ **合并图像**
  combined_plot <- (p1 + p2) + plot_annotation(title = title_combined)
  
  # 显示图像
  print(p1)
  print(p2)
  print(combined_plot)
  
  return(list(pca = pca, pca_plot1 = p1, pca_plot2 = p2, combined_plot = combined_plot, 
              var_explained = var_explained, imputed_data = df_imputed))
}



# 示例：运行自动化 PCA 分析
df <- read_table("./PCA/class.txt")

# 确保数据框的列名正确
print(colnames(df))

# 运行自动化 PCA 分析，设置缺失值插补方法为 "pmm"，插补 5 次
result <- automated_pca_analysis(df, category_col = 'major', missing_method = "pmm", missing_m = 5)
result
