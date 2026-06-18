# ------------------------------------------------------------------------------
# 0. 環境設定與套件載入 (Environment & Dependencies)
# ------------------------------------------------------------------------------
library(tidyverse) # 包含 dplyr, tidyr, ggplot2, stringr, readr 等
library(caret)     # 資料集切分與機器學習輔助
library(car)       # OLS 迴歸診斷 (VIF 計算)
library(pls)       # 主成分迴歸 (PCR 實作)
library(corrplot)  # 繪製相關矩陣
library(scales)    # 視覺化尺度調整 (squish)
library(patchwork) # 專業併圖與版面配置
library(ggrepel)   # 避免標籤重疊

# 設定工作目錄 (請依據實際環境修改)
folder_path <- "資料集所在資料夾"


# ==============================================================================
# 3.1 資料說明與特徵工程 (Data Ingestion, Merging & Feature Engineering)
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1.1 自動化資料讀取模組
# ------------------------------------------------------------------------------
file_paths <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
file_names <- str_remove(basename(file_paths), "\\.csv$")

read_double_header_csv <- function(file_path) {
  guess <- guess_encoding(file_path, n_max = 1000)
  detected_enc <- guess$encoding[1]
  
  if (!is.na(detected_enc) && grepl("BIG5|CP950|ISO-8859|windows", detected_enc, ignore.case = TRUE)) {
    my_locale <- locale(encoding = "Big5")
  } else {
    my_locale <- locale(encoding = "UTF-8")
  }
  
  col_names_eng <- names(read_csv(file_path, skip = 1, n_max = 0, 
                                  show_col_types = FALSE, locale = my_locale))
  df <- read_csv(file_path, skip = 2, col_names = col_names_eng, 
                 show_col_types = FALSE, locale = my_locale)
  return(df)
}

df_list <- setNames(lapply(file_paths, read_double_header_csv), file_names)
list2env(df_list, envir = .GlobalEnv)
cat(">>> 3.1.1 所有原始 CSV 檔案已成功載入環境。\n")

# ------------------------------------------------------------------------------
# 3.1.2 特徵工程：人口結構千分比轉換與資料整併
# ------------------------------------------------------------------------------
edu_data_processed <- `112年行政區15歲以上人口五歲年齡組教育程度統計_鄉鎮市區` %>% 
  mutate(total_pop = rowSums(pick(ends_with("人口數")), na.rm = TRUE)) %>% 
  mutate(across(ends_with("人口數"), ~ if_else(total_pop == 0, 0, (. / total_pop) * 1000))) %>% 
  select(-total_pop) %>% 
  rename_with(.cols = ends_with("人口數"), .fn = ~ str_replace(.x, "人口數$", "千分比"))

# 定義目標變數 (Y) 與解釋變數 (X, C1~C5)
df_y <- `112年行政區不動產實價登錄建物成交資訊(交易日)—按建物型態分_鄉鎮市區` %>%
  select(鄉鎮市區代碼, 不含車位_透天厝中位數房價 = `不含車位_透天厝中位數房價`)
df_x <- edu_data_processed %>% select(鄉鎮市區代碼, contains("歲")) 
df_c1 <- `112年綜合所得稅所得總額申報統計_鄉鎮市區` %>% select(鄉鎮市區代碼, 中位數)

# 修復政府資料 VLOOKUP 錯誤 (純淨複合鍵)
correct_mapping <- `112年12月行政區工商家數_鄉鎮市區` %>%
  select(縣市名稱, 鄉鎮市區名稱, 鄉鎮市區代碼) %>%
  distinct() %>%
  mutate(Merge_Key = paste0(str_replace_all(縣市名稱, "臺", "台"), 
                            str_replace_all(鄉鎮市區名稱, "臺", "台")))

df_c2 <- `112年行政區鄉鎮市區社會風險地圖(測試之資料)` %>%
  mutate(Merge_Key = paste0(str_replace_all(縣市名稱, "臺", "台"), 
                            str_replace_all(鄉鎮市區名稱, "臺", "台"))) %>%
  select(-鄉鎮市區代碼) %>%
  left_join(correct_mapping %>% select(Merge_Key, 鄉鎮市區代碼), by = "Merge_Key") %>%
  select(鄉鎮市區代碼, 扶養比, 電信信令平日夜間停留人數)

df_c3 <- `112年12月行政區中低收入戶統計指標_鄉鎮市區` %>% select(鄉鎮市區代碼, 中低收入戶比例 = 中低收入戶占總戶數比例)
df_c4 <- `112年12月行政區低收入戶統計指標_鄉鎮市區` %>% select(鄉鎮市區代碼, 低收入戶比例 = 低收入戶占總戶數比例)
df_industry <- `112年12月行政區工商家數_鄉鎮市區` %>% select(-縣市代碼, -縣市名稱, -鄉鎮市區名稱, -工商業總家數, -資料時間) 

# 合併設計矩陣 (Design Matrix)
df_master <- df_y %>%
  inner_join(df_x, by = "鄉鎮市區代碼") %>%
  inner_join(df_industry, by = "鄉鎮市區代碼") %>%
  inner_join(df_c1, by = "鄉鎮市區代碼") %>%
  inner_join(df_c2, by = "鄉鎮市區代碼") %>%
  inner_join(df_c3, by = "鄉鎮市區代碼") %>%
  inner_join(df_c4, by = "鄉鎮市區代碼")

# ------------------------------------------------------------------------------
# 3.1.3 資料清理與維度縮減 (Sparsity Filtering & Train-Test Split)
# ------------------------------------------------------------------------------
cols_to_drop <- c(
  "15-19歲博士千分比", "20-24歲博士千分比", "15-19歲碩士千分比",
  "15-19歲自修千分比", "15-19歲不識字千分比", "20-24歲自修千分比", 
  "20-24歲不識字千分比", "25-29歲自修千分比", "25-29歲不識字千分比",
  "30-34歲自修千分比", "30-34歲不識字千分比", "35-39歲自修千分比", 
  "35-39歲不識字千分比", "公共行政及國防；強制性社會安全"
)

df_clean <- df_master %>%
  select(-any_of(cols_to_drop)) %>% 
  drop_na() %>%
  tibble::column_to_rownames(var = "鄉鎮市區代碼") %>%
  mutate(across(everything(), as.numeric))

# 尺度轉換：房價轉為「萬元」
df_clean$不含車位_透天厝中位數房價 <- df_clean$不含車位_透天厝中位數房價 / 10000 

cat(">>> 3.1.3 資料清理完成。總樣本數 (n)：", nrow(df_clean), 
    "| 解釋變數數量 (p)：", ncol(df_clean) - 1, "\n")

set.seed(202606) 
train_index <- createDataPartition(df_clean$不含車位_透天厝中位數房價, p = 0.70, list = FALSE)
train_data <- df_clean[train_index, ]
test_data  <- df_clean[-train_index, ]


# ==============================================================================
# 3.2.1 傳統 OLS 診斷與病態特徵 (EDA & OLS Diagnostics)
# ==============================================================================

# --- 繪製圖表 1：特徵相關係數矩陣圖 ---
cor_matrix_all <- cor(train_data[, -1])
rownames(cor_matrix_all) <- stringr::str_trunc(rownames(cor_matrix_all), 12, "right")
colnames(cor_matrix_all) <- stringr::str_trunc(colnames(cor_matrix_all), 12, "right")

png("plot_0_corr_matrix.png", width = 2400, height = 2400, res = 300)
corrplot(cor_matrix_all, method = "color", type = "full", tl.col = "black", 
         tl.cex = 0.4, addCoef.col = NULL, diag = TRUE, mar = c(0, 0, 2, 0))
dev.off() 

# --- 配適 OLS 模型與共線性診斷 ---
ols_model <- lm(不含車位_透天厝中位數房價 ~ ., data = train_data)

cat("\n=== 3.2.1 OLS 共線性與病態矩陣診斷 ===\n")
aliased_vars <- alias(ols_model)$Complete
if(!is.null(aliased_vars)) cat("[警告] 偵測到完全共線性！\n")

vif_results <- tryCatch({
  vif_vals <- car::vif(ols_model)
  cat("\n部分變數之 VIF (極端變異數膨脹)：\n")
  print(head(sort(vif_vals, decreasing = TRUE), 10))
}, error = function(e) cat("\n[錯誤] 存在完全共線性，無法計算 VIF。\n"))

# 計算交叉乘積矩陣 X^T X 條件數 (Condition Number)
X_matrix <- model.matrix(ols_model)
XtX <- t(X_matrix) %*% X_matrix
ev <- eigen(XtX)$values
cond_num_eigen <- max(ev) / min(ev)
cat("最大特徵值:", max(ev), "最小特徵值:", min(ev), "\n")
cat("矩陣條件數 Kappa:", cond_num_eigen, "(> 10^3 屬嚴重病態矩陣)\n")

summary(ols_model)

# ==============================================================================
# 3.2.2 主成分分析 (PCA) 降維 (PCA, Scree Plot, Scores & Loadings)
# ==============================================================================

# 配適 PCR 模型 (scale = TRUE 強制標準化)
pcr_model <- pcr(不含車位_透天厝中位數房價 ~ ., data = train_data, scale = TRUE, validation = "CV", segments = 10) 

cat("\n=== 3.2.2 PCA 降維與特徵萃取 ===\n")
cat("前 10 個主成分之累積解釋變異率 (%):\n")
print(cumsum(explvar(pcr_model)[1:10]) / sum(explvar(pcr_model)))

# --- 繪製圖表 2：Scree Plot 陡坡圖 ---
eigenvalues   <- pcr_model$Xvar 
cusum_var_pct <- cumsum(explvar(pcr_model))
max_show      <- min(length(eigenvalues), 25)
scale_factor  <- max(cusum_var_pct) / max(eigenvalues)

plot_data <- data.frame(PC = 1:length(eigenvalues), Eigenvalue = eigenvalues, CumVar = cusum_var_pct) %>% filter(PC <= max_show)

plot_scree <- ggplot(plot_data, aes(x = PC)) +
  geom_col(aes(y = CumVar / scale_factor), fill = "darkred", alpha = 0.3) +
  geom_line(aes(y = CumVar / scale_factor), color = "darkred", linewidth = 1.2) +
  geom_point(aes(y = CumVar / scale_factor), color = "darkred", size = 3) +
  geom_line(aes(y = Eigenvalue), color = "blue", linewidth = 1) +
  geom_point(aes(y = Eigenvalue), color = "blue", size = 2) +
  geom_text(data = plot_data %>% filter(PC <= 10), aes(y = CumVar / scale_factor, label = sprintf("%.0f%%", CumVar)), color = "darkred", vjust = -1.5, size = 3.5, fontface = "bold") +
  scale_y_continuous(name = "特徵值 (Eigenvalue)", sec.axis = sec_axis(~ . * scale_factor, name = "累積貢獻率 (%)")) +
  scale_x_continuous(breaks = seq(1, max_show, by = 2)) +
  theme_minimal() + labs(x = "主成分數量 (k)")
ggsave("plot_A_scree_plot.png", plot = plot_scree, width = 9, height = 4, dpi = 300, bg = "white")

# --- 繪製圖表 6：主成分特徵空間得分散佈圖 (PC1 vs PC2) ---
train_scores_df_clean <- as.data.frame(unclass(scores(pcr_model))[, 1:2]) %>%
  rename(PC1 = 1, PC2 = 2) %>% tibble::rownames_to_column(var = "TOWNCODE") %>% mutate(TOWNCODE = as.character(TOWNCODE)) %>%
  left_join(correct_mapping %>% select(鄉鎮市區代碼, Merge_Key), by = c("TOWNCODE" = "鄉鎮市區代碼")) %>%
  left_join(df_y %>% select(鄉鎮市區代碼, 不含車位_透天厝中位數房價), by = c("TOWNCODE" = "鄉鎮市區代碼")) %>%
  mutate(不含車位_透天厝中位數房價 = 不含車位_透天厝中位數房價 / 10000)

plot_scores_scatter_y <- ggplot(train_scores_df_clean, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.8, size = 2.5, aes(color = 不含車位_透天厝中位數房價)) +
  scale_color_distiller(palette = "YlOrRd", direction = 1, name = "透天厝中位數房價\n(萬元)") +
  geom_text_repel(data = subset(train_scores_df_clean, PC2 < -5), aes(label = Merge_Key), size = 3.5, color = "black", max.overlaps = 15, box.padding = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() + labs(x = "PC1 Score", y = "PC2 Score")
ggsave("plot_scores_scatter_y.png", plot = plot_scores_scatter_y, width = 9, height = 6, dpi = 300, bg = "white")

# --- 繪製圖表 3, 4, 5：特徵負載熱度圖 (Loadings) ---
raw_loadings <- unclass(loadings(pcr_model))[, 1:2] 
colnames(raw_loadings) <- c("PC1", "PC2")

all_loadings_df <- as.data.frame(raw_loadings) %>%
  tibble::rownames_to_column(var = "orig_variable")

excel_style_scale <- function(...) {
  scale_fill_gradient2(
    low = "#D73027", mid = "white", high = "#1A9850", 
    midpoint = 0, limits = c(-0.2, 0.2), oob = scales::squish, name = "Loading"
  )
}

# --- 圖3：教育與年齡結構 ---
edu_order <- c("博士", "碩士", "大學院校", "專科", "高中職", "國中初職", "小學", "自修", "不識字")
age_order <- c("15-19歲", "20-24歲", "25-29歲", "30-34歲", "35-39歲", "40-44歲",
               "45-49歲", "50-54歲", "55-59歲", "60-64歲", "65歲以上")

edu_bv_df <- all_loadings_df %>%
  filter(str_detect(orig_variable, "歲")) %>%
  mutate(AgeGroup = str_extract(orig_variable, "65歲以上|[0-9-]+歲"),
         EduLevel = str_remove(orig_variable, AgeGroup),
         EduLevel = str_remove(EduLevel, "千分比"),
         EduLevel = str_remove_all(EduLevel, "`"),
         EduLevel = factor(EduLevel, levels = edu_order),
         AgeGroup = factor(AgeGroup, levels = rev(age_order))) %>%
  pivot_longer(cols = c(PC1, PC2), names_to = "PC", values_to = "Loading") %>%
  drop_na(EduLevel, AgeGroup) 

plot_edu <- ggplot(edu_bv_df, aes(x = EduLevel, y = AgeGroup, fill = Loading)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", Loading), color = abs(Loading) > 0.15), size = 3) +
  scale_color_manual(values = c("black", "white"), guide = "none") +
  excel_style_scale() + facet_wrap(~PC, nrow = 2) +
  scale_x_discrete(position = "top") + 
  labs(x = "教育程度", y = "年齡組") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14), panel.grid = element_blank(),
        axis.text.x.top = element_text(angle = 0, hjust = 0.5, face = "bold"),
        axis.text.y = element_text(face = "bold"), legend.position = "right")

ggsave("plot_1_loadings_education_age.png", plot = plot_edu, width = 10, height = 8, dpi = 300, bg = "white")

# --- 圖4：地方產業結構 ---
industry_plot_df_loadings <- all_loadings_df %>%
  filter(!str_detect(orig_variable, "歲"),
         !orig_variable %in% c("中位數", "扶養比", "電信信令平日夜間停留人數", "中低收入戶比例", "低收入戶比例")) %>%
  mutate(Variable_Clean = str_remove_all(orig_variable, "`")) %>%
  pivot_longer(cols = c(PC1, PC2), names_to = "PC", values_to = "Loading") %>%
  mutate(Variable_Clean = factor(Variable_Clean, levels = rev(sort(unique(Variable_Clean)))))

plot_industry <- ggplot(industry_plot_df_loadings, aes(x = PC, y = Variable_Clean, fill = Loading)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", Loading), color = abs(Loading) > 0.15), size = 5) +
  scale_color_manual(values = c("black", "white"), guide = "none") +
  excel_style_scale() + scale_x_discrete(position = "top") + 
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14), panel.grid = element_blank(),
        axis.text.x.top = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12), legend.position = "right")

ggsave("plot_2_loadings_industry.png", plot = plot_industry, width = 8, height = 6, dpi = 300, bg = "white")

# --- 圖5：其他 (稅務、風險與社經弱勢) ---
others_plot_df_loadings <- all_loadings_df %>%
  filter(orig_variable %in% c("中位數", "扶養比", "電信信令平日夜間停留人數", "中低收入戶比例", "低收入戶比例")) %>%
  mutate(Variable_Clean = case_when(
    orig_variable == "中位數"                 ~ "綜合所得稅中位數",
    orig_variable == "扶養比"                 ~ "扶養比",
    orig_variable == "電信信令平日夜間停留人數" ~ "電信夜間停留",
    orig_variable == "中低收入戶比例"         ~ "中低收入戶比例",
    orig_variable == "低收入戶比例"           ~ "低收入戶比例"
  )) %>%
  pivot_longer(cols = c(PC1, PC2), names_to = "PC", values_to = "Loading") %>%
  mutate(Variable_Clean = factor(Variable_Clean, levels = rev(sort(unique(Variable_Clean)))))

plot_others <- ggplot(others_plot_df_loadings, aes(x = PC, y = Variable_Clean, fill = Loading)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", Loading), color = abs(Loading) > 0.15), size = 4) +
  scale_color_manual(values = c("black", "white"), guide = "none") +
  excel_style_scale() + scale_x_discrete(position = "top") + 
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12), panel.grid = element_blank(),
        axis.text.x.top = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12), legend.position = "right")

ggsave("plot_3_loadings_others.png", plot = plot_others, width = 6, height = 4, dpi = 300, bg = "white")

# ==============================================================================
# 3.2.3(a) 主成分迴歸配適與特徵還原 (PCR Modeling & Coefficients)
# ==============================================================================

# --- 繪製圖表 7：基於交叉驗證之 Bias-Variance 權衡實證圖 ---
cv_rmsep_vals  <- RMSEP(pcr_model)$val["CV", 1, ]
cv_mse_vals <- (cv_rmsep_vals)^2

# 迴圈計算各個 k 值下的訓練集 MSE (代理 Bias^2)
train_mse_vals <- sapply(0:(length(cv_mse_vals) - 1), function(k) {
  if(k == 0) {
    # k=0 時僅有截距
    mean((train_data$不含車位_透天厝中位數房價 - mean(train_data$不含車位_透天厝中位數房價))^2)
  } else {
    pred <- as.numeric(predict(pcr_model, train_data, ncomp = k))
    mean((train_data$不含車位_透天厝中位數房價 - pred)^2)
  }
})

# 變異數代理 (Variance Proxy) = CV MSE - Train MSE
variance_vals <- cv_mse_vals - train_mse_vals
variance_vals[variance_vals < 0] <- 0 

pcr_bv_df <- tibble(
  Components = 0:(length(cv_mse_vals) - 1),
  `1_Total_Error (CV MSE)` = as.numeric(cv_mse_vals),
  `2_Bias^2_Proxy (Train MSE)` = as.numeric(train_mse_vals),
  `3_Variance_Proxy (CV - Train)` = as.numeric(variance_vals)
) %>%
  pivot_longer(cols = -Components, names_to = "Metric", values_to = "MSE")

plot_pcr_bv <- ggplot(pcr_bv_df, aes(x = Components, y = MSE, color = Metric, linetype = Metric)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 6, linetype = "dotted", color = "black", linewidth = 0.8) +
  scale_color_manual(values = c("1_Total_Error (CV MSE)" = "#D55E00", 
                                "2_Bias^2_Proxy (Train MSE)" = "#0072B2", 
                                "3_Variance_Proxy (CV - Train)" = "#009E73")) +
  scale_linetype_manual(values = c("1_Total_Error (CV MSE)" = "solid", 
                                   "2_Bias^2_Proxy (Train MSE)" = "dashed", 
                                   "3_Variance_Proxy (CV - Train)" = "dotdash")) +
  labs(x = "Number of Principal Components (k)",
       y = "Mean Squared Error (MSE)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank())

ggsave("plot_B_bias_variance.png", plot = plot_pcr_bv, width = 10, height = 6, dpi = 300, bg = "white")

optimal_k <- 2
cat(sprintf("\n=== 3.2.3(a) PCR 迴歸係數 (k=%d) ===\n", optimal_k))

# 標準化資料再配適一次，用於萃取純標準化還原係數
train_data_z <- train_data %>% mutate(across(everything(), ~scale(.)[, 1]))
pcr_z_model <- pcr(不含車位_透天厝中位數房價 ~ ., data = train_data_z, scale = FALSE)

# 提取主成分空間係數 (Alpha)
gamma_coefs_std <- pcr_z_model$Yloadings[, 1:optimal_k]
gamma_df_std <- data.frame(Principal_Component = paste0("Comp ", 1:optimal_k), Gamma_Coefficient = as.numeric(gamma_coefs_std))
print(gamma_df_std)

# 提取特徵空間還原係數 (Beta)
cat(sprintf("\n還原原始變數標準化估計係數 (前 10 項):\n"))
print(head(coef(pcr_z_model, ncomp = optimal_k, intercept = FALSE), 10))

# --- 繪圖：標準化尺度下綜合解釋係數熱度圖 (圖 8) ---
excel_style_gradient <- function(limits) {
  scale_fill_gradient2(
    low = "#D73027", mid = "white", high = "#1A9850", 
    midpoint = 0, limits = limits, oob = scales::squish, name = "Coef"
  )
}
all_coefs_std <- coef(pcr_z_model, ncomp = optimal_k, intercept = FALSE)

coefs_df_std <- as.data.frame(all_coefs_std) %>%
  tibble::rownames_to_column(var = "orig_variable") %>%
  rename(Coefficient = 2) %>%
  mutate(Variable_Clean = str_remove_all(orig_variable, "`"))

# --- 教育與年齡結構係數熱力圖 ---
coef_edu_df_std <- coefs_df_std %>%
  filter(str_detect(orig_variable, "歲")) %>%
  mutate(AgeGroup = str_extract(Variable_Clean, "65歲以上|[0-9-]+歲"),
         EduLevel = str_remove(Variable_Clean, AgeGroup),
         EduLevel = str_remove(EduLevel, "千分比"),
         EduLevel = factor(EduLevel, levels = edu_order),
         AgeGroup = factor(AgeGroup, levels = rev(age_order))) %>%
  drop_na(EduLevel, AgeGroup)

plot_coef_edu_std <- ggplot(coef_edu_df_std, aes(x = EduLevel, y = AgeGroup, fill = Coefficient)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", Coefficient), color = abs(Coefficient) > 0.016), size = 5) +
  scale_color_manual(values = c("black", "white"), guide = "none") +
  excel_style_gradient(c(-0.02, 0.02)) + scale_x_discrete(position = "top") + 
  labs(x = "教育程度", y = "年齡組") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14), panel.grid = element_blank(),
        axis.text.x.top = element_text(size = 14, angle = 0, hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"), legend.position = "right",
        axis.title = element_text(size = 14))

# --- 地方產業結構係數熱力圖 ---
industry_plot_df_coef_std <- coefs_df_std %>%
  filter(!str_detect(orig_variable, "歲"),
         !orig_variable %in% c("中位數", "扶養比", "電信信令平日夜間停留人數", "中低收入戶比例", "低收入戶比例")) %>%
  mutate(Variable_Clean = factor(Variable_Clean, levels = rev(sort(unique(Variable_Clean)))))

plot_coef_industry_std <- ggplot(industry_plot_df_coef_std, aes(x = "Coef", y = Variable_Clean, fill = Coefficient)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", Coefficient), color = abs(Coefficient) > 0.016), size = 5) +
  scale_color_manual(values = c("black", "white"), guide = "none") +
  excel_style_gradient(c(-0.02, 0.02)) + scale_x_discrete(position = "top") + 
  labs(x = NULL, y = NULL) + theme_minimal() +
  theme(panel.grid = element_blank(), axis.text.x.top = element_text(face = "bold", size = 11),
        axis.text.y = element_text(face = "bold", size = 11), legend.position = "right")

# --- 其他社經弱勢指標 ---
others_plot_df_coef_std <- coefs_df_std %>%
  filter(orig_variable %in% c("中位數", "扶養比", "電信信令平日夜間停留人數", "中低收入戶比例", "低收入戶比例")) %>%
  mutate(Variable_Clean = case_when(
    orig_variable == "中位數"                 ~ "綜合所得稅中位數",
    orig_variable == "扶養比"                 ~ "扶養比",
    orig_variable == "電信信令平日夜間停留人數" ~ "電信夜間停留",
    orig_variable == "中低收入戶比例"         ~ "中低收入戶比例",
    orig_variable == "低收入戶比例"           ~ "低收入戶比例"
  )) %>%
  mutate(Variable_Clean = factor(Variable_Clean, levels = rev(sort(unique(Variable_Clean)))))

plot_coef_others_std <- ggplot(others_plot_df_coef_std, aes(x = "Coef", y = Variable_Clean, fill = Coefficient)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", Coefficient), color = abs(Coefficient) > 0.016), size = 5) +
  scale_color_manual(values = c("black", "white"), guide = "none") +
  excel_style_gradient(c(-0.02, 0.02)) + scale_x_discrete(position = "top") + 
  labs(x = NULL, y = NULL) + theme_minimal() +
  theme(panel.grid = element_blank(), axis.text.x.top = element_text(face = "bold", size = 11),
        axis.text.y = element_text(face = "bold", size = 11), legend.position = "right")

# --- 併圖與儲存 ---
combined_plot_chameleon_std <- (plot_coef_edu_std) / (plot_coef_industry_std | plot_coef_others_std) +
  plot_layout(heights = c(1, 1))

print(combined_plot_chameleon_std)
ggsave("plot_综合解釋係數熱度圖_標準化尺度_chameleon.png", plot = combined_plot_chameleon_std, 
       width = 12, height = 10, dpi = 300, bg = "white")
cat(">>> 綜合解釋係數熱度圖已生成並儲存。\n")


# ==============================================================================
# 3.2.3(b) 預測效能評估 (Out-of-Sample Evaluation & Scatter Plot)
# ==============================================================================
calc_metrics_ext <- function(actual, predicted) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae  <- mean(abs(actual - predicted))
  mape <- mean(abs((actual - predicted) / actual)) * 100
  r2   <- 1 - (sum((actual - predicted)^2) / sum((actual - mean(actual))^2))
  return(data.frame(RMSE = rmse, MAE = mae, MAPE = mape, R2 = r2))
}

actual_y       <- test_data$不含車位_透天厝中位數房價
ols_pred_clean <- as.numeric(predict(ols_model, newdata = test_data))
pcr_pred_clean <- as.numeric(predict(pcr_model, newdata = test_data, ncomp = optimal_k))

if(length(actual_y) != length(ols_pred_clean) || length(actual_y) != length(pcr_pred_clean)) {
  stop("嚴重錯誤：預測值與真實值長度不符！")
}

eval_metrics <- bind_rows(
  calc_metrics_ext(actual_y, ols_pred_clean),
  calc_metrics_ext(actual_y, pcr_pred_clean)
) %>%
  mutate(Model = c("OLS", "PCR"), .before = 1) 

cat("\n=== 3.2.3(b) 獨立測試集預測效能指標 ===\n")
print(eval_metrics)


# 建立資料表並切分房價級距
analysis_df <- data.frame(
  Actual = actual_y,
  OLS    = ols_pred_clean,
  PCR    = pcr_pred_clean
) %>%
  mutate(
    # 將真實房價切分為 3 組 (1:低價, 2:中價, 3:高價)
    Price_Group = ntile(Actual, 3),
    Price_Label = case_when(
      Price_Group == 1 ~ "1_Low_Price (低價區)",
      Price_Group == 2 ~ "2_Medium_Price (中價區)",
      Price_Group == 3 ~ "3_High_Price (高價區)"
    )
  )

# 3. 分組計算 MAE 與 MAPE 進行對比
grouped_metrics <- analysis_df %>%
  group_by(Price_Label) %>%
  summarise(
    N_Samples = n(),
    Min_Actual = min(Actual),
    Max_Actual = max(Actual),
    # OLS 表現
    OLS_MAE  = mean(abs(Actual - OLS)),
    OLS_MAPE = mean(abs((Actual - OLS) / Actual)) * 100,
    # PCR 表現
    PCR_MAE  = mean(abs(Actual - PCR)),
    PCR_MAPE = mean(abs((Actual - PCR) / Actual)) * 100
  ) %>%
  arrange(Price_Label)

cat("\n=== 測試集依房價級距分組之誤差分析 ===\n")
print(grouped_metrics, width = Inf)

# --- 圖 9: 預測模型效能對比散佈圖（OLS vs. PCR)
pred_df <- data.frame(
  鄉鎮市區代碼   = rownames(test_data), # 直接提取代碼
  Actual         = actual_y,
  OLS_Predicted  = ols_pred_clean,
  PCR_Predicted  = pcr_pred_clean   
) %>%
  # 透過代碼 Join 中文名稱
  left_join(correct_mapping, by = "鄉鎮市區代碼") %>%
  pivot_longer(
    cols      = c(OLS_Predicted, PCR_Predicted), 
    names_to  = "Model", 
    values_to = "Predicted"
  ) %>%
  mutate(
    Model = str_replace(Model, "_Predicted", ""),
    # 計算絕對誤差
    Abs_Error = abs(Predicted - Actual)
  )
outliers_df <- bind_rows(
  pred_df %>% group_by(Model) %>% slice_max(order_by = Abs_Error, n = 5),
  pred_df %>% filter(Model == "PCR") %>% slice_min(order_by = Actual, n = 2),
  pred_df %>% filter(Predicted < 0)
  
) %>%
  distinct() %>% # 避免同一個行政區同時符合多個條件而重複出現
  ungroup()
plot_pred <- ggplot(pred_df, aes(x = Actual, y = Predicted, color = Model, shape = Model)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", linewidth = 1) +
  
  geom_text_repel(
    data = outliers_df,
    aes(label = Merge_Key),  
    size = 3.5,
    box.padding = 0.6,
    point.padding = 0.4,
    segment.color = "grey50", 
    show.legend = FALSE,      
    max.overlaps = Inf        
  ) +
  scale_color_manual(values = c(
    "OLS"    = "#E69F00",   
    "PCR"    = "#56B4E9"   
  )) +
  scale_shape_manual(values = c(
    "OLS"    = 16, 
    "PCR"    = 17
  )) +
  labs(
    x     = "Actual Median House Price (Ten Thousands)", 
    y     = "Predicted House Price",
    color = "Model Type",
    shape = "Model Type"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold"), 
    legend.position = "bottom"
  )
ggsave("plot_C_prediction_scatter.png", plot = plot_pred, width = 8, height = 8, dpi = 300, bg = "white")

# ==============================================================================
# 4.1 偏誤與變異數權衡及拔靴重抽樣 (Bias-Variance Tradeoff & Bootstrapping)
# ==============================================================================


# --- 繪製圖表 10：基於 Bootstrapping 的嚴謹理論分解 (Out-of-Sample) ---
cat("\n=== 4.1 執行 Bootstrapping 理論分解 (B=100) ===\n")
set.seed(2026)
B <- 100
max_k <- pcr_model$ncomp
n_test <- nrow(test_data)
boot_preds <- array(0, dim = c(n_test, max_k + 1, B))

for (b in 1:B) {
  boot_train <- train_data[sample(1:nrow(train_data), replace = TRUE), ]
  boot_pcr <- pcr(不含車位_透天厝中位數房價 ~ ., data = boot_train, scale = TRUE)
  
  boot_preds[, 1, b] <- rep(mean(boot_train$不含車位_透天厝中位數房價), n_test) # k=0
  for (k in 1:max_k) {
    boot_preds[, k + 1, b] <- as.numeric(predict(boot_pcr, newdata = test_data, ncomp = k))
  }
}

theoretical_bv_df <- tibble(Components = 0:max_k, Bias2 = numeric(max_k + 1), Variance = numeric(max_k + 1), Total_MSE = numeric(max_k + 1))
for (i in 1:(max_k + 1)) {
  preds_k <- boot_preds[, i, ]
  theoretical_bv_df$Bias2[i] <- mean((rowMeans(preds_k) - actual_y)^2)
  theoretical_bv_df$Variance[i] <- mean(apply(preds_k, 1, var))
  theoretical_bv_df$Total_MSE[i] <- mean((preds_k - actual_y)^2)
}

plot_data_theory <- theoretical_bv_df %>%
  pivot_longer(cols = c(Bias2, Variance, Total_MSE), names_to = "Metric", values_to = "Value")

plot_theoretical_bv <- ggplot(plot_data_theory, aes(x = Components, y = Value, color = Metric, linetype = Metric)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("Bias2" = "#0072B2", "Variance" = "#009E73", "Total_MSE" = "#D55E00"),
                     labels = c("Bias2" = "偏誤平方 (Bias^2)", "Variance" = "模型變異數 (Variance)", "Total_MSE" = "總預測誤差 (Total MSE)")) +
  scale_linetype_manual(values = c("Bias2" = "dashed", "Variance" = "dotdash", "Total_MSE" = "solid"),
                        labels = c("Bias2" = "偏誤平方 (Bias^2)", "Variance" = "模型變異數 (Variance)", "Total_MSE" = "總預測誤差 (Total MSE)")) +
  scale_x_continuous(breaks = seq(0, max_k, by = 5)) + 
  labs(x = "Number of Principal Components (k)", y = "Expected Loss (MSE)",
       # title = "Theoretical Bias-Variance Decomposition via Bootstrapping"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom", legend.title = element_blank())

print(plot_theoretical_bv)
ggsave("plot_B_theoretical_bias_variance_with_0.png", plot = plot_theoretical_bv, width = 10, height = 6, dpi = 300, bg = "white")
cat(">>> 所有圖表與分析程序執行完畢。\n")

# ==============================================================================
# 附圖
# ==============================================================================

all_coefs_raw <- coef(pcr_model, ncomp = optimal_k, intercept = TRUE)

coefs_df <- as.data.frame(all_coefs_raw) %>%
  tibble::rownames_to_column(var = "orig_variable") %>%
  rename(Coefficient = 2) %>%
  filter(orig_variable != "(Intercept)") %>%
  mutate(Variable_Clean = str_remove_all(orig_variable, "`"))

# --- 教育與年齡結構係數熱力圖 ---
coef_edu_df <- coefs_df %>%
  filter(str_detect(orig_variable, "歲")) %>%
  mutate(AgeGroup = str_extract(Variable_Clean, "65歲以上|[0-9-]+歲"),
         EduLevel = str_remove(Variable_Clean, AgeGroup),
         EduLevel = str_remove(EduLevel, "千分比"),
         EduLevel = factor(EduLevel, levels = edu_order),
         AgeGroup = factor(AgeGroup, levels = rev(age_order))) %>%
  drop_na(EduLevel, AgeGroup)

plot_coef_edu <- ggplot(coef_edu_df, aes(x = EduLevel, y = AgeGroup, fill = Coefficient)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", Coefficient), color = abs(Coefficient) > 9.5), size = 5) +
  scale_color_manual(values = c("black", "white"), guide = "none") +
  excel_style_gradient(c(-12, 12)) + scale_x_discrete(position = "top") + 
  labs(x = "教育程度", y = "年齡組") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14), panel.grid = element_blank(),
        axis.text.x.top = element_text(size = 14, angle = 0, hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"), legend.position = "right",
        axis.title = element_text(size = 14))

# --- 地方產業結構係數熱力圖 ---
industry_plot_df_coef <- coefs_df %>%
  filter(!str_detect(orig_variable, "歲"),
         !orig_variable %in% c("中位數", "扶養比", "電信信令平日夜間停留人數", "中低收入戶比例", "低收入戶比例")) %>%
  mutate(Variable_Clean = factor(Variable_Clean, levels = rev(sort(unique(Variable_Clean)))))

plot_coef_industry <- ggplot(industry_plot_df_coef, aes(x = "Coef", y = Variable_Clean, fill = Coefficient)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", Coefficient), color = abs(Coefficient) > 7), size = 5) +
  scale_color_manual(values = c("black", "white"), guide = "none") +
  excel_style_gradient(c(-9, 9)) + scale_x_discrete(position = "top") + 
  labs(x = NULL, y = NULL) + theme_minimal() +
  theme(panel.grid = element_blank(), axis.text.x.top = element_text(face = "bold", size = 11),
        axis.text.y = element_text(face = "bold", size = 11), legend.position = "right")

# --- 其他社經弱勢指標 ---
others_plot_df_coef <- coefs_df %>%
  filter(orig_variable %in% c("中位數", "扶養比", "電信信令平日夜間停留人數", "中低收入戶比例", "低收入戶比例")) %>%
  mutate(Variable_Clean = case_when(
    orig_variable == "中位數"                 ~ "綜合所得稅中位數",
    orig_variable == "扶養比"                 ~ "扶養比",
    orig_variable == "電信信令平日夜間停留人數" ~ "電信夜間停留",
    orig_variable == "中低收入戶比例"         ~ "中低收入戶比例",
    orig_variable == "低收入戶比例"           ~ "低收入戶比例"
  )) %>%
  mutate(Variable_Clean = factor(Variable_Clean, levels = rev(sort(unique(Variable_Clean)))))

plot_coef_others <- ggplot(others_plot_df_coef, aes(x = "Coef", y = Variable_Clean, fill = Coefficient)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", Coefficient), color = abs(Coefficient) > 8), size = 5) +
  scale_color_manual(values = c("black", "white"), guide = "none") +
  excel_style_gradient(c(-10, 10)) + scale_x_discrete(position = "top") + 
  labs(x = NULL, y = NULL) + theme_minimal() +
  theme(panel.grid = element_blank(), axis.text.x.top = element_text(face = "bold", size = 11),
        axis.text.y = element_text(face = "bold", size = 11), legend.position = "right")

# --- 併圖與儲存 ---
combined_plot_chameleon <- (plot_coef_edu) / (plot_coef_industry | plot_coef_others) +
  plot_layout(heights = c(1, 1))

print(combined_plot_chameleon)
ggsave("plot_综合解釋係數熱度圖_chameleon.png", plot = combined_plot_chameleon, 
       width = 12, height = 10, dpi = 300, bg = "white")
