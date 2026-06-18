# Taiwan-Socio-Economic-PCR-Analysis

應用台灣各鄉鎮市區社經開放資料進行透天厝中位數房價之預測實證。實證結果表明，相較於 OLS 嚴重的過度擬合與極端預測偏差，PCR 不僅大幅縮減了樣本外的預測誤差（RMSE），更透過特徵還原技術，將原先發散且符號錯亂的迴歸係數，重塑為平滑且符合總體經濟直覺的梯度結構。本報告證實，PCR 結合還原映射技術，能有效解決共線性造成的數值退化，並完美恢復高維度社經模型在統計推論上的可解釋性與預測穩健性。

## 📁 專案架構 (Repository Structure)

本專案包含實證研究所需之資料、程式碼與視覺化產出：

* `MainCode.R`: 本研究的核心執行程式碼，包含資料前處理、模型建立（OLS 與 PCR）、預測評估與視覺化圖表生成。
* `*.csv`: 112年台灣各鄉鎮市區之社經開放資料（如：綜合所得稅、教育程度、工商家數、不動產實價登錄等）。
* `plot_*.png`: 由程式碼自動生成的分析圖表，包含：
    * 相關係數矩陣 (`plot_0_corr_matrix.png`)
    * PCR 負荷量分析 (`plot_1` ~ `plot_3`)
    * 陡坡圖與模型解釋力 (`plot_A_scree_plot.png`)
    * 偏差與變異權衡分析 (`plot_B_bias_variance.png`)
    * 預測結果散佈圖 (`plot_C_prediction_scatter.png`)
    * 綜合解釋係數熱度圖 (`plot_综合解釋係數熱度圖_chameleon.png`)

## 📊 資料來源 (Data Sources)

本實證模型使用之特徵變數與目標變數資料來源：內政部社會經濟資料服務平台

## 💻 執行環境與依賴套件 (Requirements & Usage)

本程式碼使用 R 語言撰寫。在執行 `MainCode.R` 之前，請確保已安裝以下依賴套件：

* `pls` (用於主成分迴歸分析)
* `ggplot2` / `corrplot` (用於視覺化)
* (請根據你的程式碼實際使用的套件補充，例如 `dplyr`, `readr` 等)

**執行步驟：**
1. 將本儲存庫克隆 (Clone) 至本地端。
2. 確保工作目錄 (Working Directory) 設定為本專案根目錄。
3. 執行 `MainCode.R` 即可重現所有模型配適結果與 `png` 圖表。
