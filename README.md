# AI Data Center Customer Segmentation (Europe)

**Latent Profile Analysis for Strategic Market Planning**

---

## 🚀 Project Overview

This project leverages **Latent Profile Analysis (LPA)** to segment potential customers for AI data centers in the European market.
Inspired by the NVIDIA CEO’s vision for new data center infrastructure in Europe, the goal is to identify actionable customer segments and provide data-driven recommendations for product, pricing, and go-to-market strategies.

---

## 🔎 Methodology

* **Data Simulation:** Created a realistic dataset of 300 organizations (Universities, Startups, Enterprises, Regulated).
* **Features:** Annual Budget, Monthly Compute Hours, Compliance Score, AI Maturity.
* **Latent Profile Analysis:** Used R (`tidyLPA`, `ggplot2`, `dplyr`) to find the optimal number of customer segments.
* **Model Selection:** Chose a 4-segment solution using AIC/BIC and business interpretability.

---

## 📊 Results

**Segmentation identified 4 distinct customer profiles:**

| Profile                        | Size | Avg Budget (€) | Avg Compute | Dominant Org Type |
| ------------------------------ | ---- | -------------- | ----------- | ----------------- |
| Budget-Conscious Researchers   | 83   | 270,653        | 1,143       | Startup           |
| Agile Tech Innovators          | 56   | 2,117,064      | 2,432       | Regulated         |
| Enterprise Powerhouses         | 62   | 389,741        | 1,463       | University        |
| Compliance-First Organizations | 99   | 1,520,161      | 3,019       | Enterprise        |

* **Average membership probability:** 0.92
* **Total annual revenue potential (simulated):** **€27,131,487**

---

### 📈 Example Plots

**Customer Profile Characteristics** <img src="https://github.com/saverin0/ai-dc-customer-segmentation-lpa/blob/main/Rplot1.png" alt="Profile Means Plot" width="700"/>

**Budget vs Compute Hours by Profile** <img src="https://github.com/saverin0/ai-dc-customer-segmentation-lpa/blob/main/Rplot2.png" alt="Budget vs Compute Plot" width="700"/>

**Profile Composition by Organization Type** <img src="https://github.com/saverin0/ai-dc-customer-segmentation-lpa/blob/main/Rplot3.png" alt="Profile Composition Plot" width="700"/>

**Annual Revenue Potential by Customer Profile** <img src="https://github.com/saverin0/ai-dc-customer-segmentation-lpa/blob/main/Final%20Revenue%20Plot.png" alt="Revenue Plot" width="700"/>

---

## 💡 Strategic Recommendations

**1. Budget-Conscious Researchers**
*Target:* Startups & universities with limited budgets.
*Actions:* Academic discounts, shared resource pools, grant partnerships.

**2. Agile Tech Innovators**
*Target:* AI-native startups & regulated orgs.
*Actions:* Freemium/pay-as-you-grow, developer support, accelerators.

**3. Enterprise Powerhouses**
*Target:* Large enterprises, steady AI workloads.
*Actions:* Premium support, dedicated clusters, custom consulting.

**4. Compliance-First Organizations**
*Target:* Regulated industries, strict security needs.
*Actions:* Audit-ready features, GDPR/SOC2, dedicated compliance environments.

---

## 📈 Business Value

* Enables tailored marketing, pricing, and product development for each segment.
* Guides investment for new data center infrastructure in line with real market demand.

---

## 📁 How to Run

1. Clone this repo.
2. Install R packages: `dplyr`, `ggplot2`, `tidyLPA`, `viridis`, `knitr`, `gridExtra`, `scales`.
3. Run the `segmentation_analysis.R` script or open in RStudio.
4. Review the results and plots.

---

## 📣 Inspired by NVIDIA CEO's vision for a new era of AI data centers in Europe.

---

**Contact:** [Abhishek Singh on LinkedIn](https://www.linkedin.com/in/abhishekzsingh/)
**License:** MIT
