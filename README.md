# Portfolio Rebalancing Simulation in R

## Executive Summary:
In today’s dynamic financial markets, portfolio rebalancing is critical for maintaining risk-adjusted returns and ensuring long-term performance stability.  
This project simulates an **automated portfolio rebalancing strategy** for a $5 million stock portfolio composed of **10 high-tech equities**, using **R** for quantitative analysis and visualization.

The simulation evaluates how different **rebalancing frequencies (3-day vs 5-day intervals)** influence portfolio growth, volatility, and cumulative returns over a 6-month period.  
Through systematic rebalancing and comparative analysis, the project demonstrates that timely reallocation can **enhance returns by up to 4%** compared to a static (buy-and-hold) approach.

---

## Business Problem:
Investors often struggle to determine the optimal timing for rebalancing portfolios in response to market fluctuations.  
Key challenges addressed in this project include:
- How frequently should rebalancing occur to maximize returns without incurring excessive transaction costs?  
- What is the impact of rebalancing frequency on portfolio volatility and drawdowns?  
- How does currency conversion (USD → JPY) influence total performance for international investors?  

The goal was to build a **simulation framework in R** that quantifies the performance trade-offs across different rebalancing intervals and provides actionable insights for global portfolio optimization.

---

## Methodology:
The simulation follows a structured, data-driven workflow:

### **1. Portfolio Construction**
- Created a diversified portfolio of **10 high-tech U.S. stocks** with an initial capital of **$5 million**, evenly allocated.  
- Pulled historical price data for each stock to serve as the foundation for performance calculations.

### **2. Data Preparation**
- Imported adjusted closing prices and formatted time-series data using **`dplyr`** for pipeline transformations.  
- Calculated daily returns and cumulative values for each asset in the portfolio.  

### **3. Rebalancing Algorithm**
- Implemented a custom **R simulation** that automatically rebalances holdings every **3 days** and **5 days**.  
- Each rebalance recalculates optimal weights to restore equal allocation based on updated market prices.  

### **4. Performance Analysis**
- Evaluated portfolio value, return differentials, and cumulative growth across both rebalancing strategies.  
- Integrated **USD–JPY currency conversion** to measure foreign exchange impact.  

### **5. Visualization**
- Utilized **`ggplot2`** for clear, comparative plots of portfolio trajectories and rebalance outcomes.  

---

## Skills:
**Programming Language:** R  
**Libraries:** ggplot2, dplyr, broom, glmnet  
**Techniques:** Portfolio simulation, time-series analysis, rebalancing optimization, performance benchmarking  
**Finance Concepts:** Risk-adjusted returns, rebalancing thresholds, currency exposure  

---

## Results & Key Insights:

### **1. Optimal Rebalancing Frequency**
Frequent rebalancing (every **3 days**) generated higher stability and reduced drift in portfolio weights, leading to a **4% increase in cumulative portfolio value** over 6 months compared to the 5-day strategy.

---

### **2. Currency Conversion Effects**
When denominating returns in **Japanese Yen (JPY)**, currency fluctuations introduced an additional layer of performance variation — underscoring the importance of **hedging strategies** for global investors.

---

### **3. Portfolio Growth Comparison**
- **3-Day Rebalancing:** Faster correction to market movements, higher short-term transaction volume.  
- **5-Day Rebalancing:** Lower turnover and cost efficiency, but slightly higher volatility.  

---

## Summary of Insights:
- **Frequent Rebalancing:** Improves return consistency and controls asset drift.  
- **Currency Sensitivity:** Currency exposure significantly influences international returns.  
- **Automation Benefits:** Data-driven rebalancing ensures disciplined investment strategy execution.  

---

## Business Impact:
- **Return Improvement:** Up to **4% gain** from optimal rebalancing frequency.  
- **Risk Control:** Improved weight stability across assets, reducing deviation from target allocations.  
- **Investor Decision Support:** Framework supports backtesting and rebalancing policy evaluation.  

---

## Next Steps:
- Extend simulation to include **transaction cost modeling**.  
- Test adaptive rebalancing thresholds using **volatility-based triggers**.  
- Integrate **machine learning forecasting** (e.g., LASSO regression via `glmnet`) for predictive allocation.  
- Visualize rolling Sharpe ratios and performance attribution across time.  

---

## Tools & Architecture:
**Language:** R  
**Environment:** RStudio  
**Libraries:** ggplot2, dplyr, broom, glmnet  
**Data Source:** Historical stock price CSVs (Yahoo Finance)  
**Visualization:** ggplot2 plots of cumulative portfolio values  
**Storage:** Local CSV datasets for each stock  
**Simulation Output:** Comparative charts and summary metrics  

---

## Author:
**Aurel Sahiti** 
Data Science Graduate Student | Quantitative Finance & Investment Analytics  
[LinkedIn](https://linkedin.com/in/aurelsahiti) | [GitHub](https://github.com/aurelsahiti)
