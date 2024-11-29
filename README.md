# Sign restricted vector Autoregressions
## Project Overview

This code is a part of a paper that investigates the the impact of monetary policy shocks on inflation expectations. It used  market-based measures **Breakeven Inflation Rate (BEIR)** at different time-horizons. The study focuses on key relationships among variables like unemployment, inflation, monetary policy (Wu Xia shadow rates), exchange rates, and commodity prices. 
#### Assumptions:
- Monetary policy shocks (via Fed Funds rate):
  - Decrease inflation.
  - Increase unemployment.
  - Positively relate to their own lagged values.
---

## The structure of the code is as follows:

### Dataset Processing
- Data is imported and labeled.
- Time-series transformations.
- Missing values are checked and omitted.

### Variable Selection
The analysis incorporates the following variables which have been obtained from the Federal Reserve Bank of St. Louis’ database (FRED) and the Federal Reserve Bank of Atlanta:
- **5-Year Breakeven Inflation**: Inflation expectations over a 5-year horizon. The exercise has been repeated for 10-year, 20-year and 30-year BEIRs
- **Unemployment Rate**.
- **Inflation Rate**.
- **Wu Xia Shadow Rate**: A proxy for the monetary policy stance.
- **USD/EUR and USD/JPY Exchange Rates**.
- **Global Energy Prices**.
- **Global Raw Material Prices**.

### Modeling Techniques
1. **Johansen Cointegration Test**
2. **Dickey-Fuller Test**
3. **Sign-restricted Vector Autoregressions** 

  a. **Uhlig’s Rejection Method**
  b. **Rubio-Ramirez’s Rejection Method**
  c. **Uhlig’s Penalty Method**

5. **Variance Decomposition and Impulse Response Analysis**:

---
