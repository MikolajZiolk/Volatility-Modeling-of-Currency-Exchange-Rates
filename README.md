# Volatility Modeling of Currency Exchange Rates  

## Project Description  
This project explores **volatility estimation and modeling** for daily exchange rates, with a focus on EUR/PLN (2012–2023) and additional analysis of EUR/GBP and EUR/USD (2021). The study applies rolling volatility, EWMA, and GARCH(1,1) models, comparing their accuracy and sensitivity to market shocks.  

## Methodology  
1. **Data Preparation**  
   - Daily closing prices (EUR/PLN, EUR/GBP, EUR/USD).  
   - Computed log and simple returns.  

2. **Rolling Volatility**  
   - Computed moving volatility with window sizes (m = 10, 25, 50, 100).  
   - Comparison of two volatility formulas.  

3. **EWMA Model**  
   - Estimated volatility with λ = 0.94 and multiple λ values (0.85–0.99).  
   - Comparison with rolling volatility.  

4. **GARCH(1,1) Model**  
   - Applied using the `rugarch` package.  
   - Captures sudden changes and reacts strongly to extreme values.  

5. **Correlation Analysis**  
   - EUR/PLN, EUR/GBP, EUR/USD returns show weak correlations.  

6. **Statistical Tests**  
   - Durbin-Watson → no first-order autocorrelation.  
   - Ljung-Box → significant higher-order autocorrelation.  

7. **Model Evaluation**  
   - RMSE comparison between EWMA (different λ) and rolling volatility.  
   - Best result: **EWMA with λ = 0.95**.  

## Results & Insights  
- Shorter windows (m=10) → highly spiky volatility; longer windows (m=100) → smoother estimates.  
- EWMA provides flexibility, with λ = 0.95 offering the best trade-off between stability and responsiveness.  
- GARCH(1,1) is more precise and sensitive to shocks but produces more volatile paths.  
- Weak correlations between exchange rates suggest diversification benefits.  
