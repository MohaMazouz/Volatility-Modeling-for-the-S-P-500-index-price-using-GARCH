📈 Volatility Modeling with t-GARCH(1,1)
This project analyzes the volatility of financial returns using GARCH-type models, focusing on the t-GARCH(1,1) specification.
We extract, annualize, and forecast conditional volatilities, and evaluate the model’s quality through residual diagnostics.

📚 Summary
Models compared: GARCH(1,1), t-GARCH(1,1), eGARCH(1,1)

Selection criteria: AIC, LogLik

Best model: t-GARCH(1,1)

Key findings:

t-GARCH(1,1) captures fat tails effectively (Student-t distribution).

Forecasted annualized volatility ≈ 36%.

Residuals show no autocorrelation (good model fit).

Future directions:
Explore eGARCH-t models or multivariate GARCH extensions.

🔧 Technologies
R (analysis and plotting)

Packages: rugarch, forecast, tseries, ggplot2

📊 Key Outputs
Volatility annualization

Volatility forecast (T+1 and T+2)

Residual diagnostics

Final graphs showing volatility evolution

📈 Example Visualization
Conditional volatility over time

Forecasted volatility for future periods

📌 Project Structure
bash
Copier
Modifier
.
├── code/
│   ├── garch_modeling.R
│   ├── volatility_forecasting.R
│   └── diagnostic_tests.R
├── graphs/
│   ├── volatility_plot.png
│   └── forecast_plot.png
├── README.md
└── report/
    └── final_interpretation.pdf
