# DK1 day-ahead price forecasting

**Dashboard:** https://jacobazu.github.io/dk1-price-forecasting

Every morning before the noon auction, this repo forecasts the 24 hourly day-ahead prices in DK1
(western Denmark) for the next day. The forecast is saved and never changed, and it is scored once the
real prices are known. It all runs on GitHub Actions, and the dashboard is rebuilt from the results
every day.


## Models

- **BMT-X**: boosting with multiple testing (Kapetanios, Sarafidis & Ventouri), the variable selection
  method I evaluated in my thesis. It adds one variable at a time as long as it passes a multiple-testing
  threshold, then fits OLS. One model per hour.
- **LEAR-X**: one lasso per hour, lambda chosen by BIC, after Lago et al. (2021).
- **XGBoost-X**: gradient boosted trees, one model for all 24 hours.
- **Ensemble**: the plain average of the three.
- **Benchmark**: the weekly naive (last week's prices on Mondays, Saturdays and Sundays, yesterday's
  on the other days).

Inputs: DK1 prices from D-1, D-2, D-3 and D-7, neighbour prices (DE, DK2, NO2, SE3, SE4) from D-1,
holidays, and weather forecasts made 48 hours ahead (wind speed and solar radiation from Open-Meteo).
Everything is known before the auction. The models are re-estimated every day on the previous two years.

## Results

Backtest from 1 October 2025 to 10 October 2026 (375 days), all models on the same hours:

| Model | MAE (€/MWh) | vs weekly naive |
|---|---|---|
| Ensemble | 19.0 | −44% |
| LEAR-X | 20.3 | −41% |
| BMT-X | 20.4 | −40% |
| XGBoost-X | 20.5 | −40% |
| Weekly naive | 34.2 | |

The ensemble is significantly more accurate than each of the single models (Diebold-Mariano,
p < 0.001). The dashboard also has Giacomini-White tests, a model confidence set, bias checks,
probabilistic forecasts (5–95% quantiles, three methods compared), and a battery backtest where a
2 MWh battery plans each day with each forecast.
The current numbers, including the live track record, are on the dashboard.

## License

The code in this repository is released under the [MIT License](LICENSE).

The forecasts are produced for research and portfolio purposes only and come
with no guarantee of accuracy. They are not trading or investment advice.

### Data

Contains data used pursuant to "Conditions for use of Danish public-sector data"
from the Energi Data Service portal (www.energidataservice.dk), published by
Energinet. The price data is not covered by the MIT License, and this project
is not endorsed by Energinet.

Weather data by [Open-Meteo.com](https://open-meteo.com), licensed under
[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
