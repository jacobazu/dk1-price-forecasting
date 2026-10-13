# DK1 day-ahead price forecasting

Every morning before the noon auction this forecasts tomorrow's hourly day-ahead prices in DK1 (western Denmark). Each forecast is saved, never changed, and scored once the prices are known. It all runs on GitHub Actions.

Dashboard: [jacobazu.github.io/dk1-price-forecasting](https://jacobazu.github.io/dk1-price-forecasting)

## The real test

A backtest shows that a method would have worked. It doesn't show that it works. I made my choices with the backtest period in view, so the backtest is probably a bit too good. The live forecasts are saved before the prices are known, so they can't be adjusted afterwards. That's the interesting part, so the dashboard's main page only shows the live forecasts and their track record. The backtest has its own page.

## Models

- BMT-X: boosting with multiple testing (Kapetanios, Sarafidis & Ventouri), the method from my thesis. One model per hour.
- LEAR-X: one lasso per hour, lambda by BIC (Lago et al. 2021).
- XGBoost-X: boosted trees, one model for all hours.
- Ensemble: the average of the three. This is the forecast.
- Benchmark: the weekly naive (last week's prices on Mon, Sat and Sun, yesterday's on the other days).

Inputs: DK1 prices from D-1, D-2, D-3 and D-7, neighbour prices from D-1, holidays, and weather forecasts made 48 hours ahead for DK1 and Germany. All known before the auction. The models are re-estimated every day on the last two years.

## Backtest

1 Oct 2025 to 10 Oct 2026, 375 days:

| Model | MAE (€/MWh) | vs naive |
|---|---|---|
| Ensemble | 17.0 | −50% |
| XGBoost-X | 18.1 | −47% |
| BMT-X | 18.2 | −47% |
| LEAR-X | 18.5 | −46% |
| Weekly naive | 34.2 | |

- The ensemble beats each single model (Diebold-Mariano with Newey-West, p < 0.001).
- German wind and solar forecasts cut the MAE from 19.0 to 17.0 (p < 0.001). Used from the forecast for 14 Oct 2026.
- The forecasts run about 6 €/MWh too low. A bias correction (MAE 17.0 to 16.3, p = 0.02) runs live as a challenger. It replaces the ensemble on 1 Mar 2027 only if its live MAE is lower with p < 0.05.
- ENTSO-E load forecasts are downloaded too, but add nothing once German weather is in, so they aren't used.
- Battery revenue at risk: of four methods only EWMA-filtered historical simulation passes the Kupiec, Christoffersen and Acerbi–Székely tests, and only just.

The backtest page has the rest: ranges, the chance of a negative price, the variables study and the battery backtest.

## License

Code: [MIT](LICENSE). Not trading advice.

Data from [Energi Data Service](https://www.energidataservice.dk) (Energinet), used under "Conditions for use of Danish public-sector data". Not covered by the MIT License or endorsed by Energinet. Weather from [Open-Meteo.com](https://open-meteo.com), [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). Load forecasts from the [ENTSO-E Transparency Platform](https://transparency.entsoe.eu).
