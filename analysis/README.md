# Analysis

Uses actual data from ENTSO-E, known only after the fact. It explains prices and forecast errors. It is not used by any model and not part of the backtest.

- `01_download_entsoe.R`: actual generation by type and actual load for DK1 and Germany, hourly. Needs `ENTSOE_TOKEN`.

First question: why are the forecasts too low at 18-21 on expensive days?
