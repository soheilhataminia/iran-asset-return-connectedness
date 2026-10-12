# Return connectedness across Iran’s financial and durable-asset markets: A generalized Diebold–Yilmaz and TVP–VAR analysis

## Code

R code (`code.R`) for analyzing return connectedness across five Iranian markets.

### Methods

Generalized Diebold–Yilmaz connectedness, TVP–VAR with forgetting factors, Baruník–Křehlík frequency decomposition, generalized impulse responses, bootstrap inference, and robustness checks.

### License

The R code is available under the MIT License.

## Data

Monthly data for April 2016–May 2026 (Jalali 1395/01–1405/02; 122 observations; 121 log returns).

- **Free-market U.S. dollar exchange rate:** TGJU Financial Markets Network.
- **Emami gold coin price:** TGJU Financial Markets Network.
- **Tehran Exchange Dividend and Price Index (TEDPIX):** Tehran Securities Exchange Technology Management Company (TSETMC).
- **Consumer Price Index sub-index for purchase of vehicles:** Statistical Center of Iran (SCI).
- **Average residential apartment transaction price per square metre in Tehran (IRR/m²):** Central Bank of Iran (CBI) and Ministry of Roads and Urban Development (MRUD).
- **Consumer Price Index (CPI):** SCI.
- **Interbank interest rate:** CBI.
- **Iran’s crude oil and condensate export volume:** U.S. Energy Information Administration (EIA) and United Against Nuclear Iran (UANI).
- **Iran Heavy crude oil price:** Organization of the Petroleum Exporting Countries (OPEC).

Full replication requires [Global Sanctions Database (GSDB), Release 4](https://www.globalsanctionsdatabase.com/), obtained separately from its provider.

Selected data are publicly available. The complete input dataset cannot be posted publicly due to third-party redistribution restrictions. To request access, email hatamysoheyl@gmail.com.

## How to Run

1. Obtain the complete input data (`data.xlsx`) and GSDB Release 4.
2. Install the required R packages and update the input paths in `code.R`.
3. From the folder containing `code.R`, run:

```r
source("code.R")
```

## Citation

Hataminia, S., Manochehri, S., Veisi, M., & Mohammadzadeh Asl, N. (2026). *Return connectedness across Iran’s financial and durable-asset markets: A generalized Diebold–Yilmaz and TVP–VAR analysis.* Unpublished manuscript.
