# Return connectedness across Iran’s financial and durable-asset markets: A generalized Diebold–Yilmaz and TVP–VAR analysis

R code for analyzing return connectedness across five Iranian financial and durable-asset markets, April 2016–May 2026 (122 monthly observations; 121 log returns).

## Data and Sources

**Five markets:**
- **USD exchange rate** and **Emami gold coin**: TGJU
- **TEDPIX stock market index**: TSETMC
- **Vehicle CPI sub-index**: Statistical Center of Iran (SCI)
- **Tehran residential housing prices (IRR/m²)**: Central Bank of Iran (CBI) and Ministry of Roads and Urban Development (MRUD)

**Contextual variables:** Consumer Price Index (SCI), interbank interest rate (CBI), crude oil and condensate exports (EIA/UANI), and Iran Heavy crude oil price (OPEC).

**Frequency:** Monthly, using the Iranian Jalali calendar (1395/01–1405/02).

## Methods

Generalized Diebold–Yilmaz connectedness, forgetting-factor TVP-VAR, Baruník–Křehlík frequency decomposition, generalized impulse responses, bootstrap inference, and robustness analysis.

## Code and Data Availability

- **Code:** `code.R`, publicly available under the MIT License.
- **Data:** Analysis-ready input data (`data.xlsx`) available from the corresponding author upon reasonable request, subject to third-party restrictions.
- **Contact:** hatamysoheyl@gmail.com

Full replication additionally requires [GSDB Release 4](https://www.globalsanctionsdatabase.com/), obtained from its original provider.

## Citation

Hataminia, S., Manochehri, S., Veisi, M., & Mohammadzadeh Asl, N. (2026). Return connectedness across Iran’s financial and durable-asset markets: A generalized Diebold–Yilmaz and TVP–VAR analysis. Unpublished manuscript.
