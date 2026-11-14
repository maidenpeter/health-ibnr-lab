<p align="center">
  <img src="assets/banner.svg" alt="Health IBNR Lab: reserving a medical book, and what breaks when claims slow down" width="100%">
</p>

<p align="center">
  <img alt="Built with R" src="https://img.shields.io/badge/built%20with-R%20%7C%20Shiny%20%7C%20ChainLadder-3d5a9e?style=flat-square&labelColor=0F1117">
  <img alt="Data" src="https://img.shields.io/badge/data-synthetic-c47a06?style=flat-square&labelColor=0F1117">
  <img alt="Tests" src="https://img.shields.io/badge/tests-17%20of%2017%20pass-00998a?style=flat-square&labelColor=0F1117">
</p>

# Health IBNR Lab

How much we owe on health claims, how sure we are, and what breaks the estimate.

On a synthetic medical book of 64,646 claims valued at 30 September 2026, the best estimate of outstanding claims (pipeline plus IBNR) is **KES 138,821,204**, with a Mack coefficient of variation of 12.0%. The estimate holds up in normal conditions and breaks when payments slow down. **A 3-month, 30% payment slowdown makes chain ladder under-reserve by KES 93,608,488 (50.0%).** A chain ladder on received claims, which do not slow down, stays within 1.0% of the truth.

<table>
  <tr>
    <td align="center" width="33%"><h2>KES 138.8m</h2><sub>Best estimate outstanding claims, pipeline plus IBNR</sub></td>
    <td align="center" width="33%"><h2>12.0%</h2><sub>Mack coefficient of variation</sub></td>
    <td align="center" width="33%"><h2>-50.0%</h2><sub>Paid chain ladder error in the default slowdown (KES 93.6m short)</sub></td>
  </tr>
</table>

Live app: link to follow once deployed.

Built in R (shiny, bslib, ChainLadder). Synthetic portfolio. Illustrative only.

<p align="center"><img src="assets/divider.svg" width="100%" alt=""></p>

## Screenshots

| | |
|---|---|
| ![Book](assets/screenshots/1-book.png) | ![Backlog](assets/screenshots/5-backlog.png) |
| Book | Backlog (the hero page) |
| ![Triangles](assets/screenshots/2-triangles.png) | ![Reserves](assets/screenshots/3-reserves.png) |
| Triangles | Reserves |
| ![Uncertainty](assets/screenshots/4-uncertainty.png) | ![IFRS 17](assets/screenshots/6-ifrs17.png) |
| Uncertainty | IFRS 17 |
| ![Methods](assets/screenshots/7-methods.png) | |
| Methods | |

Walkthrough GIF: to follow.

## Why health IBNR matters

Medical cover is the largest class of general insurance in Kenya. Industry figures for 2025 put medical premiums at KES 93.20bn, up 22.3% on the year ([Capital FM](https://capitalfm.africa/nairobi-mombasa-and-kiambu-account-for-most-insurance-premiums/), [KBC](https://www.kbc.co.ke/microinsurance-posts-fastest-premium-growth-to-reach-ksh-2-17b/), both reporting Insurance Regulatory Authority data). Medical claims ratios run high: press coverage of the IRA's second quarter 2025 release put the medical loss ratio at 81%, against 73% for the industry ([Eastleigh Voice](https://eastleighvoice.co.ke/business/216146/insurance-sector-profits-slide-as-claims-soar-by-sh17-billion); [IRA Q2 2025 release](https://www.ira.go.ke/lib.html?f=quarter-2-2025-industry-release)).

Health claims are short tail, but they arrive as provider invoices with no case reserves, and they are paid after vetting. The reserve at any month end is mostly invoices in the pipeline and claims not yet received. Small changes in how fast claims are paid move it a lot.

The data here is synthetic, generated from the assumptions in [config.yml](config.yml). No insurer's data, name or branding is used. The industry figures above are context only; the BF prior in this lab (78%) is an assumption and is not fitted to them or to the book. The book's true loss ratio, which the generator produces and no reserving team would know, is 87.9%.

<p align="center"><img src="assets/divider.svg" width="100%" alt=""></p>

## Method

- **Triangles.** 36 monthly accident months, October 2023 to September 2026, by monthly development. A paid triangle, and a received triangle (invoices valued at the expected settlement ratio for their benefit type) that stands in for the incurred triangle a health book does not have.
- **Chain ladder, Mack, Bornhuetter-Ferguson.** Volume weighted chain ladder, checked against `ChainLadder::MackChainLadder`. BF with prior ELR times earned premium. The selected basis uses BF for the latest 3 accident months, where paid chain ladder multiplies the newest month by about 26. Mack standard error KES 16,122,375.
- **Bootstrap.** Over-dispersed Poisson bootstrap (`ChainLadder::BootChainLadder`, 5,000 replicates). 75th percentile KES 143,128,695, 99.5th percentile KES 219,084,630.
- **The backlog test.** A share of the payments due in the last N months before valuation is held back and paid after it. Report months and amounts do not change, so the true ultimate is the same in every scenario and every gap is method error. A grid of N from 1 to 6 months and 0% to 60% held back.
- **IFRS 17 view.** The best estimate spread over its expected payment months, discounted on an illustrative curve (KES 136,946,191 present value), plus a risk adjustment at the 75th percentile of the bootstrap (KES 8,337,493), giving a liability for incurred claims of KES 145,283,684. Illustrative, not an implementation.

## What the backlog test found

For the default slowdown (3 months, 30% of due payments held back, KES 45,065,584 of payments):

| Method | Error against the true outstanding |
|---|---|
| Paid chain ladder, all diagonals | -50.0% |
| Paid chain ladder, slowed diagonals excluded | -48.9% |
| Received claims chain ladder | 1.0% |
| BF on paid, ELR prior | -28.9% |

- **Excluding the slowed diagonals barely helps.** Of the KES 93,608,488 shortfall, KES 45,065,584 is payments that slipped past valuation and KES 37,555,979 is chain ladder multiplying that gap by its development factors. Wrong factors account for only KES 2,978,482. Excluding the slowed diagonals fixes the factors, not the short latest diagonal. We expected it to help more.
- **BF helps but does not fix it.** BF does not multiply the gap, but it cannot see it either.
- **The received claims chain ladder sees through it**, because received claims do not slow down.
- **Paid chain ladder can pass its own back-test while it is badly short.** Holding out the latest month, its actual against expected is 104.3% with the slowdown (and 111.4% without), because the slowdown depresses the forecast and the actual together. The received claims chain ladder's A/E is 78.1%, which does flag it.
- **The early warnings work.** The pipeline of received but unpaid claims rises to 3.9 months of paid claims against 1.8 without the slowdown, and an actual against expected heatmap anchored on received claims shows the slowed months as a cold band.

<p align="center"><img src="assets/divider.svg" width="100%" alt=""></p>

## What I would do with real data

- Use case reserves where the claims system has them, and reconcile the received triangle to the claims system's pipeline of invoices.
- Add claims handling expenses, and reinsurance where it applies.
- Check the data before any triangle: duplicates, reversals, date errors, claims paid before they were reported, large claims, and changes in benefit mix.
- Watch payment speed every month (pipeline, A/E against received) before choosing factors, and talk to the claims team when it moves.
- Document the selections, review them with a second actuary, and take them through sign-off.

<p align="center"><img src="assets/divider.svg" width="100%" alt=""></p>

## Run locally

```r
renv::restore()
source("scripts/build_results.R")   # generates the data, runs every method and the tests, about a minute
shiny::runApp()
```

Tests: 17 of 17 pass (`data/test_results.csv`). Every number in this README is filled in from `data/headline.json` by the build script.

## Why I built this

To follow.

<p align="center"><img src="assets/divider.svg" width="100%" alt=""></p>

<p align="center">
  <img src="www/mark.svg" width="28" alt=""><br>
  <sub>Made by <b>Marion Peter</b>, Actuarial Engineer</sub>
</p>
