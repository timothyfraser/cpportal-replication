# CP Portal — replication

Replication materials for the CP Portal congestion-pricing air-quality study.

> **Paper title:** _placeholder — set when the manuscript title is final._
> **Author:** Timothy Fraser (Cornell University), `tmf77@cornell.edu`
> **Data DOI:** _placeholder — the Harvard Dataverse deposit is not published yet._

## What this repository is

This is a **mirror**, not a rewrite. Every estimation file here is copied
byte-for-byte, by CI, from the private `cpportal` repository that actually
trains the models behind the paper and the live portal. There is no second
implementation to drift: if the estimator changes upstream, this repository
changes on the next push to `main`.

| path | what it is |
|---|---|
| `job/model/fect/train/functions.R` | the training pipeline: spec catalogue, outcome/covariate declarations, panel assembly, treatment encoding, the anchored (M10) path |
| `job/model/fect/functions_fect.R` | the `fect` wrapper: `get_fect()`, per-cell effects, ATT pooling, back-transformation |
| `job/model/fect/train/gap_shape.R` | the monotone-constrained gap-distance model used by the anchored path |
| `job/model/fect/train/episode_lib.R` | episode scoring / exclusion |
| `job/model/did/functions_did.R` | DiD helpers the FECT path reuses |
| `job/model/functions.R` | shared model utilities (treatment-zone assignment, grouping, collinearity screening) |
| `replication/` | the replication-only wrapper: this README, the panel contract, the Dataverse puller, the runner |
| `MIRROR_MANIFEST.json` | source commit SHA and a sha256 for every mirrored file |

`MIRROR_MANIFEST.json` is how you check what you have: its `source_sha` is the
exact `cpportal` commit these files came from, and each `sha256` lets you verify
a file was not altered in transit.

## Quick start

```sh
# 1. dependencies (R >= 4.4)
Rscript -e 'install.packages(c("dplyr","tidyr","lubridate","tibble","httr2","gtools","lme4","car"))'
Rscript -e 'remotes::install_github("xuyiqing/fect")'

# 2. prove the pipeline runs, with no data and no credentials
Rscript replication/run_replication.R --dry-run

# 3. the real thing, once the deposit is published
export DATAVERSE_API_KEY=...            # Harvard Dataverse -> Account -> API Token
Rscript replication/pull_dataverse.R --doi doi:10.7910/DVN/XXXXXX
Rscript replication/run_replication.R
```

Run everything from the repository root.

### The dry run

`--dry-run` reads `replication/fixtures/`: a **synthetic** panel of 200
monitor-days across 2 metros, built by `replication/fixtures/make_fixture.R`.
Every value in it is fabricated. It exists to prove the mirrored code path
executes outside the private infrastructure — not to produce a number. **Do not
cite anything the dry run prints.**

It does exercise the real machinery, including the awkward parts: the fixture's
treated metro hosts both a Mon–Fri cordon and a 7/7 scheme, so the either/or
charged-days encoder has to keep weekends for the monitors covered by both and
drop them for the monitors covered by only one.

## What the model estimates

Counterfactual fixed effects (`fect`, `method = "cfe"`) on a monitor-by-day
panel. Air-quality outcomes are fitted on the **sqrt** scale for variance
stabilisation and back-transformed to native units by Monte-Carlo moments at
pooling time. The paper spec is the `all_priority` sample with outcome
`aq_daily_mean`; ten more AQ outcomes are trained by identical code.

Two estimand rules that are easy to get wrong, and that the mirrored code
implements for you:

1. **The treated set is `cordon` AND `ULEZ`.** Both, always. `cordon` alone
   silently excludes London's ULEZ periods and understates London's treated set.
   Narrowing it changes the estimand, not a setting.
2. **A charging scheme that is not charging is not treating.** Non-charging days
   (Singapore Sundays; Stockholm / Milan / Gothenburg weekends; London weekends
   before 2019-04-08) are `treated = FALSE`. The rows are **kept** — outcome and
   covariates still inform the day-of-week and seasonal structure — only the
   flag moves. Resolution is **either/or across schemes**: a monitor-day stays
   treated if *any* covering scheme charges that weekday. Treatment is therefore
   non-absorbing where flipped, which `fect` handles natively.

`replication/panel_contract.md` is the column contract. It is **generated** from
the training declarations by `replication/generate_panel_contract.R` — it cannot
drift from the code that consumes the columns.

## What is and is not reproducible

**Reproducible here**, from the deposit plus this code: panel assembly from
`fetch_panel()` onward, the treatment encoding, the `fect` fit, the per-cell
effects, and the per-metro fitted-basis ATTs.

**Not reproducible here:**

- **The ETL that builds the panel.** AirNow / EPA AQS / EEA ingest, Open-Meteo
  weather, WorldPop rasters, ACAG satellite PM2.5, OSM road distances, and the
  background-concentration procedure all run against a private PostGIS database
  and third-party APIs under their own terms. The deposit is that pipeline's
  *output*, which is the artefact worth checking anyway.
- **Licensed traffic columns.** The TomTom-derived `traffic_*` outcomes are not
  redistributable and are absent from the deposit.
- **The serving layer.** The published tables apply a serving-side charged-days
  pooling cut and, for metros `fect` cannot fit, the anchored (M10) satellite
  path. Both live in the portal's API, not in the training code, so the runner's
  output is the **fitted-basis** ATT — comparable to, but not identical to, the
  published pooled row for anchored cities.
- **Exact bootstrap replication.** Standard errors come from a bootstrap; the
  point estimates reproduce, the SE digits will not match bit-for-bit.

## Citation

```bibtex
@misc{fraser_cpportal_replication,
  author = {Fraser, Timothy},
  title  = {CP Portal: replication materials},
  year   = {2026},
  note   = {Data DOI: placeholder. Code mirrored from the private cpportal repository.},
  url    = {https://github.com/timothyfraser/cpportal-replication}
}
```

Please cite both the paper and the Dataverse deposit when the DOI is available.

## License

Code: MIT. Deposited data: CC BY 4.0, subject to each upstream provider's terms
(EEA, EPA, AirNow, Open-Meteo, WorldPop, ACAG, OpenStreetMap). Licensed TomTom
data is excluded and is not covered by either.

## Issues

This repository is **generated**. Pull requests against it will be overwritten by
the next mirror push — open an issue instead, or contact the author.
