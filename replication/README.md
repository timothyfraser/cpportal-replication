# CP Portal — replication

Replication materials for the CP Portal congestion-pricing air-quality study.

> **Paper title:** _Congestion Pricing Durably Cut PM2.5 in Seven Global Cities, 2000–2026_
> **Author:** Timothy Fraser (Cornell University), `tmf77@cornell.edu`
> **Data Snapshot:** Stored directly in `replication/data/` (API stamp: `2026-09-13T06:12:32Z`)

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

# 2. prove the pipeline runs on the synthetic fixture
Rscript replication/run_replication.R --dry-run

# 3. the real thing: data snapshot stored directly in repository (replication/data/)
# Dated with API date-time stamp: 2026-09-13T06:12:32Z
# Reads panel.csv, panel.csv.gz, panel.zip, or reassembles from by_metro/
Rscript replication/run_replication.R --data-dir replication/data --out out
Rscript replication/reproduce_paper.R  --run out --out REPRODUCTION_REPORT.md
```

Run everything from the repository root.

### Runtime and resources

Measured on the reference run, 2026-08-28, Windows 11, 8 cores
(`run_meta_aq_daily_mean.json` in that run's output directory):

| stage | wall time | notes |
|---|---|---|
| build the deposit (private only) | ~2 min | `make_extract.R`; not runnable outside the private repo |
| `fect` fit, `aq_daily_mean` | **19.9 min** | 816,134 monitor-days, 462 monitors, 20 metros, `tol = 0.003` |
| QoI aggregates + episode postprocess | ~1.5 min | `overall`, `per_metro`, `per_metro_month`, `per_day_metro_unit` |
| `reproduce_paper.R` | < 1 min | reads the written CSVs, no refit |

`aq_daily_max` fitted in **8.6 min** on the same machine — the daily-max panel
is smaller. Peak memory stays in the low single-digit GB on the analytic path.
There is **no bootstrap**: production computes standard errors analytically
(`vartype = "analytic"`, `twoway_resid`), and so does this runner.

### The three failed runs, and why they are documented here

Runs 1-3 of the reference fit died — twice on memory, once on
`std::out_of_range: Mat::elem()`. None of them was a hardware problem. The
runner had been written from `get_fect()`'s **signature defaults and its
docstring** instead of from the **trainer's call site**
(`job/model/fect/train/functions.R:3426-3442`), and the two disagree on exactly
the arguments that matter:

| argument | `get_fect()` default | what the trainer passes |
|---|---|---|
| `vartype` | `"bootstrap"` | `"analytic"` |
| `use_metro_index` | `TRUE` | `FALSE` |

With `vartype = "bootstrap"` the fit entered a branch **production never runs**,
serialising a multi-GB closure to parallel workers. With `use_metro_index = TRUE`
it attempted a fit that the trainer's own comments record as "silently failing
on every spec we inspected", paid its full cost, and only then fell back.

The lesson is general enough to be worth stating in a public README: **when
mirroring a call, mirror the CALL SITE, not the callee's documentation.** A
wrapper's defaults describe what it does when nobody configures it; production
configures it. `replication/reproduce_paper.R` and the fit block in
`run_replication.R` both carry the argument-by-argument audit table against that
call site, and the failed runs' logs are kept beside the successful one.

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
- **The anchored (M10) path.** For metros `fect` cannot fit, the published
  number comes from the satellite-anchored estimator in the portal's API. At
  zone scope that is **Oslo (956) alone**. A fitted-only run has no fitted row
  for it — a design boundary, not a missing number.
- **Bit-identical point estimates.** The published values read a *pinned* fit;
  this runner *refits*. Same code, same data, same window, but a different
  realisation of an EM fit stopped at `tol = 0.003`. On the reference run every
  city ATT landed within 0.04-0.24 ug/m3 of the paper, all in the same
  direction, while the pooled cell sets matched **exactly**. See
  `REPRODUCTION_REPORT.md`.

The serving-side **charged-days and episode pooling cuts are no longer on this
list**: `app/v2/api/R/cross_city_att.R` is mirrored, and
`replication/reproduce_paper.R` calls its filters directly rather than copying
the weekday map.

## Paper artifact -> script -> output

Every published table, figure and quantity of interest, with the script that
regenerates it and the file it lands in. A **NOT YET** line is a real gap with
its reason — there are no silent omissions.

| paper artifact | script | output | status |
|---|---|---|---|
| Table 1 — per-city / per-era ATT, CI, pct | `run_replication.R` -> `reproduce_paper.R` | `REPRODUCTION_REPORT.md`, `parity_<outcome>.csv` | **reproduced** (cell sets exact; see the report for the point-estimate offset) |
| Table 1 — London CCZ / ULEZ era rows | same | same | **reproduced** — the era windows travel in `paper_values.json` |
| Table 1 — pooled `equal_weight`, `monitor_day` | `reproduce_paper.R` | same | **reproduced** |
| Table 2 — per-city + per-era, pooled rows | `reproduce_paper.R` | `parity_<outcome>.csv` (group `t2`) | **reproduced** where the row is a fitted metro; anchored rows out of scope |
| Table 4 — year-horizon Y1..Yk pooled | — | — | **NOT YET.** Needs `metro_window_estimate()`'s per-metro treatment-start dates; the deposit does not expose them in that shape. The pooling function itself is mirrored. |
| Table A7 — daily mean / median / max per city | `run_replication.R --outcome aq_daily_{mean,med,max}` | one output dir per outcome | **mean reproduced**; `med` and `max` need their own extract (see below) and were still fitting when this README was written |
| Figure `fig_att_by_year` inputs | — | — | **NOT YET.** Same dependency as Table 4. |
| Oslo (956), any table | — | — | **NOT applicable.** Anchored (M10); no fitted row exists. |
| `traffic_*` outcomes, any table | — | — | **NOT redistributable.** TomTom-licensed; excluded from the deposit by `check_no_licensed.R`. |

Two mechanics worth knowing before you run the A7 row:

* **The deposit is single-outcome.** `make_extract.R` writes only the spec's own
  outcome column, so `aq_daily_med` and `aq_daily_max` each need their own
  extract directory and their own fit. They do not share the mean's panel.
* **Episode scoring reads `aq_daily_mean`.** In production every outcome's
  episode flags are scored on the daily-mean series. A single-outcome `med`/`max`
  deposit cannot supply it, so the episode postprocess degrades. This is
  reported by the runner, not swallowed.

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
