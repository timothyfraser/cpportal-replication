# Harvard Dataverse deposit — specification

**Status: the extract EXISTS and has been fitted end-to-end; nothing is
uploaded yet and the DOI is still a placeholder.** This file says exactly what
to upload when Tim is ready; it does not upload anything, and no script in this
repository will invent a DOI.

## 0. The extract that actually exists

`replication/make_extract.R` was run for real on **2026-08-28** and its output
was fitted end-to-end by `replication/run_replication.R` (19.9 min) and compared
against the paper by `replication/reproduce_paper.R`. The section below
describes **what was produced**, not what was planned. Sections 1-5 remain the
upload specification.

| | |
|---|---|
| generator | `replication/make_extract.R` @ `82d9b6c` |
| model pin it is aligned to | `2026-08-15T03:38:29Z` — **the same pin `paper_values.json` is generated from**, which is what makes the parity comparison meaningful |
| spec | `fect_all_priority_aq_daily_mean`, zone scope, `system_types = cordon,ULEZ` |
| window | `2000-01-01 .. 2026-04-28` (truncated at the pin's `date_to`) |
| build time | ~2 minutes, of which 0.2 min is the panel query |
| licensed gate | PASS — no `traffic_*` column in any written header, checked on the SELECT allowlist AND on the written file |

Files written, with the row counts and column sets the manifest records:

| file | rows | cols | size | contents |
|---|---:|---:|---:|---|
| `panel.csv.gz` | 816,134 | 15 | 18.0 MB | 462 monitors, 20 metros, 9,577 distinct days |
| `zone_pairs.csv.gz` | 654 | 5 | 2.5 KB | 317 treated monitors, 19 policies, 8 metros |
| `policy_schedules.csv.gz` | 19 | 4 | 296 B | `policy_id`, `policy_uid`, `system_type`, `metro_id` |
| `metro_roster.csv.gz` | 32 | 3 | 792 B | `metro_id`, `name`, `iso_id` |
| `MANIFEST.json` | — | — | — | per-file sha256, row/col counts, pin stamp, per-metro row tallies |

**Three departures from the spec below, stated rather than quietly reconciled:**

1. **The panel is SINGLE-OUTCOME, not all 11.** `make_extract.R` writes only
   `spec$outcome_var`. Reproducing the Table A7 median/max columns therefore
   needs one extract *per outcome*, each in its own directory. If the deposit is
   to serve A7 from a single download, `make_extract.R` must be extended to
   write the outcome columns as a set — that is a code change, not a re-run.
2. **Episode scoring needs `aq_daily_mean` regardless of outcome.** Production
   scores episode flags on the daily-mean series for every outcome. A
   single-outcome `med`/`max` deposit cannot supply it, and the postprocess
   degrades (observed on the `aq_daily_max` run). Either ship `aq_daily_mean`
   alongside every outcome, or accept that A7's non-mean columns exclude a
   different day set from the published ones.
3. **London's Table 1 monitor set is not in the deposit.** The published London
   cell is pooled over the CCZ cordon monitors (`zone_id` 16, 16 ids) rather
   than the whole metro. `paper_values.json` carries only the label
   `ccz_cordon_zone16`. Until `make_extract.R` writes
   `london_ccz_monitors.csv`, that membership lives as a constant in
   `replication/reproduce_paper.R` — which works, but makes a published number
   depend on a script instead of on data.

The extract lives **outside the git tree** (18 MB of panel; the deposit is the
distribution channel, not the repo). Its path is a runner argument, not a
committed constant.

## 1. Dataset record

| field | value |
|---|---|
| Repository | Harvard Dataverse (`dataverse.harvard.edu`) |
| Title | CP Portal: monitor-day panel for congestion-pricing air-quality analysis |
| Author | Timothy Fraser, Cornell University (ORCID: _fill in_) |
| Contact | `tmf77@cornell.edu` |
| Subject | Earth and Environmental Sciences; Social Sciences |
| Keywords | congestion pricing, air quality, PM2.5, causal inference, counterfactual fixed effects, London ULEZ, Stockholm, Milan, Singapore, Oslo, Bergen, Gothenburg, New York |
| License | CC BY 4.0 |
| Related material | `https://github.com/timothyfraser/cpportal-replication` |

## 2. Files

Three CSVs, at the repository root of the deposit. Filenames are load-bearing:
`replication/pull_dataverse.R` looks them up by exact name.

### 2.1 `panel.csv` — the monitor-day panel

One row per (monitor, metro-local calendar day, partition key). Columns are
enumerated in **`replication/panel_contract.md` section 4**, which is generated
from the training code — do not transcribe them by hand into the deposit
description; paste that section.

In short: 4 keys (`fullaqsid`, `date`, `metro_id`, `partition_key`), the 11 AQ
outcomes, the 10 time-varying covariates, and the 1 time-invariant covariate.

Scope to deposit:

- All treated metros and the control metros used in the paper.
- The full modelled date window.
- Partition key `<pollutant>-1HR` plus `<pollutant>-24HR` where the outcome is
  daily-compatible (the trainer prefers 1HR and uses 24HR to break the tie).

**Exclude** every `traffic_*` column — TomTom-derived, licensed, not
redistributable — and any column not named in the contract.

### 2.2 `zone_pairs.csv` — treated (monitor, policy) pairs

Exactly the output of `fetch_zone_treated_pairs()` at **zone** scope with
`system_types = {cordon, ULEZ}`:

| column | type | note |
|---|---|---|
| `metro_id` | int | |
| `fullaqsid` | text | monitor inside a charging zone polygon |
| `policy_id` | int | `congestion_pricing_periods.id` |
| `start_date` | date | policy window start |
| `end_date` | date | window end; empty = open-ended |

Depositing the resolved pairs means a replicator needs **neither PostGIS nor the
zone polygons** to reproduce the paper spec.

If the metro-scope variant is also deposited, name it `zone_pairs_metro.csv`
(output of `fetch_metro_treated_pairs()`; identical schema).

### 2.3 `policy_schedules.csv` — policy rows

Exactly the output of `fect_policy_schedules()`:

| column | type | note |
|---|---|---|
| `policy_id` | int | |
| `policy_uid` | text | e.g. `LON-ULEZ-2019-CENTRAL` |
| `system_type` | text | `cordon`, `ULEZ`, … — drives the 7/7 rule |
| `metro_id` | int | drives the metro cordon weekday map |

The weekday schedule itself is **code, not data**: it is resolved from
`system_type` and the metro map inside `fect_untreat_uncharged_days()`. Deposit
these four columns and nothing more, so the schedule cannot be silently forked.

### 2.4 Optional extras

- `metros.csv` — `metro_id`, name, country, ISO code. Labelling only.
- `zones.geojson` — charging-zone polygons, for anyone re-deriving the pairs.
  Not needed for the paper spec.

## 3. Deposit README

Upload `replication/README.md` as the dataset description, with the DOI filled
in. Add one paragraph naming the upstream data providers and their terms: EEA,
EPA AQS, AirNow, Open-Meteo, WorldPop, ACAG (satellite PM2.5), OpenStreetMap.

## 4. Publishing checklist

1. Export the three CSVs. Confirm no `traffic_*` column survives:
   `head -1 panel.csv | tr ',' '\n' | grep -c '^traffic_'` must print `0`.
2. Verify the header against the contract:
   `Rscript replication/pull_dataverse.R --validate-only --out <dir>`
3. Create the dataset, upload, publish, record the DOI.
4. In the **private** `cpportal` repo (never by editing the public mirror):
   - set `DOI_PLACEHOLDER` in `replication/pull_dataverse.R`,
   - fill the DOI and the paper title in `replication/README.md`,
   - fill the DOI in the citation block.
   Merge to `main`; the mirror workflow publishes the update.
5. End-to-end check from a clean clone of the public repo:
   `Rscript replication/pull_dataverse.R && Rscript replication/run_replication.R`

## 5. Versioning

Re-deposit as a new **version** of the same dataset (the DOI is stable) whenever
the panel is rebuilt in a way that moves published numbers. Note the `cpportal`
commit SHA from `MIRROR_MANIFEST.json` in the version note, so a deposit version
and a code state can always be paired.
