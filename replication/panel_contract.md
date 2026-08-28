<!-- GENERATED FILE — DO NOT EDIT.
     Source: Rscript replication/generate_panel_contract.R
     Derived by SOURCING the real training code and calling
     fect_outcome_covariates() / fect_spec_panel_cols(). Any hand edit is
     reverted by the next run and fails `--check` in CI. -->

# Panel contract — what the FECT model actually consumes

This is the column contract for the Harvard Dataverse deposit. It is generated
from the mirrored training code, so it cannot drift from what
`job/model/fect/train/functions.R` really asks `model_panel_daily` for.

## 1. Keys (every row)

The panel grain is **one row per (monitor, local calendar day, partition key)**.

- `metro_id`
- `fullaqsid`
- `date`

| key | meaning |
|---|---|
| `fullaqsid` | monitor identity — the FECT *unit*. Stable across the panel; a monitor belongs to exactly one metro for life. |
| `date` | the **metro-local** calendar day (not UTC). The FECT *time* index. |
| `metro_id` | metro the monitor sits in; used for the metro index and per-metro ATT pooling. |
| `partition_key` | `"<pollutant>-1HR"` / `"<pollutant>-24HR"`. Filtered on, used to prefer 1HR over 24HR in the (monitor, day) dedupe, then dropped before the fit. |

## 2. Outcomes (11 AQ + 5 traffic)

### 11 air-quality outcomes

Daily aggregates of the monitor's pollutant concentration. `mp`/`ep`/`do`/`no`
are the morning-peak / evening-peak / daytime-off-peak / night windows;
`mean`/`med`/`max` are the within-window statistics. All are fitted on the
**sqrt** scale for variance stabilisation (`fect_outcome_formula()`), and
back-transformed to native units by Monte-Carlo moments at pooling time.

- `aq_daily_mean`
- `aq_daily_med`
- `aq_daily_max`
- `aq_mp_mean`
- `aq_mp_med`
- `aq_ep_mean`
- `aq_ep_med`
- `aq_do_mean`
- `aq_do_med`
- `aq_no_mean`
- `aq_no_med`

### 5 traffic outcomes (speeds; identity transform)

- `traffic_daily_mean`
- `traffic_mp_mean`
- `traffic_ep_mean`
- `traffic_do_mean`
- `traffic_no_mean`

The paper spec is `all_priority` x `aq_daily_mean`. The other outcomes are
trained by the same code with the same covariates; the deposit should carry
all of them so a replicator can reproduce the robustness grid.

## 3. Covariates — AQ specs (10 time-varying X + 1 time-invariant Z)

### 10 time-varying covariates (X)

- `temp_daily_mean`
- `rhum_daily_mean`
- `precip_daily_mean`
- `ws_daily_mean`
- `wd_daily_mean_deg`
- `barpr_daily_mean`
- `population`
- `bg3`
- `sat_monthly_mean`
- `metro_pop_density_year`

### 1 time-invariant covariate(s) (Z)

- `dist_km_motorway_trunk_primary_secondary`

Z enters separately because `fect(method = "cfe")` partials time-invariant
terms out differently from X; under `method = "mc"` the same code folds Z into X.

### 4 covariates — traffic specs

- `temp_daily_mean`
- `rhum_daily_mean`
- `population`
- `dist_km_motorway_trunk_primary_secondary`

## 4. The exact fetched column set for the paper spec

`fect_spec_panel_cols()` for `all_priority` x `aq_daily_mean`, plus the
`partition_key` that `fetch_panel()` adds for the dedupe — 16 columns:

```
metro_id
fullaqsid
date
aq_daily_mean
temp_daily_mean
rhum_daily_mean
precip_daily_mean
ws_daily_mean
wd_daily_mean_deg
barpr_daily_mean
population
bg3
sat_monthly_mean
metro_pop_density_year
dist_km_motorway_trunk_primary_secondary
partition_key
```

## 5. Non-panel tables the model also needs

`train_fect_bundle()` reads three things the panel does not carry. The deposit
must ship them as flat extracts; `replication/run_replication.R` reads them and
hands them to the same functions the trainer calls.

### 5.1 Treated (zone, monitor, period) pairs — `zone_pairs`

Produced privately by `fetch_zone_treated_pairs()` (zone scope; the paper
default) or `fetch_metro_treated_pairs()` (metro scope) via PostGIS
`ST_Intersects(monitors.geometry, zones.geometry)`. Both return the identical
schema, so everything downstream is byte-for-byte unchanged. Deposit columns:

- `fullaqsid` — monitor
- `metro_id` — metro
- `zone_id` — charging zone the monitor falls inside
- `policy_id` — `congestion_pricing_periods` row id
- `policy_uid` — stable text id (e.g. `LON-ULEZ-2019-CENTRAL`)
- `system_type` — `cordon` / `ULEZ` / ... . **The paper's treated set is
  `cordon,ULEZ` — both, always.** `cordon` alone silently excludes London's ULEZ.
- `date_start`, `date_end` — the active policy window (`date_end` NA = open)

Because the pairs are pre-resolved, **a replicator does not need PostGIS or the
zone polygons** to reproduce the paper spec. The polygons are still worth
depositing for anyone who wants to re-derive the pairs; see
`replication/DATAVERSE_DEPOSIT.md`.

### 5.2 Policy charging schedules — `policy_schedules`

Feeds `fect_untreat_uncharged_days()`. A monitor-day is **treated iff at least
one covering (zone, period) pair charges on that weekday** — an EITHER/OR rule
across schemes, not a single per-metro weekday map. London is the reason: the
CCZ charges Mon-Fri, but the ULEZ charges 7/7 from 2019-04-08, so an in-ULEZ
CCZ monitor keeps its weekends and a CCZ-only monitor does not. Deposit columns:

- `policy_id`, `policy_uid`, `system_type`
- `excluded_wdays` — comma-separated `strftime('%u')` weekday numbers the scheme
  does **not** charge (empty = charges 7/7)

Non-charging days are **kept** in the panel (outcome and covariates still inform
the day-of-week / seasonal / factor structure); only the `treated` flag moves.
Treatment therefore becomes non-absorbing where flipped, which `fect`'s `cfe`
method handles natively.

### 5.3 Metro roster

`metro_id -> metro name / country`, for labelling per-metro ATT rows. Cosmetic;
the fit does not use it.

## 6. What a replicator gets, and what they do not

Reproducible from the deposit + the mirrored code: the panel assembly from
`fetch_panel()` onward, the treatment encoding, the `fect` fit, the per-cell
effects, and the per-metro ATTs.

Not reproducible without the private infrastructure: the ETL that BUILDS
`model_panel_daily` (AirNow / EPA AQS / EEA ingest, Open-Meteo weather, WorldPop
rasters, ACAG satellite PM2.5, OSM road distances, the bg3 background
concentration procedure) and the licensed TomTom traffic columns, which are not
redistributable. The deposit is the OUTPUT of that ETL.
