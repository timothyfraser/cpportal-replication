#!/usr/bin/env bash
# =============================================================================
# launch_real_run.sh — start the real replication fit DETACHED.
#
#   bash replication/launch_real_run.sh [outcome] [extract_dir]
#
# The fit runs for hours. It must survive the shell (and the agent) that started
# it, so it is launched with stdin closed and stdout/stderr redirected into the
# extract directory, which lives OUTSIDE the git tree. Nothing this script
# writes ever enters a commit.
#
# Files it produces, all under $EXTRACT/logs and $EXTRACT/output:
#   logs/run_<outcome>.log        full stdout+stderr of the run
#   logs/run_<outcome>.heartbeat  one timestamped line per milestone
#   logs/run_<outcome>.pid        the PID, for `kill`
#   output/att_<outcome>*.csv     the ATT grains
#   output/run_meta_<outcome>.json  seed / nboots / tol / guard verdict
#
# Kill it with:  kill $(cat <EXTRACT>/logs/run_<outcome>.pid)
# =============================================================================
set -euo pipefail

OUTCOME="${1:-aq_daily_mean}"
EXTRACT="${2:-C:/Users/tmf77/cpportal-replication-extract}"
REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

LOG="$EXTRACT/logs/run_${OUTCOME}.log"
HB="$EXTRACT/logs/run_${OUTCOME}.heartbeat"
PIDF="$EXTRACT/logs/run_${OUTCOME}.pid"
OUT="$EXTRACT/output"

mkdir -p "$EXTRACT/logs" "$OUT"

# Rotate any previous log/heartbeat rather than truncating it. A failed run's
# log is the only record of WHY it failed, and the next launch would otherwise
# erase it.
if [ -s "$LOG" ]; then
  n=1
  while [ -e "$EXTRACT/logs/run_${OUTCOME}.failed${n}.log" ]; do n=$((n + 1)); done
  mv "$LOG" "$EXTRACT/logs/run_${OUTCOME}.failed${n}.log"
  [ -s "$HB" ] && mv "$HB" "$EXTRACT/logs/run_${OUTCOME}.failed${n}.heartbeat"
  echo "rotated previous log -> run_${OUTCOME}.failed${n}.log"
fi
: > "$HB"

cd "$REPO"

# PRODUCTION KNOBS, stated explicitly rather than inherited:
#   CPPORTAL_FECT_TOL=0.003        what the nightly runs (NOT the fect default)
#   CPPORTAL_FECT_SAT_ANCHOR=1     must always be 1 (the unset default is a footgun)
#   CPPORTAL_FECT_UNTREAT_UNCHARGED=1  charged-days flip ON (default, made explicit)
#   CPPORTAL_REPLICATION_RUN=1     set again by arm_no_pin_guard(); belt and braces
export CPPORTAL_FECT_TOL=0.003
export CPPORTAL_FECT_SAT_ANCHOR=1
export CPPORTAL_FECT_UNTREAT_UNCHARGED=1
export CPPORTAL_REPLICATION_RUN=1

nohup Rscript replication/run_replication.R \
  --extract "$EXTRACT" \
  --outcome "$OUTCOME" \
  --out     "$OUT" \
  --heartbeat "$HB" \
  --pidfile "$PIDF" \
  --seed 20260828 \
  > "$LOG" 2>&1 < /dev/null &

WRAPPER=$!
# Seed the pidfile with the wrapper pid so it is never empty if R dies early;
# run_replication.R then OVERWRITES it with its own Sys.getpid(). Under Git Bash
# on Windows `$!` is a shell wrapper, not Rterm.exe, so a watcher tracking it can
# report "alive" after the real process has died — or fail to kill it.
echo "$WRAPPER" > "$PIDF"
sleep 3
PID="$(cat "$PIDF" 2>/dev/null || echo "$WRAPPER")"
echo "launched  outcome=$OUTCOME  wrapper=$WRAPPER  R pid=$PID"
echo "  log       $LOG"
echo "  heartbeat $HB"
echo "  kill with kill $PID"
