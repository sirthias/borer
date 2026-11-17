#!/usr/bin/env bash
# Runs borer's JMH benchmarks for one exact commit on a remote compute box (see `benchmarks/AGENTS.md`).
#
# Expects the code in a bare repo on the box (default `/root/bench/borer.git`), pushed via SSH:
#   git push ssh://root@<host>:<port>/root/bench/borer.git HEAD:refs/heads/master
# `REPO=/root/bench/borer.bundle` works as well, since `git clone` also accepts bundles.
#
# Start it detached from the SSH session, with a hard time cap:
#   systemd-run --unit=borer-bench -p RuntimeMaxSec=7200 /root/bench/run.sh <full-commit-sha>
# Quick end-to-end check first (Borer benchmarks only, ~4 min):
#   systemd-run --unit=borer-bench-smoke --setenv=SMOKE=1 -p RuntimeMaxSec=1800 /root/bench/run.sh <full-commit-sha>
#
# Results go to `/root/bench/results/<YYYY-MM-DDTHHMMZ>_<sha7>[-smoke]/` (symlinked as `results/latest`).
set -uo pipefail

SHA_ARG="${1:?usage: run.sh <commit-sha>}"
BENCH=/root/bench
REPO="${REPO:-$BENCH/borer.git}"
export HOME=/root
export JAVA_HOME=/root/.sdkman/candidates/java/current
export PATH="$JAVA_HOME/bin:/root/.sdkman/candidates/sbt/current/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

# JMH settings (budget: 356 runs in < 2h incl. build)
JMH_ARGS=(-f 1 -t 1 -wi 5 -w 1s -i 5 -r 2s)
GROUPS_ORDER=(model dom decoding encoding)   # priority order: Model results feed `benchmarkResults`
declare -A GROUP_REGEX=(
  [model]='io\.bullet\.borer\.benchmarks\.[A-Za-z]+ModelBenchmark\.'
  [dom]='io\.bullet\.borer\.benchmarks\.[A-Za-z]+DomBenchmark\.'
  [decoding]='io\.bullet\.borer\.benchmarks\.[A-Za-z]+DecodingBenchmark\.'
  [encoding]='io\.bullet\.borer\.benchmarks\.[A-Za-z]+EncodingBenchmark\.'
)

# SMOKE=1: quick end-to-end pipeline check (Borer benchmarks only, single 100ms iteration)
if [[ "${SMOKE:-0}" == 1 ]]; then
  JMH_ARGS=(-f 1 -t 1 -wi 0 -i 1 -r 100ms)
  for g in "${GROUPS_ORDER[@]}"; do GROUP_REGEX[$g]="${GROUP_REGEX[$g]/\[A-Za-z\]+/Borer}"; done
fi

RUN_ID="$(date -u +%Y-%m-%dT%H%MZ)_${SHA_ARG:0:7}${SMOKE:+-smoke}"
[[ "${SMOKE:-0}" == 1 ]] || RUN_ID="${RUN_ID%-smoke}"
OUT="$BENCH/results/$RUN_ID"
SRC="$BENCH/src/$RUN_ID"
mkdir -p "$OUT" "$BENCH/src"
exec > >(tee -a "$OUT/run.log") 2>&1
ln -sfn "$OUT" "$BENCH/results/latest"

log() { echo "[$(date -u +%H:%M:%S)] $*"; }
STATUS=1
VMSTAT_PID=
cleanup() {
  [[ -n "$VMSTAT_PID" ]] && kill "$VMSTAT_PID" 2>/dev/null
  log "re-enabling unattended upgrades"
  systemctl start apt-daily.timer apt-daily-upgrade.timer unattended-upgrades 2>/dev/null
  echo "finished: $(date -u +%FT%TZ)" >> "$OUT/meta.txt"
  echo "$STATUS" > "$OUT/exit-code"
  [[ "$STATUS" == 0 ]] && touch "$OUT/DONE"
  log "exit status $STATUS"
}
trap cleanup EXIT
trap 'log "received SIGTERM (time cap?)"; exit 143' TERM

log "run $RUN_ID starting"

# --- quiet the box ---
systemctl stop unattended-upgrades apt-daily.timer apt-daily-upgrade.timer apt-daily.service apt-daily-upgrade.service 2>/dev/null
while pgrep -x 'apt|apt-get|dpkg|unattended-upgr' >/dev/null; do log "waiting for apt/dpkg"; sleep 10; done

# --- checkout exact commit from bundle ---
git -c init.defaultBranch=master clone -q "$REPO" "$SRC" || exit 1
cd "$SRC"
git checkout -q --detach "$SHA_ARG" || exit 1
SHA="$(git rev-parse HEAD)"
log "checked out $SHA"

# --- build (in-process sbt, no lingering server) ---
log "building"
BUILD_START=$SECONDS
sbt --server --no-server -batch "benchmarks/Jmh/compile" "export benchmarks/Jmh/fullClasspath" > "$OUT/build.log" 2>&1 \
  || { log "build failed, see build.log"; exit 1; }
log "build done in $((SECONDS - BUILD_START))s"

# resolve sbt 2 virtual file refs to real paths
CP_RAW="$(sed 's/\x1b\[[0-9;]*[A-Za-z]//g' "$OUT/build.log" | grep '^List(' | tail -1)"
[[ -n "$CP_RAW" ]] || { log "no classpath found in build.log"; exit 1; }
CP_LIST="$(echo "$CP_RAW" | sed 's/^List(//; s/)$//' | tr ',' '\n' | sed 's/^ *//; s/>.*//' \
  | sed "s#\${OUT}#$SRC/target/out#; s#\${CSR_CACHE}#$HOME/.cache/coursier/v1#; s#\${SBT_BOOT}#$HOME/.sbt/boot#")"
MISSING=0
while read -r p; do [[ -e "$p" ]] || { log "missing classpath entry: $p"; MISSING=1; }; done <<< "$CP_LIST"
[[ "$MISSING" == 0 ]] || exit 1
CP="$(echo "$CP_LIST" | paste -sd:)"
echo "$CP" > "$OUT/classpath.txt"

if pgrep -f 'sbt-launch|xsbt.boot' >/dev/null; then log "WARNING: sbt process still running, killing"; pkill -f 'sbt-launch|xsbt.boot'; sleep 3; fi

N_BENCH="$(java -cp "$CP" org.openjdk.jmh.Main -l | grep -c '^io\.bullet')"
log "JMH sees $N_BENCH benchmark methods"

# --- metadata ---
{
  echo "run-id: $RUN_ID"
  echo "commit: $SHA"
  echo "commit-subject: $(git log -1 --format=%s)"
  echo "commit-date: $(git log -1 --format=%cI)"
  echo "describe: $(git describe --tags --always)"
  echo "version.sbt: $(cat version.sbt)"
  echo "host: $(hostname) / $(uname -srm)"
  echo "cpu: $(lscpu | sed -n 's/^Model name: *//p'), nproc=$(nproc), $(lscpu | sed -n 's/^Thread(s) per core: */threads-per-core=/p'), hypervisor=$(lscpu | sed -n 's/^Hypervisor vendor: *//p')"
  echo "memory: $(free -h | awk '/^Mem:/{print $2}')"
  echo "java: $(java -version 2>&1 | tr '\n' ' ')"
  echo "sbt: $(grep sbt.version project/build.properties)"
  echo "jmh-args: ${JMH_ARGS[*]}"
  echo "jmh-benchmark-methods: $N_BENCH"
  echo "started: $(date -u +%FT%TZ)"
} > "$OUT/meta.txt"

# --- run ---
vmstat -t 5 > "$OUT/vmstat.log" 2>&1 &
VMSTAT_PID=$!

FAILED=0
for g in "${GROUPS_ORDER[@]}"; do
  log "group $g starting"
  T0=$SECONDS
  java -cp "$CP" org.openjdk.jmh.Main "${JMH_ARGS[@]}" \
    -rf json -rff "$OUT/$g.json" -o "$OUT/$g.log" "${GROUP_REGEX[$g]}"
  RC=$?
  DUR=$((SECONDS - T0))
  echo "group-$g: exit=$RC duration=${DUR}s results=$(grep -c '"benchmark"' "$OUT/$g.json" 2>/dev/null || echo 0)" >> "$OUT/meta.txt"
  log "group $g done: exit=$RC in ${DUR}s"
  [[ "$RC" == 0 ]] || FAILED=1
done

STATUS=$FAILED
