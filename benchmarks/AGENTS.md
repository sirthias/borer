# benchmarks — working with the JMH benchmark setup

Notes for agents (and humans) on running borer's JMH benchmarks, locally and on a remote compute box,
and on producing results that can be stored in `benchmarks/results/` and correlated with a commit.

## What's in the module

- `src/main/scala/io/bullet/borer/benchmarks/`
  - `AbstractBenchmarks.scala` — abstract bases `EncodingBenchmark`, `DecodingBenchmark`, `DomBenchmark`.
    **The fork JVM args live here** in `@Fork(jvmArgs = ...)` (2 GB fixed heap, ParallelGC, `AlwaysPreTouch`,
    `--add-opens java.base/java.lang` for Jackson Afterburner). `DomBenchmark` carries the `fileName` `@Param`
    over the 18 JSON files in `src/main/resources/`.
  - One file per library: `BorerBenchmarks`, `CirceDefinitions`, `JacksonDefinitions`,
    `JsoniterScalaDefinitions`, `UPickleDefinitions`, each with `*EncodingBenchmark`, `*DecodingBenchmark`,
    `*DomBenchmark` (no Dom for jsoniter-scala) and `*ModelBenchmark` classes. `Model.scala` holds the case
    class models for the 18 files.
- `results/` — stored JMH JSON result files (`2019-10-18_v1.1.0.json` is the old GraalVM EE / JDK 8 baseline;
  `benchmark-results.afdesign` is a design file for result charts).
- `project/src/main/scala/` (build-level): `BenchmarkResultsPlugin` (adds the `benchmarkResults` command),
  `Analysis` (computes Borer-vs-X factors), `JmhJsonModel` (borer codecs for JMH's JSON output).

### Size of the benchmark matrix

50 benchmark methods; 18 of them (all `*Dom*` / `*Model*` methods) run once per JSON file:
18 × 18 + 32 = **356 benchmark runs** per full run. Wall time ≈ 356 × (warmup + measurement + ~0.5 s fork
overhead). Measured fork overhead is ~0.5 s per run on a 2-vCPU EPYC box.

| JMH settings | per run | full run |
|---|---|---|
| `-f 1 -wi 0 -i 1 -r 100ms` (smoke test) | ~0.6 s | ~4 min |
| `-f 1 -wi 5 -w 1s -i 5 -r 2s` (fits a 2 h budget) | ~15.5 s | ~92 min |
| `-f 2 -wi 10 -i 10` @ 10 s (scaladoc suggestion / 2019 style) | ~400 s | ~40 h |

Use regexes to restrict; groups that match exactly one family (no overlap, since `*ModelBenchmark` classes
extend `DomBenchmark` but aren't named like it):

```
io\.bullet\.borer\.benchmarks\.[A-Za-z]+ModelBenchmark\.     # 180 runs — the only ones `benchmarkResults` reads
io\.bullet\.borer\.benchmarks\.[A-Za-z]+DomBenchmark\.       # 144 runs
io\.bullet\.borer\.benchmarks\.[A-Za-z]+DecodingBenchmark\.  # 16 runs
io\.bullet\.borer\.benchmarks\.[A-Za-z]+EncodingBenchmark\.  # 16 runs
```

Single file: add `-p fileName=bitcoin.json`. List everything: `-l` (or `-lp` to include params).

## sbt 2 gotchas

The build uses **sbt 2.1.0**, whose launcher runs as a **thin client by default**:

- A background sbt server (~3 GB RSS) **keeps running after the command**. Kill it with `sbt shutdown`, or avoid
  it entirely with `sbt --server --no-server -batch ...` (in-process, exits when done). Never leave a server
  running on a benchmark box.
- In client mode, separate command-line args are joined, so `sbt "project benchmarks" benchmarkResults` fails
  (`Expected project ID`). Use one `;`-separated arg: `sbt "project benchmarks;benchmarkResults;project /"`.
- `export benchmarks/Jmh/fullClasspath` prints **virtual file refs**, not paths, e.g.
  `${OUT}/jvm/scala-3.9.0/benchmarks/benchmarks_3-jmh.jar>sha256-…/3449542`, `${CSR_CACHE}/https/…`,
  `${SBT_BOOT}/scala-3.9.0/lib/scala-library.jar` (still true with `--no-server`). Resolve as
  `${OUT}` → `<repo>/target/out`, `${CSR_CACHE}` → `~/.cache/coursier/v1`, `${SBT_BOOT}` → `~/.sbt/boot`, strip
  everything from `>` on, and check that every entry exists.

## Running benchmarks

Quick local check (fine on a laptop, not for real numbers):

```bash
sbt "benchmarks/Jmh/run -f 1 -wi 1 -w 1s -i 1 -r 1s -p fileName=bitcoin.json BorerModelBenchmark"
sbt shutdown
```

For real runs, **build with sbt, then run JMH with plain `java`** so no sbt JVM is around during measurement:

```bash
sbt --server --no-server -batch "benchmarks/Jmh/compile" "export benchmarks/Jmh/fullClasspath" > build.log 2>&1
CP=$(sed 's/\x1b\[[0-9;]*[A-Za-z]//g' build.log | grep '^List(' | tail -1 \
  | sed 's/^List(//; s/)$//' | tr ',' '\n' | sed 's/^ *//; s/>.*//' \
  | sed "s#\${OUT}#$PWD/target/out#; s#\${CSR_CACHE}#$HOME/.cache/coursier/v1#; s#\${SBT_BOOT}#$HOME/.sbt/boot#" \
  | paste -sd:)
java -cp "$CP" org.openjdk.jmh.Main -l | grep -c '^io\.bullet'     # expect 50
java -cp "$CP" org.openjdk.jmh.Main -f 1 -t 1 -wi 5 -w 1s -i 5 -r 2s \
  -rf json -rff model.json -o model.log 'io\.bullet\.borer\.benchmarks\.[A-Za-z]+ModelBenchmark\.'
```

- **JMH writes the `-rf json` file only when the whole invocation finishes.** If a run is killed, all of its
  results are lost. Split long runs into several invocations (e.g. the four groups above, Model first) with
  separate JSON files, so a time cap or crash only costs the group that was running.
- JMH on JDK 26 prints `sun.misc.Unsafe::objectFieldOffset` deprecation warnings. They're harmless.
- `-XX:-UseBiasedLocking` (used in 2019) no longer exists on JDK 18+. Don't add it back.

## Using a remote compute box

A laptop is too noisy for baselines. Use a dedicated remote box (access details for the current one are in
`../temp-compute/AGENTS.md`: Hetzner, Debian 13, 2 vCPU EPYC-Milan = **1 physical core with SMT**, KVM,
7.6 GiB RAM, Java 26 via SDKMAN, SSH only, on a non-standard port).

### SSH hygiene

The box rate-limits new SSH connections (20/min). Use one multiplexed connection for everything:

```bash
SSH_OPTS='-p 28272 -o ControlMaster=auto -o ControlPath=~/.ssh/cm-%r@%h:%p -o ControlPersist=10m'
ssh $SSH_OPTS root@<host> 'uptime'
rsync -az -e "ssh $SSH_OPTS" root@<host>:/root/bench/results/ ./remote-results/
```

(`scp` takes `-P`, not `-p`, for the port.) `rsync` is installed on the local VM.

### One-time box setup

```bash
ssh $SSH_OPTS root@<host> 'sdk install sbt 2.1.0'                 # Java is preinstalled, sbt is not
ssh $SSH_OPTS root@<host> 'git init -q --bare /root/bench/borer.git'
```

### Getting code onto the box: push via SSH

The box **can't pull from `origin`**: it has no key the private git server accepts, agent forwarding is
disabled, and the server may not be publicly reachable. Instead, push from the local checkout to the bare
repo on the box over the SSH connection we already have:

```bash
git push ssh://root@<host>:28272/root/bench/borer.git HEAD:refs/heads/master
# then on the box: git clone -q /root/bench/borer.git /root/bench/src/<run-id> && git checkout --detach <sha>
```

(Fallback: `git bundle create borer.bundle HEAD master` + copy + `git clone borer.bundle`.) Always build
from a **fresh clone of an exact, committed sha**, never from a copied working tree. That's what makes results
attributable to a commit.

### Running detached

Run the whole thing (clone, build, JMH groups) as a **transient systemd unit**. It survives SSH disconnects
and laptop sleep, logs to the journal, and `RuntimeMaxSec` enforces a hard time cap:

```bash
ssh $SSH_OPTS root@<host> \
  'systemd-run --unit=borer-bench -p RuntimeMaxSec=7200 /root/bench/run.sh <full-sha>'
ssh $SSH_OPTS root@<host> 'systemctl status borer-bench; tail /root/bench/results/latest/run.log'
ssh $SSH_OPTS root@<host> 'journalctl -u borer-bench -f'
```

There's no tmux/screen on the box, and `nohup` is weaker than a systemd unit. A systemd unit's environment
doesn't source `.bashrc`, so the script must set `HOME=/root`, `JAVA_HOME=/root/.sdkman/candidates/java/current`
and `PATH` (incl. `/root/.sdkman/candidates/sbt/current/bin`) itself. Use a different `--unit` name for
smoke runs, and `systemctl reset-failed` if a unit name is "already loaded".

The script is [`remote/run.sh`](remote/run.sh). Copy it to `/root/bench/run.sh` on the box
(`rsync -az -e "ssh $SSH_OPTS" benchmarks/remote/run.sh root@<host>:/root/bench/`). By default it clones from
`/root/bench/borer.git`; set `REPO=` (e.g. `--setenv=REPO=/root/bench/borer.bundle`) to use a bundle instead.
Its JMH settings are in `JMH_ARGS` at the top. What it does, in order (keep this list in sync when changing it):

1. `set -uo pipefail`, `tee` all output to `<out>/run.log`, and set an `EXIT` trap that restores the box, writes
   `exit-code`, touches `DONE` on success and appends `finished:` to `meta.txt`. Also
   `trap 'exit 143' TERM`, so the trap runs when the time cap hits.
2. Quiet the box: `systemctl stop unattended-upgrades apt-daily.timer apt-daily-upgrade.timer` (and the
   `.service`s), wait while `pgrep -x 'apt|apt-get|dpkg|unattended-upgr'`. Restart them in the EXIT trap.
   (`fuser` isn't installed.)
3. Fresh clone + `git checkout --detach <sha>`, then build with `sbt --server --no-server -batch …` as above
   (~2 min on the box with a warm coursier cache), resolve the classpath, and check that no sbt JVM is left.
4. Write `meta.txt`: run id, full commit sha, commit subject/date, `git describe`, `version.sbt`, host/kernel,
   `lscpu` model / nproc / threads-per-core / hypervisor, memory, `java -version`, sbt version, exact JMH args,
   number of benchmark methods JMH sees, start time.
5. Start `vmstat -t 5 > vmstat.log &` (the `st` column = CPU steal from neighbours, the key noise indicator on
   shared VMs). Kill it in the trap.
6. Run the JMH groups in priority order **model → dom → decoding → encoding**, each with its own
   `-rf json -rff <group>.json -o <group>.log`. Record `group-<g>: exit=… duration=… results=<count>` in
   `meta.txt` and carry on with the next group if one fails.
7. `SMOKE=1` (`--setenv=SMOKE=1`) switches to Borer classes only with `-wi 0 -i 1 -r 100ms` and suffixes the
   run id with `-smoke`. **Always do a smoke run before a long run**. It checks clone, build, classpath, all groups and cleanup in ~4 min.

Output layout: `/root/bench/results/<YYYY-MM-DDTHHMMZ>_<sha7>/` with `meta.txt`, `run.log`, `build.log`,
`classpath.txt`, `vmstat.log`, `{model,dom,decoding,encoding}.{json,log}`, `exit-code`, `DONE`, plus a
`/root/bench/results/latest` symlink.

## Storing results in `benchmarks/results/`

Pull the run directory back with `rsync`, then store **one merged JMH JSON array plus a metadata sidecar**:

```bash
RUN=2026-11-17T0932Z_316abd8; NAME=2026-11-17_316abd8
python3 -c 'import json,sys; print(json.dumps([e for f in sys.argv[1:] for e in json.load(open(f))], indent=2))' \
  $RUN/{model,dom,decoding,encoding}.json > benchmarks/results/$NAME.json
cp $RUN/meta.txt benchmarks/results/$NAME.meta.txt
```

- Name files `YYYY-MM-DD_<sha7>.json` (or `…_v<version>.json` for releases). The date prefix matters:
  `benchmarkResults` without an argument picks the file that sorts last by name. The sidecar **must not end
  in `.json`**: `benchmarkResults` treats `*.json` in `results/` as JMH result lists and would fail on it.
- JMH's JSON has no commit info. The sidecar `meta.txt` (full sha, JVM, box, JMH args) is what ties results
  to the project state. Don't drop it.
- `JmhJsonModel` decodes non-finite doubles rendered as strings (`"NaN"` from single-iteration runs), so
  smoke-run output parses too.

### `benchmarkResults`

```bash
sbt "project benchmarks;benchmarkResults;project /"                     # newest result file only
sbt "project benchmarks;benchmarkResults 2026-11-17_316abd8;project /"  # one file (`.json` optional)
sbt "project benchmarks;benchmarkResults 2026-11-*;project /"          # glob over file names
```

- It logs which files it analyses and fails with the list of available files if the pattern matches nothing.
  Patterns match file names only (no paths) and support tab completion in the sbt shell.
- Only `*ModelBenchmark` entries are evaluated, as Borer-vs-X factors per library over the 18 JSON files.
- If a glob selects several files, their results are **combined**. For duplicate benchmark/param keys the file
  sorting last by name wins, so only select several files if they hold disjoint parts of one run (or if
  that's really what you want).
- If a library has no results in the selected files, it's skipped. A smoke-run file with only some
  benchmarks yields factors over fewer than 18 files.

## Interpreting numbers

- Results are only comparable **within the same box type, JVM and JMH settings**. The 2019 baseline used
  GraalVM EE 19.2 / JDK 8, 4 threads, 4 forks, 10×10 s iterations, so absolute numbers can't be compared
  with today's runs.
- The current box is temporary, and its 2 vCPUs are SMT siblings on a shared KVM host. To judge a core
  change, run the **baseline commit and the changed commit back-to-back on the same box** (A/B) rather than
  comparing against old stored numbers. Check `vmstat.log` steal time and JMH `scoreError` before trusting
  small differences.
- `-t 1` (single benchmark thread) leaves the second vCPU for JIT/GC. Don't raise `-t` on a 2-vCPU box.
