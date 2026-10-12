# inacawo-src

COAWST model source and build helpers for InaCAWO hindcast (`MODEL_BASE`).

Companion: `$HOME/inacawo-iht` (workflow), `$HOME/inacawo-deps` (conda + `COAWST_ENV`).

## Layout

```
inacawo-src/
  inacawo-coawst/                 # single COAWST tree (WRF–ROMS–SWAN–WW3)
    Projects/CAWO_3way/
      cawo_3way.h.ramp_tides      # day-1 (defines RAMP_TIDES)
      cawo_3way.h.no_ramp_tides   # continuing days (no RAMP_TIDES)
      cawo_3way.h                 # active header during build (gitignored)
  build_coawstM.bash
  cawo_3way_swell_mods_ramp_tides/coawstM      # install artifact (gitignored)
  cawo_3way_swell_mods_no_ramp_tides/coawstM   # install artifact (gitignored)
```

One source tree; two headers. Day-1 vs continuing differs only by `# define RAMP_TIDES`.

## Build

```bash
cd $HOME/inacawo-src
./build_coawstM.bash --variant both -j 16
# or: --variant ramp | --variant no_ramp
```

Requires Intel OneAPI via `$COAWST_ENV` and compiled deps under `APPS_BASE`
(`…/cawo_depends/intel_2021.5.1/build`). If `$HOME/apps` is missing, the
script falls back to `$HOME/rnd/coawst_mdcopn/apps` or the shared
`cawohdcst_ft` apps tree when present. See `inacawo-deps/README.md` to
install deps into `$HOME/apps`.

Logs: `logs/build_<variant>_*.log` (gitignored).

## Hindcast use

`inacawo-iht` copies:

| Stage | Binary |
|-------|--------|
| day1 (`copy_files_for_run_day1.bash`) | `$MODEL_BASE/cawo_3way_swell_mods_ramp_tides/coawstM` |
| continuing (`copy_files_for_run.bash`) | `$MODEL_BASE/cawo_3way_swell_mods_no_ramp_tides/coawstM` |

`$MODEL_BASE` defaults to `$HOME/inacawo-src`.

## Git

Track source + canonical headers + `build_coawstM.bash`. Do **not** commit
`coawstM`, `Build/`, or `Projects/**/input|output`. Prefer working branch
`dev` for model-source updates.
