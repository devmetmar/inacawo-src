#!/bin/bash
# Compile the InaCAWO COAWST (WRF–ROMS–SWAN) executable coawstM in two variants:
#   ramp_tides     day 1 of a hindcast       (headers/cawo_3way_ramp_tides.h,    # define RAMP_TIDES)
#   no_ramp_tides  continuing days           (headers/cawo_3way_no_ramp_tides.h, # undef RAMP_TIDES)
#
# One source tree per variant (./coawst_<variant>, extracted from the scratch
# tarball): coawst.bash re-runs the WRF registry on every build, so sharing a
# tree does not save the WRF recompile. Results:
#   binaries/ramp_tides/coawstM      binaries/no_ramp_tides/coawstM
#
# Usage:
#   bash build_coawst.bash --sbatch                    # recommended: one job per variant
#                                                      # (1 node / 96 cores each, run in parallel)
#                                                      # partition: HDCAST if account allowed, else DEV1
#   bash build_coawst.bash --sbatch --variant no_ramp_tides
#   bash build_coawst.bash [-j N] [--variant ...]      # run in the current shell / job (sequential)
#
# Requires: inacawo-deps (env + COAWST model deps in $APPS_BASE, COAWST_ENV).
#
set -euo pipefail

SRC_ROOT="${INACAWO_SRC_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)}"
SELF="${SRC_ROOT}/build_coawst.bash"
HEADERS_DIR="${SRC_ROOT}/headers"
INACAWO_BIN_DIR="${SRC_ROOT}/binaries"
LOG_DIR="${SRC_ROOT}/logs"
TARBALL="${COAWST_SRC_TARBALL:-/scratch/cawohdcst_ft2/inacawo-iht/src_tarballs/inacawo_coawst_src.tar.gz}"
DEPS_ENV="${DEPS_BASE:-${HOME}/inacawo-deps}/env"

VARIANT="all"
JOBS="${SLURM_NTASKS:-96}"
SBATCH=0
FORCE_EXTRACT=0
PARTITION_ARG=""   # empty → auto (HDCAST if allowed, else DEV1)
TIME_LIMIT="01:00:00"
PREFERRED_PARTITION="${IHT_SLURM_PARTITION_PREFERRED:-HDCAST}"
FALLBACK_PARTITION="${IHT_SLURM_PARTITION_FALLBACK:-DEV1}"

usage() {
  cat <<EOF
Usage: $0 [options]

Options:
  --variant V        ramp_tides | no_ramp_tides | all   (default: all)
  -j N               parallel make jobs (default: \$SLURM_NTASKS or 96)
  --sbatch           submit one SLURM job per variant (1 node, 96 tasks, exclusive each)
  --partition P      SLURM partition for --sbatch
                     (default: auto — ${PREFERRED_PARTITION} if account allowed, else ${FALLBACK_PARTITION})
  --time T           SLURM time limit per job for --sbatch (default: ${TIME_LIMIT})
  --tarball PATH     COAWST source tarball (default: ${TARBALL})
  --force-extract    remove ./coawst_<variant> and extract the tarball again
  -h, --help         show this help

Outputs:
  ${INACAWO_BIN_DIR}/ramp_tides/coawstM      (+ build_info.txt)
  ${INACAWO_BIN_DIR}/no_ramp_tides/coawstM   (+ build_info.txt)
  ${LOG_DIR}/build_<variant>_<timestamp>.log
EOF
}

ARGS=("$@")
while [[ $# -gt 0 ]]; do
  case "$1" in
    --variant)   VARIANT="${2:?--variant needs a value}"; shift ;;
    -j)          JOBS="${2:?-j needs a value}"; shift ;;
    --sbatch)    SBATCH=1 ;;
    --partition) PARTITION_ARG="${2:?--partition needs a value}"; shift ;;
    --time)      TIME_LIMIT="${2:?--time needs a value}"; shift ;;
    --tarball)   TARBALL="${2:?--tarball needs a path}"; shift ;;
    --force-extract) FORCE_EXTRACT=1 ;;
    -h|--help)   usage; exit 0 ;;
    *) echo "ERROR: unknown argument: $1" >&2; usage >&2; exit 2 ;;
  esac
  shift
done

case "${VARIANT}" in
  all) VARIANTS=(ramp_tides no_ramp_tides) ;;
  ramp_tides|no_ramp_tides) VARIANTS=("${VARIANT}") ;;
  *) echo "ERROR: --variant must be ramp_tides, no_ramp_tides or all" >&2; exit 2 ;;
esac

now_s() { date +%s; }
fmt_elapsed() { printf '%dm%02ds' "$(($1 / 60))" "$(($1 % 60))"; }

# ---- Slurm partition: HDCAST if account allowed, else DEV1 (avoids PENDING PartitionConfig) ----
_src_user_slurm_accounts() {
  if command -v sacctmgr >/dev/null 2>&1; then
    sacctmgr -n show associations where user="${USER}" format=Account -P 2>/dev/null \
      | awk -F'|' 'NF && $1 != "" {print $1}' | sort -u
  fi
}

_src_partition_allow_accounts() {
  local part="$1" line
  line="$(scontrol show partition "${part}" -o 2>/dev/null || true)"
  [[ -n "${line}" ]] || return 1
  if [[ "${line}" =~ AllowAccounts=([^ ]+) ]]; then
    echo "${BASH_REMATCH[1]}"
    return 0
  fi
  echo "ALL"
}

_src_account_allowed_on_partition() {
  local part="$1" allow a uacc
  allow="$(_src_partition_allow_accounts "${part}" || true)"
  [[ -n "${allow}" ]] || return 1
  [[ "${allow}" == "ALL" ]] && return 0
  while IFS= read -r uacc; do
    [[ -z "${uacc}" ]] && continue
    IFS=',' read -r -a _allowed <<< "${allow}"
    for a in "${_allowed[@]}"; do
      [[ "${uacc}" == "${a}" ]] && return 0
    done
  done < <(_src_user_slurm_accounts)
  return 1
}

# Sets PARTITION + _PARTITION_SOURCE (must not run in a subshell).
_src_resolve_slurm_partition() {
  if [[ -n "${PARTITION_ARG}" ]]; then
    _PARTITION_SOURCE="cli"
    PARTITION="${PARTITION_ARG}"
  elif [[ -n "${IHT_SLURM_PARTITION:-}" ]]; then
    _PARTITION_SOURCE="env"
    PARTITION="${IHT_SLURM_PARTITION}"
  else
    _PARTITION_SOURCE="auto"
    if command -v scontrol >/dev/null 2>&1 \
       && _src_account_allowed_on_partition "${PREFERRED_PARTITION}"; then
      PARTITION="${PREFERRED_PARTITION}"
    else
      PARTITION="${FALLBACK_PARTITION}"
    fi
  fi
  export IHT_SLURM_PARTITION="${PARTITION}"
  export SBATCH_PARTITION="${PARTITION}"
}

mkdir -p "${LOG_DIR}"

# ---- submit to SLURM: one job per variant, same resources as slurm_compile_cawo.bash ----
# make cannot spread over nodes, so extra nodes only help by building variants in parallel.
if [[ "${SBATCH}" -eq 1 ]]; then
  if ! command -v sbatch >/dev/null 2>&1; then
    echo "ERROR: sbatch not found" >&2
    exit 1
  fi
  _src_resolve_slurm_partition
  case "${_PARTITION_SOURCE}" in
    cli) echo "==> partition=${PARTITION} (--partition)" ;;
    env) echo "==> partition=${PARTITION} (IHT_SLURM_PARTITION)" ;;
    *)
      if [[ "${PARTITION}" == "${FALLBACK_PARTITION}" ]]; then
        echo "==> partition=${PARTITION} (auto: no Slurm account on ${PREFERRED_PARTITION} → fallback)"
      else
        echo "==> partition=${PARTITION} (auto: account allowed on ${PREFERRED_PARTITION})"
      fi
      ;;
  esac
  pass=()
  skip=0
  for a in "${ARGS[@]}"; do
    if [[ "${skip}" -eq 1 ]]; then skip=0; continue; fi
    case "${a}" in
      --sbatch) ;;
      --variant|--partition) skip=1 ;;
      *) pass+=("${a}") ;;
    esac
  done
  export INACAWO_SRC_ROOT="${SRC_ROOT}"
  for v in "${VARIANTS[@]}"; do
    jobid="$(sbatch --parsable \
      --partition="${PARTITION}" \
      --job-name="compile_cawo_${v}" \
      --nodes=1 \
      --ntasks-per-node=96 \
      --exclusive \
      --time="${TIME_LIMIT}" \
      --export=ALL \
      --chdir="${SRC_ROOT}" \
      --output="${LOG_DIR}/compile_cawo_${v}_%j.out" \
      "${SELF}" --variant "${v}" "${pass[@]}")"
    jobid="${jobid%%;*}"
    echo "==> Submitted compile_cawo_${v} job ${jobid} (${PARTITION}, 1 node x 96 tasks, ${TIME_LIMIT})"
    echo "    job log: ${LOG_DIR}/compile_cawo_${v}_${jobid}.out"
  done
  echo "    follow : squeue -u ${USER} ; tail -f ${LOG_DIR}/compile_cawo_*.out"
  exit 0
fi

T_START="$(now_s)"
echo "==> InaCAWO COAWST build on $(hostname) ($(date -Iseconds))"
echo "    variants: ${VARIANTS[*]}   make -j ${JOBS}"
if [[ -z "${SLURM_JOB_ID:-}" ]]; then
  echo "    WARNING: not inside a SLURM job; prefer --sbatch for the full build" >&2
fi

# ---- environment: paths + compiled deps + Intel/COAWST toolchain ----
if [[ ! -f "${DEPS_ENV}" ]]; then
  echo "ERROR: ${DEPS_ENV} not found — clone inacawo-deps into \$HOME first" >&2
  exit 1
fi
# shellcheck disable=SC1090
source "${DEPS_ENV}"
if [[ ! -e "${LIBDEP}/libnetcdff.so" ]]; then
  echo "ERROR: COAWST model deps missing (${LIBDEP}/libnetcdff.so)" >&2
  echo "       bash \$HOME/inacawo-deps/install_hindcast_env.bash --only-coawst-deps" >&2
  exit 1
fi
if ! type module >/dev/null 2>&1 && [[ -f /etc/profile.d/modules.sh ]]; then
  # shellcheck disable=SC1091
  source /etc/profile.d/modules.sh
fi
# oneAPI setvars.sh is not clean under `set -eu`
set +eu
# shellcheck disable=SC1090
source "${COAWST_ENV}" >/dev/null
set -eu
if [[ -z "${NC_CONFIG:-}" ]] || ! command -v mpiifort >/dev/null 2>&1; then
  echo "ERROR: COAWST_ENV did not provide NC_CONFIG / Intel MPI (${COAWST_ENV})" >&2
  exit 1
fi
DEPS_PREFIX="${LIBDEP%/lib}"
# WRF's ./compile honours J; its make is not reliable with very wide parallelism.
export J="-j $(( JOBS < 20 ? JOBS : 20 ))"

# ---- source tree (one per variant) ----
extract_source() {
  local tree="$1" tar_dir sums sha="unverified"
  if [[ ! -r "${TARBALL}" ]]; then
    echo "ERROR: source tarball missing or unreadable: ${TARBALL}" >&2
    exit 1
  fi
  tar_dir="$(cd "$(dirname "${TARBALL}")" && pwd)"
  sums="${tar_dir}/SHA256SUMS"
  if [[ -r "${sums}" ]] && grep -q " $(basename "${TARBALL}")\$" "${sums}"; then
    echo "    verifying sha256"
    (cd "${tar_dir}" && grep " $(basename "${TARBALL}")\$" SHA256SUMS | sha256sum -c --status) || {
      echo "ERROR: sha256 mismatch for ${TARBALL}" >&2
      exit 1
    }
    sha="$(grep " $(basename "${TARBALL}")\$" "${sums}" | awk '{print $1}')"
  fi
  rm -rf "${tree}"
  echo "    extracting ${TARBALL} -> ${tree}"
  tar -xzf "${TARBALL}" -C "${SRC_ROOT}" --no-same-owner \
    --transform "s,^coawst$,$(basename "${tree}")," \
    --transform "s,^coawst/,$(basename "${tree}")/,"
  sed -i "s|@INACAWO_DEPS_PREFIX@|${DEPS_PREFIX}|g" "${tree}/WRF/configure.wrf"
  printf 'tarball: %s\nsha256: %s\nextracted: %s\ndeps_prefix: %s\n' \
    "${TARBALL}" "${sha}" "$(date -Iseconds)" "${DEPS_PREFIX}" > "${tree}/.inacawo_src_tarball"
}

prepare_tree() {
  local tree="$1"
  if [[ "${FORCE_EXTRACT}" -eq 1 || ! -f "${tree}/coawst.bash" ]]; then
    echo "==> Preparing COAWST source in ${tree}"
    extract_source "${tree}"
  else
    echo "==> Using existing COAWST source ${tree} (--force-extract to refresh)"
  fi
  if grep -q "@INACAWO_DEPS_PREFIX@" "${tree}/WRF/configure.wrf"; then
    sed -i "s|@INACAWO_DEPS_PREFIX@|${DEPS_PREFIX}|g" "${tree}/WRF/configure.wrf"
  fi
}

# ---- build one variant ----
build_variant() {
  local v="$1" tree hdr expect n_ramp log t0 dest
  tree="${SRC_ROOT}/coawst_${v}"
  hdr="${HEADERS_DIR}/cawo_3way_${v}.h"
  log="${LOG_DIR}/build_${v}_$(date +%Y%m%d_%H%M%S).log"
  dest="${INACAWO_BIN_DIR}/${v}"
  if [[ "${v}" == "ramp_tides" ]]; then expect=1; else expect=0; fi

  echo "==> [${v}] header ${hdr}"
  if [[ ! -f "${hdr}" ]]; then
    echo "ERROR: missing ${hdr}" >&2
    return 1
  fi
  if grep -qE '^[[:space:]]*#[[:space:]]*define[[:space:]]+RAMP_TIDES' "${hdr}"; then
    [[ "${expect}" -eq 1 ]] || { echo "ERROR: ${hdr} must not define RAMP_TIDES" >&2; return 1; }
  else
    [[ "${expect}" -eq 0 ]] || { echo "ERROR: ${hdr} must define RAMP_TIDES" >&2; return 1; }
  fi

  prepare_tree "${tree}"
  export COAWST_SRC_DIR="${tree}"
  mkdir -p "${tree}/Projects/CAWO_3way"
  cp "${hdr}" "${tree}/Projects/CAWO_3way/cawo_3way.h"
  rm -f "${tree}/coawstM"

  echo "    compiling (log: ${log})"
  t0="$(now_s)"
  if ! (cd "${tree}" && ./coawst.bash -j "${JOBS}") > "${log}" 2>&1 \
     || [[ ! -x "${tree}/coawstM" ]]; then
    echo "ERROR: [${v}] build failed — last lines of ${log}:" >&2
    tail -n 30 "${log}" >&2
    return 1
  fi

  n_ramp="$(strings -a "${tree}/coawstM" | grep -c 'RAMP_TIDES' || true)"
  if [[ "${expect}" -eq 1 && "${n_ramp}" -eq 0 ]] || [[ "${expect}" -eq 0 && "${n_ramp}" -ne 0 ]]; then
    echo "ERROR: [${v}] coawstM RAMP_TIDES check failed (found ${n_ramp})" >&2
    return 1
  fi

  mkdir -p "${dest}"
  cp -p "${tree}/coawstM" "${dest}/coawstM"
  {
    echo "variant: ${v}"
    echo "built: $(date -Iseconds) by ${USER} on $(hostname) (job ${SLURM_JOB_ID:-none})"
    echo "header: headers/cawo_3way_${v}.h md5 $(md5sum < "${hdr}" | awk '{print $1}')"
    echo "coawstM md5: $(md5sum < "${dest}/coawstM" | awk '{print $1}')"
    echo "RAMP_TIDES strings: ${n_ramp}"
    sed 's/^/source /' "${tree}/.inacawo_src_tarball" 2>/dev/null || true
    echo "source tree: ${tree}"
    echo "log: ${log}"
  } > "${dest}/build_info.txt"
  echo "==> [${v}] OK $(fmt_elapsed "$(( $(now_s) - t0 ))") → ${dest}/coawstM"
}

for v in "${VARIANTS[@]}"; do
  build_variant "${v}"
done

echo
echo "==> Done ($(fmt_elapsed "$(( $(now_s) - T_START ))"))"
for v in "${VARIANTS[@]}"; do
  ls -la "${INACAWO_BIN_DIR}/${v}/coawstM"
done
