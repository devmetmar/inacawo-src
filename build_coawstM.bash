#!/bin/bash
# Build coawstM for ramp and/or no-ramp tide variants from inacawo-coawst.
#
# Usage:
#   ./build_coawstM.bash --variant ramp|no_ramp|both [-j N]
#
# Installs:
#   $MODEL_BASE/cawo_3way_swell_mods_ramp_tides/coawstM
#   $MODEL_BASE/cawo_3way_swell_mods_no_ramp_tides/coawstM
#
# Requires: inacawo-iht setup_env + COAWST_ENV (Intel OneAPI / libs).
#
set -euo pipefail

_SRC_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
_COAWST="${_SRC_ROOT}/inacawo-coawst"
_HEADER_DIR="${_COAWST}/Projects/CAWO_3way"
_LOG_DIR="${_SRC_ROOT}/logs"

VARIANT=""
JOBS=16

usage() {
  cat <<'EOF'
Usage: build_coawstM.bash --variant ramp|no_ramp|both [-j N]

  --variant   ramp (day1 / RAMP_TIDES on), no_ramp (continuing), or both
  -j N        parallel make jobs for coawst.bash (default: 16)
  -h, --help  show this help

Headers (canonical, tracked in git):
  Projects/CAWO_3way/cawo_3way.h.ramp_tides
  Projects/CAWO_3way/cawo_3way.h.no_ramp_tides

Active header during build is copied to cawo_3way.h (gitignored).
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --variant) VARIANT="${2:-}"; shift 2 ;;
    -j) JOBS="${2:-}"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *)
      echo "ERROR: unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

case "${VARIANT}" in
  ramp|no_ramp|both) ;;
  *)
    echo "ERROR: --variant ramp|no_ramp|both is required" >&2
    usage >&2
    exit 2
    ;;
esac

if [[ ! -d "${_COAWST}" ]]; then
  echo "ERROR: missing ${_COAWST}" >&2
  exit 1
fi

# Env: paths + Intel/COAWST toolchain
export IHT_QUIET=1
if [[ -f "${HOME}/inacawo-iht/setup_env.bash" ]]; then
  # shellcheck disable=SC1091
  source "${HOME}/inacawo-iht/setup_env.bash"
elif [[ -f "${_SRC_ROOT}/../inacawo-iht/setup_env.bash" ]]; then
  # shellcheck disable=SC1091
  source "${_SRC_ROOT}/../inacawo-iht/setup_env.bash"
else
  echo "ERROR: cannot find inacawo-iht/setup_env.bash" >&2
  exit 1
fi

# Resolve compiled NetCDF/HDF5/MCT deps if $HOME/apps is not installed yet
if [[ ! -e "${LIBDEP:-}/libnetcdff.so" ]]; then
  for _apps in \
    "${APPS_BASE:-}" \
    "${HOME}/rnd/coawst_mdcopn/apps" \
    "/scratch/cawohdcst_ft/home/cawohdcst/apps" \
    "${HOME}/apps"
  do
    [[ -z "${_apps}" ]] && continue
    if [[ -e "${_apps}/cawo_depends/intel_2021.5.1/build/lib/libnetcdff.so" ]]; then
      export APPS_BASE="${_apps}"
      export LIBDEP="${APPS_BASE}/cawo_depends/intel_2021.5.1/build/lib"
      echo "[build] using APPS_BASE=${APPS_BASE}"
      break
    fi
  done
fi
if [[ ! -e "${LIBDEP:-}/libnetcdff.so" ]]; then
  echo "ERROR: LIBDEP missing libnetcdff.so (${LIBDEP:-})" >&2
  echo "       install COAWST deps into \$HOME/apps (see inacawo-deps/README.md)" >&2
  echo "       or export APPS_BASE=/path/to/apps (with cawo_depends/intel_2021.5.1/build)" >&2
  exit 1
fi

if [[ -z "${COAWST_ENV:-}" || ! -f "${COAWST_ENV}" ]]; then
  echo "ERROR: COAWST_ENV unset or missing (${COAWST_ENV:-})" >&2
  exit 1
fi
# Intel oneAPI setvars.sh references unbound ZSH_VERSION under set -u
set +u
# shellcheck disable=SC1090
source "${COAWST_ENV}"
set -u

MODEL_BASE="${MODEL_BASE:-${_SRC_ROOT}}"
mkdir -p "${_LOG_DIR}"

_build_one() {
  local v="$1"   # ramp | no_ramp
  local hdr install_dir expect_ramp
  case "${v}" in
    ramp)
      hdr="${_HEADER_DIR}/cawo_3way.h.ramp_tides"
      install_dir="${MODEL_BASE}/cawo_3way_swell_mods_ramp_tides"
      expect_ramp=1
      ;;
    no_ramp)
      hdr="${_HEADER_DIR}/cawo_3way.h.no_ramp_tides"
      install_dir="${MODEL_BASE}/cawo_3way_swell_mods_no_ramp_tides"
      expect_ramp=0
      ;;
    *)
      echo "ERROR: internal variant ${v}" >&2
      return 1
      ;;
  esac

  if [[ ! -f "${hdr}" ]]; then
    echo "ERROR: missing canonical header ${hdr}" >&2
    return 1
  fi

  local has_ramp=0
  if grep -qE '^[[:space:]]*#[[:space:]]*define[[:space:]]+RAMP_TIDES' "${hdr}"; then
    has_ramp=1
  fi
  if [[ "${expect_ramp}" -eq 1 && "${has_ramp}" -ne 1 ]]; then
    echo "ERROR: ${hdr} should define RAMP_TIDES" >&2
    return 1
  fi
  if [[ "${expect_ramp}" -eq 0 && "${has_ramp}" -eq 1 ]]; then
    echo "ERROR: ${hdr} must NOT define RAMP_TIDES" >&2
    return 1
  fi

  echo "=============================================="
  echo "[build] variant=${v}  jobs=${JOBS}"
  echo "[build] header=${hdr}"
  echo "[build] install=${install_dir}/coawstM"
  echo "=============================================="

  cp -f "${hdr}" "${_HEADER_DIR}/cawo_3way.h"

  local log="${_LOG_DIR}/build_${v}_$(date +%Y%m%d_%H%M%S).log"
  echo "[build] log → ${log}"

  (
    cd "${_COAWST}"
    ./coawst.bash -j "${JOBS}"
  ) > >(tee "${log}") 2>&1

  if [[ ! -x "${_COAWST}/coawstM" ]]; then
    echo "ERROR: ${_COAWST}/coawstM not produced (see ${log})" >&2
    return 1
  fi

  mkdir -p "${install_dir}"
  cp -f "${_COAWST}/coawstM" "${install_dir}/coawstM"
  chmod +x "${install_dir}/coawstM"
  ls -lh "${install_dir}/coawstM"
  echo "[build] ${v} OK"
}

case "${VARIANT}" in
  both)
    _build_one ramp
    _build_one no_ramp
    ;;
  *)
    _build_one "${VARIANT}"
    ;;
esac

echo "[build] done"
