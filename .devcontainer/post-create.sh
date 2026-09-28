#!/usr/bin/env bash
# Runs once after the dev container is created.
set -euo pipefail

echo "==> Configuring ccache"
export PATH="/usr/lib/ccache:${PATH}"
ccache --max-size=5G || true

echo "==> Dev container ready."
echo "    Build with:   bash .devcontainer/build.sh"
echo "    Or use the CMake Tools extension (preset paths already configured)."
