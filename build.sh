#!/usr/bin/env bash

set -eu
set -o pipefail

owd="$(pwd)"
trap "cd \"$owd\"" EXIT

cd "$(dirname "$0")"
dotnet run --verbosity quiet --project ./build/build.fsproj -- -t "$@"
