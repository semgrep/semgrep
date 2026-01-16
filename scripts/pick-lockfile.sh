#!/usr/bin/env bash

set -xeu


# parse flags
if [ "${1:-}" = "--strict" ]; then
  strict=1
  shift
fi

opam_file=$1

os="$(uname -s | tr '[:upper:]' '[:lower:]')"
arch="$(uname -m | tr '[:upper:]' '[:lower:]')"

case "$os" in
   "darwin") os="mac";;
   msys*|mingw*|cygwin*) os="windows";;
   "linux") os="linux";;
esac

case "$arch" in
    arm64|aarch64) arch="arm64" ;;
    x86_64|amd64)  arch="x86" ;;
esac

# patch docker x86 to amd64
if [ "$os-$arch" = "linux-x86" ]; then
    arch="amd64"
fi

if cp "./opam-lockfiles/$opam_file.$os-$arch.locked" "./$opam_file.locked" ; then
  exit 0
else
  echo "lockfile for $os-$arch not supported"
   if [ "$strict" -eq 1 ]; then
     exit 1
   else
     exit 0
   fi
fi
