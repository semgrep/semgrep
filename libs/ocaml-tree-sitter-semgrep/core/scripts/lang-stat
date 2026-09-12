#! /usr/bin/env bash
#
# Fetch git projects from a list and run our parser on the files for
# a language of interest.
#
set -eu -o pipefail

progdir=$(dirname "$0")
progname=$(basename "$0")

usage() {
  cat <<EOF
Usage: $progname LANG PROJECTS_FILE EXTENSIONS_FILE

For each project specified by a git URL in PROJECTS_FILE, run the local
parser on all the files of the project whose extension matches one of the
extensions in EXTENSIONS_FILE.

Example: $progname projects.txt extensions.txt
EOF
}

error() {
  echo "Error: $*" >&2
  exit 1
}

### Command-line parsing ###

[[ "${BASH_VERSION%%.*}" -ge 4 ]] || error "requires bash >= 4"

if [[ $# != 3 ]]; then
  usage 2>&1
  exit 1
fi

lang="$1"
projects_file="$2"
extensions_file="$3"

### Functions ###

parse() {
  (
    cd "$proj_workspace"
    json=out.json
    cst=out.cst
    "$parser" "$input" \
      --txt-stat stat.txt \
      --json-error-log "$global_json_err_log" > /dev/null
  ) >> "$proj_err_log" 2>&1
  status=$?
  case "$status" in
    0) ;;
    11) ext_err_count=$(( ext_err_count + 1 )) ;;
    12) int_err_count=$(( int_err_count + 1 )) ;;
    *) other_err_count=$(( other_err_count + 1 ))
  esac
  stat_file="$proj_workspace"/stat.txt

  file_line_count=$(cut -f1 -d' ' "$stat_file")
  file_error_line_count=$(cut -f2 -d' ' "$stat_file")
  file_error_count=$(cut -f3 -d' ' "$stat_file")
  return "$status"
}

# Replace '.foo.bar' by '\.foo\.bar'
escape_ext() {
  sed -e 's/\./\\./g'
}

handle_project() {
  name=$(basename "${url%.git}")
  proj_workspace="$tmp"/"$name"
  mkdir -p "$proj_workspace"
  proj_err_log="$proj_workspace"/parse-error.log
  rm -f "$proj_err_log"
  touch "$proj_err_log"
  (
    cd "$proj_workspace"
    rm -f inputs
    if [[ ! -d "$name" ]]; then
      echo "Clone '$name' from '$url'."
      git clone --depth 1 "$url" "$name"
    else
      echo "Use local git repo for '$name'."
      origin_url=$(git -C "$name" remote get-url origin)
      if [[ "$url" != "$origin_url" ]]; then
        cat >&2 <<EOF
Wrong remote URL found in cloned repository '$name':
  found $origin_url
  expected $url
Check that you don't have two project URLs with the same repo name.
EOF
        exit 1
      fi
    fi
    touch inputs
    for ext in "${extensions[@]}"; do
      if [[ -n "$ext" ]]; then
        esc=$(echo "$ext" | escape_ext)
        find "$name" | grep "/[^.]\+$esc"'$' >> inputs
      fi
    done
  )

  readarray -t inputs < <(cat "$proj_workspace"/inputs)
  num_files=${#inputs[@]}
  echo "Found $num_files files."
  errors=0
  line_count=0
  error_line_count=0

  int_err_count=0
  ext_err_count=0
  other_err_count=0

  touch "$global_json_err_log"
  for input in "${inputs[@]}"; do
    echo -n "$input"
    file_line_count=$(cat "$proj_workspace/$input" | wc -l)
    echo -n " ($file_line_count lines)"
    line_count=$(( line_count + file_line_count ))
    if parse; then
      echo "  OK"
    else
      error_line_count=$(( error_line_count + file_error_line_count ))
      echo "$proj_workspace/$input" >> "$failed_inputs"
      errors=$(( errors + file_error_count ))
      echo "  ERROR"
    fi
  done

  # Aggregate results with other projects
  cat "$proj_err_log" >> "$global_err_log"
  global_line_count=$(( global_line_count + line_count ))
  global_error_line_count=$(( global_error_line_count + error_line_count ))
  cat >> "$csv" <<EOF
$name,$url,$num_files,$errors,$line_count,$error_line_count,\
$ext_err_count,$int_err_count,$other_err_count
EOF
}

aggregate_errors() {
  header='num_errors,num_lines,error_class'
  "$progdir"/aggregate-parse-errors < "$global_json_err_log" \
    > "$stat"/aggregated-errors.csv
}

main() {
  # Read lines into array after removing comments and blank lines.
  readarray -t urls < <(grep -v '^ *\(#\| *$\)' "$projects_file")
  readarray -t extensions < <(grep -v '^ *\(#\| *$\)' "$extensions_file")
  if [[ "${#extensions[@]}" = 0 ]]; then
    error "Found no file extensions in '$extensions_file'."
  fi

  # Workspace, which includes cached git repos.
  tmp=stat.tmp

  # Output folder, wiped out before every run.
  stat=$(pwd)/stat
  csv="$stat"/stat.csv
  failed_inputs="$stat"/stat.fail
  global_err_log="$stat"/parse-error.log
  global_json_err_log="$stat"/parse-error.json

  parser=$(pwd)/ocaml-src/_build/install/default/bin/parse-"$lang"
  test -x "$parser" || error "Missing parser '$parser'."

  # Initialize output folder
  rm -rf "$stat"
  mkdir -p "$stat"

  # Reuse temporary folder
  mkdir -p "$tmp"
  rm -f "$failed_inputs"

  # CSV header
  cat > "$csv" <<EOF
Name,URL,Files,"Failed files",Lines,"Failed lines",\
"External errors","Internal errors","Other errors"
EOF

  # Run the stats on each git project
  global_line_count=0
  global_error_line_count=0
  for url in "${urls[@]}"; do
    handle_project 2>&1 || echo "Failed on $url" 2>&1 \
      | tee "$stat"/lang-stat.log
  done

  aggregate_errors

  line_coverage=$(awk -f - <<EOF
BEGIN {
  printf "%.3f%%\n",
  100 * (1 - $global_error_line_count / $global_line_count)
}
EOF
  )

  tee "$stat"/summary.txt <<EOF
Line count: $global_line_count
Line coverage: $line_coverage
EOF
}

main
