#!/bin/sh

HPHP=$1
TARGET=$2

gen="@""generated"

if [ $# -ne 2 ]
then
  echo "Usage: stdlib_from_hni.sh hphp-dir output-dir"
  exit 1
fi

if [ ! -d "$HPHP/runtime/ext" ]
then
  echo "$HPHP does not appear to be an hphp directory"
  exit 1
fi

if [ ! -d "$TARGET" ]
then
  echo "$TARGET does not exist"
  exit 1
fi

for file in `find "$HPHP/runtime/ext" -name "*.php"`
do
  if [ `head -n 1 "$file"` != "<?hh" ]
  then
    echo "$file does not appear to be an HNI file?"
    continue
  fi

  bn=`basename "$file"`
  echo "Processing $bn"
  sed "s#<?hh#<?hh // decl\n// $gen from $bn by stdlib_from_hni.sh DO NOT MODIFY#" "$file" > "$TARGET/${bn}"
done
