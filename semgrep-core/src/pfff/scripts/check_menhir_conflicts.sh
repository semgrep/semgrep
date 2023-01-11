#!/bin/bash
set -e

# Expected input: first arg is the file with menhir error messages
#                 second arg is the max allowed conflicts
#                 third arg is the language
FILE=$1
num=$2
lang=$3
generated_ml_file=$4
generated_mli_file=$5
INSTR="To bypass this, manually set NUM_PERMITTED_CONFLICTS in pfff/lang_$lang/parsing/Makefile"

# Parse conflicts output
# Expect "Warning: [num] shift/reduce conflicts were arbitrarily resolved"
# and/or "Warning: [num] reduce/reduce conflicts were arbitrarily resolved"
# Split by space to get [num]

resultsr=`grep -i 'shift/reduce conflict.* arbitrarily' $FILE | cut -d ' ' -f 2`
resultrr=`grep -i 'reduce/reduce conflict.* arbitrarily' $FILE | cut -d ' ' -f 2`
if [[ $resultsr == one ]]; then
   resultsr=1
fi
if [[ "$resultrr" == "one" ]]; then
   resultrr=1
fi

result=$((resultsr+resultrr))
MSG="Error: menhir found $result arbitrarily resolved conflicts when $num are permitted. See pfff/lang_$lang/parsing/menhir_out.log for details. ($INSTR)"

if (( $result != $num )); then
   echo $MSG 1>&2
   rm -f $generated_ml_file $generated_mli_file
   exit 126
fi
