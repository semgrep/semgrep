#!/bin/bash
if [[ "$OSTYPE" == "darwin"* ]]; then
  echo "( :standard )" > flags.sexp        
else
  echo "(-ccopt -static)" > flags.sexp
fi
