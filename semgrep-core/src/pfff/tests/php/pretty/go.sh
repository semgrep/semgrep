#!/bin/bash

for i in *.php ; do
    echo "*********************************************************"
    echo "$i";
    ../../../lang_php/pretty/prettyphp $i > $i.pp;
    diff $i $i.pp > $i.error;
    cat $i.error;
    rm -f $i.pp $i.error
done
