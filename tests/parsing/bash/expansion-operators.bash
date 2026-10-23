# Parameter-expansion operators whose patterns contain shell metacharacters
H="${I#*;}"
J="${K##*;}"
L="${M%;*}"
P="${Q%|*}"
R="${S%()}"
X="${Y%<}"
C="${D%</E>*}"
F="${#!}"
printf "%s" "${seq0:-(default)}"
echo ${a[1 + 2]}
