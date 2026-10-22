echo $[ x + 1 ]
(( a = 1, b = 2 ))
echo $(( a[i + 1] * 2 ))
x=$(( 16#ff ))
echo "$(( x << 2 ))"
for (( i = 0, j = 10; i < j; i++, j-- )); do
  echo $i $j
done
