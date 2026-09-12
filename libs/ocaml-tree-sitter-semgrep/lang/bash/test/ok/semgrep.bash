# Semgrep ellipsis
...
cat ...
... --force

# Semgrep metavariables
echo $FOO
$FOO

# All-caps shell variables
echo "${BAR}"
echo ${BAR}
${BAR}

# Metavariable matching a variable
echo ${$X}
echo ${$X#/}

# Ellipsis in unquoted concatenation
$a...
...$a

# Ellipsis in quoted concatenation
"$a..."
"...$a"

# Other metavariables and ellipses
case $X in
  a)
    ;;
  *)
esac

for $X in ...; do
  ...
done

# Function definition (requires 'function' keyword with metavariable)
function $FOO() {
 ...
}

declare -r $READONLY
$MUT=$(ls)

# Array declaration and assignment
declare -a $ARR
# $ARR=(...)
