shopt -s expand_aliases
source ~/.bash_aliases

antlr4 "$1".g4
javac "$1"*.java

echo "" 
echo "Input:"
echo ""
cat "$3"
echo "" 
grun "$1" "$2" -tokens -diagnostics -gui "$3"

