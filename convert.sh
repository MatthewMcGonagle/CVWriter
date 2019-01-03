if [[ "$#" == 2 ]] ; then
    directory="$1"
    filename="$2"
elif [[ "$#" == 1 ]] ; then
    directory="./"
    filename="$1"
else
    directory="./"
    filename="info.cv"
fi

echo "Running CVWriter"
stack exec CVWriter ${directory} ${filename}
echo "Running pdflatex"
pdflatex -output-directory ${directory} *.tex
