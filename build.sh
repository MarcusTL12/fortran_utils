gfortran -cpp -c src/*.f90
gfortran *.o -o utiltest

rm *.o

time ./utiltest $1