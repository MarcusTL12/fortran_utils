gfortran -cpp -c tests.f90 src/*.f90
gfortran *.o -o utiltest

rm *.o

time ./utiltest $1