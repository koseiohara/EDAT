LIB = libedat.a
C_OBJ = c_qsort.o
OBJ = edat_sort.o edat_math.o edat_caseconverter.o edat_string.o edat_met.o edat_binio.o
EXPORT_OBJ = edat_sort.o edat_math.o edat_string.o edat_met.o edat_binio.o

DIR = ${HOME}/FortranLib

FC = ifort
CC = icc
FFLAG = -O3 -warn all -traceback -convert little_endian -assume byterecl
CFLAG = -O3 -Wall

%.o : %.f90
	${FC} -c $< ${FFLAG}

%.o : %.c
	${CC} -c $< ${CFLAG}

all : ${LIB}

${LIB} : ${C_OBJ} ${OBJ}
	ar rcs $@ $^


.PHONY : clean re install uninstall

clean : 
	rm -fv *.mod *.o ${LIB}

re : clean all

install :
	cp ${LIB} ${DIR}/lib/
	cp ${EXPORT_OBJ:.o=.mod} ${DIR}/include/

uninstall :
	rm -fv ${DIR}/lib/${LIB}
	rm -fv ${EXPORT_OBJ:%.o=${DIR}/include/%.mod}

