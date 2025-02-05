LIB = libedat.a
OBJ = edat_math.o edat_caseconverter.o edat_string.o edat_met.o edat_binio.o
EXPORT_OBJ = edat_math.o edat_string.o edat_met.o edat_binio.o

DIR = ${HOME}/FortranLib

FC = ifort
FLAG = -O3 -warn all -traceback -convert little_endian -assume byterecl

%.o : %.f90
	${FC} -c $< ${FLAG}

all : ${LIB}

${LIB} : ${OBJ}
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

