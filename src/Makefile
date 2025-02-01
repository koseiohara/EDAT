LIB = libedat.a
INTERNAL_OBJ = physics.o caseconverter.o statistic.o binio.o
EXPORT_OBJ   = edat.o

DIR = ${HOME}/FortranLib

FC = ifort
FLAG = -O3 -warn all -traceback

%.o : %.f90
	${FC} -c $< ${FLAG}

all : ${LIB}

${LIB} : ${INTERNAL_OBJ} ${EXPORT_OBJ}
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

