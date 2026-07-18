
lib_LIBRARIES = libedat.a

libedat_a_SOURCES = \
  c_qsort.c \
  c_sum_hp.c \
  c_sum_hp_interface.f90 \
  edat_sort.f90 \
  edat_math.f90 \
  edat_caseconverter.f90 \
  edat_string.f90 \
  edat_float.f90 \
  integral_sp.f90 \
  integral_dp.f90 \
  integral_qp.f90 \
  derivative_sp.f90 \
  derivative_dp.f90 \
  derivative_qp.f90 \
  edat_met.f90 \
  edat_binio.f90

modincludedir = $(includedir)

nodist_modinclude_DATA = \
  edat_sort.mod \
  edat_math.mod \
  edat_string.mod \
  edat_float.mod \
  edat_met.mod \
  edat_binio.mod

edat_math.$(OBJEXT): c_sum_hp_interface.$(OBJEXT)

edat_string.$(OBJEXT): edat_caseconverter.$(OBJEXT)

integral_sp.$(OBJEXT): edat_math.$(OBJEXT)
integral_dp.$(OBJEXT): edat_math.$(OBJEXT)
integral_qp.$(OBJEXT): edat_math.$(OBJEXT)

derivative_sp.$(OBJEXT): edat_math.$(OBJEXT)
derivative_dp.$(OBJEXT): edat_math.$(OBJEXT)
derivative_qp.$(OBJEXT): edat_math.$(OBJEXT)

edat_met.$(OBJEXT): \
  integral_sp.$(OBJEXT) \
  integral_dp.$(OBJEXT) \
  integral_qp.$(OBJEXT) \
  derivative_sp.$(OBJEXT) \
  derivative_dp.$(OBJEXT) \
  derivative_qp.$(OBJEXT)

$(nodist_modinclude_DATA): libedat.a

CLEANFILES = *.mod



