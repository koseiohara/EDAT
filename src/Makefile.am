
lib_LIBRARIES = libedat.a

AM_FCFLAGS = -I$(srcdir) $(OPENMP_FCFLAGS)

EXTRA_DIST = \
  pairwise_sum_core.F90 \
  pairwise_sum_spec.F90 \
  pairwise_mean_spec.F90 \
  pairwise_variance_spec.F90 \
  pairwise_covariance_spec.F90 \
  pairwise_corrcoef_spec.F90

libedat_a_SOURCES = \
  c_qsort.c \
  edat_sort.F90 \
  pairwise_sum.F90 \
  pairwise_mean.F90 \
  pairwise_variance.F90 \
  pairwise_covariance.F90 \
  pairwise_corrcoef.F90 \
  edat_math.F90 \
  edat_caseconverter.F90 \
  edat_string.F90 \
  edat_float.F90 \
  integral_sp.F90 \
  integral_dp.F90 \
  integral_qp.F90 \
  derivative_sp.F90 \
  derivative_dp.F90 \
  derivative_qp.F90 \
  edat_met.F90 \
  edat_binio.F90

modincludedir = $(includedir)

nodist_modinclude_DATA = \
  edat_sort.mod \
  edat_math.mod \
  edat_string.mod \
  edat_float.mod \
  edat_met.mod \
  edat_binio.mod

pairwise_sum.$(OBJEXT): \
  pairwise_sum_core.F90 \
  pairwise_sum_spec.F90

pairwise_mean.$(OBJEXT): \
  pairwise_mean_spec.F90 \
  pairwise_sum.$(OBJEXT)

pairwise_variance.$(OBJEXT): \
  pairwise_variance_spec.F90 \
  pairwise_sum.$(OBJEXT) \
  pairwise_mean.$(OBJEXT)

pairwise_covariance.$(OBJEXT): \
  pairwise_covariance_spec.F90 \
  pairwise_sum.$(OBJEXT) \
  pairwise_mean.$(OBJEXT)

pairwise_corrcoef.$(OBJEXT): \
  pairwise_corrcoef_spec.F90 \
  pairwise_variance.$(OBJEXT) \
  pairwise_covariance.$(OBJEXT)

edat_math.$(OBJEXT): \
  pairwise_sum.$(OBJEXT) \
  pairwise_mean.$(OBJEXT) \
  pairwise_variance.$(OBJEXT) \
  pairwise_covariance.$(OBJEXT) \
  pairwise_corrcoef.$(OBJEXT)

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



