FC = gfortran
FFLAGS = -O3
CPPFLAGS =
OMP_SIMD_FLAGS = -fopenmp-simd

PROJECT_SRC := ../src
BUILD_DIR := build
NO_OMP_DIR := $(BUILD_DIR)/no-omp
OMP_DIR := $(BUILD_DIR)/omp
NO_OMP_OBJECT := $(NO_OMP_DIR)/pairwise_sum.o
OMP_OBJECT := $(OMP_DIR)/pairwise_sum.o
NO_OMP_EXE := $(BUILD_DIR)/benchmark-no-omp.bin
OMP_EXE := $(BUILD_DIR)/benchmark-omp.bin
COMMON_FLAGS := $(FFLAGS) $(CPPFLAGS) -cpp -UDEBUG -I$(PROJECT_SRC)
PAIRWISE_DEPENDENCIES := \
  $(PROJECT_SRC)/pairwise_sum.F90 \
  $(PROJECT_SRC)/pairwise_sum_core.F90 \
  $(PROJECT_SRC)/pairwise_sum_spec.F90

.PHONY: all compare clean

all: $(NO_OMP_EXE) $(OMP_EXE)

compare: all
	@echo '=== OpenMP SIMD disabled ==='
	$(NO_OMP_EXE)
	@echo
	@echo '=== OpenMP SIMD enabled ==='
	$(OMP_EXE)

$(NO_OMP_DIR) $(OMP_DIR):
	mkdir -p $@

$(NO_OMP_OBJECT): $(PAIRWISE_DEPENDENCIES) | $(NO_OMP_DIR)
	$(FC) $(COMMON_FLAGS) -J$(NO_OMP_DIR) -c $(PROJECT_SRC)/pairwise_sum.F90 -o $@

$(OMP_OBJECT): $(PAIRWISE_DEPENDENCIES) | $(OMP_DIR)
	$(FC) $(COMMON_FLAGS) $(OMP_SIMD_FLAGS) -J$(OMP_DIR) -c $(PROJECT_SRC)/pairwise_sum.F90 -o $@

$(NO_OMP_EXE): benchmark.F90 $(NO_OMP_OBJECT)
	$(FC) $(COMMON_FLAGS) -I$(NO_OMP_DIR) benchmark.F90 $(NO_OMP_OBJECT) -o $@

$(OMP_EXE): benchmark.F90 $(OMP_OBJECT)
	$(FC) $(COMMON_FLAGS) $(OMP_SIMD_FLAGS) -DBENCHMARK_OMP_SIMD -I$(OMP_DIR) benchmark.F90 $(OMP_OBJECT) -o $@

clean:
	rm -rf $(BUILD_DIR)
