
#include <stdlib.h>

#ifdef DEBUG
#include <stdio.h>
#endif



static inline long largest_power_2(unsigned long n){
    long output;

    output = 1L;
    while (n > 1UL){
        n = n >> 1;
        output = output << 1;
    }

    return output;
}


float c_sum_hp_sp(long* n, float* arr){
    float* restrict work_arr;
    float           output;
    float* remain_first;

    unsigned long work_n;
    long len;
    long new_len;
    long dist;
    long k;
    long i;
    long i2;

    work_n = (unsigned long)(*n);
    len    = largest_power_2(work_n);
    k      = (long)work_n - len;

    work_arr     = malloc(sizeof(float) * (len >> 1));
    remain_first = &arr[len];

    for (i = 0L; i < k; i = i + 1L){
        dist = (i+i+1L) * len / (k+k);
        #ifdef DEBUG
        printf("DEBUG: Distribute arr[%ld] to %ld\n", i+len, dist);
        #endif
        arr[dist] = arr[dist] + remain_first[i];
    }

    while (len >= 16L){
        new_len = len >> 1;
        for (i = 0L; i < new_len; i = i + 8L){
            i2 = i + i;
            work_arr[i]    = arr[i2]     + arr[i2+ 1L];
            work_arr[i+1L] = arr[i2+ 2L] + arr[i2+ 3L];
            work_arr[i+2L] = arr[i2+ 4L] + arr[i2+ 5L];
            work_arr[i+3L] = arr[i2+ 6L] + arr[i2+ 7L];
            work_arr[i+4L] = arr[i2+ 8L] + arr[i2+ 9L];
            work_arr[i+5L] = arr[i2+10L] + arr[i2+11L];
            work_arr[i+6L] = arr[i2+12L] + arr[i2+13L];
            work_arr[i+7L] = arr[i2+14L] + arr[i2+15L];
        }

        new_len = new_len >> 1;
        for (i = 0L; i < new_len; i = i + 4L){
            i2 = i + i;
            arr[i]    = work_arr[i2]    + work_arr[i2+1L];
            arr[i+1L] = work_arr[i2+2L] + work_arr[i2+3L];
            arr[i+2L] = work_arr[i2+4L] + work_arr[i2+5L];
            arr[i+3L] = work_arr[i2+6L] + work_arr[i2+7L];
        }

        len = new_len;
    }

    while (len > 1L){
        new_len = len >> 1;
        for (i = 0L; i < new_len; i = i + 1L){
            i2 = i + i;
            work_arr[i] = arr[i2] + arr[i2+1L];
        }

        if (new_len == 1L){
            output = work_arr[0];
            free(work_arr);
            return output;
        }

        new_len = new_len >> 1;
        for (i = 0L; i < new_len; i = i + 1L){
            i2 = i + i;
            arr[i] = work_arr[i2] + work_arr[i2+1L];
        }

        len = new_len;
    }

    output = arr[0];
    free(work_arr);
    return output;
}


double c_sum_hp_dp(long* n, double* arr){
    double* restrict work_arr;
    double           output;
    double* remain_first;

    unsigned long work_n;
    long len;
    long new_len;
    long dist;
    long k;
    long i;
    long i2;

    work_n = (unsigned long)(*n);
    len    = largest_power_2(work_n);
    k      = (long)work_n - len;

    work_arr     = malloc(sizeof(double) * (len >> 1));
    remain_first = &arr[len];

    for (i = 0L; i < k; i = i + 1L){
        dist = (i+i+1L) * len / (k+k);
        #ifdef DEBUG
        printf("DEBUG: Distribute arr[%ld] to %ld\n", i+len, dist);
        #endif
        arr[dist] = arr[dist] + remain_first[i];
    }

    while (len >= 16L){
        new_len = len >> 1;
        for (i = 0L; i < new_len; i = i + 8L){
            i2 = i + i;
            work_arr[i]    = arr[i2]     + arr[i2+ 1L];
            work_arr[i+1L] = arr[i2+ 2L] + arr[i2+ 3L];
            work_arr[i+2L] = arr[i2+ 4L] + arr[i2+ 5L];
            work_arr[i+3L] = arr[i2+ 6L] + arr[i2+ 7L];
            work_arr[i+4L] = arr[i2+ 8L] + arr[i2+ 9L];
            work_arr[i+5L] = arr[i2+10L] + arr[i2+11L];
            work_arr[i+6L] = arr[i2+12L] + arr[i2+13L];
            work_arr[i+7L] = arr[i2+14L] + arr[i2+15L];
        }

        new_len = new_len >> 1;
        for (i = 0L; i < new_len; i = i + 4L){
            i2 = i + i;
            arr[i]    = work_arr[i2]    + work_arr[i2+1L];
            arr[i+1L] = work_arr[i2+2L] + work_arr[i2+3L];
            arr[i+2L] = work_arr[i2+4L] + work_arr[i2+5L];
            arr[i+3L] = work_arr[i2+6L] + work_arr[i2+7L];
        }

        len = new_len;
    }

    while (len > 1L){
        new_len = len >> 1;
        for (i = 0L; i < new_len; i = i + 1L){
            i2 = i + i;
            work_arr[i] = arr[i2] + arr[i2+1L];
        }

        if (new_len == 1L){
            output = work_arr[0];
            free(work_arr);
            return output;
        }

        new_len = new_len >> 1;
        for (i = 0L; i < new_len; i = i + 1L){
            i2 = i + i;
            arr[i] = work_arr[i2] + work_arr[i2+1L];
        }

        len = new_len;
    }

    output = arr[0];
    free(work_arr);
    return output;
}




