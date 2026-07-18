
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


float c_sum_hp_sp(long* n, const float* arr){
    float* restrict work_arr1;
    float* restrict work_arr2;
    float           output;
    const float* remain_first;

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

    work_arr1    = malloc(sizeof(float) * len);
    work_arr2    = malloc(sizeof(float) * len);
    remain_first = &arr[len];

    for (i = 0L; i < len; i = i + 1L){
        work_arr1[i] = arr[i];
    }

    for (i = 0L; i < k; i = i + 1L){
        dist = (i+i+1L) * len / (k+k);
        #ifdef DEBUG
        printf("DEBUG: Distribute arr[%ld] to %ld\n", i+len, dist);
        #endif
        work_arr1[dist] = work_arr1[dist] + remain_first[i];
    }

    while (len >= 8L){
        new_len = len >> 1;
        for (i = 0L; i < new_len; i = i + 4L){
            i2 = i + i;
            work_arr2[i]    = work_arr1[i2]    + work_arr1[i2+1L];
            work_arr2[i+1L] = work_arr1[i2+2L] + work_arr1[i2+3L];
            work_arr2[i+2L] = work_arr1[i2+4L] + work_arr1[i2+5L];
            work_arr2[i+3L] = work_arr1[i2+6L] + work_arr1[i2+7L];
        }

        // if (new_len == 1L){
        //     output = work_arr2[0];
        //     free(work_arr1);
        //     free(work_arr2);
        //     return output;
        // }

        new_len = new_len >> 1;
        for (i = 0L; i < new_len; i = i + 2L){
            i2 = i + i;
            work_arr1[i]   = work_arr2[i2]    + work_arr2[i2+1L];
            work_arr1[i+1] = work_arr2[i2+2L] + work_arr2[i2+3L];
        }

        len = new_len;
    }

    while (len > 1L){
        new_len = len >> 1;
        for (i = 0L; i < new_len; i = i + 1L){
            i2 = i + i;
            work_arr2[i] = work_arr1[i2] + work_arr1[i2+1L];
        }

        if (new_len == 1L){
            output = work_arr2[0];
            free(work_arr1);
            free(work_arr2);
            return output;
        }

        new_len = new_len >> 1;
        for (i = 0L; i < new_len; i = i + 1L){
            i2 = i + i;
            work_arr1[i] = work_arr2[i2] + work_arr2[i2+1L];
        }

        len = new_len;
    }

    output = work_arr1[0];
    free(work_arr1);
    free(work_arr2);
    return output;
}



