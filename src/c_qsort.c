#include <stdlib.h>


int compare_i4(const void* a, const void* b){
    int a_cp = *(const int*)a;
    int b_cp = *(const int*)b;

    if (a_cp < b_cp){
        return -1;
    }else if (a_cp > b_cp){
        return 1;
    }else{
        return 0;
    }
}


int compare_sp(const void* a, const void* b){
    float a_cp = *(const float*)a;
    float b_cp = *(const float*)b;

    if (a_cp < b_cp){
        return -1;
    }else if (a_cp > b_cp){
        return 1;
    }else{
        return 0;
    }
}


int compare_dp(const void* a, const void* b){
    double a_cp = *(const double*)a;
    double b_cp = *(const double*)b;

    if (a_cp < b_cp){
        return -1;
    }else if (a_cp > b_cp){
        return 1;
    }else{
        return 0;
    }
}


// int compare_qp(const void* a, const void* b){
//     long double a_cp = *(const long double*)a;
//     long double b_cp = *(const long double*)b;
// 
//     if (a_cp < b_cp){
//         return -1;
//     }else if (a_cp > b_cp){
//         return 1;
//     }else{
//         return 0;
//     }
// }


void c_quick_sort_i4(const int* n, int array[]){
    qsort(array, *n, sizeof(int), compare_i4);
}


void c_quick_sort_sp(const int* n, float array[]){
    qsort(array, *n, sizeof(float), compare_sp);
}


void c_quick_sort_dp(const int* n, double array[]){
    qsort(array, *n, sizeof(double), compare_dp);
}

// void c_quick_sort_qp(const int* n, long double array[]){
//     qsort(array, *n, sizeof(long double), compare_qp);
// }

