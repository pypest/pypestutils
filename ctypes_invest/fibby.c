/* fibby.c */
#include<stdlib.h>
#include<stdio.h>

void c_fib(double *a, int n);

int main(int argc, char* argv[argc+1]) {
    puts("Fortran fib from C:");
    int n=7;
    double a[n];
    c_fib(a, n);
    for(int i=0; i<n; i++) {
        printf("%f ",a[i]);
    };
    return EXIT_SUCCESS;
}
