#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static void matrix_transpose(double *matrix, unsigned int d)
{
  int i,j;
  double t;
  for(i=0; i<d; i++){
    for(j=i+1; j<d; j++){
      t = matrix[j*d+i];
      matrix[j*d+i]=matrix[i*d+j];
      matrix[i*d+j] = t;
    }
  }
}
static void matrix_multiply(double *a, double *b, unsigned int d)
{
  int i,j,k1,k2;
  double t;
  //  double *c = (double*)malloc()
  double c[d*d];
  for(i=0; i<d*d; i++){
    t=0;
    k1=i/d;
    k2=i%d;
    for(j=0;j<d;j++){
      t += a[k1*d+j] * b[j*d+k2];
    }
    c[i]=t;
  }
  memcpy(a, c, sizeof(c));
}
static void dct_coefficient(double *a, unsigned int d)
{
  int i,j,k;
  double t1=sqrt(2.0/d);
  double t2=M_PI/(2*d);

  for(i=0; i<d; i++){
    a[i]=1.0/sqrt(d);
  }
  for(k=d; k<d*d; k++){
    i=k/d;
    j=k%d;
    a[k]=t1*cos(i*((j<<1)+1)*t2);
  }
}
static void do_dct(double *dst, double *src, unsigned d, int de)
{
  double a[d*d];
  
  dct_coefficient(a, d);
  
  if(de){
    memcpy(dst, a, sizeof(a));
    matrix_transpose(dst, d);    
    matrix_multiply(dst, src, d);
    matrix_multiply(dst, a, d);
  }else{
    memcpy(dst, a, sizeof(a));
    matrix_transpose(a, d);
    matrix_multiply(dst, src, d);
    matrix_multiply(dst, a, d);
  }
}
static void test_print_matrix(double *q, unsigned int d, const char *fmt)
{
  int i,j;
  for(i=0;i<d;i++){
    for(j=0; j<d; j++){
      fprintf(stdout, fmt, q[i*d+j]);
    }
    fprintf(stdout, "\n");
  }
  fprintf(stdout, "\n");
}


void dct(double *dst, double *src, unsigned d)
{
  do_dct(dst, src, d, 0);
}
void idct(double *dst, double *src, unsigned d)
{
  do_dct(dst, src, d, 1);
}
#ifdef UNIT_TEST
int main()
{
  double a[64];
  double b[64];
  int d=4;
  int i,j;
  for(i=0; i<d; i++){
    for(j=0; j<d; j++){
      a[i*d+j] = i*10+j;
    }
  }  
  memcpy(b, a, sizeof(a));
  test_print_matrix(b, d, "%.1f ");
  matrix_transpose(b, d);
  test_print_matrix(b, d, "%.1f ");
  matrix_multiply(b, b, d);
  test_print_matrix(b, d, "%.1f ");

  dct_coefficient(b, d);
  test_print_matrix(b, d, "%.3f ");
  return 0;
}
#endif
