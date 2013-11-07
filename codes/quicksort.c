#include <stdio.h>
#include <time.h>
#include <stdlib.h>

static int quicksort(int *a, int s, int e);
static void printarray(int *a, unsigned int size);

int main(int argc, char ** argv)
{
  int *A = NULL;
  int size = 10;
  int i = 0;

  srand(time(NULL));
  if(argc > 1){
    size = atoi(argv[1]);
  }
  if(size <= 0 || size > 10000){
    printf ("usage: %s size <RET> size<=10000\n", argv[0]);
    return 1;
  }
  A = (int *)malloc(size);
  if(A == NULL){
    printf("malloc failed!\n");
    return 1;
  }
  for(i=0; i<size; ++i){
    A[i] = rand()%(size*100);
  }
  printarray(A, size);
  quicksort(A, 0, size);
  printarray(A, size);
  free(A);
  return 0;
}
static void printarray(int *a, unsigned int size)
{
  int i;
  for(i=0; i<size; ++i){
    printf("%d ", a[i]);
  }
  printf("\n");
}
static void exchange(int *a, int i, int j)
{
  int t;
  t = a[i];
  a[i] = a[j];
  a[j] = t;
}
static int quicksort(int *a, int s, int e)
{
  int x, i, j;
  if(e-s < 2){
    return 0;
  }
  x = a[e-1];
  i = s-1;
  for(j=s; j<e-1; ++j){
    if(a[j] < x){
      exchange(a, ++i, j);
    }
  }
  exchange(a, ++i, e-1);
  quicksort(a, s, i);
  quicksort(a, i+1, e);
  return 0;
}
