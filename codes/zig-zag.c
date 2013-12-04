
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define UNIT_TEST 0

#if UNIT_TEST
static void zig_zag_test_print_matrix(int *q, int d)
{
  int i,j;
  for(i=0;i<d;i++){
    for(j=0; j<d; j++){
      fprintf(stdout, "%2d ", q[i*d+j]);
    }
    fprintf(stdout, "\n");
  }
}
static void zig_zag_test()
{
  int q[16]={0};
  int p[16];
  int i;
  int d = 4;
  int n = d*d;
  int m;

  m = 0;
  for(i=1; i<=d; i++){
    m += i;
  }
  for(i=0;i<n;i++){
    p[i]=i;
  }

  q[n-1]=d-1;
  q[0]=0;
  make_queue(0, q, m);
  make_queue_backword(0, q, m, n);
  for(i=0;i<n;i++){
    fprintf(stdout, "%d ", q[i]);
  }
  fprintf(stdout, "\n");
  make_queue(1, q, m);
  make_queue_backword(1, q, m, n);
  for(i=0;i<n;i++){
    fprintf(stdout, "%d ", q[i]);
  }
  fprintf(stdout, "\n");

  zig_zag(q, p, d, 0);
  zig_zag_test_print_matrix(q, d);
  fprintf(stdout, "==============\n");
  memcpy(p, q, sizeof(p));
  zig_zag(q, p, d, 1);
  zig_zag_test_print_matrix(q, d);
}

int main()
{
  zig_zag_test();
  return 0;
}
#endif

static void make_queue(int init, int *queue, int len)
{
  int i, tmp;
  int loop = init;

  i = 1;
  while(1){
    tmp = loop;
    while(loop > 0){
      queue[i]=queue[i-1]+1;
      if(++i >= len){return;}
      loop-- ;
    }
    while(loop < tmp){
      queue[i]=queue[i-1]-1;
      if(++i >= len){return;}
      loop++;
    }
    queue[i]=queue[i-1];
    if(++i >= len){return;}

    loop += 2;
  }
}
static void make_queue_backword(int init, int *queue, int start, int len)
{
  int i, tmp;
  int loop = init;

  i = len-2;
  while(1){
    tmp = loop;
    while(loop > 0){
      queue[i]=queue[i+1]-1;
      if(--i < start){return;}
      loop-- ;
    }
    while(loop < tmp){
      queue[i]=queue[i+1]+1;
      if(--i < start){return;}
      loop++;
    }
    queue[i]=queue[i+1];
    if(--i < start){return;}
    loop += 2;
  }
}

void zig_zag(int *dest, int *src, unsigned int d, int decode)
{ 
  unsigned int m, i;
  unsigned int n = d*d;
  int * row_queue = (int*)malloc(sizeof(int)*n);
  int * col_queue = (int*)malloc(sizeof(int)*n);

  m = 0;
  for(i=1; i<=d; i++){
    m += i;
  }
  row_queue[0]=col_queue[0]=0;
  make_queue(0, row_queue, m);
  make_queue(1, col_queue, m);

  row_queue[n-1]=col_queue[n-1]=d-1;
  make_queue_backword(0, row_queue, m, n);
  make_queue_backword(1, col_queue, m, n);

  if(decode){
    for(m=0; m<n; m++){
      dest[row_queue[m]*d+col_queue[m]] = src[m];
    }
  }else{
    for(m=0; m<n; m++){
      dest[m]=src[row_queue[m]*d+col_queue[m]];
    }
  }

  free(row_queue);
  free(col_queue);
}

