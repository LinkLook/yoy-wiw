#include <stdio.h>
#include <stdlib.h>

static int swap_1(int a[], int len)
{
  int i,j,t;

  for(i=0; i<len/2; i++){
    for(j=i+1; j<len-i-1; j+=2){
      t=a[j];
      a[j]=a[j+1];
      a[j+1]=t;
    }
  }
  return 0;
}
static int swap_2(int a[], int len)
{
  int i=0,j=len-1, t;
  while(j>i){
    if(a[i]%2){i++;}
    else if(a[j]%2 == 0){j--;}
    else{
      t=a[i];
      a[i]=a[j];
      a[j]=t;
      i++;j--;
    }
  }
  return 0;
}
static int merge_1(int a[], int m, int b[], int n, int c[], int sz)
{
  //assert(m>=0 && n>=0 && sz>1);
  if(m==0 && n==0){
    return 0;
  }
  if(m>0 && n>0){
    if(a[0] > b[0]){
      c[0] = b[0];
    }else{
      c[0] = a[0];
    }
  }else if(m>0){
    c[0]=a[0];
  }else{
    c[0]=b[0];
  }
  int i=0,j=0,k=1;
  while(i<m && j<n){
    if(a[i] < b[j]){
      if(c[k-1] < a[i]){
        c[k++] = a[i];
      }
      i++;
    }else{
      if(c[k-1] < b[j]){
        c[k++] = b[j];
      }
      j++;
    }
    if(k >= sz){return k;}
  }
  if(i >= m){
    while(j<n){
      if(c[k-1] < b[j]){
        c[k++]=b[j];
      }
      j++;
      if(k >= sz){break;}
    }
  }else{
    while(i<m){
      if(c[k-1] < a[i]){
        c[k++]=a[i];
      }
      i++;
      if(k >= sz){break;}
    }
  }
  return k;
}
static void print_a(int a[], int len, char *fmt)
{
  int i;
  for(i=0; i<len; i++){
    printf(fmt, a[i]);
  }
  printf("\n");
}
static int compare(const void *e1, const void *e2)
{
  return *(int*)e1>*(int*)e2;
}
static void test_merge()
{
  srand(time(NULL));
  int a[48],b[48],c[100];
  int m, n, r, i;
  m = rand()%5+10;
  n = rand()%5;
  for(i=0; i<m; i++){
    a[i]=rand()%10;
  }
  for(i=0; i<n; i++){
    b[i]=rand()%10;
  }
  qsort(a, m, sizeof(int), compare);
  qsort(b, n, sizeof(int), compare);
  print_a(a, m, "%d ");
  print_a(b, n, "%d ");
  r = merge_1(a, m, b, n, c, 100);
  print_a(c, r, "%d ");
}
static void test_swap()
{
  int a[52]={0};
  int i,len=51;
  for(i=0; i<len; i++){
    if(i%2 == 0){
      a[i]='A'+i/2;
    }else{
      a[i]='a'+i/2;
    }
  }
  print_a(a, len, "%c ");
  swap_1(a, len);
  print_a(a, len, "%c ");
  swap_2(a, len);
  print_a(a, len, "%d ");
}
int main()
{
  test_swap();
  test_merge();
  return 0;
}
