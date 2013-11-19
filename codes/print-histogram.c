#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIN_INT (1<<(sizeof(int)*8-1))
#define MAX_INT (-(1+MIN_INT))
#define RECT_WIDTH 5
#define OUT_BUFF_POS(x) (RECT_WIDTH*(x))

static FILE *   dest_file = NULL;
static int print_histogram(int a[], int len);
static int search_max_bellow_x(int *a, int len, int x);

int main()
{
  int a[]={4,10,6,15,7,5,9,12};
  int i;
  int len = sizeof(a)/sizeof(int);

  dest_file = stdout;

  for(i=0; i<len; i++){
    printf("    %d", a[i]);
  }
  printf("\n");

  print_histogram(a, len);
  return 0;
}

static void fix_out_buff_line(char out_buff[], int line, int a[], int max_idx_a[], int len)
{
  int i;
  int val;
  int pos;

  memset(out_buff, ' ', OUT_BUFF_POS(len));
  for(i=0; i<len; i++){
    val = a[max_idx_a[i]];
    pos = OUT_BUFF_POS(max_idx_a[i]);
    if(val == line){
      out_buff[pos] = '-';
      out_buff[pos+1] = '-';
      out_buff[pos+2] = '-';
      out_buff[pos+3] = '-';
    }else if(val == line+1){
      out_buff[pos] = '|';
      if(val > 9){
        out_buff[pos+1] = val/10+'0'  ;
      }
      out_buff[pos+2] = val%10+'0';
      out_buff[pos+3] = '|';
    }else if(line < val){
      out_buff[pos] = '|';
      out_buff[pos+3] = '|';
    }
  }
}

static int do_print_histogram(int a[], int max_idx_a[], int len)
{
  int line = a[max_idx_a[0]];
  int i = 0;
  
  char *out_buff = NULL;
  out_buff = malloc(OUT_BUFF_POS(len)+2);
  if(out_buff == NULL){
    printf("oom:%d\n", OUT_BUFF_POS(len));
    return -1;    
  }
  
  fprintf(dest_file, "\n  ^\n");
  while(line > 0){
    fix_out_buff_line(out_buff, line, a, max_idx_a, len);
    out_buff[OUT_BUFF_POS(len)]='\0';
    fprintf(dest_file, "  | %s\n", out_buff);
    line--;
  }
  fprintf(dest_file, "  +");
  for(i=0; i<len*RECT_WIDTH; i++){
    fprintf(dest_file, "-");
  }
  fprintf(dest_file, "->\n\n");
  free(out_buff);
  return 0;
}
static int print_histogram(int a[], int len)
{
  int *max_idx_a = NULL;
  int i = 0;
  int ret;

  max_idx_a = malloc(sizeof(int)*len);
  if(max_idx_a == NULL){
    printf("oom:%lu\n", sizeof(int)*len);
    return -1;
  }
  memset(max_idx_a, '\0', sizeof(int)*len);

  max_idx_a[0] = search_max_bellow_x(a, len, MAX_INT);
  for(i=1; i<len; i++){
    max_idx_a[i] = search_max_bellow_x(a, len, a[max_idx_a[i-1]]);
  }
  /*
  for(i=0; i<len; i++){
    printf("%d:%d ", max_idx_a[i], a[max_idx_a[i]]);
  }
  */
  ret = do_print_histogram(a, max_idx_a, len);
  free(max_idx_a);
  return ret;
}
static int search_max_bellow_x(int *a, int len, int x)
{
  int i;
  int max_idx = -1;
  int max = MIN_INT;

  for(i=0; i<len; i++){
    if(a[i] >= x){continue;}
    if(a[i] > max){
      max = a[i];
      max_idx = i;
    }
  }
  return max_idx;
}
