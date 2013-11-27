#include <stdio.h>
static int get_ele(int n, int i);
static void test_is(int r, int n, int i);
int main()
{
  //  test_is(19, 5, 5);
  //  test_is(16, 5, 4);
  //  test_is(7, 4, 4);
  int n, i;
  scanf("%d", &n);
  for(i=1; i<2*n; i++){
    if(0 == get_ele(n,i)%2){
      printf("%d\n", i);
      break;
    }
  }
  if(i == 2*n){
    printf("-1\n");
  }
  return 0;
}
static void test_is(int r, int n, int i)
{
  printf("get_ele(%d, %d)=%d,%d\n", n, i, get_ele(n,i), r);
}

static int get_ele(int n, int i)
{
  if(i == 1)return 1;
  if(n == 1)
    return 0;

    if(i < 1 || i > 2*n-1)      {
      return 0;
    }else{
      return get_ele(n-1, i-1)+get_ele(n-1, i)+get_ele(n-1, i-2);
    }

}
