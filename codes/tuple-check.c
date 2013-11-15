
#include <stdio.h>

static int tuple_check(char *s);
static int tuple_check_rest(char *s, char **rest);
static int parse_int(char *s, char **rest);
static int tuple_check_content(char *s, char **rest);
static void test_tuple_check();
int main()
{
  char s[100]={'\0'};

  test_tuple_check();
  
  while(1){
    scanf("%s", s);
    if(s[0] == 'q' || s[0] == 'Q'){
      break;
    }
    printf("%s=%d\n", s, tuple_check(s));
  }
  return 0;
}
static void test_tuple_check()
{
  char *test_string[]={
    "", "(","())","()",
    "(1","(1))","(1)",
    "(,)","(12,)","(1,12,123)","(1,12,123))",
    NULL
  };
  char test_buf[128];
  char **s;
  int size = sizeof(test_string)/sizeof(test_string[0]);

  srand(time(NULL));
  for(s=test_string; *s; s++){
    printf("%s=%d\n", *s, tuple_check(*s));
    sprintf(test_buf, "(%s)", *s);
    printf("%s=%d\n", test_buf, tuple_check(test_buf));

    sprintf(test_buf, "(%s,%s)", test_string[rand()%(size-1)], test_string[rand()%(size-1)]);

    printf("%s=%d\n", test_buf, tuple_check(test_buf));
  }  
}
static int tuple_check(char *s)
{
  char *rest = NULL;
  if(*s == '(' && tuple_check_rest(s+1, &rest) > 0 && *rest == '\0'){
    return 1;
  }
  return 0;
}
static int tuple_check_rest(char *s, char **rest)
{
  if(*s == ')'){
    *rest = s+1;
    return 1;
  }
  return tuple_check_content(s, rest);
}
static int tuple_check_content(char *s, char **rest)
{
  if(*s == '('){
    if(tuple_check_rest(s+1, rest) <= 0){
      return 0;
    }
  }else if(isdigit(*s)){
    parse_int(s, rest);
  }else{
    return 0;
  }
  s = *rest;
  if(*s == ','){
    return tuple_check_content(s+1, rest);
  }else if(*s == ')'){
    *rest = s+1;
    return 1;
  }

  return 0;
}
static int parse_int(char *s, char **rest)
{
    do{
      s++;
    }while(isdigit(*s));
    *rest = s;
    return 0;
}
