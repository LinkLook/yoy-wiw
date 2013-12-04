
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "huffman.h"

#define UNIT_TEST
//#define HUFFMAN_LONG_CODE

#ifdef HUFFMAN_LONG_CODE
typedef unsigned long long huffman_code[4];
#define HUFFMAN_CODE_INIT(code) do{memset((code),'\0',sizeof(huffman_code));}while(0);
#define HUFFMAN_CODE_COPY(dest, src) do{memcpy((dest),(src),sizeof(huffman_code));}while(0);
static inline void huffman_code_inf(huffman_code code)
{
  int i;
  for(i=0; i<4; i++){
    code[i]++;           
    if(code[i] != 0){break;}    
  }
}
static inline unsigned long long huffman_code_get_bit(huffman_code code, unsigned char n)
{
  if(n>0)n--;
  return (code[n/64] & (1<<(n%64)))?1:0;
}

static inline void huffman_code_ash_1(huffman_code code)
{
  int i;
  code[3]<<=1;        
  for(i=2; i>=0; i--){
    code[i+1] |= huffman_code_get_bit(code, 64*(i+1));
    code[i]<<=1;
  }
}
static inline void huffman_code_ash(huffman_code code, unsigned int n)
{
  while(n--){
    huffman_code_ash_1(code);
  }
}
#else
typedef unsigned long long huffman_code;
#define HUFFMAN_CODE_INIT(code) do{code = 0;}while(0);
#define HUFFMAN_CODE_COPY(dest, src) do{dest = src;}while(0);
#define huffman_code_inf(code) do{(code)++;}while(0);
static inline unsigned long long huffman_code_get_bit(huffman_code code, unsigned char n)
{
  if(n>0)n--;
  return (code & (1<<n))?1:0;
}
#define huffman_code_ash_1(code) do{code<<=1;}while(0);
#define huffman_code_ash(code, n) do{code<<=n;}while(0);
#endif

struct huffman_tree_node
{
  struct huffman_tree_node *left;
  struct huffman_tree_node *right;
  unsigned char weight;
  unsigned char code_length;//how many bits in code. max=16
  huffman_code code;
};

#define HUFFMAN_NODE_IS_LEAF(n) ((n)->left == (n)->right)

static void print_huffman_tree_node(struct huffman_tree_node *node);
static int huffman_tree_insert_node(struct huffman_tree_node *root, 
                                    struct huffman_tree_node *node,
                                    struct huffman_tree_node *free_nodes);

inline static unsigned int stream_get_bit(const unsigned char *stream, unsigned long bits)
{
  return (stream[bits>>3] >> (7-(bits&0x7))) & 1;
}

int huffman_match(const unsigned char *stream, unsigned long start_bits, struct huffman_tree_node *root, unsigned int *weight)
{
  unsigned long bits=start_bits;

  while(root && !HUFFMAN_NODE_IS_LEAF(root)){
    if(stream_get_bit(stream, bits)){
      root = root->right;
    }else{
      root = root->left;
    }
    bits++;
  }
  if(!root){
    fprintf(stderr, "Not match:root=NULL,bits=%lu\n", bits-start_bits);
    return -1;
  }
  *weight = root->weight;
  return bits-start_bits;
}
struct huffman_tree_node *build_huffman_tree(const unsigned char *ht_data)
{
  const unsigned char *ht_bit;
  const unsigned char *p, *w;
  struct huffman_tree_node *t = NULL, *node, *root;
  huffman_code code;
  unsigned char code_length=0;
  int count=0;

  for(p=ht_data; p<ht_data+16; p++){
    count += *p;
  }
  
  fprintf(stdout, "huffman_tree:leaf_count:%d\n", count);

  count = count*2;//Full tree?
  //TODO if it is not a Full tree.
  t = malloc(sizeof(*node) * count);
  if(t == NULL){
    perror("OOM");
    return NULL;
  }
  memset(t, '\0', sizeof(*node)*count);
  root = t;
  node = t+1;

  ht_bit = ht_data;
  w = ht_data+16;

  HUFFMAN_CODE_INIT(code);
  for(p=ht_bit; p<ht_bit+16; p++){
    count = *p;
    while(count > 0){
      if(code_length == p-ht_bit+1){
        //        code += 1;
        huffman_code_inf(code);
      }else if(code_length == 0){
        //First leaf.
        code_length = p-ht_bit+1;
      }else{
        //        code += 1;
        huffman_code_inf(code);
        huffman_code_ash(code, p-ht_bit+1-code_length);
        //        code <<= (p-ht_bit+1-code_length);
        code_length = p-ht_bit+1;
      }
      HUFFMAN_CODE_COPY(node->code, code);
      //      node->code = code;
      node->code_length = code_length;
      node->weight = *w++;
      
      count--;      
      node += (huffman_tree_insert_node(root, node, node+1) + 1);
    }
  }  
  fprintf(stdout, "nodes:%ld\n", node-root);
  for(t=root; t<node; t++){
    print_huffman_tree_node(t);
  }

  return root;
}
static int huffman_tree_insert_node(struct huffman_tree_node *root, 
                                    struct huffman_tree_node *node,
                                    struct huffman_tree_node *free_nodes)
{
  //  unsigned long long code = node->code;
  huffman_code code;
  unsigned char code_length = node->code_length;
  struct huffman_tree_node *t = free_nodes;

  HUFFMAN_CODE_COPY(code, node->code);
  while(code_length > 1){
    code_length-- ;
    //    if(code & (1<<code_length))
    if(huffman_code_get_bit(code, code_length+1)){
      if(root->right == NULL){
        t->code_length = node->code_length - code_length;
        HUFFMAN_CODE_COPY(t->code, root->code);
        huffman_code_ash_1(t->code);
        huffman_code_inf(t->code);
        //        t->code = (root->code<<1)|1;
        root->right = t;
        t++;
      }
      root = root->right;        
    }else{
      if(root->left == NULL){
        t->code_length = node->code_length - code_length;
        //        t->code = (root->code<<1);
        HUFFMAN_CODE_COPY(t->code, root->code);
        huffman_code_ash_1(t->code);
        root->left = t;
        t++;
      }
      root = root->left;        
    }
  }
  if(huffman_code_get_bit(node->code,1)){
    root->right = node;
  }else{
    root->left = node;
  }

  return t-free_nodes;
}
static void print_huffman_tree_node_info(struct huffman_tree_node *node)
{
  unsigned char i;
  fprintf(stdout, "\t%2d:", node->code_length);
  if(node->code_length > 0){
    for(i=node->code_length-1; i>0; i--){
      fprintf(stdout, "%c", huffman_code_get_bit(node->code, i+1)? '1':'0');
    }
    fprintf(stdout, "%llu %02x ", huffman_code_get_bit(node->code, 1), node->weight);
  }
}
static void print_huffman_tree_node(struct huffman_tree_node *node)
{
  if(!HUFFMAN_NODE_IS_LEAF(node)){
    // return;
  }
  print_huffman_tree_node_info(node);

  if(node->left != NULL){
    print_huffman_tree_node_info(node->left);
    fprintf(stdout, "[L] ");
  }
  if(node->right != NULL){
    print_huffman_tree_node_info(node->right);
    fprintf(stdout, "[R] ");
  }
  fprintf(stdout, "\n");
}
static void print_huffman_tree_r(struct huffman_tree_node *node)
{
  if(node){
    if(HUFFMAN_NODE_IS_LEAF(node)){
      print_huffman_tree_node(node);
    }else{
      print_huffman_tree_r(node->left);
      print_huffman_tree_r(node->right);
    }
  }
}
void print_huffman_tree(struct huffman_tree_node *root)
{
  /*
  fprintf(stdout, "nodes:%ld\n", node-root);
  for(t=root; t<node; t++){
    print_huffman_tree_node(t);
  }
  */
  

  print_huffman_tree_r(root);
}
void free_huffman_tree(struct huffman_tree_node *tree)
{
  free(tree);
}

#ifdef UNIT_TEST
static void test_build_huffman()
{
  unsigned char ht[]={
    0,2,2,0,5,1,6,1,
    0,0,0,0,0,0,0,0,
    0,1,0x11,2,0x21,3,0x31,0x41,
    0x12,0x51,0x61,0x71,0x81,0x91,0x22,0x13,0x32
  };
  struct huffman_tree_node *root=build_huffman_tree(ht);
  print_huffman_tree(root);
  free_huffman_tree(root);
}
static void test_huffman()
{
  unsigned char stream[]={0xd3,0x5e,0x6e,0x4d,0x35,0xf5,0x8a};

  struct huffman_tree_node *ac_root;
  test_build_huffman();
}
int main()
{
  test_huffman();
}
#endif
