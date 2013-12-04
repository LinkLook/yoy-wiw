#ifndef _HUFFMAN_H_
#define _HUFFMAN_H_

struct huffman_tree_node;

/**
   @param stream
   @param start_bits
   @param root
   @param *weight if matched, *weight=node->weight;
   @return <0 if none leaf node was matched; else, return how many bits matched.
 */
int huffman_match(const unsigned char *stream, unsigned long start_bits, struct huffman_tree_node *root, unsigned int *weight);

struct huffman_tree_node *build_huffman_tree(const unsigned char *ht_data);
void free_huffman_tree(struct huffman_tree_node *tree);
void print_huffman_tree(struct huffman_tree_node *root);
#endif
