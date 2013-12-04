#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <assert.h>

#include "huffman.h"

#define UNIT_TEST 1
#define TMP_FILE_DIR   "/home/weida/quicklisp/local-projects/jpeg-tmp"

struct jpeg_segment_struct
{
  unsigned char type_id;
  char desc[7];
  const unsigned char *address;
  unsigned long length;
  struct jpeg_segment_struct *next;  
};
struct jpeg_component
{
  unsigned char com_type;//1=Y,2=Cb,3=Cr,4=I,5=Q
  unsigned char com_hv;//(2,2),(1,1)
  unsigned char qt_quality;//8,16
  const unsigned char *ac_ht_data;
  const unsigned char *dc_ht_data;
  const unsigned char *qt_data;
};
struct jpeg_info
{
  unsigned long height;
  unsigned long width;
  unsigned int component_count;
  unsigned int vmax, hmax;
  const unsigned char *image_data;
  unsigned int image_data_length;
  struct jpeg_component components[3];
};

#define INTEGER_TO_UNSIGNED_CHAR(uc, x)           \
  do{                                             \
    if(((unsigned)(x)) >> 8){                     \
      if((x)<0)(uc)=0;                            \
      else (uc)=255;                              \
    }else{ (uc)=(x);}                             \
  }while(0);

inline static void YCbCr2RGB(double y, double cb, double cr, unsigned char *rgb)
{
  int r, g, b;
  y-=16;
  cr-=128;
  cb-=128;
  r=(298.082*y + 408.58*cr)/256.0;
  g=(298.082*y - 100.291*cb - 208.12*cr)/256.0;
  b=(298.082*y + 516.411*cb)/256.0;
  INTEGER_TO_UNSIGNED_CHAR(rgb[0], r);
  INTEGER_TO_UNSIGNED_CHAR(rgb[1], g);
  INTEGER_TO_UNSIGNED_CHAR(rgb[2], b);
}
inline static void RGB2YCbCr(int r, int g, int b, double *ycbcr)
{
  ycbcr[0]=(65.738*r + 129.057*g + 25.06*b)/256 + 16;
  ycbcr[1]=(-37.945*r - 74.494*g + 112.439*b)/256 + 128;
  ycbcr[2]=(112.439*r - 94.154*g - 18.28*b)/256 + 128;
}
#if UNIT_TEST
static void test_rgb2ycc()
{
  unsigned char rgb[64];
  unsigned char rgb2[64];
  double ycc[64];
  srand(time(NULL));
  int n=60;
  int i;
  for(i=0; i<n; i++){
    rgb[i]=rand()%256;
  }
  for(i=0; i<n; i+=3){
    RGB2YCbCr(rgb[i], rgb[i+1], rgb[i+2], ycc+i);
  }
  for(i=0; i<n; i+=3){
    YCbCr2RGB(ycc[i], ycc[i+1], ycc[i+2], rgb2+i);
  }
  int sum=0;
  for(i=0; i<n; i++){
    if(abs(rgb[i]-rgb2[i]) <= 1){
      printf("==\n");sum++;
    }else{
      printf("%d,%d\n", rgb[i], rgb2[i]);
    }
  }
  printf("%d/%d\n", sum, n);
}
static void test_integer_to_unsigned_char()
{
  int a;
  unsigned char c;
  a=-1;
  INTEGER_TO_UNSIGNED_CHAR(c, a);
  printf("%d %d\n", c, a);
  a=256;
  INTEGER_TO_UNSIGNED_CHAR(c, a);
  printf("%d %d\n", c, a);
  a=0;
  INTEGER_TO_UNSIGNED_CHAR(c, a);
  printf("%d %d\n", c, a);
  a=255;
  INTEGER_TO_UNSIGNED_CHAR(c, a);
  printf("%d %d\n", c, a);
  a=128;
  INTEGER_TO_UNSIGNED_CHAR(c, a);
  printf("%d %d\n", c, a);
}
#endif
inline static void stream_iter_init(unsigned long *iter)
{
  *iter = 0;
}
inline static int stream_iter_next_bit(const unsigned char *stream, unsigned long *iter)
{
  unsigned int bytes=*iter>>3;
  unsigned int bits_in_byte=7-(*iter&7);
  (*iter)++;
  return (stream[bytes] >> bits_in_byte) & 1;
}

inline static unsigned int stream_iter_next_n_bit(const unsigned char *stream, unsigned long *iter, unsigned int n)
{
  unsigned int r=0;
  while(n > 0){
    r = (r<<1)|stream_iter_next_bit(stream, iter);
    n--;
  }
  return r;
}
inline static int translate_nbit_to_number(unsigned int code, unsigned int n)
{
  if(code >> (n-1)){
    return code;
  }else{
    return -(code^((1<<n)-1));
  }
}

static const char *get_segment_desc(unsigned char type_id);
static int jpeg_scan(const char *file_name);

int main(int argc, char ** argv)
{
  int rc;
  if(argc < 2){
    fprintf(stderr, "missing file name\n");
    return 0;
  }

  rc = jpeg_scan(argv[1]);
  return rc;
}
static void safe_close(int fd)
{
  if(close(fd) < 0){
    perror("close");
  }
}

static void print_segment_sof0(struct jpeg_segment_struct *seg)
{
  const unsigned char * p = seg->address;
  if(seg->type_id != 0xc0){
    return;
  }
  int ncom,i;
  fprintf(stdout, "\tp=%d,h=%d,w=%d\n", *p, *(p+1)*256+*(p+2), *(p+3)*256+*(p+4));
  p+=5;
  ncom = *p;
  fprintf(stdout, "\tcom:%d\n", *p);
  p++;
  for(i=0; i<ncom; i++){
    fprintf(stdout, "\t\tid=%d ", *p);
    p++;
    fprintf(stdout, "\tv:%d,h:%d ", *p&0xf, (*p&0xf0)>>4);
    p++;
    fprintf(stdout, "\tdqt:%d\n", *p);
    p++;
  }
}
static void print_segment_dht(struct jpeg_segment_struct *seg)
{
  const unsigned char * p = seg->address, *q;
  int n;
  int i;
  if(seg->type_id != 0xc4){
    return;
  }
  fprintf(stdout, "\tht:%d,%s\n", *p&0xf, *p&0xf0?"AC":"DC");
  p++;
  fprintf(stdout, "\t");
  n = 0;
  q = p;
  for(; p<q+16; p++){
    fprintf(stdout, "%3d ", *p);
    n += *p;
  }
  i = 0;
  fprintf(stdout, "\t\tn=%d\n", n);
  q = p;
  for(; p<q+n; p++){
    if(i++%16 == 0){
      fprintf(stdout, "\n\t");
    }
    fprintf(stdout, "%3d ", *p);
  }
  fprintf(stdout, "\n");
}
static void print_segment_sos(struct jpeg_segment_struct *seg)
{
  const unsigned char * p = seg->address;
  int ncom,i;

  if(seg->type_id != 0xda){
    return;
  }  
  ncom = *p;
  fprintf(stdout, "\tcom=%d\n", *p);
  p++;
  for(i=0; i<ncom; i++){
    fprintf(stdout, "\t\tid=%d ", *p);
    p++;
    fprintf(stdout, "\tht=%d(AC),%d(DC)\n", *p&0xf, (*p&0xf0)>>4);
    p++;
  }
  fprintf(stdout, "\t%02x %02x %02x\n", *p, *(p+1), *(p+2));
}
static void print_segment_dqt(struct jpeg_segment_struct *seg)
{
  const unsigned char * p = seg->address;
  int i;
  if(seg->type_id != 0xdb){
    return;
  }
  fprintf(stdout, "\tid:%d, p=%d\n", *p&0xf, (*p&0xf0)>>4);
  p++;
  i=0;
  for(; p<seg->address+seg->length-2; p++){
    if(i++%16 == 0){
      fprintf(stdout, "\n\t");
    }
    fprintf(stdout, "%02x ", *p);
  }
  fprintf(stdout, "\n");
}
static void print_segment_app0(struct jpeg_segment_struct *seg)
{
  const unsigned char * p = seg->address;
  if(seg->type_id != 0xe0){
    return;
  }
  fprintf(stdout, "\t%s\n", p);
  p+=5;
  fprintf(stdout, "\tversion:%d-%d\n", *p, *(p+1));
  p+=2;
  fprintf(stdout, "\tp=%d,x=%d,y=%d\n", *p, *(p+1)*256+*(p+2), *(p+3)*256+*(p+4));
  p+=5;
  fprintf(stdout, "\tx=%d,y=%d\n", *p, *(p+1));
}
static const char *get_segment_desc(unsigned char type_id)
{
  switch(type_id){
  case 0xd8:        return "SOI";
  case 0xd9:return "EOI";
  case 0xc0:return "SOF0";
  case 0xc1:return "SOF1";
  case 0xc4:return "DHT";
  case 0xda:return "SOS";
  case 0xdb:return "DQT";
  case 0xdd:return "DRI";
  case 0xe0:return "APP0";
  case 0xfe:return "COM";
  case 0xdc:return "DNL";
  default:
    fprintf(stdout, "Unknown!!!%x\n", type_id);
  }
  return "Unkn";
}
static int jpeg_scan_stream_buf_count_segment(const unsigned char *buf, unsigned long length, 
                                              unsigned int *address_off, unsigned long max_segment)
{
  unsigned long i;
  int seg_count = 0, st=0;

  for(i=0; i<length; i++){
    if(buf[i] == 0xff){
      st=1;
      continue;
    }
    if(st == 1){
      if(buf[i] == 0x0){
        ;//ignore
      }else{
        address_off[seg_count++] = i;
        if(seg_count >= max_segment){
          fprintf(stderr, "Too many segments!\n");
          return -1;
        }
      }
      st=0;
    }
  }
  return seg_count;
}
static struct jpeg_segment_struct *jpeg_scan_stream_buf_split(const unsigned char *buf, unsigned long length)
{
  unsigned int address[1024]={0};
  int seg_count = 0, n;
  struct jpeg_segment_struct *segments = NULL;
  const unsigned char *p;

  seg_count = jpeg_scan_stream_buf_count_segment(buf, length, address, 1024);
  if(seg_count < 0){
    return NULL;
  }

  segments = (struct jpeg_segment_struct*)malloc(sizeof(*segments)*seg_count);
  if(segments == NULL){
    fprintf(stderr, "OOM\n");
    return NULL;
  }
  fprintf(stdout, "set_count:%d\n", seg_count);

  for(n=0; n<seg_count; n++){
    p = buf+address[n];
    segments[n].type_id = *p;
    strncpy(segments[n].desc, get_segment_desc(*p), sizeof(segments[n].desc));
    p++;
    if(segments[n].type_id == 0xd8 || segments[n].type_id == 0xd9){
      segments[n].length = 0;
      segments[n].address= p-2;
    }else{
      segments[n].length = *p*256+*(p+1);
      segments[n].address = p+2;
    }
    segments[n].next = segments+n+1;
  }
  segments[n-1].next = NULL;

  return segments;
}
static const unsigned char *find_qt_address(struct jpeg_segment_struct *segments, unsigned char qt_id, unsigned char *qt_quality)
{
  struct jpeg_segment_struct *s;
  for(s=segments; s; s=s->next){
    if(s->type_id == 0xdb && (*s->address&0xf) == qt_id){
      *qt_quality = (((*s->address&0xf0)>>4)+1)*8;
      return s->address+1;
    }
  }
  return NULL;
}
static const unsigned char *find_ht_address(struct jpeg_segment_struct *segments, unsigned char ht_id, int is_ac)
{
  struct jpeg_segment_struct *s;
  if(is_ac){
    ht_id |= 0x10;
  }
  for(s=segments; s; s=s->next){
    if(s->type_id == 0xc4 && *s->address == ht_id){
      return s->address+1;
    }
  }
  return NULL;
}
static int find_component(struct jpeg_component *com, int ncom, unsigned char com_type)
{
  int i;

  for(i=0; i<ncom; i++){
    if(com[i].com_type == com_type){
      return i;
    }
  }
  return -1;
}
static int jpeg_fill_info_with_sof0(struct jpeg_segment_struct *segments, struct jpeg_info *info)
{
  struct jpeg_segment_struct *s;
  const unsigned char *p;
  struct jpeg_component *com;
  int i;

  for(s=segments; s; s=s->next){
    if(s->type_id == 0xc0){
      p = s->address+1;
      info->height = *p*256+*(p+1);
      p+=2;
      info->width = *p*256+*(p+1);
      p+=2;
      info->component_count = *p++;
      com = info->components;
      info->hmax = info->vmax = 0;
      for(i=0; i<info->component_count; i++){
        com[i].com_type = *p++;
        if((*p&0xf) > info->vmax){
          info->vmax = *p&0xf;
        }
        if((*p&0xf0)>>4 > info->hmax){
          info->hmax = (*p&0xf0)>>4;
        }
        com[i].com_hv = *p++;
        com[i].qt_data = find_qt_address(segments, *p, &com[i].qt_quality);
        p++;
      }
    }
  }
  return 0;
}
static int jpeg_fill_info_stream_filter(struct jpeg_segment_struct *segments, struct jpeg_info *info, unsigned char *start)
{
  struct jpeg_segment_struct *s;
  const unsigned char *p;

  for(s=segments; s; s=s->next){
    if(s->type_id == 0xd9){
      break;
    }
  }
  
  info->image_data = start;
  for(; start < s->address; start++){
    if(*start == 0xff){ break; }
  }
  p = start;
  for(; p < s->address; p++){
    if(*p == 0xff){
      if(*(p+1) == 0x0){
        *start++ = *p++;
      }else if(*(p+1) == 0xff){
        fprintf(stdout, "0xff,0xff\n");
      }else{
        fprintf(stderr, "unsupport:%x\n", *(p+1));
        return -1;
      }
    }else{
      *start++ = *p;
    }
  }
  info->image_data_length=start-info->image_data;
  fprintf(stdout, "image_data_length=%d\n", info->image_data_length);
  return 0;
}
static int jpeg_fill_info_with_sos(struct jpeg_segment_struct *segments, struct jpeg_info *info)
{
  struct jpeg_segment_struct *s;
  const unsigned char *p;
  struct jpeg_component *com = info->components;
  int i, ncom = info->component_count;
    
  for(s=segments; s; s=s->next){
    if(s->type_id == 0xda){
      break;
    }
  }
  p = s->address;
  if(ncom != *p){
    fprintf(stderr, "sof0.ncom != sos.ncom [%d,%d]\n", ncom, *p);
    return -1;
  }
  p++;
  for(; p<s->address+1+ncom*2; p+=2){
    i = find_component(com, ncom, *p);
    if(i<0){
      fprintf(stderr, "sof0 and sos not match\n");
      return -1;
    }
    com[i].ac_ht_data = find_ht_address(segments, *(p+1)&0xf, 1);
    com[i].dc_ht_data = find_ht_address(segments, (*(p+1)&0xf0)>>4, 0);
  }
  p = s->address+1+ncom*2;
  if(*p != 0x00 || *(p+1) != 0x3f || *(p+2) != 0x00){
    fprintf(stderr, "Flag:00 3f 00 NOT find.\n");        
  }

  return jpeg_fill_info_stream_filter(segments, info, p+3);
}
static void print_jpeg_info(struct jpeg_info *info)
{
  struct jpeg_component *com = info->components;
  int i;
  
  fprintf(stdout, "width=%lu, height=%lu\n", info->width, info->height);
  fprintf(stdout, "hmax=%d, vmax=%d\n", info->hmax, info->vmax);
  for(i=0; i<info->component_count; i++){
    fprintf(stdout, "\ttype:%d (h:v)=%02x qt-off:%d ac-off:%d dc-off:%d\n", 
            com[i].com_type, com[i].com_hv, 
            info->image_data - com[i].qt_data,
            info->image_data - com[i].ac_ht_data,
            info->image_data - com[i].dc_ht_data);
  }
}
static void jpeg_decode_image_data_du_idct(double *du_result, int *du_table, unsigned int du_size)
{
  double tmp[du_size*du_size];
  unsigned int i;
  for(i=0; i<du_size*du_size; i++){
    tmp[i]=du_table[i];
  }
  idct(du_result, tmp, du_size);
}
static void jpeg_decode_image_data_du_qt(int *du_table, unsigned int du_size, int qt_quality, const unsigned char *qt_data)
{
  int value;
  unsigned int i;

  //  fprintf(stdout, "qt_quality=%d\n", qt_quality);

  for(i=0; i<du_size*du_size; i++){
    if(qt_quality == 8){
      value = qt_data[i];
    }else{
      value = ((const unsigned short*)qt_data)[i];
    }
    
    du_table[i] = du_table[i]*value;
  }
}
static void jpeg_decode_image_data_du_odd_line_negative(int *du_table, unsigned int du_size)
{
  unsigned int i, j;
  for(i=1; i<du_size; i+=2){
    for(j=0; j<du_size; j++){
      du_table[i*du_size+j] = -du_table[i*du_size+j];
    }
  }
}
static void jpeg_decode_image_data_du_zig_zag(int *du_table, unsigned int du_size)
{
  int du2[du_size*du_size];
  memcpy(du2, du_table, du_size*du_size*sizeof(*du_table));
  zig_zag(du_table, du2, du_size, 1);
}
static int jpeg_decode_image_data_du_huffman(const unsigned char *stream, unsigned long *iter,
                                             struct huffman_tree_node *ac, struct huffman_tree_node *dc,
                                             int *ac_last, int *du_table, unsigned int du_size)
{
  int bits, i;
  unsigned int value, weight;

  memset(du_table, '\0', du_size*du_size);

  bits = huffman_match(stream, *iter, ac, &weight);
  if(bits <= 0){
    fprintf(stderr, "huffman_match()<0\n");
    return -1;
  }

  *iter += bits;
  value = stream_iter_next_n_bit(stream, iter, weight);

  du_table[0] = translate_nbit_to_number(value, weight) + *ac_last;
  *ac_last = du_table[0];

  for(i=1; i<du_size*du_size; i++){
    bits = huffman_match(stream, *iter, dc, &weight);
    if(bits <= 0){
      fprintf(stderr, "huffman_match()<0\n");
      return -1;
    }
    *iter += bits;
    if(weight == 0){
      //      fprintf(stdout, "weight=0\n");
      break;
    }
    i += weight & 0xf0;
    value = stream_iter_next_n_bit(stream, iter, weight&0xf);
    du_table[i]=translate_nbit_to_number(value, weight&0xf);
  }

  return 0;
}
static void print_du(double *du_buf, unsigned int du_size, const char *prompt)
{
  /*
  unsigned int i, j;
  fprintf(stdout, "%s", prompt);
  for(i=0; i<du_size; i++){
    for(j=0; j<du_size; j++){
      fprintf(stdout, "%.f ", du_buf[i*du_size+j]);
    }
    fprintf(stdout, "\n");
  }
  */
}
static void print_du_int(int *du_buf, unsigned int du_size, const char *prompt)
{
  /*
  unsigned int i, j;
  fprintf(stdout, "%s", prompt);
  for(i=0; i<du_size; i++){
    for(j=0; j<du_size; j++){
      fprintf(stdout, "%d ", du_buf[i*du_size+j]);
    }
    fprintf(stdout, "\n");
  }
  */
}

static int jpeg_decode_image_data_du(struct jpeg_component *com,
                                     const unsigned char *stream, unsigned long *iter,
                                     int *ac_last,
                                     double *du_result, unsigned int du_size)
{
  struct huffman_tree_node *ac, *dc;
  int rc;
  int du[du_size*du_size];

  ac = build_huffman_tree(com->ac_ht_data);
  //print_huffman_tree(ac);
  dc = build_huffman_tree(com->dc_ht_data);
  //  print_huffman_tree(dc);
  rc = jpeg_decode_image_data_du_huffman(stream, iter, ac, dc, ac_last, du, du_size);
  free_huffman_tree(ac);
  free_huffman_tree(dc);
  if(rc < 0){
    //    fprintf(stderr, "");
    return rc;
  }

  print_du_int(du, du_size, "After:huffman\n");

  jpeg_decode_image_data_du_qt(du, du_size, com->qt_quality, com->qt_data);

  print_du_int(du, du_size, "After:qt\n");
  jpeg_decode_image_data_du_zig_zag(du, du_size);

  print_du_int(du, du_size, "After:zig_zag\n");
  jpeg_decode_image_data_du_odd_line_negative(du, du_size);

  print_du_int(du, du_size, "After:odd_line_negative\n");
  jpeg_decode_image_data_du_idct(du_result, du, du_size);

  print_du(du_result, du_size, "After:idct\n");
  return 0;
}
static int jpeg_decode_image_data_du_y(struct jpeg_info *info,
                                     const unsigned char *stream, unsigned long *iter,
                                     int *ac_last,
                                     double *du_result, unsigned int du_size)
{
  return jpeg_decode_image_data_du(&info->components[0], stream, iter, ac_last, du_result, du_size);
}
static int jpeg_decode_image_data_du_cr(struct jpeg_info *info,
                                     const unsigned char *stream, unsigned long *iter,
                                     int *ac_last,
                                     double *du_result, unsigned int du_size)
{
  return jpeg_decode_image_data_du(&info->components[1], stream, iter, ac_last, du_result, du_size);
}
static int jpeg_decode_image_data_du_cb(struct jpeg_info *info,
                                     const unsigned char *stream, unsigned long *iter,
                                     int *ac_last,
                                     double *du_result, unsigned int du_size)
{
  return jpeg_decode_image_data_du(&info->components[2], stream, iter, ac_last, du_result, du_size);
}

static int jpeg_decode_image_data_mcu(struct jpeg_info *info,
                                      const unsigned char *stream, unsigned long *iter, 
                                      int *y_ac_last, int *cr_ac_last, int *cb_ac_last,
                                      unsigned int y_du_count, unsigned int cr_du_count, unsigned int cb_du_count,
                                      unsigned char *rgb_buf)
{
  double *du_buf;
  unsigned int i, j, k, x, y, z;
  unsigned int du_size=8;
  unsigned int cr_begin, cb_begin;
  int rc;

  du_buf = (double *)malloc(sizeof(double)*du_size*du_size*(y_du_count+cr_du_count+cb_du_count));
  if(du_buf == NULL){
    fprintf(stderr, "OOM\n");
    return -1;
  }
  k=0;
  for(i=0; i<y_du_count; i++){
    rc = jpeg_decode_image_data_du_y(info, stream, iter, y_ac_last, du_buf+k, du_size);
    if(rc < 0){
      fprintf(stderr, "du_y()failed\n");
      break;
    }
    //    print_du(du_buf+k, du_size);
    //    fprintf(stdout, "==k=%d==\n", k);
    k += du_size*du_size;
  }
  //  fprintf(stdout, "du_y()end,k=%d\n",k);
  cr_begin=k;
  for(i=0; i<cr_du_count; i++){
    rc = jpeg_decode_image_data_du_cr(info, stream, iter, cr_ac_last, du_buf+k, du_size);
    if(rc < 0){
      fprintf(stderr, "du_cr()failed\n");
      break;
    }
    k += du_size*du_size;
  }
  //  fprintf(stdout, "du_cr()end,k=%d\n",k);
  cb_begin=k;
  for(i=0; i<cb_du_count; i++){
    rc = jpeg_decode_image_data_du_cb(info, stream, iter, cb_ac_last, du_buf+k, du_size);
    if(rc < 0){
      fprintf(stderr, "du_cb()failed\n");
      break;
    }
    k += du_size*du_size;
  }
  //  fprintf(stdout, "du_cb()end,k=%d\n",k);

  for(z=0; z<y_du_count*du_size*du_size; z++){
    x=z/16;
    y=z%16;
    i=x%du_size;
    j=y%du_size;
    k=((x/du_size)<<1)|(y/du_size);
    YCbCr2RGB(du_buf[k*du_size*du_size+i*du_size+j],
              du_buf[cb_begin+(i/2+(x/du_size)*4)*du_size+j/2+(y/du_size)*4], 
              du_buf[cr_begin+(i/2+(x/du_size)*4)*du_size+j/2+(y/du_size)*4],
              rgb_buf+z*3);
  }

  free(du_buf);
  return rc;
}
static int jpeg_flush_rgb_buf(const unsigned char *rgb_buf, int buf_idx,
                              unsigned int mcu_count, 
                              unsigned int mcu_width, unsigned int mcu_height,
                              unsigned int image_width)
{
  int fd;
  char filename[512]= {'\0'};
  unsigned int i, line;
  unsigned int mcu_pixes = mcu_width*mcu_height;
  unsigned int drop_pixes = (mcu_count*mcu_width)-image_width;
  const unsigned char *p;  

  sprintf(filename, "%s/%d.rgb", TMP_FILE_DIR, buf_idx);
  fd = open(filename, O_WRONLY|O_CREAT|O_TRUNC, 0660);
  if(fd < 0){
    fprintf(stderr, "open(%s) failed\n", filename);
    return -1;
  }
  for(line=0; line<mcu_height; line++){
    for(i=0; i<mcu_count-1; i++){
      p = rgb_buf + i*mcu_pixes*3 + line*mcu_width*3;
      write(fd, p, mcu_width*3);
    }
    p = rgb_buf + i*mcu_pixes*3 + line*mcu_width*3;
    write(fd, p, (mcu_width-drop_pixes)*3);
  }

  safe_close(fd);
  return 0;
}
static int jpeg_decode_image_data(struct jpeg_info *info, const unsigned char *buf, unsigned long length)
{
  int w_mcu_count, h_mcu_count;
  int mcu_width, mcu_height;
  int du_count_per_mcu;
  int rc, i, j;
  const unsigned char *stream;
  unsigned long iter=0;
  int y_ac_last, cb_ac_last, cr_ac_last;
  
  unsigned char *rgb_buf = NULL;

  mcu_width = info->hmax*8;
  w_mcu_count = info->width/mcu_width;
  if(info->width%mcu_width > 0){
    w_mcu_count++;
  }
  mcu_height = info->vmax*8;
  h_mcu_count = info->height/mcu_height;
  if(info->height%mcu_height > 0){
    h_mcu_count++;
  }

  du_count_per_mcu = info->hmax * info->vmax;
  if(du_count_per_mcu != 1 && du_count_per_mcu != 4){
    fprintf(stderr, "unknown hmax:%d,vmax:%d\n", info->hmax, info->vmax);
  }

  stream = info->image_data;
  stream_iter_init(&iter);

  rgb_buf = (unsigned char*)malloc(3*mcu_width*mcu_height*w_mcu_count);
  if(rgb_buf == NULL){
    fprintf(stderr, "OOM\n");
    return -1;
  }

  y_ac_last = cr_ac_last = cb_ac_last = 0;

  i=0;
  for(i=0; i<h_mcu_count; i++){

    for(j=0; j<w_mcu_count; j++){
      rc = jpeg_decode_image_data_mcu(info, stream, &iter, 
                                 &y_ac_last, &cr_ac_last, &cb_ac_last, 
                                 du_count_per_mcu, 1, 1,
                                 rgb_buf+mcu_height*mcu_width*3*j);
    }
    jpeg_flush_rgb_buf(rgb_buf, i, w_mcu_count, mcu_width, mcu_height, info->width);
  }
  fprintf(stdout, "iter=%d, image-data=%d, total=%d\n", iter, info->image_data-buf, length);
  free(rgb_buf);
  return rc;
}
static int jpeg_scan_stream_buf(int fd, const unsigned char *buf, unsigned long length)
{
  struct jpeg_segment_struct *segments = NULL, *s;
  struct jpeg_info info;
  //  const unsigned char *p;

  segments = jpeg_scan_stream_buf_split(buf, length);
  if(segments == NULL){
    return -1;
  }
  for(s=segments; s; s=s->next)
  {
    fprintf(stdout, "[%x][%s]:%lu:%lu\n", s->type_id, s->desc, s->address-buf, s->length);
    print_segment_app0(s);
    print_segment_dqt(s);
    print_segment_sof0(s);
    print_segment_dht(s);
    print_segment_sos(s);
  }
  jpeg_fill_info_with_sof0(segments, &info);
  if(jpeg_fill_info_with_sos(segments, &info) < 0){
    free(segments);
    return -1;
  }
  print_jpeg_info(&info);
  jpeg_decode_image_data(&info, buf, length);

  free(segments);
  return 0;
}

static int jpeg_scan_stream(int fd)
{
  struct stat st;
  int rc;
  unsigned char *buf=NULL;
  unsigned long size=0;
  
  rc = fstat(fd, &st);
  if(rc < 0){
    fprintf(stderr, "fstat()failed\n");
    return rc;
  }
  size = st.st_size;
  fprintf(stdout, "File size:%lu\n", size);

  while(size%8){size++;}

  buf = malloc(size);
  if(buf == NULL){
    fprintf(stderr, "malloc(%lu) failed!\n", size);
    return -1;
  }
  rc = read(fd, buf, size);
  if(rc < 0 || rc != st.st_size){
    fprintf(stderr, "read(%lu)=%d\n", st.st_size, rc);
    free(buf);
    return -1;
  }
  rc = jpeg_scan_stream_buf(fd, buf, rc);
  free(buf);
  return rc;
}
static int jpeg_scan(const char *file_name)
{
  int fd;
  int rc;
  fd = open(file_name, O_RDONLY);
  if(fd < 0){
    fprintf(stderr, "open(%s) failed!\n", file_name);
    return -1;
  }
  rc = jpeg_scan_stream(fd);
  safe_close(fd);
  return rc;
}


