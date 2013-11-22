/*=============================================================================
#     FileName: v4l2.c
#         Desc: this program aim to get image from USB camera,
#               used the V4L2 interface.
#       Author: LiXiaoming
#        Email: lixiaoming5700@gmail.com
#     HomePage: http://www.cnblogs.com/lixiaoming90
#      Version: 0.0.1
#   LastChange: 2012-08-22 15:52:37
#      History:
=============================================================================*/
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <stdlib.h>
#include <linux/types.h>
#include <linux/videodev2.h>
#include <malloc.h>
#include <math.h>
#include <string.h>
#include <sys/mman.h>
#include <errno.h>
#include <assert.h>
 
#define FILE_VIDEO  "/dev/video0"
#define JPG_FILE_DIR "/home/weida/quicklisp/tmpimage/"

static char dest_images_file_name[64]={'\0'};
 
typedef struct{
    void *start;
    int length;
}BUFTYPE;

static BUFTYPE *usr_buf;
static unsigned int n_buffer = 0;
static void deinit_mmap()
{
    unsigned int i;
    for(i = 0;i < n_buffer; i++)    {
        if(-1 == munmap(usr_buf[i].start,usr_buf[i].length)){
          printf("munmap(%p,%d) failed. But take no effert.\n", usr_buf[i].start, usr_buf[i].length);
        }
    }
    free(usr_buf);
}
static int init_mmap_buffer(int fd, struct v4l2_requestbuffers *reqbufs)
{
  int i;
      struct v4l2_buffer buf;
          printf("n_buffer = %d\n", n_buffer);
    usr_buf = malloc(reqbufs->count * sizeof(*usr_buf));
    if(usr_buf == NULL){
        printf("Out of memory\n");
        exit(-1);
    }
    memset(usr_buf, '\0', reqbufs->count*sizeof(*usr_buf));

    //map kernel cache to user process 
    for(i = 0; i < reqbufs->count; ++i){
        //stand for a frame
        memset(&buf, 0, sizeof(buf));
        buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        buf.memory = V4L2_MEMORY_MMAP;
        buf.index = i;
         
        //check the information of the kernel cache requested 
        if(-1 == ioctl(fd,VIDIOC_QUERYBUF,&buf))        {
            perror("Fail to ioctl : VIDIOC_QUERYBUF");
            exit(EXIT_FAILURE);
        }
        printf("buf.length=%d\n", buf.length);
 
        usr_buf[i].length = buf.length;
        usr_buf[i].start = 
            (char *)mmap(
                    NULL,
                    buf.length,
                    PROT_READ | PROT_WRITE,
                    MAP_PRIVATE | MAP_ANON,
                    -1,
                    buf.m.offset
                );
        if(MAP_FAILED == usr_buf[i].start)        {
            perror("Fail to mmap");
            exit(EXIT_FAILURE);
        }
        //place the kernel cache to a queue
        if(-1 == ioctl(fd, VIDIOC_QBUF, &buf)){
          perror("Fail to ioctl 'VIDIOC_QBUF'");
            exit(EXIT_FAILURE);
        }

    }

    return 0;
} 
//set video capture ways(mmap)
static int init_mmap(int fd)
{
    //to request frame cache, contain requested counts
    struct v4l2_requestbuffers reqbufs;

    //request V4L2 driver allocation video cache
    //this cache is locate in kernel and need mmap mapping
    memset(&reqbufs, 0, sizeof(reqbufs));
    reqbufs.count = 4;
    reqbufs.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    reqbufs.memory = V4L2_MEMORY_MMAP;
 
    if(-1 == ioctl(fd,VIDIOC_REQBUFS,&reqbufs)){
        perror("Fail to ioctl 'VIDIOC_REQBUFS'");
        exit(EXIT_FAILURE);
    }
 
    n_buffer = reqbufs.count;
    return init_mmap_buffer(fd, &reqbufs);
}

static int init_camera_device_fmt(int fd)
{
    //detail control value
    struct v4l2_fmtdesc fmt;
    //get the format of video supply
    memset(&fmt, 0, sizeof(fmt));
    fmt.index = 0;
    //supply to image capture
    fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    // show all format of supply
    printf("Support format:\n");
    while(ioctl(fd, VIDIOC_ENUM_FMT, &fmt) == 0){
        fmt.index++;
        printf("pixelformat = ''%c%c%c%c''\n", fmt.pixelformat & 0xFF, (fmt.pixelformat >> 8) & 0xFF,(fmt.pixelformat >> 16) & 0xFF, (fmt.pixelformat >> 24) & 0xFF);
        printf("description = ''%s''\n", fmt.description);
    }
    return 0;
}
static int init_camera_device_cap(int fd)
{
    //decive fuction, such as video input
    struct v4l2_capability cap;
    int ret;
    //check video decive driver capability
    ret = ioctl(fd, VIDIOC_QUERYCAP, &cap);
    if(ret < 0){
        perror("Fail to ioctl VIDEO_QUERYCAP");
        exit(EXIT_FAILURE);
    }
    printf("driver:%s\n", cap.driver);
    printf("card:%s\n", cap.card);
    printf ("bus_info:%s,version:%d\n",cap.bus_info, cap.version);
 
    //judge wherher or not to be a video-get device
    if(!(cap.capabilities & V4L2_BUF_TYPE_VIDEO_CAPTURE))
    {
        printf("The Current device is not a video capture device\n");
        exit(-1);
    }
 
    //judge whether or not to supply the form of video stream
    if(!(cap.capabilities & V4L2_CAP_STREAMING))
    {
        printf("The Current device does not support streaming i/o\n");
        exit(EXIT_FAILURE);
    }
    return 0;
}
static int init_camera_device_tv_fmt(int fd)
{
      //frame format
    struct v4l2_format tv_fmt;
    //set the form of camera capture data
    tv_fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    tv_fmt.fmt.pix.width = 680;
    tv_fmt.fmt.pix.height = 480;
    tv_fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_MJPEG;
    tv_fmt.fmt.pix.field = V4L2_FIELD_INTERLACED;
    if (ioctl(fd, VIDIOC_S_FMT, &tv_fmt)< 0) {
        printf("VIDIOC_S_FMT\n");
        exit(-1);
        close(fd);
    }
    return 0;
}
//initial camera device 
static int init_camera_device(int fd)
{
    //video standard,such as PAL,NTSC
    struct v4l2_standard std; 
    //check control
    struct v4l2_queryctrl query;
 
    init_camera_device_fmt(fd);
    init_camera_device_cap(fd);
    init_camera_device_tv_fmt(fd);
    //initial video capture way(mmap)
    init_mmap(fd);
    return 0;
}
 
static int open_camera_device()
{
    int fd;
    //open video device with block
    fd = open(FILE_VIDEO, O_RDONLY);
    if(fd < 0){
        perror(FILE_VIDEO);
        exit(EXIT_FAILURE);
    }
    return fd;
}
static void stop_capture(int fd)
{
    enum v4l2_buf_type type;
    type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    if(-1 == ioctl(fd,VIDIOC_STREAMOFF,&type))
    {
        perror("Fail to ioctl 'VIDIOC_STREAMOFF'");
        exit(EXIT_FAILURE);
    }
    return;
}
static int start_capture(int fd)
{
     enum v4l2_buf_type type;
     //start capture data
    type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    if(-1 == ioctl(fd, VIDIOC_STREAMON, &type)){
         perror("VIDIOC_STREAMON");
        close(fd);
        exit(EXIT_FAILURE);
    }
    return 0;
}
 
static int process_image(void *addr, int length)
{
  int dest_file_fd;
  static int num = 0;
    int len;
    
    sprintf(dest_images_file_name, "%s/frame-%d.yuv", JPG_FILE_DIR, num++);

    if((dest_file_fd = open(dest_images_file_name, O_WRONLY|O_CREAT|O_TRUNC, 0660)) < 0){
      printf("create dest image file failed:%s\n", dest_images_file_name);
    }else{
      if((len = write(dest_file_fd, addr, length)) < length){
        printf("write(%d)=%d\n", length, len);
      }
      close(dest_file_fd);
    }
    return 0;
}
 
static int read_frame(int fd)
{
    struct v4l2_buffer buf;
    unsigned int i;
    memset(&buf, 0, sizeof(buf));
    buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory = V4L2_MEMORY_MMAP;
    //put cache from queue
    if(-1 == ioctl(fd, VIDIOC_DQBUF,&buf)){
        perror("Fail to ioctl 'VIDIOC_DQBUF'");
        exit(EXIT_FAILURE);
    }

    printf("buf.index=%d, fd=%d\n", buf.index, fd);
    assert(buf.index < n_buffer);
    //read process space's data to a file
    process_image(usr_buf[buf.index].start, usr_buf[buf.index].length);
    printf("buf.index=%d, fd=%d\n", buf.index, fd);
    if(-1 == ioctl(fd, VIDIOC_QBUF,&buf)){
        perror("Fail to ioctl 'VIDIOC_QBUF'");
        exit(EXIT_FAILURE);
    }
    return 1;
}
 
static int mainloop(int fd)
{ 
    int count = 10;
 
    while(count-- > 0)
    {
        for(;;)
        {
            fd_set fds;
            struct timeval tv;
            int r;
 
            FD_ZERO(&fds);
            FD_SET(fd,&fds);
 
            /*Timeout*/
            tv.tv_sec = 2;
            tv.tv_usec = 0;
            r = select(fd + 1,&fds,NULL,NULL,&tv);
 
            if(-1 == r)
            {
                if(EINTR == errno)
                    continue;
                perror("Fail to select");
                exit(EXIT_FAILURE);
            }
 
            if(0 == r)
            {
                fprintf(stderr,"select Timeout\n");
                exit(-1);
            }
 
            if(read_frame(fd))
                break;
        }
    }
    return 0;
}
 
static void close_camera_device(int fd)
{
    if(-1 == close(fd))    {
        perror("Fail to close fd");
    }
}
 
int main()
{
    int fd;
    fd = open_camera_device();
    printf("open camera: fd=%d\n", fd);
    init_camera_device(fd);
    start_capture(fd);
    mainloop(fd);
    stop_capture(fd);
    deinit_mmap();
    close_camera_device(fd);
    return 0;
}
