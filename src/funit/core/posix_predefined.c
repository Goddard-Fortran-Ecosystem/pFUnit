#ifndef __OSX_AVAILABLE_STARTING
#  define __OSX_AVAILABLE_STARTING(_osx, ios)
#  define __OSX_AVAILABLE_BUT_DEPRECATED(_osxIntro, osxDep, iosIntro, iosDep)
#  define __OSX_AVAILABLE_BUT_DEPRECATED_MSG(_osxIntro, osxDep, iosIntro, iosDep, _msg)
#endif
#include<stdlib.h>
#include<errno.h>
#include<stdio.h>
#include<fcntl.h>
#include<sys/types.h>
#include<poll.h>
int main(){
  pthread_t thread;
  pthread_mutex_t mutex;
  pthread_mutexattr_t mutexattr;
  pthread_attr_t attr;
  pthread_cond_t cond;
  time_t tv_sec;
  suseconds_t tv_usec;
  FILE f;
  mode_t mode;
  
  printf("integer(kind=C_SHORT), parameter :: EAGAIN=%ld\n", (long) EAGAIN);
  printf("integer(kind=C_SHORT), parameter :: O_RDONLY=%ld\n", (long) O_RDONLY);
  printf("integer(kind=C_SHORT), parameter :: O_NONBLOCK=%ld\n", (long) O_NONBLOCK);
  printf("integer(kind=C_SHORT), parameter :: POLLIN=%ld\n", (long)POLLIN);

  return EXIT_SUCCESS;
}
