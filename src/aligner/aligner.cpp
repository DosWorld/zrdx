//             This file is part of the ZRDX 0.50 project
//                    (C) 1998, Sergey Belyakov

#include <io.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char buffer[0xC000];
#define h ((header*)buffer)
//void bmemcpy(void *d, void *s, unsigned n){
//  for(;--n<0xFFFF;)
//    ((char *)d)[n] = ((char*)s)[n];
//}
int main(int argc, char *argv[]){
  if(argc < 2)return 1;
  int handle = open(argv[1], O_BINARY|O_RDWR);
  if(handle < 0)return 1;
  struct header{
    unsigned short signature,
             r,
             n,
             nRelocs,
             hSize;
             char undef[14];
   unsigned short RelocOff;
  };
  memset(buffer, 0, sizeof(buffer));
  unsigned fsize = read(handle, buffer, sizeof(buffer));

  unsigned l = (h->n-1)*512+h->r;
  unsigned nl = atoi(argv[2]);
  unsigned NewDataOff, DataSize;
  if(h->RelocOff < 0x40){
    //if(h->RelocOff > 0x40){
    //}
    //if(h->nRelocs > (512-0x40)/4);// Error;
    NewDataOff = ((h->nRelocs*4+15)&(~15))+0x40;
    DataSize = l-h->hSize*16;
    memmove(buffer+NewDataOff, buffer+h->hSize*16, DataSize);
    memmove(buffer+0x40, buffer+h->RelocOff, h->nRelocs*4);
    memset(buffer+0x1C/*h->RelocOff*/, 0, 0x40 - 0x1C/*h->RelocOff*/);
    strcpy(buffer+0x1C, "Zurenava DOS extender v0.50");
    memset(buffer+NewDataOff+DataSize, 0, sizeof(buffer)-NewDataOff-DataSize);
    h->RelocOff = 0x40;
    *((unsigned long*)(buffer+0x40-4))=nl;
    h->hSize = NewDataOff/16;
  }
  if(nl < l)return 2;
  h->n = (nl+511)/512;
  h->r = nl - (h->n-1)*512;
  lseek(handle, 0, SEEK_SET);
  write(handle, buffer, nl);
  write(handle, NULL, 0);
  return 0;
}
