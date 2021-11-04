#include <stdio.h>
int main(int argc, char *argv[]){
  if(argc<3)return 2;
  FILE * in, *out, *inc;
  in  = fopen(argv[1], "rb");
  out = fopen(argv[2], "wb");
  if(in==NULL||out==NULL)return 1;
  long count = 0;
  for(;;){
    int c=fgetc(in);
    if(c==EOF)break;
    fprintf(out, "%s%u%s", (count%32) ? ",":"DB ", c, ((count+1)%32) ? "": "\r\n");
    count++;
  }
  return 0;
}
