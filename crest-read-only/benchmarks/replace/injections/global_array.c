#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>
#define Size 500
#define ARR_SIZE 500
int Num=0;
void** PArray[Size]={NULL};
int* _crest_arr[ARR_SIZE]={NULL};
//add a pointer(element) into the array
void addToArray(void** p)
{	
	int i;
    if(Num>=Size){
		for(i=0;i<Size;i++){
			free(_crest_arr[i]);
			_crest_arr[i]=NULL;
			PArray[i]=NULL;
		}
		Num=0;
	}
    PArray[Num]=p;
	_crest_arr[Num]=*p;
    Num++;
	//printf("----num:%d-----\n",Num);
}

//free all elements in the array
int freeArray()
{
  int i;
  for(i=0;i<Num;i++)
  {
    //free(*PArray[i]);
	if(_crest_arr[i]){
	  free(_crest_arr[i]);
	  printf("free:%dth\n",i);
	}
  }
  Num=0;
  return 0;
}

//all elements used
/*
void all_use()
{
  int i;
  for(i=0;i<Num;i++)
  {
    testFunction(_crest_ptr[i]);
  }
}
*/
extern void testFunction(void* p);
//partion use
void partion_use()
{
  int i;
  /*FILE* p=fopen("randomNUM","r");
  srand(time(NULL));
  int UP=random()%(Num-2);
  fprintf(p,"-----------------------------\n");
  fprintf(p,"Num of random UP BOUND IS %d \n",UP);
  for(i=0;i<UP;i++)*/
  for(i=0;i<(Num+1)/2;i++)
  {
    if(_crest_arr[i])
	  testFunction(_crest_arr[i]);
  }
}

void testFunction(void* p)
{
  //todo
}
