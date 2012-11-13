#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "global_array.h"
#define SIZE 5000

#define MAX_INJ_NUM	18
typedef struct inj_tag{
	int* crest_ptr;
	unsigned int inj_id;
}inj_info;

int Num=0;
int inj_histgram[MAX_INJ_NUM]={0};
inj_info* _crest_arr[SIZE]={NULL};
//add a pointer(element) into the array
void addToArray(void** p,unsigned int id)
{	
	int i;
	assert(id<=MAX_INJ_NUM);

    if(Num>=SIZE){
		for(i=0;i<SIZE;i++){
			free(_crest_arr[i]->crest_ptr);
			free(_crest_arr[i]);
			_crest_arr[i]=NULL;
		}
		Num=0;
	}
	_crest_arr[Num]=(inj_info*)malloc(sizeof(inj_info));
	_crest_arr[Num]->crest_ptr=*p;
	_crest_arr[Num]->inj_id=id;
    Num++;
}

//free all elements in the array
void freeArray()
{
  int i;
  for(i=0;i<Num;i++)
  {
	if(_crest_arr[i] && _crest_arr[i]->crest_ptr){
	  free(_crest_arr[i]->crest_ptr);
	  free(_crest_arr[i]);
	}
  }
  Num=0;
}

int is_all_use_id(unsigned int id){
	if(id==4)   //cat p4 L_N_LEAK
		return 1;
	else 
		return 0;
}
extern void testFunction(void* p);

void crest_use()
{
  int i;
  int* ptr;
  unsigned int id;
  
  for(i=0;i<Num;i++)
  {
    if(_crest_arr[i]){
		id = _crest_arr[i]->inj_id;
		ptr = _crest_arr[i]->crest_ptr;
	  if(is_all_use_id(id))
		  testFunction(ptr);
	  else if(inj_histgram[id]%2==1){
		  testFunction(ptr);
	  }
	  inj_histgram[id]++;
	  
	}
  }
}

void testFunction(void* p)
{
  //
}
