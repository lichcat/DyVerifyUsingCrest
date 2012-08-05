#include <stdio.h>
#include <stdlib.h>
char* foo(int size){
	char* result;
	if(size>0)
		result=(char*)malloc(size*sizeof(char));
	if(size>10)
		return NULL;

	return result;
}
