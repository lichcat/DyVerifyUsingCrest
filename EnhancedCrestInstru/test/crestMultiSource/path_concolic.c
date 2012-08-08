#include <stdio.h>
#include <stdlib.h>
#include <crest.h>
#include <path_concolic.h>
int main() { 

	int i;
	//CREST_int(i);
	char *ptr,*q;

	scanf("%d",&i);
	ptr=foo(i);
	
	q=ptr;
	
	if(i>0 && ptr){
	  ptr++;
	  *ptr='a';
	}
	
	if(i>0 && q)
	  free(q);
	
	return 0;
}
