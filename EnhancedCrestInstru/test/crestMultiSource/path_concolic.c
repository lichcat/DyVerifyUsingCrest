#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <crest.h>
#include <path_concolic.h>
int main() { 

	int i;
	CREST_int(i);
	char *ptr,*q,*ch,*ss;

	scanf("%d",&i);
	ptr=foo(i);
	
	q=(char*)calloc(2*i,sizeof(char));
	ch="1234567";
	ss=strdup(ch);
	
	if(i>0 && ptr){
	  ptr++;
	  *ptr='a';
	}
	memset(q,'b',1);
	
	if(i>0 && q)
	  free(q);
	
	return 0;
}
