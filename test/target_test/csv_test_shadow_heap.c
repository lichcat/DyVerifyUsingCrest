#include <stdio.h>
#include <stdlib.h>

#define N 10

void test(int value,int* ptr){
	if(value>0)
		printf("%d\n",value);

}


int main(){
	int* p=(int*)malloc(sizeof(int)*N);
	int a,b;

	*p=26;
	a=*p;
	test(*p,p);

	free(p);
	return 0;
}
