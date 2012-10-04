#ifndef GLOBAL_ARRAY_H
#define GLOBAL_ARRAY_H
#define Size 500
#define ARR_SIZE 500
extern int Num;
extern void** PArray[Size];
int* _crest_arr[ARR_SIZE];
//int* ptrArr[ARR_SIZE];
void initPArray();
void addToArray(void** p);
int freeArray();
void all_use();
void partion_use();
void testFunction(void* p);
#endif
