// Copyright (c) 2008, Jacob Burnim (jburnim@cs.berkeley.edu)
//
// This file is part of CREST, which is distributed under the revised
// BSD license.  A copy of this license can be found in the file LICENSE.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See LICENSE
// for details.

#include <assert.h>
#include <fstream>
#include <string>
#include <vector>
#include <string.h>
#include "base/symbolic_interpreter.h"
#include "libcrest/crest.h"

#define CREST_READ_MAX CREST_INPUT_MAX 
#define CREST_GETS_MAX CREST_INPUT_MAX
#define CREST_CHARACTER_MAX CREST_INPUT_MAX
#define CREST_PER_READ_MAX CREST_PER_INPUT_MAX
#define CREST_PER_GETS_MAX CREST_PER_INPUT_MAX

#define CREST_INPUT_MAX 150
#define CREST_PER_INPUT_MAX 20
#define CREST_OPTION_MAX 5

using std::vector;
using namespace crest;

// The symbolic interpreter. */
static SymbolicInterpreter* SI;

// Have we read an input yet?  Until we have, generate only the
// minimal instrumentation necessary to track which branches were
// reached by the execution path.
static int pre_symbolic;

// Tables for converting from operators defined in libcrest/crest.h to
// those defined in base/basic_types.h.
static const int kOpTable[] =
  { // binary arithmetic
    ops::ADD, ops::SUBTRACT, ops::MULTIPLY, ops::CONCRETE, ops::CONCRETE,
    // binary bitwise operators
    ops::CONCRETE, ops::CONCRETE, ops::CONCRETE, ops::CONCRETE, ops::CONCRETE,
    // binary logical operators
    ops::CONCRETE, ops::CONCRETE,
    // binary comparison
    ops::EQ, ops::NEQ, ops::GT, ops::LE, ops::LT, ops::GE,
    // unhandled binary operators
    ops::CONCRETE,
    // unary operators
    ops::NEGATE, ops::BITWISE_NOT, ops::LOGICAL_NOT
  };


static void __CrestAtExit();


void __CrestInit() {
  /* read the input */
  vector<value_t> input;
  std::ifstream in("input");
  value_t val;
  while (in >> val) {
    input.push_back(val);
  }
  in.close();

  SI = new SymbolicInterpreter(input);
  //SI->ReadReachability();
  SI->ReadChecklists();
  pre_symbolic = 1;

  assert(!atexit(__CrestAtExit));
}


void __CrestAtExit() {
  const SymbolicExecution& ex = SI->execution();
  __DyVerifyCheckShadowHeap();
  /* Write the execution out to file 'szd_execution'. */
  string buff;
  buff.reserve(1<<26);
  ex.Serialize(&buff);
  std::ofstream out("szd_execution", std::ios::out | std::ios::binary);
  out.write(buff.data(), buff.size());
  assert(!out.fail());
  out.close();

}


//
// Instrumentation functions.
//

void __CrestLoad(__CREST_ID id, __CREST_ADDR addr, __CREST_VALUE val) {
  if (!pre_symbolic)
    SI->Load(id, addr, val);
}


void __CrestStore(__CREST_ID id, __CREST_ADDR addr) {
  if (!pre_symbolic)
    SI->Store(id, addr);
}


void __CrestClearStack(__CREST_ID id) {
  if (!pre_symbolic)
    SI->ClearStack(id);
}


void __CrestApply1(__CREST_ID id, __CREST_OP op, __CREST_VALUE val) {
  assert((op >= __CREST_NEGATE) && (op <= __CREST_L_NOT));

  if (!pre_symbolic)
    SI->ApplyUnaryOp(id, static_cast<unary_op_t>(kOpTable[op]), val);
}


void __CrestApply2(__CREST_ID id, __CREST_OP op, __CREST_VALUE val) {
  assert((op >= __CREST_ADD) && (op <= __CREST_CONCRETE));

  if (pre_symbolic)
    return;

  if ((op >= __CREST_ADD) && (op <= __CREST_L_OR)) {
    SI->ApplyBinaryOp(id, static_cast<binary_op_t>(kOpTable[op]), val);
  } else {
    SI->ApplyCompareOp(id, static_cast<compare_op_t>(kOpTable[op]), val);
  }
}


void __CrestBranch(__CREST_ID id, __CREST_BRANCH_ID bid, __CREST_BOOL b) {
  if (pre_symbolic) {
    // Precede the branch with a fake (concrete) load.
    SI->Load(id, 0, b);
  }

  SI->Branch(id, bid, static_cast<bool>(b));
}


void __CrestCall(__CREST_ID id, __CREST_FUNCTION_ID fid) {
  SI->Call(id, fid);
}


void __CrestReturn(__CREST_ID id) {
  SI->Return(id);
}


void __CrestHandleReturn(__CREST_ID id, __CREST_VALUE val) {
  if (!pre_symbolic)
    SI->HandleReturn(id, val);
}


//
// Symbolic input functions.
//

void __CrestUChar(unsigned char* x) {
  pre_symbolic = 0;
  *x = (unsigned char)SI->NewInput(types::U_CHAR, (addr_t)x);
}

void __CrestUShort(unsigned short* x) {
  pre_symbolic = 0;
  *x = (unsigned short)SI->NewInput(types::U_SHORT, (addr_t)x);
}

void __CrestUInt(unsigned int* x) {
  pre_symbolic = 0;
  *x = (unsigned int)SI->NewInput(types::U_INT, (addr_t)x);
}

void __CrestChar(char* x) {
  pre_symbolic = 0;
  *x = (char)SI->NewInput(types::CHAR, (addr_t)x);
}

void __CrestShort(short* x) {
  pre_symbolic = 0;
  *x = (short)SI->NewInput(types::SHORT, (addr_t)x);
}

void __CrestInt(int* x) {
  pre_symbolic = 0;
  *x = (int)SI->NewInput(types::INT, (addr_t)x);
}

size_t __CrestReadString(char* x) {     //read and fread
    int i,inputlen;
	static int __crest_read_count = 0;
    
	pre_symbolic = 0;

	if(__crest_read_count >= CREST_READ_MAX)
        return 0;
    if(__crest_read_count + CREST_PER_READ_MAX < CREST_READ_MAX){
        inputlen = CREST_PER_READ_MAX;
    }else{
        inputlen = CREST_READ_MAX - __crest_read_count;
    }
    for( i = 0; i < inputlen; i++){
        __CrestChar(x+i);
        __crest_read_count++;
    }

    return inputlen;
}

char* __CrestOption(){
    
}

char* __CrestGetsString(char* x) {      //gets and fgets
    int i, strlen ;
	static int __crest_gets_count=0;

	pre_symbolic = 0;

    if(__crest_gets_count >= CREST_GETS_MAX)
        return NULL;
    if(__crest_gets_count + CREST_PER_GETS_MAX < CREST_GETS_MAX){
        strlen = CREST_PER_GETS_MAX;
    }else{
        strlen = CREST_GETS_MAX - __crest_gets_count;
    }
    for( i = 0; i < strlen-1; i++){
        __CrestChar(x+i);
        __crest_gets_count++;
    }
    x[strlen-1]=0;

    return x;
}
int __CrestGetCharacter(char* x) {      //getchar getc fgetc
	pre_symbolic = 0;
	static int __crest_character_count=0;
    if(__crest_character_count < CREST_CHARACTER_MAX){
        if(__crest_character_count >0 && 
                (__crest_character_count % CREST_PER_INPUT_MAX ==0)){
            *x=0;
        }else
            __CrestChar(x);

        __crest_character_count ++;
        return *x;	
    }else
        return EOF;
}

void __CrestNoop(){}

void __DyVerifyMalloc(__CREST_ID id, __CREST_ADDR memAddr, __CREST_SIZE size) {
	if(memAddr==(__CREST_ADDR)((void*)NULL))
		return;
	SI->DyVerifyMalloc(id,memAddr,size);
}
void __DyVerifyCalloc(__CREST_ID id, __CREST_ADDR memAddr, __CREST_SIZE num, __CREST_SIZE size){
	if(memAddr==(__CREST_ADDR)((void*)NULL))
		return;
	SI->DyVerifyMalloc(id,memAddr,num*size);
}

/*void __DyVerifyRealloc(__CREST_ID id, __CREST_ADDR newAddr, __CREST_ADDR oldAddr, __CREST_SIZE size) {
    //fprintf(stderr,"id: %d newAddr : %lu , oldAddr : %lu , size: %u\n",id,newAddr,oldAddr,size);
	bool oNULL=(oldAddr==(__CREST_ADDR)((void*)NULL)); 
	bool nNULL=(newAddr==(__CREST_ADDR)((void*)NULL));
	bool zsize=(size==(__CREST_SIZE)0);
	if(size<0)
		return;
	if(oNULL && nNULL)	//malloc fail
		return ;
	if(oNULL && !nNULL){	//malloc sucess
		SI->DyVerifyMalloc(id,newAddr,size);
		return;
	}
	if(!oNULL && zsize){	//free oNULL
		SI->DyVerifyFree(id,oldAddr);
		return;
	}
	if(!oNULL && !zsize && nNULL)	//realloc fail
		return;
	if(!oNULL && !zsize && !nNULL){	//realloc success
		//if(newAddr==oldAddr)
		//	SI->DyVerifyChangeSize(id,oldAddr,size);	//expand original size
		//else{
			SI->DyVerifyFree(id,oldAddr);			//move to new addr
			SI->DyVerifyMalloc(id,newAddr,size);
		//}

	}
}*/

void __DyVerifyAllocNoop(__CREST_ID id){
}

void __DyVerifyStrDup(__CREST_ID id, __CREST_ADDR newAddr, __CREST_ADDR oldAddr){
	if(newAddr==(__CREST_ADDR)(void*)NULL)
		return ;
	int len= strlen((char*)newAddr);
	SI->DyVerifyMalloc(id,newAddr,len);
}


void __DyVerifyFree(__CREST_ID id, __CREST_ADDR memAddr) {
	SI->DyVerifyFree(id,memAddr);
}
void __DyVerifyLiveMemory(__CREST_ID id, __CREST_ADDR memAddr){
	SI->DyVerifyLiveMemory(id,memAddr);
}
void __DyVerifyStaticPathEnd(__CREST_ID id, __CREST_ID pid){
	SI->DyVerifyStaticPathEnd(id,pid);
}
void __StaticPathMark(__CREST_ID id,__CREST_ID pathId,__CREST_ID pathStmtId){
	SI->DyVerifyPathMark(pathId,pathStmtId);
}

void __DyVerifyIsWarningMem(__CREST_ID id, __CREST_ADDR memAddr, __CREST_ID pid){
	SI->DyVerifyIsWarningMem(id,memAddr,pid);
}

void __DyVerifyCheckShadowHeap(){
	SI->DyVerifyCheckShadowHeap();
}
