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

#include "base/symbolic_interpreter.h"
#include "libcrest/crest.h"

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

/* modified by lichcat(mengchenli.nju@gmail.com)
 * Enhanced Functions for confirming Memory Leak
 * CSV: C_Static_result_Validate
 * malloc,realloc,calloc in <stdlib.h>
 * __csvMalloc		for	void* malloc( size_t size );
 * __csvRealloc	for void *realloc( void *ptr, size_t new_size );
 * __csvCalloc		for void* calloc( size_t num, size_t size );
 * __csvFree		for void free( void* ptr );
 *
 */

void __DyVerifyMalloc(__CREST_ID id, __CREST_ADDR memAddr, __CREST_SIZE size) {
	SI->DyVerifyMalloc(id,memAddr,size);
}
/*
void __csvRealloc(__CREST_ID id, __CREST_ADDR ptrAddr, __CREST_ADDR ptrAddr, __CREST_ADDR memAddr, __CREST_SIZE size) {

}
void __csvCalloc(__CREST_ID, __CREST_ADDR ptrAddr, __CREST_ADDR, __CREST_SIZE, __CREST_SIZE) {

}
*/
void __DyVerifyFree(__CREST_ID id, __CREST_ADDR memAddr) {
	SI->DyVerifyFree(id,memAddr);
}
/*
void __csvLoadPointer(__CREST_ID id, __CREST_ADDR ptrAddr, __CREST_ADDR memAddr){

}
void __csvStorePointer(__CREST_ID id, __CREST_ADDR ptrAddr, __CREST_ADDR memAddr){

}
void __csvPointerApply1(__CREST_ID id, __CREST_OP op, __CREST_VALUE value){

}
void __csvPointerApply2(__CREST_ID id, __CREST_OP op, __CREST_VALUE value){

}
void __csvHandleReturnPointer(__CREST_ID id, __CREST_ADDR retPtrAddr, __CREST_ADDR retPtrValue){

}
void __csvClearPointerStack(__CREST_ID id){

}
*/
void __DyVerifyLiveMemory(__CREST_ID id, __CREST_ADDR memAddr, __CREST_VALUE value){
	SI->DyVerifyLiveMemory(id,memAddr,value);
}
void __DyVerifyStaticPathEnd(__CREST_ID id){
	SI->DyVerifyStaticPathEnd(id);
}
void __DyVerifyPathMark(__CREST_ID id,__CREST_ID pathId,__CREST_ID pathStmtId){
	SI->DyVerifyPathMark(pathId,pathStmtId);
}
/*
void __DyVerifyIsWarningMem(__CREST_ID id, __CREST_ADDR memAddr){
	SI->DyVerifyIsWarningMem(id,memAddr);
}
*/
void __DyVerifyCheckShadowHeap(){
	SI->DyVerifyCheckShadowHeap();
}
