// Copyright (c) 2008, Jacob Burnim (jburnim@cs.berkeley.edu)
//
// This file is part of CREST, which is distributed under the revised
// BSD license.  A copy of this license can be found in the file LICENSE.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See LICENSE
// for details.

#include <algorithm>
#include <assert.h>
#include <stdio.h>
#include <utility>
#include <vector>
#include <map>
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

#include "base/symbolic_interpreter.h"
#include "base/yices_solver.h"

using std::make_pair;
using std::swap;
using std::vector;
using std::istringstream;
using std::ifstream;


#ifdef DEBUG
#define IFDEBUG(x) x
#else
#define IFDEBUG(x)
#endif

#ifdef MDEBUG
#define IFMDEBUG(x) x
#else
#define IFMDEBUG(x)
#endif

namespace crest {

typedef map<addr_t,SymbolicExpr*>::const_iterator ConstMemIt;
typedef map<addr_t,ShadowHeap*>::const_iterator ConstShadowHeapIt;
typedef map<int,unsigned char>::iterator LFMapIt;
typedef map<int,unsigned char>::const_iterator ConstLFMapIt;

SymbolicInterpreter::SymbolicInterpreter()
  : pred_(NULL), ex_(true), num_inputs_(0) {
  stack_.reserve(16);
}

SymbolicInterpreter::SymbolicInterpreter(const vector<value_t>& input)
  : pred_(NULL), ex_(true) {
  stack_.reserve(16);
  ex_.mutable_inputs()->assign(input.begin(), input.end());
}

void SymbolicInterpreter::DumpMemory() {
  for (ConstMemIt i = mem_.begin(); i != mem_.end(); ++i) {
    string s;
    i->second->AppendToString(&s);
    fprintf(stderr, "\t%lu: %s [%d]\n", i->first, s.c_str(), *(int*)(i->first));
  }
  for (size_t i = 0; i < stack_.size(); i++) {
    string s;
    if (stack_[i].expr) {
      stack_[i].expr->AppendToString(&s);
    } else if ((i == stack_.size() - 1) && pred_) {
      pred_->AppendToString(&s);
    }
    fprintf(stderr, "\ts%d: %lld [ %s ]\n", i, stack_[i].concrete, s.c_str());
  }
}


void SymbolicInterpreter::ClearStack(id_t id) {
  IFDEBUG(fprintf(stderr, "clear\n"));
  for (vector<StackElem>::const_iterator it = stack_.begin(); it != stack_.end(); ++it) {
    delete it->expr;
  }
  stack_.clear();
  ClearPredicateRegister();
  return_value_ = false;
  IFDEBUG(DumpMemory());
}


void SymbolicInterpreter::Load(id_t id, addr_t addr, value_t value) {
  IFDEBUG(fprintf(stderr, "load %lu %lld\n", addr, value));
  ConstMemIt it = mem_.find(addr);
  if (it == mem_.end()) {
    PushConcrete(value);
  } else {
    PushSymbolic(new SymbolicExpr(*it->second), value);
  }
  ClearPredicateRegister();
  IFDEBUG(DumpMemory());
}


void SymbolicInterpreter::Store(id_t id, addr_t addr) {
  IFDEBUG(fprintf(stderr, "store %lu\n", addr));
  if(stack_.size()<=0)   return ;       //function pointer have no Load,but here Store
  //assert(stack_.size() > 0);

  const StackElem& se = stack_.back();
  if (se.expr) {
    if (!se.expr->IsConcrete()) {
      mem_[addr] = se.expr;
    } else {
      mem_.erase(addr);
      delete se.expr;
    }
  } else {
    mem_.erase(addr);
  }

  stack_.pop_back();
  ClearPredicateRegister();
  IFDEBUG(DumpMemory());
}


void SymbolicInterpreter::ApplyUnaryOp(id_t id, unary_op_t op, value_t value) {
  IFDEBUG(fprintf(stderr, "apply1 %d %lld\n", op, value));
  assert(stack_.size() >= 1);
  StackElem& se = stack_.back();

  if (se.expr) {
    switch (op) {
    case ops::NEGATE:
      se.expr->Negate();
      ClearPredicateRegister();
      break;
    case ops::LOGICAL_NOT:
      if (pred_) {
	pred_->Negate();
	break;
      }
      // Otherwise, fall through to the concrete case.
    default:
      // Concrete operator.
      delete se.expr;
      se.expr = NULL;
      ClearPredicateRegister();
    }
  }

  se.concrete = value;
  IFDEBUG(DumpMemory());
}


void SymbolicInterpreter::ApplyBinaryOp(id_t id, binary_op_t op, value_t value) {
  IFDEBUG(fprintf(stderr, "apply2 %d %lld\n", op, value));
  assert(stack_.size() >= 2);
  StackElem& a = *(stack_.rbegin()+1);
  StackElem& b = stack_.back();

  if (a.expr || b.expr) {
    switch (op) {
    case ops::ADD:
      if (a.expr == NULL) {
	swap(a, b);
	*a.expr += b.concrete;
      } else if (b.expr == NULL) {
	*a.expr += b.concrete;
      } else {
	*a.expr += *b.expr;
	delete b.expr;
      }
      break;

    case ops::SUBTRACT:
      if (a.expr == NULL) {
	b.expr->Negate();
	swap(a, b);
	*a.expr += b.concrete;
      } else if (b.expr == NULL) {
	*a.expr -= b.concrete;
      } else {
	*a.expr -= *b.expr;
	delete b.expr;
      }
      break;

    case ops::MULTIPLY:
      if (a.expr == NULL) {
	swap(a, b);
	*a.expr *= b.concrete;
      } else if (b.expr == NULL) {
	*a.expr *= b.concrete;
      } else {
	swap(a, b);
	*a.expr *= b.concrete;
	delete b.expr;
      }
      break;

    default:
      // Concrete operator.
      delete a.expr;
      delete b.expr;
      a.expr = NULL;
    }
  }

  a.concrete = value;
  stack_.pop_back();
  ClearPredicateRegister();
  IFDEBUG(DumpMemory());
}


void SymbolicInterpreter::ApplyCompareOp(id_t id, compare_op_t op, value_t value) {
  IFDEBUG(fprintf(stderr, "compare2 %d %lld\n", op, value));
  assert(stack_.size() >= 2);
  StackElem& a = *(stack_.rbegin()+1);
  StackElem& b = stack_.back();

  if (a.expr || b.expr) {
    // Symbolically compute "a -= b".
    if (a.expr == NULL) {
      b.expr->Negate();
      swap(a, b);
      *a.expr += b.concrete;
    } else if (b.expr == NULL) {
      *a.expr -= b.concrete;
    } else {
      *a.expr -= *b.expr;
      delete b.expr;
    }
    // Construct a symbolic predicate (if "a - b" is symbolic), and
    // store it in the predicate register.
    if (!a.expr->IsConcrete()) {
      pred_ = new SymbolicPred(op, a.expr);
    } else {
      ClearPredicateRegister();
      delete a.expr;
    }
    // We leave a concrete value on the stack.
    a.expr = NULL;
  }

  a.concrete = value;
  stack_.pop_back();
  IFDEBUG(DumpMemory());
}


void SymbolicInterpreter::Call(id_t id, function_id_t fid) {
  ex_.mutable_path()->Push(kCallId);
}


void SymbolicInterpreter::Return(id_t id) {
  ex_.mutable_path()->Push(kReturnId);

  // There is either exactly one value on the stack -- the current function's
  // return value -- or the stack is empty.
  assert(stack_.size() <= 1);

  return_value_ = (stack_.size() == 1);
}


void SymbolicInterpreter::HandleReturn(id_t id, value_t value) {
  if (return_value_) {
    // We just returned from an instrumented function, so the stack
    // contains a single element -- the (possibly symbolic) return value.
    assert(stack_.size() == 1);
    return_value_ = false;
  } else {
    // We just returned from an uninstrumented function, so the stack
    // still contains the arguments to that function.  Thus, we clear
    // the stack and push the concrete value that was returned.
    ClearStack(-1);
    PushConcrete(value);
  }
}


void SymbolicInterpreter::Branch(id_t id, branch_id_t bid, bool pred_value) {
  IFDEBUG(fprintf(stderr, "branch %d %d\n", bid, pred_value));
  assert(stack_.size() == 1);
  stack_.pop_back();

  if (pred_ && !pred_value) {
    pred_->Negate();
  }

  ex_.mutable_path()->Push(bid, pred_);
  pred_ = NULL;

  IFDEBUG(DumpMemory());

  /*
  //for unpassed pathMark branch should reach all of them
  //if current branch can not reach all pathMark
  //Note: currentPathMark equal to Pa_Sb 's b ,so smaller than b needn't to check
  bool continue_run = true;
  vector<bool> currentReachable = branch2pathMarkMap_[bid];
  for(unsigned int i=currentPathMarkNum_;i<currentReachable.size();i++){
	if(!currentReachable[i]){
		continue_run = false;
	}
  }
  if(!continue_run){
	fprintf(stderr,"branch %d cannot reach path!\n",bid);
	exit(101);
  }

  */

}


value_t SymbolicInterpreter::NewInput(type_t type, addr_t addr) {
  mem_[addr] = new SymbolicExpr(1, num_inputs_);
  ex_.mutable_vars()->insert(make_pair(num_inputs_ ,type));

  value_t ret = 0;
  if (num_inputs_ < ex_.inputs().size()) {
    ret = ex_.inputs()[num_inputs_];
  } else /*{
    // New inputs are initially randomly instead.)
	unsigned long long val =0;
	for(size_t j=0;j<8;j++)
		val = ((val<<8) + rand()/256);

	const char CCH[] = "_0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
	int x = val%(sizeof(CCH) - 2);
	char ch = CCH[x];	
	
	switch (type){
		case types::U_CHAR:	ex_.mutable_inputs()->push_back((unsigned char)ch); break;
		case types::CHAR:	ex_.mutable_inputs()->push_back((char)ch); break;
		case types::U_SHORT:	ex_.mutable_inputs()->push_back((unsigned short)val); break;
		case types::SHORT:	ex_.mutable_inputs()->push_back((short)val); break;
		case types::U_INT:	ex_.mutable_inputs()->push_back((unsigned int)val); break;
		case types::INT:	ex_.mutable_inputs()->push_back((int)val); break;
		case types::U_LONG:	ex_.mutable_inputs()->push_back((unsigned long)val); break;
		case types::LONG:	ex_.mutable_inputs()->push_back((long)val); break;
		case types::U_LONG_LONG:	ex_.mutable_inputs()->push_back((unsigned long long)val); break;
		case types::LONG_LONG:	ex_.mutable_inputs()->push_back((long long)val); break;

	}
  }*/
  {
    // Generate a new random input.
    //
    ret = CastTo(rand(), type);
    ex_.mutable_inputs()->push_back(ret);
  }

  num_inputs_ ++;
  return ret;
}


void SymbolicInterpreter::PushConcrete(value_t value) {
  PushSymbolic(NULL, value);
}


void SymbolicInterpreter::PushSymbolic(SymbolicExpr* expr, value_t value) {
  stack_.push_back(StackElem());
  StackElem& se = stack_.back();
  se.expr = expr;
  se.concrete = value;
}


void SymbolicInterpreter::ClearPredicateRegister() {
  delete pred_;
  pred_ = NULL;
}

bool SymbolicInterpreter::isRepeat(vector<string> &inarr, string &line){
    for(int i=0;i<inarr.size();i++){
        if(!(inarr[i].compare(line))){
            //std::cerr << inarr[i] << "equals to " << line<< std::endl;
            return true;
        }
    }
    return false;
}

//read file "reachability"
void SymbolicInterpreter::ReadReachability(){
	ifstream in("reachability");
	assert(in);

	string line;
	branch_id_t branchId;
	int b_reach;
	while(getline(in,line)){
		istringstream line_in(line);
		line_in >>branchId;
		line_in.get();
		while(line_in){
			line_in>>b_reach;
			branch2pathMarkMap_[branchId].push_back((b_reach==1)?true:false);
			line_in.get();
		}
	}
	in.close();	
}


// Read checklists 
void SymbolicInterpreter::ReadChecklists(){
  ifstream checklistsIn("checklists");
  assert(checklistsIn);

  vector<string> allreadyIn;
  int pid;
  while(checklistsIn>>pid){
      int nstmt=0;
      string line;
      allreadyIn.clear();
      getline(checklistsIn,line);
      do{
          getline(checklistsIn,line);
          if(!line.compare("END_PATH"))
              break;
          if(line.length()>1 && (line.at(line.length()-2)!='P' || line.at(line.length()-1)!='P') && !(isRepeat(allreadyIn,line))){
              nstmt++;
              allreadyIn.push_back(line);
          }
      }while(1);
      exeTrace_.push_back(vector<int> ());

      for(int i=0;i<nstmt;i++)
          exeTrace_[pid].push_back(0);
  }
  checklistsIn.close();

}
void SymbolicInterpreter::DyVerifyMalloc(id_t id,addr_t memAddr,value_t size){
    //fprintf(stderr,"id %d, malloc @%lu %u bytes\n",id,memAddr,size);
	ShadowHeap* sMem=new ShadowHeap(memAddr,size);
	bool inserted=(shadowHeap_.insert(std::pair<addr_t,ShadowHeap*>(memAddr,sMem))).second;

	if(!inserted){
		fprintf(stderr,"Err: in Malloc id:%d\n\t Malloc %lld bytes @ %lu ,already exists in ShadowHeap!\n ",id,size,memAddr);
		delete sMem;
		//exit(-1);
	}
	//IFMDEBUG(DyVerifyDumpShadowHeap());
}
/*void SymbolicInterpreter::DyVerifyChangeSize(id_t id, addr_t memAddr,value_t size){
    fprintf(stderr,"id: %u Realloc Changesize @%lu of size %lu\n",id,memAddr,size);
	ConstShadowHeapIt it=shadowHeap_.find(memAddr);
	if(it==shadowHeap_.end()){
		fprintf(stderr,"Err: in ChangeSize find no memAddr:%lu\n",memAddr);
	}else{
		if(it->second!=NULL)
			it->second->sizeOfMem=size;
	}
}*/
void SymbolicInterpreter::DyVerifyFree(id_t id,addr_t memAddr){
    //fprintf(stderr,"id:%u Free mem @%lu\n",id,memAddr);
    if(memAddr==(addr_t)((void*)NULL))
        return ;
	ConstShadowHeapIt it=shadowHeap_.find(memAddr);
	if(it==shadowHeap_.end()){
		fprintf(stderr,"Err: in Free id:%d\n\tTry to Free %lu , no find in ShadowHeap!\n",id,memAddr);
		//exit(-1);
        return ;
	}
    ShadowHeap * mem = it->second;
    
    map<int,unsigned char> & memLFMap = mem ->liveFlagMap_ ;
    ConstLFMapIt lfmapIt;
    for(lfmapIt=memLFMap.begin();lfmapIt!=memLFMap.end();lfmapIt++){
        if(lfmapIt->second=='S'){
            if(sFreedMap_.find(lfmapIt->first)==sFreedMap_.end())
                sFreedMap_[lfmapIt->first]=1;
            else
                sFreedMap_[lfmapIt->first]++;
        }
        else if(lfmapIt->second=='L'){
            if(lFreedMap_.find(lfmapIt->first)==lFreedMap_.end())
                lFreedMap_[lfmapIt->first]=1;
            else
                lFreedMap_[lfmapIt->first]++;
        }
    }
        
	delete it->second;
	shadowHeap_.erase(it->first);
	//IFMDEBUG(DyVerifyDumpShadowHeap());
}
void SymbolicInterpreter::DyVerifyLiveMemory(id_t id, addr_t memAddr){
	IFMDEBUG(fprintf(stderr,"LiveUse \tmemAddr: %lu \n",memAddr));
	if(shadowHeap_.begin()==shadowHeap_.end()){
		IFMDEBUG(fprintf(stderr,"empty shadowHeap\n"));
		return ;
	}
	ConstShadowHeapIt itup=shadowHeap_.upper_bound(memAddr);
	if(itup==shadowHeap_.begin()){					//least one (begin) is bigger than memAddr
		IFMDEBUG(fprintf(stderr,"not in shadowHeap\n"));
		return ;
	}	
	itup--;
	ConstShadowHeapIt checkIt=itup;
	ShadowHeap* mem=checkIt->second;
	addr_t begin=mem->memAddr;
	addr_t end=begin+mem->sizeOfMem;
	bool isInRange=(memAddr>=begin)&&(memAddr<end);
	if(isInRange){
        map<int,unsigned char> & memLFMap = mem->liveFlagMap_;
        LFMapIt lfmapIt;
        for(lfmapIt=memLFMap.begin();lfmapIt!=memLFMap.end();lfmapIt++){
            if(lfmapIt->second=='S')
                lfmapIt->second='L';
        }
	}
}
void SymbolicInterpreter::DyVerifyDumpShadowHeap(){
	fprintf(stderr,"SHeap: \n");
	ConstShadowHeapIt it;
	for(it=shadowHeap_.begin();it!=shadowHeap_.end();it++){
		ShadowHeap* mem=it->second;
        fprintf(stderr, "\tmemAdrr: %lu sizeofMem: %u\n", mem->memAddr,mem->sizeOfMem);
        map<int,unsigned char> & memLFMap = mem -> liveFlagMap_ ;
        map<int,unsigned char>::const_iterator memLFMIt;
        for(memLFMIt=memLFMap.begin();memLFMIt!=memLFMap.end();memLFMIt++){
            fprintf(stderr,"\tliveFlag[%d]=%c",memLFMIt->first,memLFMIt->second);
        }
        fprintf(stderr,"\n");
	}

}
void SymbolicInterpreter::DyVerifyStaticPathEnd(id_t id,id_t pid){
	ConstShadowHeapIt it;
	for(it=shadowHeap_.begin();it!=shadowHeap_.end();it++){
        map<int,unsigned char> &currLFMap_ = it->second->liveFlagMap_;
		if(currLFMap_.find(pid)!=currLFMap_.end()){
            currLFMap_[pid]='S';
        }
	}
}
void SymbolicInterpreter::DyVerifyPathMark(id_t pathId,id_t pathStmtId){
    exeTrace_[pathId][pathStmtId-1]=1;	
}
void SymbolicInterpreter::DyVerifyCheckShadowHeap(){
    int i,j;
    //output for debug path coverage
    /*for(i=0;i<exeTrace_.size();i++){
        fprintf(stderr,"path %d:\n",i);
        for(j=0;j<exeTrace_[i].size();j++){
            fprintf(stderr,"\tstmt %d: %d\n",j+1,exeTrace_[i][j]);
        }
    }*/
    //end output
    for(i=0;i<exeTrace_.size();i++){
        int currPathLen=exeTrace_[i].size();
        for(j=0;j<currPathLen;j++){
            if(!exeTrace_[i][j])
                break;
        }
        if(j==currPathLen && j!=0){
            fprintf(stderr,"warning %d: ",i);
            //iter ShadowHeap_ to count leak
            int nleak = 0;
            ConstShadowHeapIt it;
            for(it=shadowHeap_.begin();it!=shadowHeap_.end();it++){
                ShadowHeap* mem=it->second;
                map<int,unsigned char> & memLFMap = mem -> liveFlagMap_ ;
                
                if(memLFMap.find(i)!=memLFMap.end())
                    nleak++;
            }
            //find pid=i in sfreed and lfreed
            int sfree,lfree;
            map<int,int>::const_iterator sfreedIt=sFreedMap_.find(i);
            if(sfreedIt==sFreedMap_.end())
                sfree=0;
            else
                sfree=sfreedIt->second;
            map<int,int>::const_iterator lfreedIt=lFreedMap_.find(i);
            if(lfreedIt==lFreedMap_.end())
                lfree=0;
            else
                lfree=lfreedIt->second;
                
            if(nleak>0)
                fprintf(stderr,"Leak");
            else{
                if(sfree>0)
                    fprintf(stderr,"Bloat");
                else if(lfree>0)
                    fprintf(stderr,"L_NOT_Leak");
                else if(lfree==0)
                    fprintf(stderr,"ERROR");
            }
            fprintf(stderr," wl:%d,sf:%d,lf:%d\n",nleak,sfree,lfree);
        }
    }
	//release our ShadowHeap
    //DyVerifyDumpShadowHeap();
	ConstShadowHeapIt it;
	for(it=shadowHeap_.begin();it!=shadowHeap_.end();++it){
		delete it->second;	
	}
	shadowHeap_.clear();
	
}

void SymbolicInterpreter::DyVerifyIsWarningMem(id_t id, addr_t memAddr, id_t pid){
	//IFMDEBUG(fprintf(stderr,"IsWarningMemory: %lu \n", memAddr));
	ConstShadowHeapIt it=shadowHeap_.find(memAddr);
	if(it==shadowHeap_.end()){
		fprintf(stderr, "Err: in DyVerifyIsWarningMem id:%d\n\t Try mark %lu isWaringMem ,where couldn't find in ShadowHeap\n",id,memAddr);
		//exit(-1);	
        //if(memAddr!=(addr_t)((void*)NULL))
	}else{
		it->second->liveFlagMap_[pid]='I';
    }
	//IFMDEBUG(DyVerifyDumpShadowHeap());
	//DyVerifyDumpShadowHeap();
}
}  // namespace crest
