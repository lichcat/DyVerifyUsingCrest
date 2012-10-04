// copyright (c) 2008, jacob burnim (jburnim@cs.berkeley.edu)
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
#include <ctype.h>
#include <fstream>
#include <limits>
#include <set>
#include <sstream>
#include <string>
#include <vector>
#include <ext/hash_map>

#include <regex.h>
#include <assert.h>
#include <queue>
#include <map>
#include <stack>

#ifdef DEBUG
#define IFDEBUG(x) x
#else
#define IFDEBUG(x)
#endif


using namespace std;
using __gnu_cxx::hash_map;

//typedef pair<int,int> edge_t;
typedef vector<int> adj_list_t;
//typedef adj_list_t::iterator nbhr_it;
//typedef adj_list_t::const_iterator const_nbhr_it;
typedef vector<adj_list_t> graph_t;
typedef set<int>::const_iterator BranchIt;

typedef enum{white,black,gray} color_t;

namespace __gnu_cxx {
	template<> struct hash<string> {
		size_t operator()(const string& x) const {
			return hash<const char*>()(x.c_str());
		}
	};
}

hash_map<int,bool>  funcNodeMap;
hash_map<int,bool> notExitNodesMap;
vector<pair<int,int> > loopsMap;
hash_map<int,hash_map<int,bool> > vertexVisited;
queue<pair<int,int> > copyLaterPairs;
unsigned int pathLen=0;		// count staticpathstmt number


void readBranches(set<int>* branches) {
	ifstream in("branches");

	int fid, numBranches;
	while (in >> fid >> numBranches) {
		for (int i = 0;  i < numBranches; i++) {
			int b1, b2;
			assert(in >> b1 >> b2);
			branches->insert(b1);
			branches->insert(b2);
		}
	}

	in.close();
}
bool isPathMark(string funcname){
	regex_t regex;
	int reti;
	char msgbuf[100];
	//reti = regcomp(&regex,"__StaticPathMark_P[0-9]+_S[0-9]+",0);
	reti = regcomp(&regex,"__StaticPathMark.*",0);
	if(reti){
		fprintf(stderr,"could not compile regex \n");
		exit(1);
	}

	reti=regexec(&regex,funcname.c_str(),0,NULL,0);
	if(!reti)
		return true;
	else if(reti == REG_NOMATCH)
		return false;
	else{
		regerror(reti,&regex,msgbuf,sizeof(msgbuf));
		fprintf(stderr,"Regex match failed : %s\n",msgbuf);
		exit(1);
	}
}
void readCfg(graph_t* graph) {

	// No we can read in the CFG edges, substituting the correct CFG nodes
	// for function calls.
	ifstream in("cfg");

	string line;
	while (getline(in, line)) {
		istringstream line_in(line);
		int src;
		line_in >> src;
		line_in.get();
		assert(src>0);

		if (graph->size() <= src)
			graph->resize(src+1);
		adj_list_t& nbhrs = (*graph)[src];

		while (line_in) {
			if (isdigit(line_in.peek())) {
				int dst;
				line_in >> dst;
				nbhrs.push_back(dst);
			} else {
				string func;
				line_in >> func;
				/*hash_map<string,int>::iterator it = funcNodeMap.find(func);
				if (it != funcNodeMap.end()){
					printf("----------func: %s %d \n",it->first.c_str(),it->second);
					nbhrs.push_back(it->second);
				}
				else */
				if (isPathMark(func)){
					pathLen++;
					string pNsN,stmtStr;
					pNsN=func.substr(16);
					size_t stmtIdPos=pNsN.find_last_of("S");
					stmtStr = pNsN.substr(stmtIdPos+1);
					int stmtValue=atoi(stmtStr.c_str());
					//cout<<"match "<<(-stmtValue)<<endl;
					nbhrs.push_back(-stmtValue);
				}else if(!func.compare("exit")){
					notExitNodesMap[src]=true;
				}
			}
			line_in.get();
		}
	}
	in.close();
}
void dumpCFG(graph_t *graph){
	//printf("size: %d\n",graph->size());
	unsigned int i,j;
	for(i=0;i<graph->size();i++){
		fprintf(stderr,"%d",i);
		adj_list_t &nbhrs =  (*graph)[i];
		for(j=0;j<nbhrs.size();j++)
			fprintf(stderr," %d",nbhrs[j]);
		fprintf(stderr,"\n");	
	}
}


void addToFuncOut(graph_t *graph,int addedIndex, int funcInId){
	//from funcInId , bfs find the out node of function and add the addedIndex to out node(s)
	unsigned int k,i;
	hash_map<int,bool>	visitedMap;
	for(k=0;k< graph->size(); k++)
		visitedMap[k]=false;	
	
	int cur;

	queue<int> q;
	q.push(funcInId);
	while(!q.empty()){
		cur=q.front();
		visitedMap[cur]=true;	//visit cur
		q.pop();
		adj_list_t &nbhrs=(*graph)[cur];

		for(i=0;i<nbhrs.size();i++){
			if(nbhrs[i]<=0 || (funcNodeMap.find(nbhrs[i])!=funcNodeMap.end()))
				continue;
			if(!visitedMap[nbhrs[i]])
				q.push(nbhrs[i]);
		}
		hash_map<int,bool>::const_iterator it=notExitNodesMap.find(cur);
		if(nbhrs.size()==0 && (it==notExitNodesMap.end()))	//the out of func cfg
			nbhrs.push_back(addedIndex);
	}
}
void addFuncNb2FuncOut(graph_t *graph){
	unsigned int i,j;

	for(i=0;i<graph->size();i++){
		adj_list_t &nbhrs = (*graph)[i];
		for(j=0;j<nbhrs.size();j++){
			hash_map<int,bool>::iterator it = funcNodeMap.find(nbhrs[j]);
			if(it != funcNodeMap.end() && !funcNodeMap[it->first]){		//call a unAddedOut func here
				int k= j-1;
				while(k>=0){
					if(funcNodeMap.end() == funcNodeMap.find(nbhrs[k])){
						funcNodeMap[nbhrs[j]]=true;				//mark func as added
						addToFuncOut(graph,nbhrs[k],nbhrs[j]);	
					}
					k--;
				}
			}
		}
	}
}
void copyReach(vector<bool> &srcReach,vector<bool> &dstReach){
	unsigned int i;
	for(i=0;i<pathLen;i++){
		if(srcReach[i])
			dstReach[i]=true;
	}
}
void dfSearch(graph_t *graph,int currId,vector<vector<bool> > &reachability,vector<bool> &sourceReach,hash_map<int,color_t> &visited){
	unsigned int i;
	/*if(currId<0){		//pathMark case
		visited[currId]=black;
		sourceReach[-currId]=true;
		return ;
	}*/
	//gray the currId
	visited[currId]=gray;
	adj_list_t &nbhrs = (*graph)[currId];

	for(i=0;i<nbhrs.size();i++){
		if(nbhrs[i]<0){
			unsigned int pathStmtId=-nbhrs[i];
			reachability[currId][pathStmtId-1]=true;
		}else if(visited[nbhrs[i]]==white){
			dfSearch(graph,nbhrs[i],reachability,reachability[currId],visited);
		}else if(visited[nbhrs[i]]==black ){
			copyReach(reachability[nbhrs[i]],reachability[currId]);
		}else if(visited[nbhrs[i]]==gray){
			loopsMap.push_back(pair<int,int>(nbhrs[i],currId));		//nbhrs is gray means have loop,and from nbhrs[i]->currId has trace
		}
	}

	//leave with black
	visited[currId]=black;

	copyReach(reachability[currId],sourceReach);
}
bool nothingCopy(vector<bool> &reach){	//nothing to copy ->true 
	unsigned int i;
	for(i=0;i<reach.size();i++){
		if(reach[i])
			return false;
	}
	return true;
}
bool sameReachable(vector<bool> &reachPrt,vector<bool> &reachSon){
	unsigned int i;
	for(i=0;i<pathLen;i++){
		if(reachPrt[i]!=reachSon[i])
			return false;
	}
	return true;
}

void dumpStack(vector<int> &mystack){
	unsigned int i;
	fprintf(stderr,"bottom: ");
	for(i=0;i<mystack.size();i++){
		fprintf(stderr,"%d->",mystack[i]);
	}
	fprintf(stderr,"top\n");
}
void findAllPath(graph_t *graph,int begin,int end,vector<vector<bool> > &reachability,unsigned int bound){
	unsigned int k,j,i;
	hash_map<int,bool>	isInStack;
	for(k=1;k< graph->size(); k++){
		isInStack[k]=false;
		adj_list_t &nbhrs=(*graph)[k];
		adj_list_t::iterator it;
		for(it=nbhrs.begin();it!=nbhrs.end();){
			if(*it<0){
				it=nbhrs.erase(it);
			}else{
				it++;
			}
		}
	}
	
	vector<int>	myStack;	//int type element ,use vector to store
	myStack.push_back(begin);
	isInStack[begin]=true;
	while(!myStack.empty()){
		//dumpStack(myStack);				//print

		int peek=myStack.back();

		//check bound
		unsigned int dist=(peek>end)?(peek-end+1):(end-peek+1);
		
		if(dist>bound){
			isInStack[peek]=true;
			myStack.pop_back();
			continue;
		}

		adj_list_t &nbhrs=(*graph)[peek];
		int v=-1;
		for(j=0;j<nbhrs.size();j++){	//get a adjVertex that not in stack && not from peek visited
			if(nbhrs[j]<0)
				continue;
			if(isInStack[nbhrs[j]])
				continue;
			if((vertexVisited[peek].find(nbhrs[j]))!=(vertexVisited[peek].end()))
				continue;
			if(funcNodeMap.find(nbhrs[j])!=funcNodeMap.end()){		//nbhrs[j] is a func call, record and copyReach later
				int h=j-1;
				while(h>=0 && funcNodeMap.find(nbhrs[h])==funcNodeMap.end()){
					copyLaterPairs.push(pair<int,int>(nbhrs[j],nbhrs[h]));
					h--;
				}
				h=j+1;
				while(h<nbhrs.size()){
					copyLaterPairs.push(pair<int,int>(nbhrs[j],nbhrs[h]));
					h++;
				}
				continue;
			}		
			v=nbhrs[j];
		}
		if(v==-1){			//not found
			vertexVisited[peek].clear();
			myStack.pop_back();
			isInStack[peek]=false;
		}else{
			vertexVisited[peek][v]=true;
			myStack.push_back(v);
			isInStack[v]=true;
		}
		//dumpStack(myStack);				//print

		if((!myStack.empty()) && (myStack.back()==end)){
			fprintf(stderr,"---------find:%d--------\n",end);
			//get a trace in myStack
			assert(myStack[0]==begin);
			for(i=1;i<myStack.size();i++){
				copyReach(reachability[begin],reachability[myStack[i]]);
			}
			myStack.pop_back();
			isInStack[end]=false;
		}
	
	
	}

}

void handleLoop(graph_t *graph,vector<vector<bool> > &reachability){
	unsigned int i;
	int first,second;
	//fprintf(stderr,"size %d\n",loopsMap.size());
	vector<pair<int,int> > prevent;	
	for(i=0;i<loopsMap.size();i++){
		first=loopsMap[i].first;
		second=loopsMap[i].second;
		fprintf(stderr,"Trace: %d=>%d:\n",first,second);
		if(first==second)
			continue;	
		if(nothingCopy(reachability[first])){		//optimize: nothing to copy
			prevent.push_back(pair<int,int>(first,second));
			continue;
		}
		if(sameReachable(reachability[first],reachability[second]))
			continue;
		//a find all path algorithm
		//bound is |first-second|+1
		int bound=(first>second)?(first-second+1):(second-first+1);
		assert(bound>1);
		findAllPath(graph,first,second,reachability,bound);

	}
	vector<pair<int,int> >::iterator it;
	bool changed=true;
	while(changed){
		changed=false;
		for(it=prevent.begin();it!=prevent.end();){
			first=prevent[i].first;
			second=prevent[i].second; 
			if(!nothingCopy(reachability[prevent[i].first])){
				changed=true;
				int bound=(first>second)?(first-second+1):(second-first+1);
				findAllPath(graph,prevent[i].first,prevent[i].second,reachability,bound);
				it=prevent.erase(it);
			}else{
				it++;
			}
		}
	}




}
void copyToFunctionReach(graph_t *cfg_origin,int dst,int src,vector<vector<bool> > &reachability){		//copy src to all func node of dst
	
	unsigned int k,i;
	hash_map<int,bool>	visitedMap;
	for(k=0;k< cfg_origin->size(); k++)
		visitedMap[k]=false;	
	
	int cur;

	queue<int> q;
	q.push(dst);
	while(!q.empty()){
		cur=q.front();
		visitedMap[cur]=true;	//visit cur
		q.pop();
		adj_list_t &nbhrs=(*cfg_origin)[cur];

		for(i=0;i<nbhrs.size();i++){
			if(nbhrs[i]<=0)
				continue;
			if(funcNodeMap.find(nbhrs[i])!=funcNodeMap.end()){		//in dst ,call another func nbhrs[i],just add pair to copyLater
				int h=i-1;
				while(h>=0 && funcNodeMap.find(nbhrs[h])==funcNodeMap.end()){
					copyLaterPairs.push(pair<int,int>(nbhrs[i],nbhrs[h]));
					h--;
				}
				h=i+1;
				while(h<nbhrs.size()){
					copyLaterPairs.push(pair<int,int>(nbhrs[i],nbhrs[h]));
					h++;
				}
				continue;
			}		
			if(!visitedMap[nbhrs[i]]){
				copyReach(reachability[src],reachability[nbhrs[i]]);
				q.push(nbhrs[i]);
			}
		}
	}
}
void reachability(graph_t *graph,int mainId,set<int> &branches,graph_t *cfg_origin){
	unsigned int i;
	vector<bool> reach(pathLen,false);
	vector<vector<bool> > reachability(graph->size(),reach);
	hash_map<int,color_t>	visitedMap;
	/*for(i=1;i<=pathLen;i++)
		visitedMap[-i]=white;
		*/
	for(i=1;i< graph->size(); i++)
		visitedMap[i]=white;
	dfSearch(graph,mainId,reachability,reachability[mainId],visitedMap);
	//	handleLoops
	handleLoop(graph,reachability);
	//	copyToFunctionReach;

	while(!copyLaterPairs.empty()){

		int first=copyLaterPairs.front().first;
		int second=copyLaterPairs.front().second;
		copyLaterPairs.pop();
		//copy Reach from second to function node of first
		
		if(nothingCopy(reachability[second]))
			continue;
		assert(funcNodeMap.find(first)!=funcNodeMap.end());
		copyToFunctionReach(cfg_origin,first,second,reachability);
	}

	//write reachability file

	ofstream out("reachability");
	//ofstream cfg_out("cfg_reachability");
	unsigned j,k;
	//unsigned h;
	for(j=0;j< reachability.size();j++){
		if(branches.find(j)==branches.end()){
			/*cfg_out<<j;
			for(h=0;h< (reachability[j]).size();h++){
				cfg_out<<" "<<(reachability[j][h]==true?1:0);	
			}
			cfg_out<<endl;
			*/
			continue;
		}
		out<<j;
		//cfg_out<<j;
		for(k=0;k< (reachability[j]).size();k++){
			out<<" "<<(reachability[j][k]==true?1:0);
			//cfg_out<<" "<<(reachability[j][k]==true?1:0);
		}
		out<<endl;
		//cfg_out<<endl;
	}
	out.close();
	//cfg_out.close();

}
int main(void) {
	int mainId=0;
	// First we have to read in the function -> CFG node map.
	{ ifstream in("cfg_func_map");
		string func;
		int sid;
		while (in >> func >> sid) {
			if(!func.compare("main"))
				mainId=sid;
			funcNodeMap[sid] = false;
		}
		in.close();
	}
	if(mainId==0){
		fprintf(stderr,"not found main\n");
		exit(1);
	}
	// Read in the set of branches.
	set<int> branches;
	readBranches(&branches);
	fprintf(stderr, "Read %d branches.\n", branches.size());

	// Read in the CFG.

	graph_t cfg;
	cfg.reserve(1000000);
	readCfg(&cfg);
	fprintf(stderr, "Read %d nodes.\n", cfg.size());
	graph_t cfg_origin(cfg);
	//IFDEBUG(dumpCFG(&cfg));
	addFuncNb2FuncOut(&cfg);

	//dumpCFG(&cfg_origin);
	//dumpCFG(&cfg);

	//IFDEBUG(dumpCFG(&cfg));

	reachability(&cfg,mainId,branches,&cfg_origin);

	//IFDEBUG(fprintf(stderr,"-----------pathLen %d\n",pathLen));

	return 0;
}
