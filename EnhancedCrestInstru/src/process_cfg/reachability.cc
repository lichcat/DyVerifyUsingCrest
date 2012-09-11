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
#include <queue>
#include <map>

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

//typedef enum{white,black,gray} color_t;

namespace __gnu_cxx {
	template<> struct hash<string> {
		size_t operator()(const string& x) const {
			return hash<const char*>()(x.c_str());
		}
	};
}

map<int,string>  funcNodeMap;
hash_map<int,bool> notExitNodesMap;
//vector<pair<int,int> > loopsMap;
//hash_map<int,hash_map<int,bool> > vertexVisited;
//queue<pair<int,int> > copyLaterPairs;

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
				map<int,string>::iterator it;
				for(it=funcNodeMap.begin();it!=funcNodeMap.end();++it){
					if(0==func.compare(it->second))
						break;
				}
				if(it!=funcNodeMap.end()){
					nbhrs.push_back(it->first);	
				}else if (isPathMark(func)){
					//pathLen++;  can have more than 1 PathMarks who have same stmtId so ,can not just count to decide pathLen
					string pNsN,stmtStr;
					pNsN=func.substr(16);
					size_t stmtIdPos=pNsN.find_last_of("S");
					stmtStr = pNsN.substr(stmtIdPos+1);
					int stmtValue=atoi(stmtStr.c_str());
					pathLen=(pathLen>=stmtValue)?pathLen:stmtValue;
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

void addToFuncOut(graph_t *graph,graph_t *auxiliary,vector<int> &addedIndex, int funcInId){
	//from funcInId , bfs find the out node of function and add the addedIndex to out node(s)
	unsigned int k,i,j;
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
			if(nbhrs[i]<=0 || (funcNodeMap.count(nbhrs[i])==1))
				continue;
			if(!visitedMap[nbhrs[i]])
				q.push(nbhrs[i]);
		}
		hash_map<int,bool>::const_iterator it=notExitNodesMap.find(cur);
		if((nbhrs.size()==0) && (it==notExitNodesMap.end())){	//the out of func cfg
			adj_list_t &auxiliary_nbhrs=(*auxiliary)[cur];
			for(j=0;j<addedIndex.size();j++){
				if(addedIndex[j]!=cur){
					auxiliary_nbhrs.push_back(addedIndex[j]);
				}
			}
		}
	}
}
void addFuncNb2FuncOut(graph_t *graph,graph_t *auxiliary){
	unsigned int i,j;
	map<int,vector<int> > funcAddMap;
	for(i=0;i<graph->size();i++){
		adj_list_t &nbhrs = (*graph)[i];
		for(j=0;j<nbhrs.size();j++){
			if(1==funcNodeMap.count(nbhrs[j])){		//nbhrs[j] is func 
				int k= j-1;
				while(k>=0){
					if(0 == funcNodeMap.count(nbhrs[k])){
						funcAddMap[nbhrs[j]].push_back(nbhrs[k]);
					}
					k--;
				}
				k=j+1;
				while(k<nbhrs.size()){
					if(0==funcNodeMap.count(nbhrs[k])){
						funcAddMap[nbhrs[j]].push_back(nbhrs[k]);
						fprintf(stderr,"------------%d not func after func:%d----------\n",nbhrs[k],nbhrs[j]);
						}
					k++;
				}
			}
		}
	}
	
	map<int,vector<int> >::iterator it;
	for(it=funcAddMap.begin();it!=funcAddMap.end();it++){
		addToFuncOut(graph,auxiliary,it->second, it->first);
	}

}
void reverseGraph(graph_t *graph,graph_t * reverseCfg){
	unsigned int i,j;
	unsigned int size=graph->size();
	int pathnode;
	reverseCfg->resize(size+pathLen);
	for(i=0;i<size;i++){
		adj_list_t &nbhrs = (*graph)[i];
		for(j=0;j<nbhrs.size();j++){
			//edge of i->nbhrs[j]
			if(nbhrs[j]<0){
				pathnode=(-nbhrs[j])+size-1;
				adj_list_t &r_nbhrs=(*reverseCfg)[pathnode];
				r_nbhrs.push_back(i);
			}
			else{
				adj_list_t &r_nbhrs=(*reverseCfg)[nbhrs[j]];
				r_nbhrs.push_back(i);
			}
		}
	}
}
void reachability(int size,set<int> &branches,graph_t *reverseCfg){
	vector<bool> reach(pathLen,false);
	vector<vector<bool> > reachability(size,reach);
	queue<int> q;
	unsigned int i,j,k;
	hash_map<int,bool>	visitedMap;
	//hash_map<int,int> parent;
	for(i=0;i<pathLen;i++){
		//BFS
		for(j=0;j<reverseCfg->size();j++)
			visitedMap[j]=false;
		
		int begin=size+i;
		q.push(begin);
		visitedMap[begin]=true;
		//parent[begin]=0;
		while(!q.empty()){
			int peek=q.front();
			q.pop();
			adj_list_t &nbhrs=(*reverseCfg)[peek];
			for(k=0;k<nbhrs.size();k++){
				if(!visitedMap[nbhrs[k]]){
					q.push(nbhrs[k]);
					//parent[nbhrs[k]]=peek;
					visitedMap[nbhrs[k]]=true;
					if(nbhrs[k]<size)
						reachability[nbhrs[k]][i]=true;
				}
			}
		}
	}

	//write reachability file

	ofstream out("reachability");
	unsigned m,n;
	for(m=0;m< reachability.size();m++){
		if(branches.find(m)==branches.end()){
			continue;
		}
		out<<m;
		for(n=0;n< (reachability[m]).size();n++){
			out<<" "<<(reachability[m][n]==true?1:0);
		}
		out<<endl;
	}
	out.close();

	/*ofstream cfr_out("cfg_reachability");
	for(m=0;m< reachability.size();m++){
		cfr_out<<m;
		for(n=0;n< (reachability[m]).size();n++){
			cfr_out<<" "<<(reachability[m][n]==true?1:0);
		}
		cfr_out<<endl;
	}
	cfr_out.close();
*/	

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
			funcNodeMap[sid] = func;
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

	fprintf(stderr, "Instrument path fragemtn's length: %d\n",pathLen);
	//dumpCFG(&cfg);
	graph_t auxiliaryCfg(cfg);
	addFuncNb2FuncOut(&cfg,&auxiliaryCfg);

	graph_t reverseCfg;
	reverseCfg.reserve(1000000);
	reverseGraph(&auxiliaryCfg,&reverseCfg);
	//dumpCFG(&cfg);
	//dumpCFG(&auxiliaryCfg);
	//dumpCFG(&reverseCfg);

	//IFDEBUG(dumpCFG(&cfg));

	reachability(cfg.size(),branches,&reverseCfg);



	return 0;
}
