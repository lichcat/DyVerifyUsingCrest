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
#include <ctype.h>
#include <fstream>
#include <limits>
#include <set>
#include <sstream>
#include <string>
#include <vector>
#include <ext/hash_map>

#include <regex.h>
//for print debug
//#include <iostream>
#include <assert.h>

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

namespace __gnu_cxx {
	template<> struct hash<string> {
		size_t operator()(const string& x) const {
			return hash<const char*>()(x.c_str());
		}
	};
}

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
	// First we have to read in the function -> CFG node map.
	hash_map<string,int> funcNodeMap;
	{ ifstream in("cfg_func_map");
		string func;
		int sid;
		while (in >> func >> sid) {
			funcNodeMap[func] = sid;
		}
		in.close();
	}

	// No we can read in the CFG edges, substituting the correct CFG nodes
	// for function calls.
	ifstream in("cfg");

	string line;
	int pathLen=0;		// count staticpathstmt number
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
				hash_map<string,int>::iterator it = funcNodeMap.find(func);
				if (it != funcNodeMap.end())
					nbhrs.push_back(it->second);
				else if (isPathMark(func)){
					pathLen++;
					string pNsN,stmtStr;
					pNsN=func.substr(16);
					size_t stmtIdPos=pNsN.find_last_of("S");
					stmtStr = pNsN.substr(stmtIdPos+1);
					int stmtValue=atoi(stmtStr.c_str());
					//cout<<"match "<<(-stmtValue)<<endl;
					nbhrs.push_back(-stmtValue);
				}
			}
			line_in.get();
		}
	}
	//cout<<"pathLen "<<pathLen<<endl;
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
int main(void) {

	// Read in the set of branches.
	set<int> branches;
	readBranches(&branches);
	fprintf(stderr, "Read %d branches.\n", branches.size());

	// Read in the CFG.
	graph_t cfg;
	cfg.reserve(1000000);
	readCfg(&cfg);
	fprintf(stderr, "Read %d nodes.\n", cfg.size());
	//IFDEBUG(dumpCFG(&cfg);)

	return 0;
}
