#include <iostream>
#include <fstream>
#include <string>

#include <stdio.h>
#include <assert.h>

using namespace std;

int main(){
	ifstream fin("checklists");
	assert(fin);

	string count;
	while(fin>>count){
		string fn("currentCheck");
		fn+=count;
		ofstream fout(fn.c_str());
		fout<<count;
		string str;
		do{
			getline(fin,str);
			fout<<str<<endl;
		}while(str.compare("END_PATH"));
		fout.close();
	}
	fin.close();
	return 0;
}
