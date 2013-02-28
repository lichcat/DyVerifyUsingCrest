#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <regex.h>
#include <libxml/parser.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>

int matchReg(char* str1,char* str2){
	regex_t regex;
	int reti;
	char msgbuf[100];

	reti=regcomp(&regex, str2,0);
	if(reti){
		fprintf(stderr,"could not compile regex \n");
		exit(1);
	}
	reti=regexec(&regex,str1,0,NULL,0);
	if(!reti)
		return 1;
	else if(reti == REG_NOMATCH)
		return 0;
	else{
		regerror(reti,&regex,msgbuf,sizeof(msgbuf));
		fprintf(stderr,"Regex match failed: %s\n",msgbuf);
		exit(1);
	}
}

xmlDocPtr getFvdlDoc(char* docname){

	xmlDocPtr doc;
	xmlNodePtr cur;
	xmlKeepBlanksDefault(0);
	doc=xmlParseFile(docname);
	if(NULL==doc){
		fprintf(stderr,"Document not parsed successfully. \n");
		exit(1);
	}
	cur=xmlDocGetRootElement(doc);

	if(NULL==cur){
		fprintf(stderr,"empty document\n");
		xmlFreeDoc(doc);
		return NULL;
	}
	//printf("%d\n",xmlStrcmp(cur->name,(const xmlChar*)"FVDL"));	FVDL->0 same->0
	if(xmlStrcmp(cur->name,(const xmlChar*)"FVDL")){
		fprintf(stderr,"%s is not a FVDL file!\n",docname);
		xmlFreeDoc(doc);
		return NULL;
	}
	
	return doc;
}

xmlXPathObjectPtr getnodeset(xmlDocPtr doc, xmlChar *xpath){
	int status;
	xmlXPathContextPtr context;
	xmlXPathObjectPtr result;

	context = xmlXPathNewContext(doc);
	status = xmlXPathRegisterNs(context,(const xmlChar*)"fvdl",
			(const xmlChar*)"xmlns://www.fortifysoftware.com/schema/fvdl");
	if(status!=0){
		fprintf(stderr,"XML Namespace register fail\n");
		xmlXPathFreeContext(context);
		exit(1);	
	}
	result = xmlXPathEvalExpression(xpath,context);
	if(xmlXPathNodeSetIsEmpty(result->nodesetval)){
		//fprintf(stderr,"No result\n");
		xmlXPathFreeContext(context);
		return NULL;
	}
	xmlXPathFreeContext(context);
	return result;

}
int isAssignOrReturn(xmlChar* actionType){
	if(!xmlStrcmp(actionType,(const xmlChar*)"Assign"))
		return 1;
	else if(!xmlStrcmp(actionType,(const xmlChar*)"Return"))
		return 1;
	else 
		return 0;
}
void getTraceInfo(xmlDocPtr doc,xmlNodePtr cur,FILE* fp){
	int i,j;
	xmlChar* nodeXpath=(unsigned char*)"//fvdl:Primary/fvdl:Entry/fvdl:Node";
	xmlNodeSetPtr nodeset;
	xmlXPathObjectPtr result;
	xmlNodePtr nodeChild;
	xmlNodePtr reasonChildNode;
	xmlChar* refxpath=NULL;
	xmlNodeSetPtr refNodeset=NULL;
	xmlXPathObjectPtr refResult=NULL;

	xmlChar *file,*line,*actionType,*actionString;
	//xmlChar *lineEnd
	xmlChar *markWord=NULL;
	xmlChar *traceRefId;
	
	result=getnodeset((xmlDocPtr)cur,nodeXpath);
	if(result){
		nodeset = result ->nodesetval;
		for(i=0;i<nodeset->nodeNr;i++){
			nodeChild=nodeset->nodeTab[i]->xmlChildrenNode;
			while(nodeChild!=NULL){
				if(!xmlStrcmp(nodeChild->name,(const xmlChar*)"SourceLocation")){
					file=xmlGetProp(nodeChild,(const xmlChar*)"path");
					line=xmlGetProp(nodeChild,(const xmlChar*)"line");
					//lineEnd=xmlGetProp(nodeChild,(const xmlChar*)"lineEnd");
					//assert(!xmlStrcmp(line,lineEnd));
					fprintf(fp,"%s\t%s\t",file,line);
					
					xmlFree(file);
					xmlFree(line);
					//xmlFree(lineEnd);
				}else if(!xmlStrcmp(nodeChild->name,(const xmlChar*)"Action")){
					actionType=xmlGetProp(nodeChild,(const xmlChar*)"type");
					actionString=xmlNodeListGetString(doc,nodeChild->xmlChildrenNode,1);
					if(!xmlStrcmp(actionType,(const xmlChar*)"BranchNotTaken")){
                        if(matchReg((char*)actionString,(char*)"^Branch not taken: (create_html_directory(.*) != 0)$")){
                            markWord=(unsigned char*)"BT";      //!func() -> cil reverse branch , and we have to reverse
                        }else
                            markWord=(unsigned char*)"BF";		//branch false
                    }
					else if(!xmlStrcmp(actionType,(const xmlChar*)"BranchTaken")){
                        if(matchReg((char*)actionString,(char*)"Branch taken: (create_heml_directory(.*) == 0)$")){
                            markWord=(unsigned char*)"BF";
                        }else
                            markWord=(unsigned char*)"BT";		//branch true
                    }
					else if(isAssignOrReturn(actionType)
						&& (matchReg((char*)actionString,(char*)"^.* malloc.*$") || matchReg((char*)actionString,(char*)"^malloc.*$")))
						markWord=(unsigned char*)"MA";
					else if(isAssignOrReturn(actionType)
						&& (matchReg((char*)actionString,(char*)"^.* calloc.*$") || matchReg((char*)actionString,(char*)"^calloc.*$")))
						markWord=(unsigned char*)"CA";
					else if(isAssignOrReturn(actionType)
						&& (matchReg((char*)actionString,(char*)"^.* realloc.*$") || matchReg((char*)actionString,(char*)"^realloc.*$")))
						markWord=(unsigned char*)"RA";
					else if(isAssignOrReturn(actionType)
						&& (matchReg((char*)actionString,(char*)"^.* xmalloc.*$") || matchReg((char*)actionString,(char*)"^xmalloc.*$")))
						markWord=(unsigned char*)"MA";
					else if(isAssignOrReturn(actionType)
						&& (matchReg((char*)actionString,(char*)"^.* xstrdup.*$") || matchReg((char*)actionString,(char*)"^xstrdup.*$")))
						markWord=(unsigned char*)"XSD";
					else if(isAssignOrReturn(actionType)
						&& (matchReg((char*)actionString,(char*)"^.* xcalloc.*$") || matchReg((char*)actionString,(char*)"^xcalloc.*$")))
						markWord=(unsigned char*)"CA";
					else if(isAssignOrReturn(actionType)
						&& (matchReg((char*)actionString,(char*)"^.* xrealloc.*$") || matchReg((char*)actionString,(char*)"^xrealloc.*$")))
						markWord=(unsigned char*)"RA";
					else if((!xmlStrcmp(actionType,(const xmlChar*)"EndScope")) 
								&& matchReg((char*)actionString,(char*)"^.*Memory leaked$"))
						markWord=(unsigned char*)"LK";		//leak point
					else
						markWord=(unsigned char*)"PP";		//normal path point

					fprintf(fp,"%s\n",markWord);
					xmlFree(actionType);
					xmlFree(actionString);
				}else if(!xmlStrcmp(nodeChild->name,(const xmlChar*)"Reason")){
					reasonChildNode = nodeChild->xmlChildrenNode;
					if(!xmlStrcmp(reasonChildNode->name,(const xmlChar*)"TraceRef")){
						traceRefId=xmlGetProp(reasonChildNode,(const xmlChar*)"id");	
						//printf("%s\n",traceRefId);
						refxpath=(unsigned char*)"//fvdl:UnifiedTracePool/fvdl:Trace";
						refResult=getnodeset(doc,refxpath);
						if(refResult){
							refNodeset = refResult ->nodesetval;
							for(j=0;j< refNodeset->nodeNr; j++){
								if(!xmlStrcmp(traceRefId,xmlGetProp(refNodeset->nodeTab[j],(const xmlChar*)"id"))){
									//fprintf(fp,"BEGIN_TraceRef\tRefId:%s\n",traceRefId);
									getTraceInfo(doc,refNodeset->nodeTab[j],fp);
									//fprintf(fp,"END_TraceRef");
								}
							}
							xmlXPathFreeObject(refResult);
						}
					}
				}
				nodeChild=nodeChild->next;
			}
			//fprintf(fp,"\n");
		}
		xmlXPathFreeObject(result);
	}
}
int main(int argc, char** argv){
	int i;
	char* fvdlDocName;
	xmlDocPtr doc;

	FILE *fp=NULL;
	xmlChar *xpath=(unsigned char*)"//fvdl:Vulnerability[./fvdl:ClassInfo/fvdl:Type='Memory Leak']"
					"/fvdl:AnalysisInfo/fvdl:Unified/fvdl:Trace";
	xmlNodeSetPtr nodeset;
	xmlXPathObjectPtr result;

	if(argc <=1){
		printf("Usage: %s docname\n",argv[0]);
		return (0);
	}

	fvdlDocName=argv[1];
	doc=getFvdlDoc(fvdlDocName);

	if(NULL!=doc)
		result=getnodeset(doc,xpath);
	else
		return (0);
		
	if(result){
		fp=fopen("checklists","w");
		nodeset = result->nodesetval;
		for(i=0;i< nodeset->nodeNr; i++){
			fprintf(stderr,"%d\n",i);
			fprintf(fp,"%d\n",i);
			getTraceInfo(doc,nodeset->nodeTab[i],fp);
			fprintf(fp,"END_PATH\n");
		}
		xmlXPathFreeObject(result);
	}else
		printf("no result for xpath: %s\n",xpath);

	if(fp)
		fclose(fp);
	xmlFreeDoc(doc);
	xmlCleanupParser();
	return(1);

}
