#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/xpath.h>

xmlDocPtr getFvdlDoc(char* docname){

	xmlDocPtr doc;
	xmlNodePtr cur;

	doc=xmlParseFile(docname);
	if(NULL==doc){
		fprintf(stderr,"Document not parsed successfully. \n");
		return NULL;
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
		return NULL;
	}
	result = xmlXPathEvalExpression(xpath,context);
	if(xmlXPathNodeSetIsEmpty(result->nodesetval)){
		printf("No result\n");
		xmlXPathFreeContext(context);
		return NULL;
	}
	xmlXPathFreeContext(context);
	return result;

}
int isTypeMemoryLeak(xmlDocPtr doc,xmlNodePtr cur){
	xmlChar* key;
	xmlNodePtr curChild;
	cur = cur->xmlChildrenNode;
	while(NULL!=cur){
		if(!xmlStrcmp(cur->name,(const xmlChar*)"ClassInfo")){
			//printf("in ClassInfo\n");
			curChild=cur->xmlChildrenNode;
			while(NULL!=curChild){
				if(!xmlStrcmp(curChild->name,(const xmlChar*)"Type")){
					key= xmlNodeListGetString(doc,curChild->xmlChildrenNode,1);
					//printf("ClassInfo/Type: %s \n",key);
					if(!xmlStrcmp(key,(const xmlChar*)"Memory Leak")){
						xmlFree(key);
						return 1;
					}
					xmlFree(key);
				}
				curChild = curChild ->next;
			}
		}
		cur = cur->next;
	}
	return 0;
}
xmlNodePtr getTrace(xmlNodePtr cur){
	xmlNodePtr curChild,curGrandSon;
	cur = cur->xmlChildrenNode;
	while(NULL!=cur){
		if(!xmlStrcmp(cur->name,(const xmlChar*)"AnalysisInfo")){
			curChild = cur ->xmlChildrenNode;
			while(NULL!=curChild){
				if(!xmlStrcmp(curChild->name,(const xmlChar*)"Unified")){
					curGrandSon=curChild->xmlChildrenNode;
					while(NULL!=curGrandSon){
						if(!xmlStrcmp(curGrandSon->name,(const xmlChar*)"Trace")){
							return curGrandSon;
						}
						curGrandSon=curGrandSon->next;
					}
				}
				curChild = curChild->next;
			}
		}
		cur = cur->next;
	}
	return NULL;
}
void getTraceInfo(xmlDocPtr doc,xmlNodePtr cur){
	xmlChar * string;
	//printf("%s\n",cur->name);
	
	//string=xmlNodeListGetString(doc, cur->xmlChildrenNode,1);
	//xmlFree(string);
}
int main(int argc, char** argv){
	int i;
	char* fvdlDocName;
	xmlDocPtr doc;
	xmlChar *xpath="//fvdl:Vulnerability";
	xmlNodeSetPtr nodeset;
	xmlXPathObjectPtr result;
	xmlNodePtr tracenode;

	if(argc <=1){
		printf("Usage: %s docname\n",argv[0]);
		return (0);
	}
	fvdlDocName=argv[1];
	doc=getFvdlDoc(fvdlDocName);

	if(NULL!=doc)
		result=getnodeset(doc,xpath);
	else
		return (0)
			;
	if(result){
		nodeset = result->nodesetval;
		for(i=0;i< nodeset->nodeNr; i++){
			if(isTypeMemoryLeak(doc, nodeset->nodeTab[i])){
				tracenode=getTrace(nodeset->nodeTab[i]);
				if(NULL==tracenode)
					printf("Resolve Error: no trace node\n");
				else
					getTraceInfo(doc,tracenode);
			}
		}
		xmlXPathFreeObject(result);
	}
	xmlFreeDoc(doc);
	xmlCleanupParser();
	return(1);

}
