#include <stdio.h>
#include <assert.h>
#include <libxml/parser.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>

xmlDocPtr getFvdlDoc(char* docname){

	xmlDocPtr doc;
	xmlNodePtr cur;
	xmlKeepBlanksDefault(0);
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
void getTraceInfo(xmlDocPtr doc,xmlNodePtr cur){
	int i;
	xmlChar* nodeXpath=(unsigned char*)"//fvdl:Entry/fvdl:Node";
	xmlNodeSetPtr nodeset;
	xmlXPathObjectPtr result;
	xmlNodePtr nodeChild;

	xmlChar *file,*line,*lineEnd,*actionType,*actionString;

	result=getnodeset((xmlDocPtr)cur,nodeXpath);
	if(result){
		nodeset = result ->nodesetval;
		for(i=0;i<nodeset->nodeNr;i++){
			nodeChild=nodeset->nodeTab[i]->xmlChildrenNode;
			while(nodeChild!=NULL){
				if(!xmlStrcmp(nodeChild->name,(const xmlChar*)"SourceLocation")){
					file=xmlGetProp(nodeChild,(const xmlChar*)"path");
					line=xmlGetProp(nodeChild,(const xmlChar*)"line");
					lineEnd=xmlGetProp(nodeChild,(const xmlChar*)"lineEnd");
					assert(!xmlStrcmp(line,lineEnd));
					//printf("sourcelocation::file: %s \t sourcelocation::line:%s \n",file,line);

					xmlFree(file);
					xmlFree(line);
					xmlFree(lineEnd);
				}else if(!xmlStrcmp(nodeChild->name,(const xmlChar*)"Action")){
					actionType=xmlGetProp(nodeChild,(const xmlChar*)"type");
					actionString=xmlNodeListGetString(doc,nodeChild->xmlChildrenNode,1);
					//printf("action::type %s \t action::string %s \n",actionType,actionString);
					xmlFree(actionType);
					xmlFree(actionString);
				}

				nodeChild=nodeChild->next;
			}
		}
		xmlXPathFreeObject(result);
	}
}
int main(int argc, char** argv){
	int i;
	char* fvdlDocName;
	xmlDocPtr doc;
	//xmlChar *xpath="//fvdl:Vulnerability";
	xmlChar *xpath=(unsigned char*)"//fvdl:Vulnerability[./fvdl:ClassInfo/fvdl:Type='Memory Leak']"
					"/fvdl:AnalysisInfo/fvdl:Unified/fvdl:Trace/fvdl:Primary";
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
		return (0)
			;
	if(result){
		nodeset = result->nodesetval;
		for(i=0;i< nodeset->nodeNr; i++){
			getTraceInfo(doc,nodeset->nodeTab[i]);
		}
		xmlXPathFreeObject(result);
	}
	xmlFreeDoc(doc);
	xmlCleanupParser();
	return(1);

}
