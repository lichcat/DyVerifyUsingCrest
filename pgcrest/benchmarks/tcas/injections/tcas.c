
/*  -*- Last-Edit:  Fri Jan 29 11:13:27 1993 by Tarak S. Goradia; -*- */
/* $Log: tcas.c,v $
 * Revision 1.2  1993/03/12  19:29:50  foster
 * Correct logic bug which didn't allow output of 2 - hf
 * */

#include <stdio.h>
/* CREST compile */
#ifdef  CREST
#include <crest.h>
#endif
/* injections */
#include "global_array.h"
/* end */

#define OLEV       600		/* in feets/minute */
#define MAXALTDIFF 600		/* max altitude difference in feet */
#define MINSEP     300          /* min separation in feet */
#define NOZCROSS   100		/* in feet */
				/* variables */

typedef int bool;

int Cur_Vertical_Sep;
bool High_Confidence;
bool Two_of_Three_Reports_Valid;

int Own_Tracked_Alt;
int Own_Tracked_Alt_Rate;
int Other_Tracked_Alt;

int Alt_Layer_Value;		/* 0, 1, 2, 3 */
int Positive_RA_Alt_Thresh[4];

int Up_Separation;
int Down_Separation;

				/* state variables */
int Other_RAC;			/* NO_INTENT, DO_NOT_CLIMB, DO_NOT_DESCEND */
#define NO_INTENT 0
#define DO_NOT_CLIMB 1
#define DO_NOT_DESCEND 2

int Other_Capability;		/* TCAS_TA, OTHER */
#define TCAS_TA 1
#define OTHER 2

int Climb_Inhibit;		/* true/false */

#define UNRESOLVED 0
#define UPWARD_RA 1
#define DOWNWARD_RA 2

void initialize()
{
    int* __CREST_p1 = (int*)malloc(sizeof(int)*10);
    addToArray((void**)&__CREST_p1,1);
    Positive_RA_Alt_Thresh[0] = 400;
    Positive_RA_Alt_Thresh[1] = 500;
    Positive_RA_Alt_Thresh[2] = 640;
    Positive_RA_Alt_Thresh[3] = 740;
}

int ALIM ()
{
 int* __CREST_p2 = (int*)malloc(sizeof(int)*10);
 addToArray((void**)&__CREST_p2,2);
 return Positive_RA_Alt_Thresh[Alt_Layer_Value];
}

int Inhibit_Biased_Climb ()
{
    int* __CREST_p3 = (int*)malloc(sizeof(int)*10);
    addToArray((void**)&__CREST_p3,3);
    return (Climb_Inhibit ? Up_Separation + NOZCROSS : Up_Separation);
}

bool Non_Crossing_Biased_Climb()
{
    int* __CREST_p4 = (int*)malloc(sizeof(int)*10);
    addToArray((void**)&__CREST_p4,4);
    int upward_preferred;
    int upward_crossing_situation;
    bool result;

    upward_preferred = Inhibit_Biased_Climb() > Down_Separation;
    if (upward_preferred)
    {
	result = !(Own_Below_Threat()) || ((Own_Below_Threat()) && (!(Down_Separation >= ALIM())));
    }
    else
    {	
	result = Own_Above_Threat() && (Cur_Vertical_Sep >= MINSEP) && (Up_Separation >= ALIM());
    }
    return result;
}

bool Non_Crossing_Biased_Descend()
{
    int* __CREST_p5 = (int*)malloc(sizeof(int)*10);
    addToArray((void**)&__CREST_p5,5);
    int upward_preferred;
    int upward_crossing_situation;
    bool result;

    upward_preferred = Inhibit_Biased_Climb() > Down_Separation;
    if (upward_preferred)
    {
	result = Own_Below_Threat() && (Cur_Vertical_Sep >= MINSEP) && (Down_Separation >= ALIM());
    }
    else
    {
	result = !(Own_Above_Threat()) || ((Own_Above_Threat()) && (Up_Separation >= ALIM()));
    }
    return result;
}

bool Own_Below_Threat()
{
    int* __CREST_p6 = (int*)malloc(sizeof(int)*10);
    //addToArray((void**)&__CREST_p6,6);
    return (Own_Tracked_Alt < Other_Tracked_Alt);
}

bool Own_Above_Threat()
{
    int* __CREST_p7 = (int*)malloc(sizeof(int)*10);
    //addToArray((void**)&__CREST_p7,7);
    return (Other_Tracked_Alt < Own_Tracked_Alt);
}

int alt_sep_test()
{
    int* __CREST_p8 = (int*)malloc(sizeof(int)*10);
    addToArray((void**)&__CREST_p8,8);
    bool enabled, tcas_equipped, intent_not_known;
    bool need_upward_RA, need_downward_RA;
    int alt_sep;

    enabled = High_Confidence && (Own_Tracked_Alt_Rate <= OLEV) && (Cur_Vertical_Sep > MAXALTDIFF);
    tcas_equipped = Other_Capability == TCAS_TA;
    intent_not_known = Two_of_Three_Reports_Valid && Other_RAC == NO_INTENT;
    
    alt_sep = UNRESOLVED;
    
    if (enabled && ((tcas_equipped && intent_not_known) || !tcas_equipped))
    {
	need_upward_RA = Non_Crossing_Biased_Climb() && Own_Below_Threat();
	need_downward_RA = Non_Crossing_Biased_Descend() && Own_Above_Threat();
	if (need_upward_RA && need_downward_RA)
        /* unreachable: requires Own_Below_Threat and Own_Above_Threat
           to both be true - that requires Own_Tracked_Alt < Other_Tracked_Alt
           and Other_Tracked_Alt < Own_Tracked_Alt, which isn't possible */
	    alt_sep = UNRESOLVED;
	else if (need_upward_RA)
	    alt_sep = UPWARD_RA;
	else if (need_downward_RA)
	    alt_sep = DOWNWARD_RA;
	else
	    alt_sep = UNRESOLVED;
    }
    
    return alt_sep;
}
#ifdef CREST
int main()
#else
main(argc, argv)
int argc;
char *argv[];
#endif
{
#ifndef CREST
    if(argc < 13)
    {
	fprintf(stdout, "Error: Command line arguments are\n");
	fprintf(stdout, "Cur_Vertical_Sep, High_Confidence, Two_of_Three_Reports_Valid\n");
	fprintf(stdout, "Own_Tracked_Alt, Own_Tracked_Alt_Rate, Other_Tracked_Alt\n");
	fprintf(stdout, "Alt_Layer_Value, Up_Separation, Down_Separation\n");
	fprintf(stdout, "Other_RAC, Other_Capability, Climb_Inhibit\n");
	exit(1);
    }
#endif
    initialize();
    atexit(freeArray);
    atexit(crest_use);
#ifdef  CREST
    CREST_int(Cur_Vertical_Sep); 
    CREST_int(High_Confidence); 
    CREST_int(Two_of_Three_Reports_Valid); 
    CREST_int(Own_Tracked_Alt);
    CREST_int(Own_Tracked_Alt_Rate);
    CREST_int(Other_Tracked_Alt);
    CREST_int(Alt_Layer_Value);
    CREST_int(Up_Separation);
    CREST_int(Down_Separation);
    CREST_int(Other_RAC);
    CREST_int(Other_Capability);
    CREST_int(Climb_Inhibit);
#else
    Cur_Vertical_Sep = atoi(argv[1]);
    High_Confidence = atoi(argv[2]);
    Two_of_Three_Reports_Valid = atoi(argv[3]);
    Own_Tracked_Alt = atoi(argv[4]);
    Own_Tracked_Alt_Rate = atoi(argv[5]);
    Other_Tracked_Alt = atoi(argv[6]);
    Alt_Layer_Value = atoi(argv[7]);
    Up_Separation = atoi(argv[8]);
    Down_Separation = atoi(argv[9]);
    Other_RAC = atoi(argv[10]);
    Other_Capability = atoi(argv[11]);
    Climb_Inhibit = atoi(argv[12]);
#endif

    fprintf(stdout, "%d\n", alt_sep_test());
    exit(0);
}
