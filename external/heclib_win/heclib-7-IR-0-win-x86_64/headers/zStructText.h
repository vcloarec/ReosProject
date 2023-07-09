#ifndef zSTRUCT_TEXT_H
#define zSTRUCT_TEXT_H

#include "zStructAllocation.h"

typedef struct {

	/*
	  This struct may hold a simple text string,
	  a text list, or a text table.

	  A text string goes into array textString, null terminated with
	  numberTextChars characters set to number (including null)
	  with the C part being the name e.g., "/Color/" for below

	  For a text table, this follows essentially the conventions of paired data
	  The first column is the ordinate or independent value,
	  the second and subsequent columns are dependent values.
	  As a example, we might have a list of colors and their characteristics:

	  Color		wave length		temperature		Energy
	  Red			long			hot			  low
	  Blue			short			cool		  high
	  Yellow		med-long		warm		  medium

	  The first column contains the ordinates (colors) and is the independent variable
	  The second and subsequent columns are dependent values
	  The top line (header) are lables.
	  textTable = "long\0short\0med-long\0hot\0cool\warm\0low\0high\medium\0"
	  The labels array would be labels = "Color\0wave length\0temperature\0energy\0"
	  Note - as a deviation from paired data, the first label applies to the
	  independent variable (in paired data, the first label applies to the first dependent)
	  The C part of the pathname contians the name of the dependent, a dash, then the
	  name of the independent, eg. "/Colors-Characteristics/"

	  A text list is just the ordinate (first) column with the C part being the name "/Colors/"

	*/

	//  Private
	int structType;

	/*  Required  */
	char *pathname;

	//  For a simple text string
	char *textString;
	int numberTextChars;

	char *textTable;
	//  The total number of characters, including nulls
	int numberTableChars;

	//  If a  table, then the number of rows and columns
	//  If a list, then numberColumns = 1, numberRows = number items
	//  If a test string, then both are 1 or 0.
	int numberRows;
	int numberColumns;


	/*  ____________ Optional __________________________   */
	//  If a text table, then numberColumns labels, each label null terminated
	char *labels;
	//  The total number of characters in *labels
	int numberLabelChars;


	/*  User header, used for further description of data/gage */
	/*  Not often used */
	int *userHeader;
	int userHeaderNumber;

	//  Informational only (on return only)
	int dataType;
	long long lastWrittenTime;  //  Seconds since 1970
	long long fileLastWrittenTime;
	char programName[17];

	//  Private - knowing which variables were allocated by the ztsNew functions,
	//  instead of the calling program
	char allocated[zSTRUCT_length];

} zStructText;


#endif // zSTRUCT_TEXT_H

