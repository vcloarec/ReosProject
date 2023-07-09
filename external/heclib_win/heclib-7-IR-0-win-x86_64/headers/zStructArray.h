#ifndef zSTRUCT_ARRAY_H
#define zSTRUCT_ARRAY_H

#include "zStructAllocation.h"

typedef struct {

	//  Private
	int structType;

	/*  Required  */
	char *pathname;

	int *intArray;
	int numberIntArray;

	float *floatArray;
	int numberFloatArray;

	double *doubleArray;
	int numberDoubleArray;

	/*  User header, used for further description of data/gage */
	/*  Not often used */
	int *userHeader;
	int userHeaderSize;    //  Size user header array; maximum to read
	int userHeaderNumber;  //  number read


	//  Informational only (on return only)
	int dataType;
	long long lastWrittenTime;  //  Seconds since 1970
	long long fileLastWrittenTime;
	char programName[17];

	int numberAttributes;
	char **attributeKeys;
	char **attributes;

	//  Private - knowing which variables were allocated by the new functions,
	//  instead of the calling program
	char allocated[zSTRUCT_length];

} zStructArray;


#endif  //zSTRUCT_ARRAY_H

