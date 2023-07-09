#ifndef zSTRUCT_TRANSFER_H
#define zSTRUCT_TRANSFER_H

#include "zStructAllocation.h"


//  This is primarily an internal struct
//
//  For reading:
//		Mode == 0:   Don't read this array.
//		Mode == 1:	 Allocate space and read into new space.
//					 Number will contain number allocated.
//		Mode >  1:	 Space has already been allocated and this is
//					 the maximum number ints available.
//

typedef struct {

	//  Private
	int structType;

	char *pathname;
	int pathnameLength;
	int dataType;

	int *internalHeader;
	int internalHeaderNumber;
	int internalHeaderMode;

	int *header2;
	int header2Number;
	int header2Mode;

	int *userHeader;
	int userHeaderNumber;
	int userHeaderMode;

	int *values1;
	int values1Number;
	int values1Mode;

	int *values2;
	int values2Number;
	int values2Mode;

	int *values3;
	int values3Number;
	int values3Mode;

	int numberValues;
	int logicalNumberValues;
	int totalAllocatedSize;
	int totalExpandedSize;

	int version;
	int insufficientSpace;
	long long lastWrittenTime;
	long long fileLastWrittenTime;
	char programName[17];

	long long *info;

	char allocated[zSTRUCT_length];

} zStructTransfer;


#endif // zSTRUCT_TRANSFER_H

