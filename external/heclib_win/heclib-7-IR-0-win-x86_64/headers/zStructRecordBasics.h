#ifndef zSTRUCT_RECORD_BASICS_H
#define zSTRUCT_RECORD_BASICS_H

#include "zStructAllocation.h"


typedef struct {

	//  Private
	int structType;

	char *pathname;

	//  Basic record information for all data types
	int recordType;
	int version;
	int numberValues;		//  The actual number stored (excluding missing) (Daily = 1 to 365)
	int logicalNumberValues;	//  What the user expects including missing (Daily = 365)
	//  Length (4 byte) of each data array
	int values1Number;  //  (For TS, this is data values)
	int values2Number;	//  (For TS, this is quality array)
	int values3Number;	//  (For TS, this is notes array)
	//  Length (4 byte) of the 3 header arrays
	int internalHeaderNumber;
	int header2Number;
	int userHeaderNumber;
	int allocatedSize;	//  Total size of all (in ints)

	long long recLastWriteTimeMillis;
	long long recCreationTimeMillis;
	long long fileLastWriteTimeMillis;
	long long fileCreationTimeMillis;

	int tableHash;
	long long pathnameHash;

	//  Private
	char allocated[zSTRUCT_length];

} zStructRecordBasics;


#endif  //  zSTRUCT_RECORD_BASICS_H

