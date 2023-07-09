#ifndef zSTRUCT_RECORD_ADDRESSES_H
#define zSTRUCT_RECORD_ADDRESSES_H

#include "zStructAllocation.h"

//  Contains info not passed (not normally needed) for a single record

typedef struct {

	//  Private
	int structType;

	//  Pathname, etc., is usually passed in zStructRecordSize

	//  Internals
	long long tableHash;
	long long hashCode;
	int binSize;
	int infoLength;
	int totalAllocatedSize;  // (4 byte words)

	long long hashAddress;
	long long binAddress;
	long long infoAddress;
	long long internalHeaderAddress;
	long long header2Address;
	long long userHeaderAddress;
	long long values1Address;
	long long values2Address;
	long long values3Address;

} zStructRecordAddresses;

#endif  //  zSTRUCT_RECORD_ADDRESSES_H

