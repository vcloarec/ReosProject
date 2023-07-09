#ifndef zSTRUCT_RECORD_SIZE_H
#define zSTRUCT_RECORD_SIZE_H

#include "zStructAllocation.h"


typedef struct {

	//  Private
	int structType;

	char *pathname;

	//  Record information for all data types
	int dataType;
	int version;
	int numberValues;
	int logicalNumberValues;
	//  Length (4 byte) of each data array
	int values1Number;  //  (For TS, this is data values)
	int values2Number;	//  (For TS, this is quality array)
	int values3Number;	//  (For TS, this is notes array)
	//  Length (4 byte) of the 3 header arrays
	int internalHeaderNumber;
	int header2Number;
	int userHeaderNumber;
	int allocatedSize;
	long long lastWriteTimeMillis;

	char programLastWrite[17];

	//  Time Series parameters
	int numberRecordsFound;
	int itsTimePrecisionStored;
	int tsPrecision;
	int tsTimeOffset;
	int tsProfileDepthsNumber;
	int tsBlockStartPosition;
	int tsBlockEndPosition;
	int tsValueSize;
	int tsValueElementSize;
	int tsValuesCompressionFlag;
	int tsQualityElementSize;
	int tsQualityCompressionFlag;
	int tsInotesElementSize;
	int tsInotesCompressionFlag;
	int tsCnotesLength;

	//  Paired Data parameters
	int pdNumberCurves;
	int pdNumberOrdinates;
	int ipdValueSize;
	int pdBoolIndependentIsXaxis;
	int pdLabelsLength;
	int pdPrecision;



	//  Grid parameters


	//  Text parameters

	//  Private - knowing which variables were allocated by the ztsNew functions,
	//  instead of the calling program
	char allocated[zSTRUCT_length];

} zStructRecordSize;


#endif  //  zSTRUCT_RECORD_SIZE_H

