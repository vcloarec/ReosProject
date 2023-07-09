#ifndef zSTRUCT_BASIC_H
#define zSTRUCT_BASIC_H


/*
*  A little trick to try and make C use inheritance
*
*  structType is the data type, if there is only one for that kind of data,
*  or the first type for the set (e.g., 100 for time series, but there are many TS),
*  and 1-9 for fundamental (low level) structs
*
*  structType:
*		0:  Invalid
*		1:  zStructTransfer
*		2:  zStructRecordSize
*		20 (DATA_TYPE_LOCATION): zStructLocation
*		100:  zStructTimeSeries
*		200:  zStructPairedData.h
*		300:  Text
*		310 (DATA_TYPE_TEXT_TABLE):
*/

typedef struct {

	int structType;

} zStructBasic;



#endif  //  zSTRUCT_BASIC_H

