#ifndef zSTRUCT_PAIRED_DATA_H
#define zSTRUCT_PAIRED_DATA_H

#include "zStructAllocation.h"


typedef struct {

	//  Private
	int structType;

	/*  Required  */
	char *pathname;

	int numberCurves;		//  (Total number of columns)
	int numberOrdinates;	//  (Total number of rows)

	//  For storing or retrieving a portion of a data set
	//  (Data set must exist!)
	//  The corresponding ordinate set is always read (but never written)
	//  Set to zero to ignore (store / read all)
	int startingCurve;
	int endingCurve;
	int startingOrdinate;
	int endingOrdinate;
	int numberCurvesInStruct;  //  Only on return
	int numberOrdinatesInStruct;

	//  values is a double character pointer array, e.g., char *table[50][10];  (50 rows by 10 columns).
	//  C doesn't do multi-dimensioned arrays very well, so we get a char *table[500] style array,
	//  and figure out the row-column positions from that.
	//  Since, these are just pointers, we'll keep the actual strings in textArray.
	//  When storing data, you can have the actual strings wherever and we'll copy into an array
	//  When retrieving, carray will contain the data.
	//  Don't forget to call zstructFree when done to release the data
	//  (Otherwise, you'll have a huge memory leak)

	//  Data
	float *floatOrdinates;
	float *floatValues;			//  Either float or double, not both
	double *doubleOrdinates;	//  The array not used must be null
	double *doubleValues;

	int sizeEachValueRead;  //  1 = float, 2 = double
	//int xprec = precision / 10;
	//int yprec = precision - (xprec * 10);
	int xprecision;			//  -1 = not set, otherwise decimal places for each value
	int yprecision;

	char *unitsIndependent;
	char *typeIndependent;
	char *unitsDependent;
	char *typeDependent;
	int boolIndependentIsXaxis;

	/*  ____________ Optional __________________________   */

	//  labels is a string of characters that contain a label for each curve.
	//  Each label must be null terminated.  The total length of labels is labelsLength
	//  e.g., labels = "Residential Commercial Ag", where ' ' is really '\0'
	char *labels;
	int labelsLength;

	char *timeZoneName;   //  Time zone of the data (may or may not match location time zone)

	/*  User header, used for further description of data/gage */
	/*  Not often used */
	int *userHeader;
	int userHeaderNumber;

	int *otherInfo;
	int otherInfoNumber;

	zStructLocation *locationStruct;

	//  Informational only (on return only)
	int dataType;
	long long lastWrittenTime;  //  Seconds since 1970
	long long fileLastWrittenTime;
	char programName[17];

	//  Private - knowing which variables were allocated by the ztsNew functions,
	//  instead of the calling program
	char allocated[zSTRUCT_length];


} zStructPairedData;

#endif  //zSTRUCT_PAIRED_DATA_H

