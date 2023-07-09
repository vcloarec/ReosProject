#ifndef zSTRUCT_ALLOCATION_H
#define zSTRUCT_ALLOCATION_H

#include "zStructBasic.h"

#define	zSTRUCT_pathname 0
#define	zSTRUCT_pathnameInternal 1
#define	zSTRUCT_timeZoneName 2
#define	zSTRUCT_otherInformation 3
#define	zSTRUCT_userHeader 4

#define	zSTRUCT_locationPathInternal 23

//  Time Series
#define	zSTRUCT_TS_units 5
#define	zSTRUCT_TS_type 6
#define	zSTRUCT_TS_quality 7
#define	zSTRUCT_TS_inotes 8
#define	zSTRUCT_TS_cnotes 9
#define	zSTRUCT_TS_timeWindow 10
#define	zSTRUCT_TS_times 11
#define	zSTRUCT_TS_floatValues 12
#define	zSTRUCT_TS_doubleValues 13
#define	zSTRUCT_TS_profileLabels 14
#define	zSTRUCT_TS_profileLabelArray 15
#define	zSTRUCT_TS_profileFloatDepths 16
#define	zSTRUCT_TS_profileFloatValues 17
#define	zSTRUCT_TS_profileDoubleDepths 18
#define	zSTRUCT_TS_profileDoubleValues 19
#define	zSTRUCT_TS_profileUnitsDepths 20
#define	zSTRUCT_TS_profileUnitsValues 21
#define	zSTRUCT_TS_locationStruct 22

//  Paired Data
#define	zSTRUCT_unitsIndependent 5
#define	zSTRUCT_typeIndependent 6
#define	zSTRUCT_unitsDependent 7
#define	zSTRUCT_typeDependent 8
#define	zSTRUCT_PD_floatOrdinates 9
#define	zSTRUCT_PD_floatValues 10
#define	zSTRUCT_PD_doubleOridnates 11
#define	zSTRUCT_PD_doubleValues 12
#define	zSTRUCT_PD_labels 13
#define	zSTRUCT_PD_locationStruct 14

//  Text data
#define	zSTRUCT_TX_textString 7
#define	zSTRUCT_TX_textTable 8
#define	zSTRUCT_TX_labels	9

//  Arrays
#define	zSTRUCT_ARRAY_INT		7
#define	zSTRUCT_ARRAY_FLOAT		8
#define	zSTRUCT_ARRAY_DOUBLE	9


//  Tin data
#define	zSTRUCT_TIN_xCoordinate				7
#define	zSTRUCT_TIN_yCoordinate				8
#define	zSTRUCT_TIN_value					9
#define	zSTRUCT_TIN_pointType				10
#define	zSTRUCT_TIN_numberConnections		12
#define	zSTRUCT_TIN_SpatialReferenceSystem	13
#define	zSTRUCT_TIN_SRSName					14
#define	zSTRUCT_TIN_SRSUnits				15
#define	zSTRUCT_TIN_units					16
#define	zSTRUCT_TIN_type					17
#define	zSTRUCT_TIN_timeZoneName			18
#define	zSTRUCT_TIN_connection				19
#define	zSTRUCT_TIN_label					20

//  Internal transfer data
#define	zSTRUCT_TRANS_internalHeader	5
#define	zSTRUCT_TRANS_header2			6
#define	zSTRUCT_TRANS_values3			7
#define	zSTRUCT_TRANS_values1			8
#define	zSTRUCT_TRANS_values2			9


#define zSTRUCT_length 25


//  Time series internal header

#define INT_HEAD_timeGranularity		0
#define INT_HEAD_precision				1
#define INT_HEAD_timeOffset				2
#define INT_HEAD_profileDepthsNumber	3
#define INT_HEAD_blockStartPosition		4
#define INT_HEAD_blockEndPosition		5
#define INT_HEAD_valuesNumber			6
#define INT_HEAD_valueSize				7
#define INT_HEAD_valueElementSize		8
#define INT_HEAD_valuesCompressionFlag	9
#define INT_HEAD_qualityNumber			10
#define INT_HEAD_qualityElementSize		11
#define INT_HEAD_qualityCompressionFlag 12
#define INT_HEAD_inotesNumber			13
#define INT_HEAD_inotesElementSize		14
#define INT_HEAD_inotesCompressionFlag	15
#define INT_HEAD_cnotesLength			16
#define INT_HEAD_units					17

//  Paired data internal header
#define INT_HEAD_pdNumberOrdinates		0
#define INT_HEAD_pdNumberCurves			1
#define INT_HEAD_pdBoolIndependentIsXaxis 2
#define INT_HEAD_pdLabelsLength			3
#define INT_HEAD_pdPrecision			4
#define INT_HEAD_pdUnits				5


#define INT_HEAD_SIZE 200

/*
		Internal Header Definition

	int internalHeader[INT_HEAD_SIZE];

	pos	Use

	1   precision
	2   timeOffsetSeconds
	3   unused
	4   blockStartPosition
	5   blockEndPosition
	6   valuesNumber
	7   valuesSizeRead
	8   valuesCompressionFlag
	9   qualityNumber
	10  qualityElementSize
	11  qualityCompressionFlag
	12  inotesNumber
	13  inotesElementSize
	14  inotesCompressionFlag
	15  cnotesLength
	16+ units, type, timezone separated by lf
	*/

#endif  //  zSTRUCT_ALLOCATION_H 0

