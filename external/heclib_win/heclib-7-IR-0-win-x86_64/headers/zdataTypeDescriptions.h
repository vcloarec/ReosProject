
#ifndef Z_DATA_TYPE_DESCRIPTIONS_H
#define Z_DATA_TYPE_DESCRIPTIONS_H


#define STRUCT_TYPE_TRANSFER		1
#define STRUCT_TYPE_RECORD_SIZES	2
#define STRUCT_TYPE_RECORD_BASICS	3
#define STRUCT_TYPE_RECORD_ADDRESSES	4
#define STRUCT_TYPE_CATALOG			10
#define STRUCT_TYPE_ARRAY			90

#define DATA_TYPE_Other 0

//  DSS version 7.  Version 7 can hold all type in a single record.
#define DATA_TYPE_ARRAY 90
//  DSS Version 6 descriptions.  Version 6 can only hold one type in a record.
//  That type is save in internalHeader[0]
#define DATA_TYPE_INT_ARRAY 91
#define DATA_TYPE_FLOAT_ARRAY 92
#define DATA_TYPE_DOUBLE_ARRAY 93

#define DATA_TYPE_RTS 100
#define DATA_TYPE_RTS_PATTERN 101
#define DATA_TYPE_RTS_PROFILE 102
#define DATA_TYPE_RTD 105
#define DATA_TYPE_RTD_PATTERN 106
#define DATA_TYPE_RTD_PROFILE 107
#define DATA_TYPE_ITS 110
#define DATA_TYPE_ITS_PATTERN 111
#define DATA_TYPE_ITS_PROFILE 112
#define DATA_TYPE_ITD 115
#define DATA_TYPE_ITD_PATTERN 116
#define DATA_TYPE_ITD_PROFILE 117
#define DATA_TYPE_PD  200
#define DATA_TYPE_PDD 205

#define DATA_TYPE_TEXT			300
#define DATA_TYPE_TEXT_TABLE	310

#define DATA_TYPE_SPATIAL_TIN	450

#define DATA_TYPE_FILE			600
#define DATA_TYPE_IMAGE			610

#define DATA_TYPE_LOCATION 20
#define DATA_TYPE_20			"Location"
#define DATA_TYPE_ABBR_20		"LOC"
#define DATA_TYPE_90			"Array"
#define DATA_TYPE_ABBR_90		"ARR"

#define DATA_TYPE_100			"Regular-interval time series"
#define DATA_TYPE_ABBR_100	"RTS"
#define DATA_TYPE_101			"Regular-interval time series pattern"
#define DATA_TYPE_ABBR_101	"RTPA"
#define DATA_TYPE_102			"Regular-interval time series profile"
#define DATA_TYPE_ABBR_102	"RTPR"
#define DATA_TYPE_105			"Regular-interval time series doubles"
#define DATA_TYPE_ABBR_105	"RTD"
#define DATA_TYPE_107			"Regular-interval time series double profile"
#define DATA_TYPE_ABBR_107	"RTPD"
#define DATA_TYPE_110			"Irregular-interval time series"
#define DATA_TYPE_ABBR_110	"ITS"
#define DATA_TYPE_111			"Irregular-interval time series pattern"
#define DATA_TYPE_ABBR_111	"ITPA"
#define DATA_TYPE_112			"Irregular-interval time series profile"
#define DATA_TYPE_ABBR_112	"ITPR"
#define DATA_TYPE_115			"Irregular-interval time series doubles"
#define DATA_TYPE_ABBR_115	"ITD"
#define DATA_TYPE_117			"Irregular-interval time series double profile"
#define DATA_TYPE_ABBR_117	"ITPD"

#define DATA_TYPE_200			"Paired Data"
#define DATA_TYPE_ABBR_200	"PD"
#define DATA_TYPE_205			"Paired Data doubles"
#define DATA_TYPE_ABBR_205	"PDD"

#define DATA_TYPE_300			"Text Data"
#define DATA_TYPE_ABBR_300	"TXT"
#define DATA_TYPE_310			 "Text Table"
#define DATA_TYPE_ABBR_310	"TT"

#define DATA_TYPE_UGT  400
#define DATA_TYPE_UG   401
#define DATA_TYPE_HGT  410
#define DATA_TYPE_HG   411
#define DATA_TYPE_AGT  420
#define DATA_TYPE_AG   421
#define DATA_TYPE_SGT  430
#define DATA_TYPE_SG   431

#define DATA_TYPE_400  "Gridded - Undefined grid with time"
#define DATA_TYPE_401  "Gridded - Undefined grid"
#define DATA_TYPE_410  "Gridded - HRAP grid with time reference"
#define DATA_TYPE_411  "Gridded - HRAP grid"
#define DATA_TYPE_420  "Gridded - Albers with time reference"
#define DATA_TYPE_421  "Gridded - Albers"
#define DATA_TYPE_430  "Gridded - Specified Grid with time reference"
#define DATA_TYPE_431  "Gridded - Specified Grid"
#define DATA_TYPE_450  "Spatial - TIN"

#define DATA_TYPE_600 "Generic File"
#define DATA_TYPE_610 "Image"

#define DATA_TYPE_UNDEFINED  "Undefined data type"





#endif //  Z_DATA_TYPE_DESCRIPTIONS_H

