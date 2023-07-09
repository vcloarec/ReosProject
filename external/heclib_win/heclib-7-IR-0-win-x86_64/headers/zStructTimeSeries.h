#ifndef zSTRUCT_TIME_SERIES_H
#define zSTRUCT_TIME_SERIES_H

#include "zStructAllocation.h"
#include "zStructTsTimeWindow.h"
#include "zStructLocation.h"

typedef struct {

	//  Private
	int structType;

	char *pathname;  	  
	
	//  Time information
	int julianBaseDate;		//  julianBaseDate (* timeGranularitySeconds) + times[i] is real date (often just zero). 
							//  julianBaseDate is only added to times array, not start end or other julian dates
	int startJulianDate;	//  Time window start date and time (independent of base date)
	int startTimeSeconds;	//  Code uses seconds, regardless of granularity
	int endJulianDate;		//  Time window end date and time  (includes missing)
	int endTimeSeconds;		//    
	//  Number of seconds each unit in times array has.
	//  Set to zero on read for default (usually minutes).  Returns with actual value
	//  On write, zero (default) is minutes, or set to actual value
	//  Note - how the times are stored is independent of this; always seconds except for ir-century
	int timeGranularitySeconds;  
	int timeIntervalSeconds;  //  Informational (on return only)
	int timeOffsetSeconds;  //  Informational (on return only)
	int *times;	 //*  Time from julianBaseDate, using timeGranularitySeconds.  Required for irregular, ignored for regular
	int boolRetrieveAllTimes;  //  On retrieve, expand time window to get all (times) data for this path
	
	//  Data
	int numberValues;  //  Number of values and times.  For profile, number of times.
	int sizeEachValueRead;  //  1 = float, 2 = double
	int precision;      //  -1 = not set, otherwise decimal places for each value
	float *floatValues;  /*  Either float or double, not both  */
	double *doubleValues;  //  The array not used must be null
	char *units;
	char *type;

	//  Pattern data, such as average daily temp, a hydrograph, etc.
	//  Usually has a time reference, but no specific time (e.g., day of the year)
	int boolPattern;  
	
	//  Profile Data - use either arrays above or these, not both
	int profileDepthsNumber;
	//  either floats or doubles, not both!  (set other to null.)
	float *floatProfileDepths;		//   depths[numberDepths]
	float *floatProfileValues;    //  values[numberDepths][numberTimes];
	double *doubleProfileDepths;	//   depths[numberDepths]
	double *doubleProfileValues;   //  values[numberDepths][numberTimes];
	char *unitsProfileDepths;
	char *unitsProfileValues;

	
	char *timeZoneName;   //  Time zone of the data (may or may not match location time zone)
	
	//  Quality and Notes (Optional)
	//  meant to be a pointer to something like int quality[1000][2]
	int *quality;
	//  The length of each quality element (e.g., 2).  0 for no quality
	int qualityElementSize;
	int qualityArraySize;   //  Total int size used for retrieval only (will not be greater).  Not used for storing

	//   Note - you cannot have both inotes and cnotes; one or the other or neither.
	//  (they occupy the same space).  inotes are fixed length, cnotes are \0 terminated variable length.
	int *inotes;
	int inoteElementSize;
	int inotesArraySize;   //  Total int size used for retrieval only (will not be greater).  Not used for storing
	
	//   Character notes, one line per value, each line terminated by "\0"
	char *cnotes;
	int cnotesSize;   //  On retrieval, this must be the size of cnotes
	int cnotesLengthTotal;  //  Must be set for storage, returns actual value on retrieval
	
	int *userHeader;
	int userHeaderSize;    //  Size user header array; maximum to read
	int userHeaderNumber;  //  number read

	zStructLocation *locationStruct;
	
	//  Informational only (on return only)
	int dateOfFirstRecFound;

	int dataType; 	
	long long lastWrittenTime;  //  Milliseconds since 1970
	long long fileLastWrittenTime;  
	char programName[17];

	
	//  Private
	char *pathnameInternal;  	//  A COPY of the pathname.  This may be changed by dss functions.
								//  E part identifies interval.

	int processedAlready;	//  0 = not processed yet; 1 = processed for retrieve; 2 = processed for storage
	ztsTimeWindow *timeWindow;
	//  knowing which variables were allocated by the ztsNew functions,
	//  instead of the calling program
	char allocated[zSTRUCT_length];
	
} zStructTimeSeries;

#endif  //  zSTRUCT_TIME_SERIES_H
