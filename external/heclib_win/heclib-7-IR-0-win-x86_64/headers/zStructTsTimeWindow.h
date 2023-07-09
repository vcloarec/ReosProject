#ifndef zSTRUCT_TS_TIME_WINDOW_H
#define zSTRUCT_TS_TIME_WINDOW_H

typedef struct {

	//  Private

	//  An internal struct used for computing the time window and other
	//  time information.  The primary info is passed back to the main
	//  time series struct.
	int structType;

	//  Standard date and time for first and last value,
	//  Not necessarily the start / end of the time window.
	int startJulian;
	int startTimeSeconds;
	int endJulian;
	int endTimeSeconds;
	int intervalSeconds;
	int timeOffsetSeconds;
	int numberValues;
	int blockSize;
	int startBlockJulian;
	int endBlockJulian;

} ztsTimeWindow;


#endif  //  zSTRUCT_TS_TIME_WINDOW_H

