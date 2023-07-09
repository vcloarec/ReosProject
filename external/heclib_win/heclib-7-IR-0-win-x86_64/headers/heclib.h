#ifndef HECLIB_H
#define HECLIB_H

#include <float.h>

#include "heclib7.h"
#include "heclib6.h"
#include "hecdssFort.h"
#include "hecdssInternal.h"
#include "zdataTypeDescriptions.h"


#include "zStructRecordBasics.h"
#include "zStructRecordSize.h"
#include "zStructTimeSeries.h"
#include "zStructLocation.h"
#include "zStructCatalog.h"


#define UNDEFINED_TIME -2147483647
#define UNDEFINED_FLOAT -FLT_MAX
#define UNDEFINED_DOUBLE -(double)FLT_MAX


int fortranopen_(int *unit, char *filename, size_t lenFilename);
int fortranclose_(int *unit);
int isunitconnected_(int *unit);
int zckmul6_ (long long *ifltab);
void zpseudorts6_(const char *CFROMPATH, char *CTOPATH, int *INTL, int *IACTION, int *ISTATUS, size_t lenFrom, size_t lenTo);
void zstfh_ (const char *clabels, const char *citems, int *numberItems,
            float *header, int *headerMax, int *numberHeader, int *istat,
			size_t, size_t);

int zdelete(long long *ifltab, const char* pathname);
int zundelete(long long *ifltab, const char* pathname);
int zrename(long long *ifltab, const char* oldPathname, const char* newPathname);
int zpdStore6(long long *ifltab, zStructPairedData *pds, int storageFlag);
int zpdRetrieve6(long long *ifltab, zStructPairedData *pds, int retrieveSizeFlag);

//  Combined header array to access both DSS version 6 and version 7
//  This includes both Fortran and C function

//  zopen functions.  You can call any on any version; only difference is in creating new files
int  hec_dss_zopen  (long long *ifltab, const char *dssFilename);
int  zopen6 (long long *ifltab, const char *dssFilename);
int  zopen7 (long long *ifltab, const char *dssFilename);

int zsqueeze(const char *dssFilename);

//
//  For DSS version 7 only, with C code only, use heclib7.h
//


//  Utility Functions
int zcopyFile(long long *ifltab, long long *ifltabTo, int statusWanted);
int zconvertVersion(const char* fileNameFrom, const char* fileNameTo);
int zcopyRecord (long long *ifltabFrom, long long *ifltabTo, const char *pathnameFrom, const char *pathnameTo);
int zduplicateRecord (long long *ifltab, const char *pathnameFrom, const char *pathnameTo);

int deleteFile(const char* filename);
void deletefile_(const char* filename, int *status, size_t lenFilename);

//  boolean boolEndOfRecord indicates if more of same message is coming (false)
//  or this is the last part of the message and should be written (true)

void zfname(const char *dssFilenameIn, char *dssFilenameOut, int *nname, int *exists, size_t lenDssFilenameIn, size_t sizeDssFilenameOut);

//  CHANGE ME to zmissingDouble() and zmissingFloat()
double zmissingFlagDouble();
float zmissingFlag();
int zisMissingDouble(double value);
int zisMissingFloat(float value);

int sortfiles(char *unsortedIn, char *sortedOut);


int isTimeDefined(int julianDate, int timeSeconds);

int ztsGetSizes6(long long *ifltab, zStructTimeSeries *tss, zStructRecordSize *timeSeriesRecordSizes);
int zgetRecordSize6(long long *ifltab, zStructRecordSize *recordSize);
int zgetRecordBasics6(long long *ifltab, zStructRecordBasics *recordBasics);


int ztsStoreIrregArgs(long long *ifltab, const char *pathname,
			const char *baseDate, int *itimes, int boolSecondGranularity,
			int numberValues, int *values, int valueSize,
			int *quality, int qualityElementSize,
			int *inotes, int inoteElementSize,
			char *cnotes, int cnotesLengthTotal,
			int *userHeader, int userHeaderNumber,
			char *units, char *type,
			int precisionValues, char *timeZoneName,
			double *coordinates, int *coordinateDescription, int boolCoordinatesUsed,
			int storageFlag);

int ztsStoreRegArgs(long long *ifltab, const char *pathname,
			const char *startDate, const char *startTime,
			int numberValues, int *values, int valueSize,
			int *quality, int qualityElementSize,
			int *inotes, int inoteElementSize,
			char *cnotes, int cnotesLengthTotal,
			int *userHeader, int userHeaderNumber,
			char *units, char *type,
			int precisionValues, char *timeZoneName,
			double *coordinates, int *coordinateDescription, int boolCoordinatesUsed,
			int storageFlag);

#endif //  HECLIB_H

