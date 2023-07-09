#ifndef HECDSS_7_H
#define HECDSS_7_H

#include "zdataTypeDescriptions.h"
#include "zerrorCodes.h"
#include "zdssMessages.h"
#include "hecdssInternal.h"
#include "zStructTimeSeries.h"
#include "zStructCatalog.h"
#include "zStructTransfer.h"
#include "zStructRecordSize.h"
#include "zStructRecordBasics.h"
#include "zStructArray.h"
#include "zStructSpatialTin.h"
#include "zStructSpatialGrid.h"


#define STATUS_OKAY 0
//  Use STATUS_NOT_OKAY only when there is not an error code
#define STATUS_NOT_OKAY -1
//  No operation for an optional function that did not complete
#define STATUS_NO_OP 1
#define STATUS_RECORD_FOUND 0
#define STATUS_RECORD_NOT_FOUND -1
#define STATUS_TRUE 1
#define STATUS_FALSE 0

//  Open access
#define GENERAL_ACCESS 0  //  Open with either Read or Read/Write access....
#define READ_ACCESS 1
#define MULTI_USER_ACCESS 2
#define SINGLE_USER_ADVISORY_ACCESS 3
#define EXCLUSIVE_ACCESS 4
#define RELEASE_ACCESS 6

//  Public functions for DSS Version 7 library (only)
//  For the normal combined library, use heclib.h

//  Public functions
int zclose(long long *ifltab);
int zgetFileVersion(const char *dssFilename);
int zgetVersion(long long *ifltab);
int zgetFullVersion(long long *ifltab);  //  For example DSS Version "7-BG" = 70207
int hec_dss_zopen(long long *ifltab, const char *dssFilename);
int zopenExtended(long long *ifltab, const char *dssFilename, int fileVersion,
				  int access, int maxExpectedPathnames, int hashSize, int binSize);
int zset(const char* parameter, const char* charVal, int integerValue);
int zsetFile(long long *ifltab, const char* parameter, const char* charVal, int integerVal);
int zquery(const char* parameter,  char* charVal, size_t lenCharVal, int *integerValue);
long long zinquire(long long *ifltab, const char *request);
int zinquireChar(long long *ifltab, const char *request, char *creturn, size_t creturnSize, int *number);
int zfileName (char *fullDssFilename, size_t sizeofFilename, const char *dssFileName, int *permission);
int zdataType (long long *ifltab, const char* pathname);
int zgetRecordBasics(long long *ifltab, zStructRecordBasics *recordBasics);
int zgetRecordSize(long long *ifltab, zStructRecordSize *recordSize);
int zsqueezeNeeded(long long *ifltab);
int zsqueeze7(long long *ifltab, int boolOnlyIfNeeded, int boolInPlace);
int zopenLog(const char *logFileName);
void zcloseLog();

long long zgetLastWriteTimeRec(long long *ifltab, const char *pathname);
long long zgetLastWriteTimeFile(long long *ifltab);
long long zgetMyLastWriteTime(long long *ifltab);
unsigned int zgetDataCRC(long long *ifltab, const char *pathname, unsigned int crcIn);

//  Data I/O functions
int ztsStore(long long *ifltab, zStructTimeSeries *tss, int storageFlag);

//  Struct functions
void zstructFree(void *zstruct);
zStructTransfer* zstructTransferNew(const char* pathname, int mode);
zStructTimeSeries* zstructTsNew(const char* pathname);
zStructTimeSeries* zstructTsNewTimes(const char* pathname, const char* startDate, const char* startTime,
						const char* endDate, const char* endTime);
zStructTimeSeries* zstructTsNewRegFloats(const char* pathname, float *floatValues, int numberValues,
				   const char *startDate, const char *startTime, const char *units, const char *type);
zStructTimeSeries* zstructTsNewRegDoubles(const char* pathname, double *doubleValues, int numberValues,
				   const char *startDate, const char *startTime, const char *units, const char *type);
zStructTimeSeries* zstructTsNewIrregFloats(const char* pathname, float *floatValues, int numberValues, int *itimes, int timeGranularitySeconds,
				   const char* startDateBase, const char *units, const char *type);
zStructTimeSeries* zstructTsNewIrregDoubles(const char* pathname, double *doubleValues, int numberValues, int *itimes, int timeGranularitySeconds,
				   const char* startDateBase, const char *units, const char *type);
zStructTimeSeries* zstructTsClone(zStructTimeSeries *tss, const char* pathname);
zStructTimeSeries* zstructTsCloneNewTimes(zStructTimeSeries *tss, const char* pathname,
				const char* startDate, const char* startTime,
				const char* endDate, const char* endTime);
ztsTimeWindow* zstructTsNewTimeWindow();
zStructArray* zstructArrayNew(const char* pathname);
zStructCatalog* zstructCatalogNew();

//  Catalog functions
int zcatalog(long long *ifltab, const char *pathWithWild, zStructCatalog *catStruct, int boolSorted);
int zcatalogFile(long long *ifltab, const char *catalogFilename, int boolSorted, const char *pathWithWildChars);
int zcatalogToFile(long long *ifltab, int catalogHandle, int fortranUnit, int boolSorted);
int zcollectionCat(long long *ifltab, const char *seedPathname, zStructCatalog *catStruct);
int zwhatChangedSetStart(long long *ifltab, zStructCatalog *catStruct, const char *pathWithWildChars, int boolUseCRC);
int zwhatChanged(long long *ifltab, zStructCatalog *catStructChanged);
int zwhatChangedCompare(long long *ifltab, zStructCatalog *catStructBefore, zStructCatalog *catStructChanged, const char *pathWithWild, int boolUseCRC);


//  Pathname manipulation
int zpathnameClean(char *newPathname, size_t sizeofNewPathname, const char *oldPathname);
int zstringCompare(const char *string1, const char *string2, size_t length);
int zpathnameCompare(const char *pathname1, long long *lpathname2, size_t pathnameLength);
int zpathnameCompareCollection(const char *pathname1, const char *pathname2, size_t pathnameLength);
int zpathnameGetPart (const char *pathname, int partPosition, char *part, size_t sizeofPart);
int zpathnameSetPart (char *pathname, size_t sizeofPathname, const char *part, int partPosition);
int zpathnamePartLengths (const char *pathname, size_t pathnameLen, int lengths[], int dimOfLengths);
int zpathnamePartPositions (const char *pathname, size_t pathnameLen, int positions[], int dimOfPositions);
int zpathnameForm(const char *aPart, const char *bPart, const char *cPart, const char *dPart,
				  const char *ePart, const char *fPart, char *pathname, size_t sizeofPathname);
void zmaxPart(long long *ifltab, int *maxParts);
void zmaxPart7(long long *ifltab, int *maxParts);
int ztsGetEPartFromInterval(int intervalSeconds, char *ePart, size_t sizeofEpart);
char* zsetCollectionSequence(char* pathname, int sequenceNumber);

//  Missing data
float zmissingFlag();											//  Returns float missing flag
float zmissingFlagFloat();										//  Returns float missing flag
double zmissingFlagDouble();									//  Returns double missing flag
int zisMissingFloat(float value);								//  Tests float value for missing, returns 1 if missing, 0 if not
int zisMissingDouble(double value);								//  Tests double value for missing, returns 1 if missing, 0 if not
int zisMissing(void *value, int lengthValue);
void zsetMissingFloat(float *value);							//  Sets float value to missing
void zsetMissingDouble(double *value);							//  Sets double value to missing
void zsetMissing(int *value, int lengthValue);					//  Sets value to missing, length 1 for float, 2 for double
void zsetMissingFloatArray(float *values, int numberValues);	//  Sets a float array to missing
void zsetMissingDoubleArray(double *values, int numberValues);	//  Sets a double array to missing
void zsetUndefined(int *data, int dataLength);

//  Messages
void zsetMessageGroupLevel(const char *functionGroup, int level);
void zsetMessageLevel(int group, int level);
int zgetMessageLevel(int group);
void zresetMessageLevel();
void zsetMessageLevelFile(long long *ifltab, int level);

int zmessageAvailable(long long *ifltab);
const char *zgetMessage(long long *ifltab);
const char *zgetMessageAll(long long *ifltab);
void zclearMessage(long long *ifltab);
void zclearMessageAll(long long *ifltab);

//  Error processing
int zerrorCheck();
int zfileError(long long *ifltab);
int zerrorCode(long long *ifltab);
int zisError(int status);
int zcheckFile(long long *ifltab);
int zerror(hec_zdssLastError *errorStruct);

//  Alias functions
int zaliasAdd(long long *ifltab, const char* primayPathname, const char* aliasPathname);
int zaliasRemove(long long *ifltab, const char* aliasPathname);
int zaliasRemoveAll(long long *ifltab, const char* pathname);
int zaliasGetPrimary(long long *ifltab, const char* aliasPathname, char* primayPathname, size_t maxLenPrimayPathname);
int zaliasList(long long *ifltab, const char* pathname, char** pathameList, int *pathnameListLength);


//  Semi-Public functions
//  (You can access, but these are generally functions to address a
//  specific need in the library that usually not needed elsewhere.)
int zstructGetType(void *zstruct);
int zcompareDataSets(long long *ifltab, void *struct1, void *struct2, int verboseLevel, int boolExact, const char *pathname, const char *message);
char *reverseNonNumeric(const char *string);

int zcheckLinks(long long *ifltab);
int zcheckPathnames(long long *ifltab);
int zcheckHashTable(long long *ifltab);
int zfileAccessInfo(long long *ifltab, int *accessMode, int *numberAccessing,  int *numberWriting,
					int *pidsArray, int *modesArray, int arraySize, int *numberPids);

int ztsGetStandardInterval(int dssVersion, int *intervalSeconds, char *Epart, size_t sizeofEpart, int *flagDirection);
//int ztsOffset(intervalSeconds, julian, seconds);
int ztsOffsetAdjustToStandard(int intervalSeconds, int *julian, int *seconds);
int ztsOffsetAdjustToOffset(int offsetSeconds, int intervalSeconds, int *julian, int *seconds);
int ztsGetPathTimeWindow(int version, char* pathname, size_t sizeofPathname, ztsTimeWindow *timeWindow);
int ztsGetFirstLastRecordTimes(long long *ifltab, const char *pathname,
								int *firstJulian, int *firstSeconds,
								int *lastJulian, int *lastSeconds,
								int boolGetSecondsAlso);
int ztsGetDateRange(long long *ifltab, const char *pathname, int boolFullSet,
				 int *firstValidJulian, int *lastValidJulian);
int ztsGetDateTimeRange(long long *ifltab, const char *pathname, int boolFullSet,
	int *firstValidJulian, int *firstSeconds, int *lastValidJulian, int *lastSeconds);
int ztsGetFirstLastPathnames(long long *ifltab, const char *pathnameSeed,
							char *firstPath, size_t sizeofFirstPath,
							char *lastPath, size_t sizeofLastPath);
int zgetProgress(int handle);

int ztsGetInterval(int dssVersion, const char *pathname);
int zlocationStructValid(zStructLocation *locationStruct);

//  Private functions are located in hecdssInternal.h

////////////////////////////////////////////////////////////////////
//////////////  working on these /////////////////////////////////
int ztsOffset(int intervalSeconds, int julian, int seconds);

zStructTimeSeries* zstructTsNewTimes(const char* pathname, const char* startDate, const char* startTime,
											const char* endDate, const char* endTime);


long long f4toi8(float f4a, float f4b);
void i8tof4(long long integer8, float *f4a, float *f4b);

int toFixedFields(char *cstring, size_t sizeofCstring, char **fields, int *fieldLengths, int nfields,
				  int *columnStart, int *boolLeftJustify);

/************************************************
*   TEMP - unconverted functions!!!!!
************************************************/


void zsetMissingFloatArray(float *values, int numberValues);
void zsetMissingDoubleArray(double *values, int numberValues);

int zpdStore(long long *ifltab, zStructPairedData *pds, int storageFlag);
int zpdRetrieve(long long *ifltab, zStructPairedData *pds, int retrieveSizeFlag);

int zarrayStore(long long *ifltab, zStructArray *arrayStruct);
int zarrayRetrieve(long long *ifltab, zStructArray *arrayStruct);

int zread(long long *ifltab, zStructTransfer* ztransfer);

void zerrorSpecifics(int optionalErrorCode, int *severity, int *errorType, int *errorNumber,
	char *errorMessage, size_t sizeofErrorMessage, char *systemMessage, size_t sizeofSystemMessage,
	char *functionName, size_t sizeofFunctionName, char *calledByFunction, size_t sizeofCalledByFunction);



void zreleaseFileSpace(long long *ifltab, long long address, int amountLongs);
int zcheck(long long *ifltab, const char* pathname);


int zpdStoreFull(long long *ifltab, const char *pathname,
					int *values, int sizeEachValue,
					int numberCurves, int numberOrdinates,
					const char *labels, int boolStoreLabels,
					const char *unitsIndependent, const char *typeIndependent,
					const char *unitsDependent, const char *typeDependent,
					int boolIndependentIsXaxis,
					int *userHeader, int lengthUserHeader,
					double coordinates[], int numberCoordinates,
					int coordinateDescription[],  int numbCoordDescription,
					const char *timeZoneName, int timeZoneOffsetMinutes,
					int *supplemental, int numberSupplemental,
					int xprecision, int yprecision, int storageFlag);
int zsetDescriptionOld(double coordinates[], int numberCoordinates,
					  int coordinateDescription[],  int numbCoordDescription,
					  const char *timeZoneName, int timeZoneOffsetMinutes,
					  int *supplemental, int numberSupplemental,
					  int *values3, int sizeValues3,
					  int *numberValues3);
int ztsRegStoreFull7(long long *ifltab, zStructTimeSeries *tss, int storageFlag);
int ztsIrregStoreFull(long long *ifltab, const char *pathname,
					const char *baseDate, int timeGranularitySeconds,
					int *timeArray, int numberValues,
					int *values, int sizeEachValue,
					int *quality, int sizeEachQuality,
					int *notes, int sizeEachNote,
					const char *cnotes, int totalLengthCnotes,
					int *userHeader, int lengthUserHeader,
					const char *units, const char *type, int precisionValues,
					double coordinates[], int numberCoordinates,
					int coordinateDescription[],  int numbCoordDescription,
					const char *timeZoneName, int timeZoneOffsetMinutes,
					int *supplemental, int numberSupplemental,
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


int zsetLocation(zStructLocation *locationStruct,
				 int *values3, int sizeValues3);
int zreadInfo(long long *ifltab, const char *pathname, int statusWanted);

int ztsRetrieve(long long *ifltab, zStructTimeSeries *tss,
				int retrieveFlag, int retrieveDoublesFlag, int boolRetrieveQualityNotes);
int ztsRetrieveReg6(long long *ifltab, zStructTimeSeries *tss,
					int retrieveFlag, int retrieveDoublesFlag, int boolRetrieveQualityNotes);
int ztsRetrieveIrreg6(long long *ifltab, zStructTimeSeries *tss,
					int retrieveFlag, int retrieveDoublesFlag, int boolRetrieveQualityNotes);
void readProgramName(long long* ifltab, zStructTimeSeries* tss, int status);
int ztsRetrieveIrreg7(long long *ifltab, zStructTimeSeries *tss,
					int retrieveFlag, int retrieveDoublesFlag, int boolRetrieveQualityNotes);

int  ztsRetrieveRegArgs(long long *ifltab, const char *pathname,
						const char *startDate, const char *startTime, int *timeOffsetSeconds,
						int maxNumberValues, int *numberRead,
						int values[], int sizeEachValueRequested, int *sizeEachValueRead,
						int quality[], int qualityElementSizeRequested, int *qualityElementSizeRead,
						int notes[], int inoteElementSizeRequested, int *sizeEachNoteRead,
						char *cnotes, int totalSizeCNotes, int *totalNumberCnotesRead,
						int userHeader[], int userHeaderArraySize, int *userHeaderNumber,
						char *units, int sizeOfUnits, char *type, int sizeOfType,
						int *precisionValues, char *timeZoneName, int sizeOfTimeZoneName);

zStructText* zstructTextNew(const char* pathname);
zStructText* zstructTextStringNew(const char* pathname, char *text);
int ztextStore(long long *ifltab, zStructText *textStruct);
int ztextStoreFromFile(long long *ifltab, const char *pathname, const char *filename);
int ztextRetrieve(long long *ifltab, zStructText *textStruct);
int ztextRetrieveToFile(long long *ifltab, const char *pathname, const char *filename);
void ztextStructPrint(zStructText *textStruct);

zStructSpatialTin* zstructSpatialTinNew(const char* pathname);
int zspatialTinStore(long long *ifltab, zStructSpatialTin *tinStruct);
int zspatialTinRetrieve(long long *ifltab, zStructSpatialTin *tinStruct, int boolRetrieveData);

zStructRecordSize* zstructRecordSizeNew(const char* pathname);
int ztsRegProcessTimes(long long *ifltab, zStructTimeSeries *tss,
					   char *path, int pathSize,
					   const char *startDate, const char *startTime,
					   ztsTimeWindow *timeWindow,
					   int maxNumberValues, int *numberValues,
				       int *timeOffsetSeconds, int *blockSize,
					   int *startBlockJulian, int *endBlockJulian,
					   int boolRetrieve);



int ztsInternalHeaderPack(zStructTimeSeries *tss, int *internalHeader);
int ztsInternalHeaderUnpack(zStructTimeSeries *tss, int *internalHeader, int lengthInternalHeader);
int copyAndTrim(char *tostring, size_t maxToLen, const char *fromString, size_t fromStringLen);
void convertDataArray(int *dataIn, int *dataOut, int number, int dataInElementLength, int dataOutElementLength);
void stringCToFort(char *fortString, size_t maxFortLen, const char *cfromString);



char* zlocationPath(const char* pathname);
zStructLocation* zstructLocationNew(const char* pathname);
int zlocationStore(long long *ifltab, zStructLocation *locationStruct, int storageFlag);
int zlocationRetrieve(long long *ifltab, zStructLocation *locationStruct);




int ztsProcessTimes(long long *ifltab, zStructTimeSeries *tss, int boolStore);

void ztsIrregMergeBlocks(long long *ifltab, int storageFlag, 
						zStructTimeSeries *tss, int julianStartBlockDate,
						int boolFromStartOfBlock, int boolToEndOfBlock,
						int *timesToStore, int timeGranularityToStore, int numberToStore, int profileDepthsNumber,
						int *timesRead, int timeGranularityRead, int numberRead,
						int *valuesToStore, int *valuesRead, int valueElementSize,
						int *qualityToStore, int *qualityRead, int qualityElementSize,
						int *notesToStore, int *notesRead, int inoteElementSize,
						const char *cnotesToStore, int cnotesToStoreLen,
						char *cnotesRead, int cnotesReadLen,
						int *timesOut, int *valuesOut,
						int *quailtyOut, int *notesOut,
						char *cnotesOut, int cnotesOutSize, int *cnotesOutLen,
						int *totalToStore);


int ztsDisaggregate(long long *ifltab, int numberToRead, int numberStored,
	int dataInPosition, int *numberExpanded, int blockStartPosition,
	int positionRelativeFirstValid, int positionRelativeLastValid,
	int *dataIn, int dataInNumber,
	int *header2, int *internalHeader,
	int *values, int valuesArraySize, int valuesSizeRequested,
	int *quality, int qualityArraySize, int qualitySizeRequested,
	int *inotes, int inotesArraySize, int inotesSizeRequested,
	char *cnotes, int cnotesSize, int *cnotesLength);

int ztsStore(long long *ifltab, zStructTimeSeries *tss, int storageFlag);

zStructRecordBasics* zstructRecordBasicsNew(const char* pathname);
zStructPairedData* zstructPdNew(const char* pathname);
zStructPairedData* zstructPdNewFloats(const char* pathname, float *floatOrdinates, float *floatValues,
											 int numberOrdinates, int numberCurves,
											 const char *unitsIndependent, const char *typeIndependent,
											 const char *unitsDependent, const char *typeDependent);
zStructPairedData* zstructPdNewDoubles(const char* pathname, double *doubleOrdinates, double *doubleValues,
											 int numberOrdinates, int numberCurves,
											 const char *unitsIndependent, const char *typeIndependent,
											 const char *unitsDependent, const char *typeDependent);


int zlocationRetrieve(long long *ifltab, zStructLocation *locationStruct);





int ztsTrim(long long *ifltab, zStructTimeSeries *tss);
void ztsTrimAdjustTimeWindow(long long *ifltab, zStructTimeSeries *tss, int firstValid, int lastValid);


int zwrite(long long *ifltab, zStructTransfer* ztransfer);



#endif //  HECDSS_7_H

