#ifndef HECDSS_INTERNAL_H
#define HECDSS_INTERNAL_H

#include "assert.h"
#include "heclibDate.h"
#include "zdssVals.h"

#include "zStructTimeSeries.h"
#include "zStructText.h"
#include "zStructArray.h"
#include "zStructPairedData.h"
#include "zStructSpatialTin.h"
#include "zStructTransfer.h"
#include "zStructRecordBasics.h"
#include "zStructRecordSize.h"
#include "zStructTsTimeWindow.h"
#include "zStructCatalog.h"
#include "zStructRecordAddresses.h"


//  For use with malloc / calloc
//  64-bit must be 8
#define INT_SIZE (size_t)4
#define WORD_SIZE (size_t)8
#define LONG_SIZE (size_t)8
#define FLOAT_SIZE (size_t)8
#define DOUBLE_SIZE (size_t)8
#define BYTE_SIZE (size_t)1
#define CHAR_SIZE (size_t)1


#ifndef _MSC_VER
#define _MAX_PATH 300
#define _MAX_FNAME 100
#define _snprintf_s(a,b,c,...) snprintf(a,b,__VA_ARGS__)
#endif


#define DSS_VERSION "7-IR"
#define DSS_VERSION_DATE "31 May 2023" 



const char *ztypeName(int recordType, int boolAbbreviation);
void zreada(long long *ifltab, const char *path, int *npath,
			int *userHeader, int *userHeaderNumber,
			int *values, int *valuesNumber,
			int *readFlag, int *recordFound);
int zreadc (long long *ifltab, const char* pathname,
			 int *internalHeader, int internalHeaderArraySize , int *internalHeaderNumber,
			 int *header2, int header2ArraySize, int *header2Number,
			 int *values3, int values3ArraySize, int *values3Number,
			 int *userHeader, int userHeaderArraySize, int *userHeaderNumber,
			 int *values1, int values1Size, int *values1Number,
			 int *values2, int values2Size, int *values2Number,
			 int *numberValues, int *logicalNumberValues,
			 int *totalAllocatedSize, int *totalExpandedSize, int *dataType);

void zreadx(long long *ifltab, const char *pathname,
			 int *internalHeader, int *internalHeaderArraySize , int *internalHeaderNumber,
			 int *header2, int *header2ArraySize, int *header2Number,
			 int *userHeader, int *userHeaderArraySize, int *userHeaderNumber,
			 int *values, int *valuesSize, int *valuesNumber,
			 int *readPlan, int *recordFound);


int zwritec(long long *ifltab, const char* pathname,
			void *internalHeader, int internalHeaderNumber,
			void *header2, int header2Number,
			void *values3, int values3Number,
			void *userHeader, int userHeaderNumber,
			void *values1, int values1Number,
			void *values2, int values2Number,
			int numberValues, int logicalNumberValues,
			int totalAllocatedSize, int totalExpandedSize, int dataType);

void zwritex(long long *ifltab, const char *path, int *npath,
			 int *internalHeader, int *internalHeaderNumber,
			 int *header2, int *header2Number,
			 int *userHeader, int *userHeaderNumber,
			 int *values, int *valuesNumber,
			 int *dataType, int *plan,
			 int *status, int *recordFound);

void zwritea(long long *ifltab, const char *path, int *npath,
			int *userHeader, int *userHeaderNumber,
			int *values, int *valuesNumber,
			int *flag, int *recordFound);



int zcatalog6Internal(long long *ifltab, const char *pathWithWild, zStructCatalog *catStruct,
			          int catalogHandle, int ifortUnit, int numberWanted, int boolCollection, int boolForSort);

//  Internal functions for HEC-DSS Version 7
//  These functions are not to be called, except by other DSS 7 functions

//  Private functions

int zpermCreate (long long *ifltab, int maxExpectedPathnames, int hashSize, int binSize);
int zpermWrite(long long *ifltab);
int zpermRead (long long *ifltab);
void znewFileSize(long long *ifltab, int maxExpectedPaths, int hashSize, int binSize);
int zcheckKeys(long long *ifltab);
void zinit();
void zpidWrite(long long *ifltab);
void zpidUpdate(long long *ifltab);

int getVersFromChar(char version);
int zinitIfltab(long long *ifltab);
int zlockActive(long long *ifltab, int level, int lock, int flush);
int zlockPassive(long long *ifltab, int lockFlag, int accessMode);
int zlockDss (long long *ifltab, int ihandle, int mode, long long wordAddress, int nbytes);
int zopenInternal(long long *ifltab, const char *dssFilename,
				  int access, int maxExpectedPathnames, int hashSize, int binSize, int boolReopen);
int zcloseInternal(long long *ifltab, int boolReopen);
int zcheckAccessReset(long long *ifltab, int boolForce, int quiescentTimeMills);
int zgetRecordBasics7(long long *ifltab, zStructRecordBasics *recordBasics);
int zgetRecordSize7(long long *ifltab, zStructRecordSize *recordSize);
int zcopyRecordInternal(long long *ifltabFrom, long long *ifltabTo, const char *pathnameTo, int boolDuplicate);

int zmemoryGet(long long *ifltab, int arrayLoc, int size, const char* memName);
void zmemoryFree(long long *ifltab);

int zcheckInternal(long long *ifltab, const char* pathname, int statusWanted);
int zhash (long long *ifltab, const char *pathname, int pathnameLength, int *tableHash, long long *pathnameHash);
int zisaCollection(char* pathname, size_t pathnameLength);
int zcollectionsPath(char* pathname, size_t pathnameLength);
int zcompareRecordStatus(int statusRead, int statusWanted);
int zcheckMultiUser(long long *ifltab);
int zbinNew(long long *ifltab);
int zbinUpdate(long long *ifltab, const char* pathname, long long infoAddress, int recordStatus,
			   int dataType);
int zpdUnitsToHead(zStructPairedData *pds, int *internalHeader, int internalHeaderArraySize);
int zpdHeadToUnits(zStructPairedData *pds, int *internalHeader, int internalHeaderNumber);
int zcheckInfo(long long *ifltab, const char *pathname, long long *info, int numberInfo);

//  I-O
long long zfileSize(int handle);
int closeFile(int handle);
int flushFile(int ihandle);
int zget(long long *ifltab, long long iaddress,  int *iarray, int numberWords, int wordSize);
int zgetBuff (long long *ifltab, long long iaddress, int *iarray, int numberWords, int wordSize,
		  int bufferAction, long long bufferControl[4], int *buffer);
int zput(long long *ifltab, long long iaddress, int *iarray, int numberWords, int wordSize);
int zputBuff (long long *ifltab, long long iaddress, int *iarray, int numberWords, int wordSize,
			  int bufferAction, long long bufferControl[4], int *buffer);
int writeBytes(int ihandle, const char *carray, int numberBytes);
int readBytes(int ihandle, char *carray, int numberBytes);
int zopenDisk (const char *cname, int *handle, int iaccess, int exclusiveAccess);
int zreadDisk (int ihandle, int iswap, long long address, void *iarray, int numberInts);
int zwriteDisk (int ihandle, int iswap, long long address, void *iarray, int numberInts);
int zopenFile(const char *cname, int iaccess);
void zswap(long long *iarray, int numberInts);
void zswitchInts(int *iarray, int numberInts);
void zswitchDoubles(int* iarray, int numberArray, int lengthEachValue, int boolStoring, int numberTimes);
int bigEndian();
int zwriteEOF(long long *ifltab);
int zwriteEOFandFlush(long long *ifltab, long long bufferControl[4], int *buffer);
int zflushToDisk (long long *ifltab, int forceFlush);
int zflushBuffers(long long *ifltab, long long bufferControl[4],int *buffer);
long long zgetFileSpace(long long *ifltab, int sizeNeeded, int boolReclaimedOK, int *atEOF);
long long *zreclaimRead (long long *ifltab, int *iarrayNumber, int boolRelease);
int zreclaimWrite (long long *ifltab);
int zreclaimSet(long long *ifltab, int reclaimLevel);
int zreadInfoBlock (long long *ifltab, const char *pathname, int statusWanted,
					long long *info, int maxInfo, int *numberInfo);
int zreadInternal(long long *ifltab, zStructTransfer* ztransfer,
				  long long bufferControl[4], int *buffer, int boolUseBuffer);
int zwriteInternal(long long *ifltab, zStructTransfer* ztransfer, int checked,
				   long long bufferControl[4], int *buffer, int boolUseBuffer);
int zwriteNew(long long *ifltab, zStructTransfer* ztransfer,
			  long long bufferControl[4], int *buffer, int bufferAction, int *wroteAtEOF);
int zwriteExisting(long long *ifltab, zStructTransfer* ztransfer,
			  long long bufferControl[4], int *buffer, int bufferAction, int *wroteAtEOF);

//  Time series
int ztsStoreReg7(long long *ifltab, zStructTimeSeries *tss, int storageFlag);
int ztsRetrieveReg7(long long *ifltab, zStructTimeSeries *tss,
					int retrieveFlag, int retrieveDoublesFlag, int boolRetrieveQualityNotes);
int ztsStoreIrreg7(long long *ifltab, zStructTimeSeries *tss, int storageFlag);
int ztsStoreReg6(long long *ifltab, zStructTimeSeries *tss, int storageFlag);
int ztsStoreIrreg6(long long *ifltab, zStructTimeSeries *tss, int storageFlag);
int ztsProcessTimes(long long *ifltab, zStructTimeSeries *tss, int boolStore);
int ztsStorePattern(long long *ifltab, zStructTimeSeries *tss);
int ztsRetrievePattern(long long *ifltab, zStructTimeSeries *tss, int retrieveDoublesFlag);
int ztsIncrementBlock(int julianBlockDate, int blockSize);
int ztsRegGetBlockStart(int julianDate, int intervalSeconds, int *blockSize);
int ztsIrregGetBlockStart(int julian, int blockSize);
int ztsRegAddTimes(zStructTimeSeries *tss);
int ztsFirstLastValidValues(void *values, int valueElementSize,
							int profileDepthsNumber, int numberValues,
							   int *quality, int sizeQuality,
							   int *notes, int sizeNotes,
							   char *cnotes, int cnotesLengthTotal,
							   int *firstValid, int *lastValid);
int ztsRegRepeatCompress(int *dataIn, int numberIn, int sizeEachData,
						  int *dataCompressedOut, int dataCompressedOutSize, 
						  int *header2, int header2Size);
void ztsRegRepeatUncompress(int start, int end, int valueElementSize,
						   int sizeRequested, int boolIsAValue,
						   int *header2, int *dataIn, int *dataOut, int dataOutSize);
int ztsRegStoreBlock(long long *ifltab, zStructTimeSeries *tss, const char *pathname, 
					int numberToStore,  int *values, int valueSize, int valueElementSize,
					int *quality, int qualityElementSize,
					int *notes, int inoteElementSize,
					char *cnotes, int cnotesSize, int *lengthCNotes,
					int *profileDepths, int profileDepthsSize,  int profileDepthsNumber,
					int *internalHeader, 
					int *userHeader, int userHeaderNumber,
					int positionInValues, int numberInBlock,
					int startJulian, int startSeconds, int intervalSeconds,
					int julianBlockDate, int blockStartPosition,
					int boolReadBlock, int dataType, int storageFlag);
int ztsRegReadBlock(long long *ifltab, const char *pathname, int boolExists,
					int *buffer, long long bufferControl[4],
					int *values, int valuesArraySize, int valuesSizeRequested, int *valueElementSizeRead,
					int *quality, int qualityArraySize, int qualitySizeRequested, int *qualityElementSize,
					int *notes, int notesArraySize, int notesSizeRequested, int *inoteElementSize,
					char *cnotes, int sizeCNotesRemaning, int *numberCnotes,
					int *profileDepths, int profileDepthsNumberRequested,
					int profileDepthsArraySize, int *profileDepthsNumberRead,
					int *internalHeader, int internalHeaderArraySize, int *internalHeaderNumber,
					int *userHeader, int userHeaderArraySize, int *userHeaderNumber,
					int numberToRead, int blockStartPosition);
int ztsIrregStoreBlock(long long *ifltab, zStructTimeSeries *tss, const char *pathname,
					   int *buffer, long long bufferControl[4],
						int numberToStore, int *timesToStore,
						int *valuesToStore, int valueSize, int valueElementSize,
						int *qualityToStore, int qualityElementSize,
						int *notesToStore, int inoteElementSize,
						char *cnotes, int lengthCNotes, int *lengthCNotesRemaining,
						int *profileDepths, int profileDepthsSize, int profileDepthsNumber,
						int *internalHeader,
						int *userHeader, int userHeaderNumber,
						int julianStartBlockDate, int julianEndBlockDate, int blockSize,
						int boolFromStartOfBlock, int boolToEndOfBlock,
						int storageFlag, int boolReadBlock, int dataType);
int ztsIrregReadBlock(long long *ifltab, const char *pathname, long long *info,
				int *buffer, long long bufferControl[4],
				int julianStartBlockDate, ztsTimeWindow *timeWindow, int *timeArray,
				int *values, int valuesArraySize, int valuesSizeRequested, int *valueSizeRead,
				int *quality, int qualityArraySize, int qualitySizeRequested, int *qualityElementSizeRead,
				int *notes, int notesArraySize, int inotesSizeRequested, int *inoteElementSizeRead,
				char *cnotes, int sizeCNotesRemaning, int *totalNumberCnotesRead,
				int *profileDepths, int profileDepthsArraySize, int *profileDepthsNumberRequested, int profileDepthsSize,
				int *internalHeader, int internalHeaderArraySize, int *internalHeaderNumber,
				int *userHeader, int userHeaderArraySize, int *userHeaderNumber,
				int maxNumberToRead, int *numberRead,
				int boolRetrievePrevious, int boolRetrieveSucceeding);
int ztsWriteBlock (long long *ifltab, zStructTimeSeries *tss, const char* pathname,
					 int *timeArray, int boolUseTimes, int numberValues,
					 int *values, int lengthEachValue,
					 int *quality, int qualityElementSize,
					 int *notes, int inoteElementSize,
					 const char *cnotes, int lengthCNotes,
					 int *profileDepths, int profileDepthsNumber,
					 int *internalHeader, 
					 int *userHeader, int userHeaderNumber,
					 int totalAllocatedSize, int logicalNumberValues,
					 int dataType);
int ztsGetSizes(long long *ifltab, zStructTimeSeries *tss, zStructRecordSize *timeSeriesRecordSizes);
int ztsGetSizesInternal (long long *ifltab, ztsTimeWindow *timeWindow,
						 zStructRecordSize *recordSize);
int zrecordAddresses(long long *ifltab, const char* pathname, long long *addresses);
int ztsAggregate(long long *ifltab, int numberIn, int *internalHeader,
				  int *timeArray, int boolUseTimes,
				  int *values, int valueElementSize,
				  int *quality, int qualityElementSize,
				  int *notes, int inoteElementSize,
				  const char *cnotes, int cnotesLength,
				  int **arrayOut, int *arrayOutNumber,
				  int **header2, int *header2Number);
int ztsTrim(long long *ifltab, zStructTimeSeries *tss);
void ztsTrimAdjustTimeWindow(long long *ifltab, zStructTimeSeries *tss, int firstValid, int lastValid);
int isTimeInRange(int timeValue, int *lastValueStatus, int timeGranularitySeconds,
				 int baseDate, ztsTimeWindow *timeWindow);

int zhandle(long long *ifltab);

//  Messaging functions
char* zgetFunctionName(int functionID);
void zmessage(long long *ifltab, const char *message);
void zmessageDebug(long long *ifltab, int functionID, const char *message1, const char *message2);
void zmessageDebugInt(long long *ifltab, int functionID, const char *message1, int number);
void zmessageDebugLong(long long *ifltab, int functionID, const char *message1, long long number);
void zmessageDebugFloat(long long* ifltab, int functionID, const char* message1, float number);
void zmessageLength(long long *ifltab, const char *message, size_t len);  // Public includes time stamp
void zmessageLen(long long *ifltab, const char *message, size_t length);  //  Private - does not include time stamp
int zmessageLevel(long long *ifltab, int callingMethod, int level);
void zmessageNoNl(long long *ifltab, const char *message);
void zmessageNoNlLen(long long *ifltab, const char *message, size_t length);
void zmessage2(long long *ifltab, const char *message1, const char *message2);
void zmessage2Len(long long *ifltab, const char *message1, size_t len1, const char *message2, size_t len2);
void zmessageFlush(long long *ifltab);
void zmessTime(long long *ifltab);
void zmessageInt(long long *ifltab, const char *message, int number);
void zmessageLong(long long *ifltab, const char *message, long long number);
int zmessageInterface(long long *ifltab, const char *message, int boolContinuation);
char *zmessConcat1(char *messIn, size_t sizeIn, const char *message);
char *zmessConcat2(char *messIn, size_t sizeIn, const char *message1, const char *message2);
void zmessageZput(long long *ifltab, long long iaddress, int number, int wordSize, int bufferAction,
			   int status, int functionID, const char* additional);
void zmessageZget(long long *ifltab, long long iaddress, int number, int wordSize, int bufferAction,
			   int status, int functionID, const char* additional);
void ztsMessTimeWindow(long long *ifltab, int functionId, zStructTimeSeries *tss);
void ztsIrregMessage(long long *ifltab, int functionID, const char *message, int itime, int timeGranularitySeconds,
					 int baseDate, int *value, int sizeValue, int boolDebugMessage);
void zprintFileInfo(long long *ifltab);



//  Error processing and messaging
int zerrorProcessing(long long *ifltab, int functionID, int errorNumber, int infoStatus,
					 long long iaddress, int severity, const char *pathname, const char *message);
int zerrorUpdate(long long *ifltab, int errorCodeIn, int functionID);
char *zerrorMessage(char *message, size_t sizeofMessage, int severity, int errorNumber, int functionID);
int zerrorEncode(int severity, int highFunction, int lowFunction, int errorCode, int status);
int zerrorEncodeHigh(int errorCodeIn, int highFunction);
int zerrorDecode(int ierror, int *highFunction, int *lowFunction, int *errorCode, int *status);
int zerrorSeverity(int errorCode);
void zerrorSave(long long *ifltab, int errorNumber);
void zerrorStructClear();
void ztsDateMessage(long long *ifltab, int functionID, const char *message, int startJulian, int startSeconds);
void ztsTimeMessage(long long *ifltab, const char *message, int timeMinOrSec, int timeGranularitySeconds, int julianBaseDate);

int zcheckPathnameBins(long long *ifltab);

//  Catalog
int zcatalogInternal (long long *ifltab, const char *pathWithWild, zStructCatalog *catStruct,
					  int catalogHandle, int ifortUnit, int numberWanted, int boolCollection, int boolForSort);
int zcatInternalSort(long long *ifltab, const char *pathWithWild, zStructCatalog *sortedStruct,
					 int catalogHandle, int fortranUnit, int boolCollection);
int zcatSortPath(int catalogHandle, const char *pathname, size_t pathnameLen, int dataType,
				 int *partMax, int indexNumber);
int zcatParsePath(const char *cpath, int *partAction, int *lengths,
				  char *apart, size_t apartSize,
				  char *bpart, size_t bpartSize,
				  char *cpart, size_t cpartSize,
				  char *dpart, size_t dpartSize,
				  char *epart, size_t epartSize,
				  char *fpart, size_t fpartSize);
int zcatComparePath(const char *cpath, int *partAction, int *lengths,
					char *apart, char *bpart, char *cpart, char *dpart, char *epart, char *fpart, int boolCollection);
int zgetCatalogSortAddresses(long long* ifltab, long long* sortAddresses, int sortAddressesLen);
int zsetCatalogSortAddresses(long long* ifltab, long long* sortAddresses, int sortAddressesLen);
int zfindString(const char *fullString, int fullStringLength, const char *stringToFind, int stringToFindLength);
int getLastInt(const char *cline);
int zwalkBins(long long *ifltab, int catalogHandle, int statusWanted);
//  Depreciated
void zplist7 (long long *ifltab, const char *instr, int *filePos, char *pathname,
               int *nPathname, int *istatus, int len_instr, size_t sizeof_pathname);

//  Miscellaneous DSS
int zdeleteInternal(long long *ifltab, const char *pathname, int boolUndelete);
int zset7(const char* parameter, const char* charVal, int integerValue);
void zincrementCounter(long long *ifltab, int key, int item1, int item2);
void zupdatePathStats(long long *ifltab, const char *pathname, size_t pathnameLen);
int zpathnameAddPart(const char *part, int count, char *pathname, size_t sizeofPathname);
int ztsIsCurrentTimeSeries(long long *ifltab, char* pathname);
void zlocationUpdatePerm(long long *ifltab, zStructLocation *locationStruct);
char *ztsPathCompatible(int version, const char *pathname, size_t pathnameLen);

//  Miscellaneous other
void floatToDouble(int *dataIn, double *dataOut);
void doubleToFloat(double *d, float *f);
unsigned long long i4toi8(int int4a, int int4b);
void i8toi4(unsigned long long integer8, int *int4a, int *int4b);
int isOdd(int number);
int isValidChar(char ich);
int numberLongsInInts (int numberInts);
int numberIntsInLongs (long long numberLongs);
int numberIntsInBytes(int numberBytes);
int numberLongsInBytes(int numberBytes);
int charInt(void *fromBuff, void *toBuff, int numberBytes, int maxBytesTo, int boolToInt, int zeroEnd, int boolMiddleLong);
int charLong(void *from, void *to, int numberBytes, int maxBytesTo, int boolToLong, int zeroEndFlag);
int trimPositions(const char *string, int *start, int *end);
char *mallocAndCopy(const char *copyFrom);
char *mallocAndCopyTrim(const char *copyFromString);
char *mallocAndCopyPath(const char *copyFromPath);
char *stringFortToC(const char *copyFrom, size_t copyFromLen);
int copyLines(char *destination, size_t sizeOfDestination, const char *source, size_t sourceMax, int numberLines);
int copyFile(long long *ifltab, int handleTo);
void convertIntArray(int *dataIn, int *dataOut, int number, int dataInElementLength, int dataOutElementLength);
void convertDataLength(int *dataIn, int *dataOut, int dataInElementLength, int dataOutElementLength);
void convertDataType(int *dataIn, int *dataOut, int dataInElementLength, int dataOutElementLength);
void convertDataArray(int *dataIn, int *dataOut, int number, int dataInElementLength, int dataOutElementLength);
void fillArray(int *valueIn, int valueInElementLength, int *arrayOut,  int arrayOutElementLength, int numberOut);
void stringFill (char *string, char cval, size_t len);
int stringLastNonBlank (char *string, size_t stringSize);
void zeroFill (char *string, size_t len);
void zsetInterrupt(int handle);
char *zgetInternalPath(void *zstruct);
int zcharTo8Byte(char *carray, char *itemToCopy, int *ipos, int carraySize);
int appendStringToHeader(char* src, char* dest, int* ipos, int destSize);
int unitsHavePadding(int* internalHeader, int offsetToUnits);
char* readStringFromHeader(char* carray, int* ipos, int size);
//  Function primarily for DSS unit testing
//  int zcompareDataSets(long long *ifltab, void *struct1, void *struct2, int verboseLevel, int boolExact, const char *message);
//  Semi public
int zcheckStatus(long long *ifltab, int status, int verbose, const char *message);
int zcompareTimeArrays(long long *ifltab, zStructTimeSeries *struct1, zStructTimeSeries *struct2, int verboseLevel, int boolExact, const char *message);
int zcompareInts(long long *ifltab, int number1, int number2, int verbose, const char *message);
int zcompareStrings(long long *ifltab, const char *string1, const char *string2, int totalLength, int boolIgnoreCase, int verbose, const char *message);
int zcompareLongs(long long *ifltab, long long number1, long long number2, int verbose, const char *message);
int zcompareFloats(long long *ifltab, float number1, float number2, int verbose, const char *message);
int zcompareDoubles(long long *ifltab, double number1, double number2, int verbose, const char *message);
int zcompareIntArrays(long long *ifltab, int *array1, int *array2, int numberArray, int verbose, const char *message);
int zcompareFloatArrays(long long *ifltab, float *array1, float *array2, int numberArray, int verbose, const char *message);
int zcompareDoubleArrays(long long *ifltab, double *array1, double *array2, int numberArray, int verbose, const char *message);
int zcompareStringArrays(long long *ifltab, const char *string1, const char *string2, int totalBytes, int boolIgnoreCase, int verbose, const char *message);
void zprintFailMessage(long long *ifltab, int verboseLevel, const char *name, const char *pathname, const char *message);
//  Private - use zcompareDataSets instead
int zcompare_ztransferStruct(long long *ifltab, zStructTransfer *struct1, zStructTransfer *struct2, int verboseLevel, int boolExact, const char *message);
int zcompare_zStructRecordSize(long long *ifltab, zStructRecordSize *struct1, zStructRecordSize *struct2, int verboseLevel, const char *message);
int zcompare_zStructTsRecordSizes(long long *ifltab, zStructRecordSize *struct1, zStructRecordSize *struct2, int verboseLevel, const char *pathname, const char *message);
int zcompare_zStructRecordAddresses(long long *ifltab, zStructRecordAddresses *struct1, zStructRecordAddresses *struct2, int verboseLevel, const char *pathname, const char *message);
int zcompare_zStructLocation(long long *ifltab, zStructLocation *struct1, zStructLocation *struct2, int verboseLevel, const char *message);
int zcompare_zStructTimeSeries(long long *ifltab, zStructTimeSeries *struct1, zStructTimeSeries *struct2, int verboseLevel, int boolExact, const char *message);
int zcompare_zStructArray(long long *ifltab, zStructArray *struct1, zStructArray *struct2, int verboseLevel, int boolExact, const char *message);

//  Record time granularities can only be either seconds or minutes
//  Minutes are only used for irregular interval century (where int would overflow)
//  Data sets (multiple records) time granularities can be any of the following.
//  (default is minute for compatibility).  For very large date expanses, hour or day is used.
#define SECOND_GRANULARITY 1
#define MINUTE_GRANULARITY 60
#define HOUR_GRANULARITY 3600
#define DAY_GRANULARITY 86400

#define DAILY_BLOCK		1
#define MONTHLY_BLOCK	2
#define YEARLY_BLOCK	3
#define DECADE_BLOCK	4
#define CENTURY_BLOCK	5


#endif //  HECDSS_INTERNAL_H

