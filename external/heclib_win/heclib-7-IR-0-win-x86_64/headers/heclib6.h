#ifndef HECLIB6_H
#define HECLIB6_H

#include "zStructRecordSize.h"


/*  Microsoft C++ uses stdcall for FORTRAN interface  */

//#ifdef _MSC_VER
//#define VOID void __stdcall
//#define INT  size_t  __stdcall
//#else
#define VOID void
#define INT  int
//#endif


#ifdef __cplusplus
extern "C" {
#endif


	//////////////////////////////////////////////////////////////
	/////////////////////////////  new stuff

	VOID chrlnb_ (char *cstring, int *len, size_t strlen);
	VOID chrhol_ (const char *cstring, int *start, int *length, int *hol, int *start2, size_t stringLen);
	VOID getime_ (const char *line, const int *beg, const int *len,
				  int *juls, int *istime, int *jule, int *ietime, int *status,
				  size_t line_length);
	VOID holchr_ (int *hol, int *start, int *length, char *str, int *start2, size_t stringLen);
	INT idaywk_(int *jul);
	INT ihm2m_(char *ctime, size_t ctimeLen);
	INT inctim_(int *interval, int *minFlag, int *nperiods, int *juls, int *istime,
		int *jule, int *ietime);
	INT isunitconnected (int *unit);
	INT iymdjl_ (int *year, int *month, int *day);
	INT jliymd_ (int *julian, int *year, int *month, int *day);
	INT juldat_ (int *julian, int *style, char *date, int *date_len, size_t size);
	INT datjul_ (char *date, int *julian, int *error, size_t date_len);
	INT m2ihm_ (int *min, char *ctime, size_t len);
	INT nopers_ (int *intl, int *flag, int *juls, int *istime, int *jule, int *ietime);
	VOID systim_ (int *julian, int *seconds);
	VOID upcase_ (char *string, size_t len);
	//int zgetRecordSize (long long *ifltab, zStructRecordSize *recordSize);
	VOID zrecordinfo6_(long long *ifltab, const char* pathname, int *idataType, int *iversion,
     int *numberVals, int *logicalNumberVals,
     int *numberVals1, int *numberInternalHead, int *numberUserHead,
     int *isizeAllocated, int *lastWriteSecs, int *iprecisionTS,
     int *ioffset, int *ivalSize, int *iqualSize,
     int *numberCurvesPD, int *numberOrdinatesPD, int *ipdValueSize, int *iaxisFlagPD,
     int *lboolLabelsPD, int *iprecisionPD, int *istatus, size_t lenPathname);

void zsqueeze6_(const char *dssFilename, int *status, size_t lenDssFilename);

void zsetca6_(const char *instr, int *boolSelect, size_t len_instr);
void zmatca_(const char *pathname, int *IBPART, int *IEPART, int *ILPART,
	const char *CCDATE, const char *CCPROG, int *LMATCH,
	size_t len_pathname, size_t len_CCDATE, size_t len_CCPROG);

void zset6_(const char *CFLG, const char *CSTR, int *NUMB, size_t lenCflg, size_t lenCstr);

void writf_(int *ihandle, void *buff, int *nbytes, int *istat, int *ntrans);

	//const char *zpathnameList(long long *ifltab, long long *space, int *icount);
	//INT zcatalogFile(long long *ifltab, const char *catalogFileName, size_t includeDates, size_t statusWanted);

	///////////////////////////////////////////////////////////////

	//  DEPRECATED  //
	VOID closescratchdsscatalog_(int *unitNumber);

	VOID makedsscatalog_ (const char *dssFileName, int *ifltab,
                      const char *instructions, int *numberFound,
                      int *catalogUnit,
                      size_t len_dssFileName, size_t len_instructions);

	VOID zplist_ (long long *ifltab, const char *instr, int *filePos, char *pathname,
               int *nPathname, int *istatus, size_t len_instr, size_t sizeof_pathname);

//  DSS  routines

VOID zclose6_ (long long *ifltab);

VOID zcheck6_(long long *ifltab, const char* pathname, int *numberPathname,
			 int *numberHeader, int *numberData, int *lfound, size_t lenPathname);

VOID zsqueeze_ (const char *fileName, int *status, size_t lenFileName);
VOID zsqueeze7_ (const char *fileName, int *status, size_t lenFileName);

VOID zbegdt_ (int*, int*, int*, int*, int*, int*, int*);

VOID zcklnk (int *ifltab, int *nerrors);
VOID zcklnk6_ (long long *ifltab, int *nerrors);

VOID zckpnb (int *ifltab, int *nerrors);
VOID zckpnb6_ (long long *ifltab, int *nerrors);

VOID zckpat (int *ifltab, int *nerrors);
VOID zckpat6_ (long long *ifltab, int *nerrors);

VOID zrdprm6_ (long long *ifltab, int *istatus);

int zdataType6(long long *ifltab, const char *pathname);

VOID zdtype6_(long long *ifltab, const char *pathname, int *ndata, int *found, const char *cdtype, int *dataType,
		size_t lenpathname, size_t lencdtype);

VOID zcorec (int *ifltabFrom, int *ifltabTo, const char *pathnameFrom,
			  const char *pathnameTo, int *buffer1, int *buffer1Size,
			  int *buffer2, int *buffer2Size, int *boolDuplicate, int *status,
              size_t len_pathnameFrom, size_t len_pathnameTo);

int zcopyrecord6_(long long *ifltabFrom, long long *ifltabTo, const char *pathnameFrom, const char *pathnameTo,
	int *status, size_t pathnameFromLen, size_t pathnameToLen);

VOID zcofil (int *ifltabFrom, int *ifltabTo, int *buffer1, int *buffer1Size,
		      int *buffer2, int *buffer2Size, int *restoreDelete, int *retag);


VOID zcut    (int *ifltab, const char *pathname, int *ldelete, int *buffer,
			   int *sizeBuffer, int *status, size_t len_pathname);

VOID zcutsz  (int *ifltab, const char *pathname, int *size, int *status,
			   size_t len_pathname);

VOID zdcinf (int *method, float *baseValue, int *baseSet, int *deltaSize,
			  int *precision, int *status);

VOID zdblook  (int *ifltab, int *address, int *length);

VOID zdbmod  (int *ifltab, int *address, int *value, const char *characterValue, int *useCharacterValue,
			   size_t len_characterValue);

VOID zdebug1  (int *ival, long long *ival8, char *creal, char *cstrng, int *ibytes,
			   size_t len_creal, size_t len_cstrng);

VOID zdelet  (int *ifltab, const char *pathname, int *npath, int *status,
			   size_t len_pathname);

VOID zdtype_ (long long *ifltab, const char *, int *, int *, char *, int *, size_t, size_t);

VOID zgetrw (int *address, int *record, int *word);

VOID zgintl_ (int*, char*, int*, int*, size_t);

VOID zgpnp_ (const char*, char*, char*, char*, char*, char*, char*, int*,
                size_t, size_t, size_t, size_t, size_t, size_t, size_t);

VOID zgtrec (int *ifltab, int *arrayVals, int *numberWords, int *address, int* flag);

VOID zincbk_ (int*, int*, int*, int*, int*);

VOID zinqir (int *, const char *, char *, int *, size_t, size_t);

VOID zirbeg_ (int*, const char*, int*, int*, int*,
              int*, int*, int*, size_t);

VOID zndata (int* ifltab, int* ifpos, int* juls, int* isecs,
			 const char* cpath, int* npath, int* juld, int* isecd,
			 int* idtype, int* istat, size_t sizeof_cpath);

VOID znextts (int *ifltab, const char *pathname, char *nextPath,
               int *lforward, int *status, size_t len_pathname, size_t len_nextPath);


VOID zofset_ (int*, int*, int*, int*, int*);


VOID zopnca (const char *dssName, const int *icunit, const int *lgenca,
              int *lopnca, int *lcatlg, const int *icdunt, const int *lgencd,
              int *lopncd, int *lcatcd, int *nrecs, size_t lenDssName);

VOID zpaste (int *ifltab, int *buffer, int *status);

VOID zpath_ (const char*, const char*, const char*, const char*,
             const char*, const char*, char*, int*,
                size_t, size_t, size_t, size_t, size_t, size_t, size_t);

VOID zquery_ (const char *, char *, int *, size_t, size_t);

VOID zrdpat (const int *icunit, int *ipos, int *inumb, char *tag,
              char *path, int *npath, int *lend, size_t lenTag, size_t lenPath);


VOID zrenam (int *ifltab, const char *pathname, int *npath, const char *newPathname,
			  int *nnewpath,  int *found, size_t len_pathname, size_t len_newPathname);


VOID zrits (int* ifltab, const char* cpath, int* juls,
             int* istime, int* jule, int* ietime,
             int* itimes, float* values, int* kvals, int *nvals,
             int* ibdate, char* cunits, char* ctype, int* istat,
             size_t lenCpath, size_t lenCunits, size_t lenCtype);

VOID zritsc (int* ifltab, const char* cpath, int* juls,
             int* istime, int* jule, int* ietime, int* getDoubles, int* doublesRead,
             int* itimes, float* floatValues, double* doubleValues, int* maxValues,
			 int *numberRead, int* beginJulian, int* flags, int* getFlags, int* flagsRead,
			 char* cunits, char* ctype,
			 char* supplementalInfo,  int* timezoneOffset, char* timezoneName,
			 double*coordinates, int* coordinateDescription, int* coordinatesUsed,
			 int* inputFlag, int* istat,
             size_t lenCpath, size_t lenCunits, size_t lenCtype,
			 size_t lenSupplementalInfo, size_t lenTimezoneName);

VOID zritsx (int* ifltab, const char* cpath, int* juls,
             int* istime, int* jule, int* ietime,
             int* itimes, float* values, int* kvals, int *nvals,
             int* ibdate, int* flags, int* getFlags, int* flagsRead,
			 char* cunits, char* ctype, int* header, int* maxHeader,
			 int* numberHeader, int* inputFlag, int* istat,
             size_t lenCpath, size_t lenCunits, size_t lenCtype);

void zritsi6_(long long *ifltab, const char *path,
				int *startJulian, int *startTimeMinutes,
				int *endJulian, int *endTimeMinutes,
				int *boolGetDoubles, int *boolDoublesRead,
				int *timeArray, float *singles, double *doubles,
				int *maxNumberValues, int *numberRead,
				int *julianBaseDate,
				int *quality, int *boolGetQuality, int *boolQualityRead,
				char *units, char *type,
				int *userHeader, int *userHeaderArraySize, int *userHeaderNumber,
				double coordinates[], int coordinateDescription[], int *boolCoordinatesRead,
				int *readFlag, int *status,
				size_t pathLen, size_t unitsLen, size_t typeLen);

VOID zritsxd (int* ifltab, const char* cpath, int* juls,
             int* istime, int* jule, int* ietime,
             int* itimes, double* values, int* kvals, int *nvals,
             int* ibdate, int* flags, int* getFlags, int* flagsRead,
			 char* cunits, char* ctype, int* header, int* maxHeader,
			 int* numberHeader, int* inputFlag, int* istat,
             size_t lenCpath, size_t lenCunits, size_t lenCtype);

VOID zrpd (int*, const char*, int*, int*, int*,
            char*, char*, char*, char*, float*, int*, int*,
            char*, int*, int*, int*, int*, int*, int*,
            size_t, size_t, size_t, size_t, size_t, size_t);

VOID zrpdd (int*, const char*, int*, int*, int*,
            char*, char*, char*, char*, double*, int*, int*,
            char*, int*, int*, int*, int*, int*, int*,
            size_t, size_t, size_t, size_t, size_t, size_t);

VOID zrrst (int *ifltab, const char *cpath, int * ietim, const char *cloc,
             const char *catim, const char *chparm, const char *chunit,
             const char *cfparm, const char *cfunit, const char *ccmt, const char *copt,
             int *lsnum, int *isnum, int *ldatum, float *datum, int *lstage, int *ibhi,
             int *ibhu, float *huv, int *ibho, float *hov, int *ibfi, int *ibfu, float *fuv,
             int *ibfo, float *fov, int *kbase, int *nbase, float *height, float *flow,
             int *kshift, int *nshift, int *isti, int *istu, int *isto, int *isbi, int *isbu,
             float *sbuv, int *isbo, float *sbov, int *ishi, int *ishu, float *shuv, int *isho,
             float *shov, int *latmsk, int *jbdate, int *ietime, int *iatime, int *nshftp,
             int *kshval, int *nshval, float *shifts, int *lcoff, float *cofval, int *koff,
             int *noff, int *iobi, int *iobu, float *obuv, int *iobo, float *obov, int *iohi,
             int *iohu, float *ohuv, int *ioho, float *ohov, float *offset, int *ihoriz,
             int *istat, size_t cpathLen, size_t clocLen, size_t catimLen, size_t chparmLen, size_t chunitLen,
             size_t cfparmLen, size_t cfunitLen, size_t ccmtLen, size_t coptLen);

VOID zrrsti (int *ifltab, const char *cpath, int *ifcat, int *iintrp, int *iuflow, int *ioflow,
             int *lfound, int *ktimes, int *ntimes, int *itimes, int *istat, size_t npath);

VOID zrrts (int* ifltab, const char* pathname, const char* startDate,
			 const char* startTime, int* numberVals, float* values,
             char* units, char* type, int* offset, int* status,
			 size_t lenPathname, size_t lenStartDate, size_t lenStartTime,
			 size_t lenUnits, size_t lenType);

VOID zrrtsc (int* ifltab, const char* pathname, const char* startDate,
			 const char* startTime, int* maxVals, int* numberVals,
			 int* getDoubles, int* doublesRead, float* floatValues, double* doubleValues,
			 int* flags, int* readFlags, int* flagsRead,
             char* units, char* type, char* supplementalInfo,
			 int* offset, int* compression, int* timezoneOffset, char* timezoneName,
			 double*coordinates, int* coordinateDescription, int* coordinatesUsed,
			 int* status,
			 size_t lenPathname, size_t lenStartDate, size_t lenStartTime,
			 size_t lenUnits, size_t lenType, size_t lenSupplementalInfo, size_t lenTimezoneName);

VOID zrrtsi6_ (long long* ifltab, const char* pathname, const char* startDate,
			 const char* startTime, int* maxVals, int* numberVals,
			 int* getDoubles, int* doublesRead, float* floatValues, double* doubleValues,
			 int* flags, int* readFlags, int* flagsRead,
             char* units, char* type, int *userHeader, int *maxUserHead, int *numberUserHead,
			 int* offset, int* compression,
			 double* coordinates, int* coordinateDescription, int* coordinatesUsed,
			 int* status,
			 size_t lenPathname, size_t lenStartDate, size_t lenStartTime,
			 size_t lenUnits, size_t lenType);

VOID zgettz_(char* ctzone, int* itzone, size_t size_ctzone);

VOID zrrtsx (int* ifltab, const char* pathname, const char* startDate,
			 const char* startTime, int* numberVals, float* values,
			 int* flags, int* readFlags, int* flagsRead,
             char* units, char* type,
			 int* userHeader, int* userHeaderMax, int* lenUserHeader,
			 int* offset, int* compression, int* status,
			 size_t lenPathname, size_t lenStartDate, size_t lenStartTime,
			 size_t lenUnits, size_t lenType);

VOID zrrtsxd (int* ifltab, const char* pathname, const char* startDate,
			 const char* startTime, int* numberVals, double* values,
			 int* flags, int* readFlags, int* flagsRead,
             char* units, char* type,
			 int* userHeader, int* userHeaderMax, int* lenUserHeader,
			 int* offset, int* compression, int* status,
			 size_t lenPathname, size_t lenStartDate, size_t lenStartTime,
			 size_t lenUnits, size_t lenType);

              // Note: zrtext passes in a file name instead of a unit number
               //        and there is no LCCNTL argument!
VOID zrtext (int *ifltab, const char* cpath, const char* fileName,
              int *headu, int *kheadu, int *nheadu, int *nlines, int *istat,
              size_t len_cpath, size_t len_fileName);

              // Important:  For zrtxta, see the discussion under zstxta!
VOID zrtxta (int *ifltab, const char* cpath, char* carray, int *klines,
              int *nlines, int *headu, int *kheadu, int *nheadu, int *istat,
              size_t len_cpath, size_t len_carray);

VOID zrtxts (int *ifltab, const char* cpath, char* cString, int *kCString,
              int *nCString, int *headu, int *kheadu, int *nheadu, int *istat,
              size_t len_cpath, size_t len_cString);


VOID zspd (int*, const char*, int*, int*, int*,
            const char*, const char*, const char*, const char*,
            const float*, const char*, int*, int*,
            int*,  int*, int*,
            size_t, size_t, size_t, size_t, size_t, size_t);

VOID zspdd (int*, const char*, int*, int*, int*,
            const char*, const char*, const char*, const char*,
            const double*, const char*, int*, int*,
            int*,  int*, int*,
            size_t, size_t, size_t, size_t, size_t, size_t);


void zspdi6_(long long *ifltab, const char *pathname,
			 int *numberOrdinates, int *numberCurves, int *iHorizontal,
			 const char *unitsIndependent, const char *typeIndependent,
			 const char *unitsDependent, const char *typeDependent,
			 int *svalues, int *dvalues, int *boolDoubles,
			 const char *labels, int *boolStoreLabels,
			 int *userHeader, int *userHeaderNumber,
			 int *iplan, int *istat,
			 size_t pathnameLen,
			 size_t unitsIndependentLen, size_t typeIndependentLen,
			 size_t unitsDependentLen, size_t typeDependentLen,
			 size_t lablesLength);
void zrpdi6_(long long *ifltab, const char *pathname,
			 int *numberOrdinates, int *numberCurves, int *iHorizontal,
			 char *unitsIndependent, char *typeIndependent,
			 char *unitsDependent, char *typeDependent,
			 float *svalues, double *dvalues, int *boolDoubles, int *maxValues, int *numberValues,
			 char *labels, int *maxLables, int *boolLabels,
			 int *userHeader, int *maxUserHead, int *userHeaderNumber,
			 int *istat,
			 size_t pathnameLen,
			 size_t unitsIndependentLen, size_t typeIndependentLen,
			 size_t unitsDependentLen, size_t typeDependentLen,
			 size_t lablesLength);


VOID zsits (int* ifltab, const char* pathname, const int* timeArray,
			 const float* valuesArray, int* numberValues,
             int* baseDate, const char* units, const char* type,
			 int* inflag, int* status,
             size_t lenPathname, size_t lenUnits, size_t lenType);

VOID zsitsc (int* ifltab, const char* pathname, const int* timeArray,
			 const float* floatValues, const double* doubleValues,
			 int* storeDoubles, int* numberValues, int* baseDate,
			 int* flags, int* storeFlags,
			 const char* units, const char* type,
			 double* coordinates, int* numberCoordinates,
			 int* coordinateDescription, int* numberCoordDescription,
			 const char* supplementaryInfo, int* timezoneOffset, const char* timezoneName,
			 int* inflag, int* status,
             size_t lenPathname, size_t lenUnits, size_t lenType, size_t lenSupplementaryInfo,
			 size_t lenTimezoneName);


VOID zsitsc6_ (long long *ifltab, const char* pathname, const int* timeArray,
			 const float* floatValues, const double* doubleValues,
			 int* storeDoubles, int* numberValues, int* baseDate,
			 int* flags, int* storeFlags,
			 const char* units, const char* type,
			 double* coordinates, int* numberCoordinates,
			 int* coordinateDescription, int* numberCoordDescription,
			 const char* supplementaryInfo, int* timezoneOffset, const char* timezoneName,
			 int* inflag, int* status,
             size_t lenPathname, size_t lenUnits, size_t lenType, size_t lenSupplementaryInfo,
			 size_t lenTimezoneName);


VOID zsitsx (int* ifltab, const char* pathname, const int* timeArray,
			 const float* valuesArray, int* numberValues,
             int* baseDate, int* flags, int* storeFlags,
			 const char* units, const char* type,
			 int* userHeader, int* lenUserHeader,
			 int* inflag, int* status,
             size_t lenPathname, size_t lenUnits, size_t lenType);

VOID zsitsxd (int* ifltab, const char* pathname, const int* timeArray,
			 const double* valuesArray, int* numberValues,
             int* baseDate, int* flags, int* storeFlags,
			 const char* units, const char* type,
			 int* userHeader, int* lenUserHeader,
			 int* inflag, int* status,
             size_t lenPathname, size_t lenUnits, size_t lenType);

VOID zsrst (int *ifltab, const char *cpath, const char *cloc,
            const char *catim, const char *chparm, const char *chunit,
            const char *cfparm, const char *cfunit, const char *ccmt, const char *copt,
            int *lsnum, int *isnum, int *ldatum, float *datum, int *lstage, int *ibhi,
            int *ibhu, float *huv, int *ibho, float *hov, int *ibfi, int *ibfu, float *fuv,
            int *ibfo, float *fov, int *nbase, float *height, float *flow, int *nshift,
            int *isti, int *istu, int *isto, int *isbi, int *isbu, float *sbuv, int *isbo,
            float *sbov, int *ishi, int *ishu, float *shuv, int *isho, float *shov,
            int *latmsk, int *jbdate, int *ietime, int *iatime, int *nshftp, float *shifts,
            int *lcoff, float *cofval, int *noff, int *iobi, int *iobu, float *obuv,
            int *iobo, float *obov, int *iohi, int *iohu, float *ohuv, int *ioho,
            float *ohov, float *offset, int *ihoriz, int *iplan, int *istat, size_t cpathLen,
            size_t clocLen, size_t catimLen, size_t chparmLen, size_t chunitLen, size_t cfparmLen,
            size_t cfunitLen, size_t ccmtLen, size_t coptLen);

VOID zsrsti (int *ifltab, const char *cpath, int *iintrp, int *iuflow, int *ioflow, int *iplan,
             int *istat, size_t npath);

VOID zsrts (int* ifltab, const char* pathname, const char* startDate,
			 const char* startTime, int* numberVals, float* values,
             const char* units, const char* type, int* plan, int* status,
			 size_t lenPathname, size_t lenStartDate, size_t lenStartTime,
			 size_t lenUnits, size_t lenType);

VOID zsrtsx (int* ifltab, const char* pathname, const char* startDate,
			 const char* startTime, int* numberVals, float* values,
			 int* flags, int* storeFlags,
             const char* units, const char* type,
			 int* userHeader, int* lenUserHeader,
			 int* plan, int* compression, float* baseCompress,
			 int* setBaseComp, int* setDeltaHigh, int* prec, int* status,
			 size_t lenPathname, size_t lenStartDate, size_t lenStartTime,
			 size_t lenUnits, size_t lenType);

VOID zsrtsc_ (long long* ifltab, const char* pathname, const char* startDate,
			 const char* startTime, int* numberVals, int* lDouble,
			 float *floatVaues, double* doubleValues,
			 int* flags, int* storeFlags,
             const char* units, const char* type,
			 double* coordinates, int* numberCoordinates,
			 int* coordinateDescription, int* numberCoordDescription,
			 const char* supplementaryInfo, int* timezoneOffset, const char* timezoneName,
			 int* plan, int* compression, float* baseCompress,
			 int* setBaseComp, int* setDeltaHigh, int* prec, int* status,
			 size_t lenPathname, size_t lenStartDate, size_t lenStartTime,
			 size_t lenUnits, size_t lenType, size_t lenSupplementaryInfo, size_t lenTimezoneName);

VOID zrenam6_ (long long* ifltab, const char* oldPathname, int *len_oldPathname,
			const char* newPathname, int *len_newPathname, int *status,
			size_t length_oldPathname, size_t length_newPathname);

VOID zsrtsc6_ (long long* ifltab, const char* pathname, const char* startDate,
			 const char* startTime, int* numberVals, int* lDouble,
			 float *floatVaues, double* doubleValues,
			 int* flags, int* storeFlags,
             const char* units, const char* type,
			 double* coordinates, int* numberCoordinates,
			 int* coordinateDescription, int* numberCoordDescription,
			 const char* supplementaryInfo,
			 int* timezoneOffset, const char* timezoneName,
			 int* plan, int* compression, float* baseCompress,
			 int* setBaseComp, int* setDeltaHigh, int* prec, int* status,
			 size_t lenPathname, size_t lenStartDate, size_t lenStartTime,
			 size_t lenUnits, size_t lenType, size_t lenSupplementaryInfo, size_t lenTimezoneName);

VOID zsrtsxd (int* ifltab, const char* pathname, const char* startDate,
			 const char* startTime, int* numberVals, double* values,
			 int* flags, int* storeFlags,
             const char* units, const char* type,
			 int* userHeader, int* lenUserHeader,
			 int* plan, int* compression, float* baseCompress,
			 int* setBaseComp, int* setDeltaHigh, int* prec, int* status,
			 size_t lenPathname, size_t lenStartDate, size_t lenStartTime,
			 size_t lenUnits, size_t lenType);


VOID zstfh ( const char*, const char*, int*,
              int*, int*, int*, int*,
              int, size_t );

              // Note: zstext passes in a file name instead of a unit number!
VOID zstext (int *ifltab, const char* cpath, const char* fileName,
              int *headu, int *nheadu, int *nlines, int *istat,
              size_t len_cpath, size_t len_fileName);

              // For zstxta, use a single dimensioned character array, blank
              // fill.  Copy in strings, without ANY NULL characters.
              // len_carray should be the length of each "line" (no nulls!).
              // You can always use zstext instead.
VOID zstxta (int *ifltab, const char* cpath, const char* carray,
              int *nlines, int *headu, int *nheadu, int *istat,
              size_t len_cpath, size_t len_carray);

VOID ztsends (int *ifltab, const char* cpath, int *searchOption,
			  int *startJulian, int *startMinutes,
			  int *endJulian, int *endMinutes,
			  int *exists, size_t len_cpath);

VOID ztsends6_ (int *ifltab, const char* cpath, int *searchOption,
			  int *startJulian, int *startMinutes,
			  int *endJulian, int *endMinutes,
			  int *exists, size_t len_cpath);

VOID ztsinfo (int *ifltab, const char* cpath,
			  int *startJulian, int *startMinutes,
			  int *endJulian, int *endMinutes, char *units, char *type,
			  int *lqual, int *ldouble, int *lfound,
              size_t len_cpath, size_t len_units, size_t len_type);

VOID ztsrange (int *ifltab, const char* cpath, int *searchOption,
			  char *firstPath, char *lastPath, int *numberFound,
			  size_t len_cpath, size_t len_firstPath, size_t len_lastPath);

VOID zundel  (int *ifltab, const char *pathname, int *npath, int *status,
			   size_t len_pathname);

VOID zudall6_  (long long *ifltab, int *status);

VOID zudava  (int *ifltab, char *pathname, int *status, size_t len_pathname);

VOID zupath (const char* pathname, int*, int*, int*, int* istat,
              size_t pathnameLength);

VOID zustfh ( char*, char*, int*, int*, int*, int*, int*,
               size_t, size_t );

VOID zwritx (int *ifltab , const char *pathname, const int *nPathname,
             const int *internalHeader, const int *internalHeaderNumber,
             const int *compressionHeader, const int *numberHeader2,
             const int *userHeader, const int *userHeaderNumber,
             const int *data, const int *numberData,
             const int *itype, const int *iplan, int *status, int *found,
             size_t lengthPathname);


VOID zgettimezone6_(int *itzOffset, char *tzName, int *iprecision, size_t lengthtzName);

VOID zsettimezone6_(int *itzOffset, const char *tzName, int *iprecision, size_t lengthtzName);


VOID zgetinfo6_  (long long *ifltab, char *pathname, int *status, int *iarray, size_t len_pathname);

VOID opendssoutput (const char *fileName, int *status, int *fileNameLength,
                     size_t length);

VOID closedssoutput ();

#ifdef __cplusplus
};
#endif

#endif	/* HECLIB_H */

