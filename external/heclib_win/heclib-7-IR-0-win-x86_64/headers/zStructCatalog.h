#ifndef zSTRUCT_CATALOG_H
#define zSTRUCT_CATALOG_H

#include "zStructAllocation.h"

//  Catalog (pathname list) for a DSS file

typedef struct {

	//  Private
	int structType;

	//  Optional input
	//  For normal catalog, statusWanted = 0, and typeWanted = 0.
	int statusWanted;
	int typeWantedStart;
	int typeWantedEnd;

	//  Search according to record last write time (e.g., records written to
	//  since a previous time (using file header write time)
	//  Times are system times, in mills
	//  lastWriteTimeSearch == 0 for ignore (default)
	//  lastWriteTimeSearchFlag:
	//		-2:		time <  lastWriteTimeSearch
	//		-1:		time <= lastWriteTimeSearch
	//		 0:		time == lastWriteTimeSearch
	//		 1:		time >= lastWriteTimeSearch
	//		 2:		time >  lastWriteTimeSearch
	long long lastWriteTimeSearch;
	int lastWriteTimeSearchFlag;

	//  Output
	//  An array of the pathnames in the DSS file
	char **pathnameList;
	int numberPathnames;  //  number of valid pathnames in list
	int boolSorted;
	int boolIsCollection;

	//  Attributes are descriptors for records, usually used for searching in a list,
	//  but are not used for unique identity.
	//  "::" separates key from attribute, ";;" separates attribute sets
	//  For example
	//  pathnameList[43] = /Tulare/Delano/Flow/01Jan1980/1Day/Obs/"
	//  attribues[43] = "County::Kern;;State::CA;;Region::Southern"
	//  pathnameList[78] = /American/Fair Oaks Local/Flow/01Jan2200/1Hour/ReReg/"
	//  attribues[78] = "Type::Subbasin;;Order::142"
	int boolHasAttribues;
	char **attributes;

	//  If boolIncludeDates == 1 on input, then startDates and endDates
	//  will be int arrays of the Julian first and last date for each
	//  record (pathname)
	int boolIncludeDates;
	int *startDates;
	int *endDates;

	//  Always returns these (right there)
	int *recordType;
	long long *pathnameHash;
	long long *lastWriteTimeRecord;
	long long lastWriteTimeFile;

	//  CRC values - Resource intensive!  Only use if you really needed
	unsigned int *crcValues;
	int boolGetCRCvalues;   //  Set to 1 to have CRC values computed

	//  Private
	int listSize;  // size allocated
	long long *sortAddresses;  //  Used for sorting
	char *pathWithWildChars;  //  Only for info
	char allocated[zSTRUCT_length];

} zStructCatalog;

#endif  //  zSTRUCT_CATALOG_H

