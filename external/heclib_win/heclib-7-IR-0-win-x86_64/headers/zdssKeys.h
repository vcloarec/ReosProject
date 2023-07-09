#ifndef ZDSSKEYS_H
#define ZDSSKEYS_H

//  zdssKeys are positions in the main file table, ifltab, for
//  run-time items, such as the handle, etc.
//  There are 3 integrity keys to help identify if the array
//  becomes corrupt (from something overwriting it in memory)
struct {
	   int kaddTableHash;
	   int kaddInfoLastPath;    //
	   int kaddLast;
	   int kfound;
	   int kpathnameHash;    //  Points to the bin hash of the last pathname checked
	   int khashTableBinAdd;   //  Points to the bin address of the table hash (in hash table)
	   int kpathBinAddress;  //  Points to the address of the segment in the path bin (the address of the pathname hash)
	   int kbinPathLen;
	   int kbinStatus;
	   int kbinLastWrite;
	   int kbinDates;
	   int kbinTypeAndCatSort;
	   int kcatStruct;
	   int kCRCtable;
	   int kpathAddressInBin;
	   int kinfoAddInBin;
	   int kiftPathHash;  //  the bin hash code of the pathname for the buffer area
	   int kinfoAddress; //  the file address of the buffer area
	   int kinfoSize;   //  the length being used by the buffer
	   int kinfo; //  start of the buffer in ifltab
	   int kfiHeadSize;
	   int kdswap;    //
	   int kerrorCode;  //  the actual error.  See errorProcessing for different codes
	   int kerrorCondition;  //  indicates if an error has occurred and its severity
	   int kerrorSevere;
	   int kmessagesAvail;  //  Number of important messages available (generally errors) to calling program
	   int kexclusive;     //
	   int kintegrityKey1;      //  pointers to words in IFLTAB that should contain the KEY value (NKEY) after ZOPEN
	   int kintegrityKey2;      //  pointers to words in IFLTAB that should contain the KEY value (NKEY) after ZOPEN
	   int kintegrityKey3;  //  pointers to words in IFLTAB that should contain the KEY value (NKEY) after ZOPEN
	   int kfileWritten;
	   int khandle;    //
	   int kdataFirstDate;
	   int kdataLastDate;
	   int kgetLogicalNumberData;
	   int ksetLogicalNumberData;
	   int klocked;     //
	   int klockExclusiveWord;
	   int klocksDenied;    //
	   int klockLevel;
	   int klockCheckSet;
	   int klockWriteMyAddress;
	   int klockReadMyAddress;
	   int kpidMyAddress;
	   int kmyLastWriteTime;
	   int kpathBin;
	   int kmessLevel;     // points to the message level of this file.  If -1, the default is used
	   int kmultiUserAccess;
	   int kmessHandle;    // points to the output unit for this file.  If 0, the default is used
	   int kfilename;  //  Filename, in memory (excludes path)
	   int kfullFilename;  //  Full filename with path (and drive)
	   int kopenMode;   //  Mode (e.g., read only) on when the file was open (not necessarily current mode)
	   int kfortMessUnit;   //  The Fortran message unit... if 0, not written to.
	   int klenLastPath;    // length of last pathname
	   int kisaCollection;
	   int klastType;		//  Record type of last non-internal record checked (e.g., time series, not location)
	   int knumVersion;       //
	   int kbinAddCurrent;     //
	   int kbinWithSpace;
	   int kbinAddplist;
	   int kbinPosPlist;
	   int kbinCountplist;
	   int kfileHeader;     //
	   int kpathsThisHash;
	   int ksameHash;
	   int kopenStatus;    //
	   int knumberReads;
	   int knumberWrites;
	   int kreclaimLevel;
	   int kreclaimLevelFile;
	   int kreclaim;
	   int kremote;   //
	   int ksuser;    //
	   int kswap;     //
	   int ktableHash;	  //  Points to the table hash of the last pathname checked
	   int kwritingNow;  //  Used for inter function processes
	   int kwritesSinceFlush;
} zdssKeys;


//  zdssFileKeys are positions in the file header (permanent section) for
//  file related items, such as addresses, sizes and use information
struct {
	   int kdss;
	   int kfileHeaderSize;
	   int kversion;
	   int knumberRecords;
	   int knumberAliases;
	   int kfileSize;
	   int kdead;
	   int knumberExpansions;
	   int knumberCollections;
	   int knumberRenames;
	   int knumberDeletes;
	   int knumberAliasDeletes;
	   int kcreateDate;
	   int klastWriteTime;
	   int klockAddressWord;
	   int kmaxHash;
	   int khashsUsed;
	   int kmaxPathsOneHash;
	   int kmaxPathsHashCode;
	   int kaddHashTableStart;
	   int khashCollisions;
	   int kbinsPerBlock;
	   int kbinsRemainInBlock;
	   int kbinSize;
	   int kaddFirstBin;
	   int kaddNextEmptyBin;
	   int ktotalBins;
	   int kbinsOverflow;
	   int kfilePassword;
	   int kfileError;   //  indicates if an error occurred last time the file was written to (previous session)  UNUSED
	   int kfileErrorCode;  //  What, specifically that error was (according to _get_errno)  UNUSED
	   int kcatSequenceNumber;
	   int kcatSortStatus;  //  the status of the catalog sort list
	   int kcatSortNewWrites;  //  number of new records since last sort saved
	   int kcatSortDeletes;
	   int kcatSortSize;  //  physical length of sort list in longs, excluding flags and lengths
	   int kcatSortNumber;  //  number of values in sort list, usually equal to length
	   int kcatSortAddress;
	   int kreclaimMin;
	   int kreclaimMaxAvailable;
	   int kreclaimTotal;
	   int kreclaimTableAddress;
	   int kreclaimSegAvailableAdd;
	   int kreclaimSegNumber;
	   int kreclaimMaxSegment;
	   int kreclaimSegmentsUsed;
	   int kreclaimNumber;
	   int kreclaimSize;
	   int kreclaimedPaths;
	   int kreclaimedSpace;
	   int kmaxPath;
	   int kmaxA;
	   int kmaxB;
	   int kmaxC;
	   int kmaxD;
	   int kmaxE;
	   int kmaxF;
	   int kmaxInternalHeader;
	   int kmaxHeader2;
	   int kmaxUserHeader;
	   int kmaxValues1Size;
	   int kmaxValues2Size;
	   int kmaxValues3Size;
	   int knumberInternalHeader;
	   int knumberHeader2;
	   int knumberDataArea3;
	   int knumberUserHeader;
	   int knumberDataArea1;
	   int knumberDataArea2;
	   int kmaxRecordSize;
	   int kmaxRtsSize;
	   int kmaxRtdSize;
	   int kmaxItsSize;
	   int kmaxItdSize;
	   int kmaxPdSize;
	   int kmaxPddSize;
	   int kmaxOtherSize;
	   int kmaxExpectedPathnames;
	   int kerrorMemory;
	   int kerrorAddress;
	   int kerrorWrite;
	   int kerrorRead;
	   int kerrorTotal;
	   int klocBoundLR;
	   int klocBoundLL;
	   int klocBoundUR;
	   int klocBoundUL;
	   int klocBoundElev;
	   int kdetune;
	   int klockArraySizes;
	   int klockWriteArrayAddress;
	   int klockReadArrayAddress;
	   int kpidArrayAddress;
	   int kendian;
	   int kreserved1;
	   int kreserved2;
	   int kreserved3;
	   int kreserved4;	   
	   int kendFileHeader;
} zdssFileKeys;


//  zdssInfoKeys are positions in the record header (info section) for
//  information and addresses for the record.  (This does not include
//  information about the data, that is in the data's "internal header")
struct {
	   int kinfoFlag;
	   int kinfoStatus;
	   int kinfoPathnameLength;
	   int kinfoHash;
	   int kinfoTypeVersion;
	   int kinfoExpansion;
	   int kinfoLastWriteTime;
	   int kinfoProgram;
	   int kinfoFirstDate;
	   int kinfoLastDate;
	   int kinfoCreationTime;
	   int kinfoReserved1;
       int kinfoInternalHeadAddress;
	   int kinfoInternalHeadNumber;
	   int kinfoHeader2Address;
	   int kinfoHeader2Number;
	   int kinfoUserHeadAddress;
	   int kinfoUserHeadNumber;
	   int kinfoValues1Address;
	   int kinfoValues1Number;
	   int kinfoValues2Address;
	   int kinfoValues2Number;
	   int kinfoValues3Address;
	   int kinfoValues3Number;
	   int kinfoAllocatedSize;
	   int kinfoNumberData;
	   int kinfoLogicalNumber;
	   int kinfoAliasesBinAddress;
	   int kinfoReserved;
	   int kinfoPathname;
} zdssInfoKeys;

//  zdssBinKeys are positions for each pathname in a pathname bin
//  essentially the address to the record's header (info) area
struct {
	int kbinHash;
	int kbinStatus;
	int kbinPathLen;
	int kbinInfoAdd;
	int kbinTypeAndCatSort;
	int kbinLastWrite;
	int kbinDates;
	int kbinPath;
	int kbinSize;
} zdssBinKeys;



#endif //  ZDSSKEYS_H

