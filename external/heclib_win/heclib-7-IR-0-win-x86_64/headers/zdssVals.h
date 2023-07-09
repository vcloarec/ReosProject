#ifndef ZDSS_VALS_H
#define ZDSS_VALS_H

#define MAX_LEN_ERROR_MESS 500

struct hec_zdssVals {
	 long long integrityKey;      //
	 int maxInfoSize;   //  allocated space in long longs for Info
	 int fortranMessageUnit;
	 int messageHandle;
	 int buffMaxSize;  //  the maximum size (that can be used) of the buffer
	 int pid;
	 int infoSize;
	 int maxExpectedPathnames;
	 int newCollectionFile;
	 int globalErrorFlag;
	 int numberProgram;
	 int iverticalDatum;
	 int icanOverwriteLocationVerticalDatum;
	 int copyEmptyRecords;
	 char czdss[5];
	 char czVersion[5];
	 char czVersionDate[20];
	 char cprogramName[17];
	 char cverticalDatum[17];
	 char globalErrorMess[MAX_LEN_ERROR_MESS];
} ;

extern struct hec_zdssVals zdssVals;

#define DSS_END_HEADER_FLAG		-97531
#define DSS_END_FILE_FLAG		-97532
#define DSS_INFO_FLAG			-97534
#define DSS_START_CAT_SORT_FLAG	-97536
#define DSS_END_CAT_SORT_FLAG	-97537

#define DSS_NUMBER_LOCK_ATTEMPS		100

#define DSS_MIN_FILE_HEADER_SIZE	100
//  Keep track of how many other files are accessing this one
#define DSS_LOCK_ARRAY_SIZE			25

#define DSS_INTEGRITY_KEY		13579
#define DSS_MEMORY_INTEG_KEY	24680
static int messageHandle = -1;
static int fortranMessageUnit = -1;

#define MAX_PATHNAME_LENGTH 393
#define MAX_PATHNAME_SIZE   394
#define MAX_PART_SIZE 129

#define MAX_FILENAME_LENGTH 1024


//  Buffer Action
#define BUFF_NO_ACTION		0
#define BUFF_WRITE			1
#define BUFF_WRITE_FLUSH	2
#define BUFF_READ			1
#define BUFF_LOAD			2

//  Buffer Control
#define BUFF_SIZE			0
#define BUFF_STAT			1
#define BUFF_ADDRESS		2
#define BUFF_INTS_USED		3

//  Buffer statuses (for BUFF_STAT)
#define BUFF_STAT_UNUSED	0
#define BUFF_STAT_NOT_DIRTY	1
#define BUFF_STAT_DIRTY		2

#define OPEN_STAT_CLOSED	0
#define OPEN_STAT_READ_ONLY	1
#define OPEN_STAT_WRITE		2

#define REC_STATUS_VALID			0
#define REC_STATUS_PRIMARY			1
#define REC_STATUS_ALIAS			2
#define REC_STATUS_MOVED			10
#define REC_STATUS_DELETED			11
#define REC_STATUS_RENAMED			12
#define REC_STATUS_ALIAS_DELETED	13
#define REC_STATUS_REMOVED			15     //  Deleted and then space used elsewhere
#define REC_STATUS_ANY				100

//		RECLAIM_UNDEFINED	0
#define RECLAIM_NONE		1	//  Don't use space reclamation
#define RECLAIM_EXCESS		2	//  Reclaim space left over from extending records, etc.  (can recover records)
#define RECLAIM_ALL			3	//  Reclaim all unused space, including deleted records (cannot recover)



#endif //  ZDSS_VALS_H

