//  Rename me to something like function names
#ifndef ZDSSMESS_H
#define ZDSSMESS_H

/*
*	Message Levels for individual methods (operation):
*
*	0:	No messages, including error (not guaranteed).  Highly discourage
*	1:	Critical (Error) Messages.  Discouraged.
*	2:	Minimal (terse) output:  zopen, zclose, critical errors.
*	3:	General Log Messages.  Default.
*	4:	Diagnostic User Messages (e.g., input parameters)
*	5:	Diagnostic Internal Messages level 1 (debug).   Not recommended for users
*	6:	Diagnostic Internal Messages level 2 (full debug)
*
*	Note:  Levels 1-3 are for Unicode.  Level 4+ is ASCII (hardwired)
*
*
*	Compatibility for version 6 MLVL with no method set (general)
*
*	0:	No messages, including error (not guaranteed).  Highly discourage
*	1:	Critical (Error) Messages.  Discouraged
*	2:	Minimal (terse) output:  zopen, zclose, critical errors.
*	3:	General Log Messages: zwrites.  Default.
*	4:	Log messages, including zread
*	10:	Diagnostic User Messages (e.g., input parameters)
*	12:	Diagnostic Internal Messages level 1 (debug).   Not recommended for users
*	15:	Diagnostic Internal Messages level 2 (full debug)
*
*
*	As long as an initializing function (e.g., zopen) has been called,
*	the correct way to set a message level is to include this header and call
*		void zsetMessageLevel(int group, int level);
*
*	For example:
*		zopen(...
*			...
*		zsetMessageLevel(MESS_METHOD_LOCKING_ID, MESS_LEVEL_INTERNAL_DIAG_1);
*
*	If an initializing call has not been made (this is the first call), call
*		zset("mess", functionName, level);
*
*	For example:
*		zset("mess", "locking", MESS_LEVEL_INTERNAL_DIAG_2);
*
*
*/

#define MESS_LEVEL_NONE				0
#define MESS_LEVEL_CRITICAL			1
#define MESS_LEVEL_TERSE			2
#define MESS_LEVEL_GENERAL			3
#define MESS_LEVEL_USER_DIAG		4
#define MESS_LEVEL_INTERNAL_DIAG_1	5
#define MESS_LEVEL_INTERNAL_DIAG_2	6


//  Methods refer to the general process of an operation,
//  whereas functions refer to the specific C function
//  Thus, methods are used for as input messaging and
//  functions are used for reporting and error processing

//  For example, show debug messages for tswrite will include
//  several functions for different types of time series data and
//  supporting functions, but each function has its own identity
//  that is used in reporting.
//  Method = general category message requests
//  function = specific function reported

#define NUMBER_METHOD_NAMES 18

struct hec_zmessaging {
	int methodLevel[NUMBER_METHOD_NAMES];
};

extern struct hec_zmessaging zmessaging;

//  Global inlcudes DSS-6; General doesn't
#define MESS_METHOD_GLOBAL_ID		0
#define MESS_METHOD_GLOBAL			"global"
#define MESS_METHOD_GENERAL_ID		1
#define MESS_METHOD_GENERAL			"general"
#define MESS_METHOD_GET_ID			2
#define MESS_METHOD_GET				"get"
#define MESS_METHOD_PUT_ID			3
#define MESS_METHOD_PUT				"put"
#define MESS_METHOD_READ_ID			4
#define MESS_METHOD_READ			"read"
#define MESS_METHOD_WRITE_ID		5
#define MESS_METHOD_WRITE			"write"
#define MESS_METHOD_PERM_ID			6
#define MESS_METHOD_PERM			"perm"
#define MESS_METHOD_OPEN_ID			7
#define MESS_METHOD_OPEN			"open"
#define MESS_METHOD_CHECK_ID		8
#define MESS_METHOD_CHECK			"check"
#define MESS_METHOD_LOCKING_ID		9
#define MESS_METHOD_LOCKING			"locking"
#define MESS_METHOD_TS_READ_ID		10
#define MESS_METHOD_TS_READ			"time_series_read"
#define MESS_METHOD_TS_WRITE_ID		11
#define MESS_METHOD_TS_WRITE		"time_series_write"
#define MESS_METHOD_ALIAS_ID		12
#define MESS_METHOD_ALIAS			"alias"
#define MESS_METHOD_COPY_ID			13
#define MESS_METHOD_COPY			"copy"
#define MESS_METHOD_UTILITY_ID		14
#define MESS_METHOD_UTILITY			"utility"
#define MESS_METHOD_CATALOG_ID		15
#define MESS_METHOD_CATALOG			"catalog"
#define MESS_METHOD_FILE_CHECK_ID	16
#define MESS_METHOD_FILE_CHECK		"file_check"
#define MESS_METHOD_JNI_ID			17
#define MESS_METHOD_JNI				"jni"
#define MESS_METHOD_UNDEFINED_ID	18



#define maxMessageAvail 10

struct hec_zmessageAvail {
	char *messages[maxMessageAvail];
	int messHandles[maxMessageAvail];
	int messLengths[maxMessageAvail];
	int numberMessages;
};

extern struct hec_zmessageAvail zmessageAvail;


//  Number functions is number + 1, so that zero () can be null and unused.

/*
	To add (or delete) a function:
		1.  Increment numberFunctionNames
		2.  Add an int with function name any where in struct DssFunction
		3.  Go to bottom of zinit, and in the same location (in DssFunction):
		4.  Copy icount line and stringCopy line
		5.  Change int function to your name (2 locations)
		6.  Change string function name to yours (1 location)

	For example, suppose you want to add function "zmyFunct" after zget:

	   numberFunctionNames++

	   int zget;
	   int zput;

	   int zget;
	   int zmyFunct;
	   int zput;
in zinit:
	   DSS_FUNCTION_zget = icount++;
	   stringCopy(DSS_FUNCTION_names[DSS_FUNCTION_zget], functionNamesLength, "zget", functionNamesLength);
goes to:
		DSS_FUNCTION_zget = icount++;
	    stringCopy(DSS_FUNCTION_names[DSS_FUNCTION_zget], functionNamesLength, "zget", functionNamesLength);
	    DSS_FUNCTION_zmyFunct = icount++;
	    stringCopy(DSS_FUNCTION_names[DSS_FUNCTION_zmyFunct], functionNamesLength, "zmyFunct", functionNamesLength);

*/

#define DSS_FUNCTION_zopen_ID			1
#define DSS_FUNCTION_zopen				"zopen"
#define DSS_FUNCTION_zclose_ID			2
#define DSS_FUNCTION_zclose				"zclose"
#define DSS_FUNCTION_zset_ID			3
#define DSS_FUNCTION_zset				"zset"
#define DSS_FUNCTION_zget_ID			4
#define DSS_FUNCTION_zget				"zget"
#define DSS_FUNCTION_zput_ID			5
#define DSS_FUNCTION_zput				"zput"
#define DSS_FUNCTION_zgetBuff_ID		6
#define DSS_FUNCTION_zgetBuff			"zgetBuff"
#define DSS_FUNCTION_zputBuff_ID		7
#define DSS_FUNCTION_zputBuff			"zputBuff"
#define DSS_FUNCTION_zmemoryGet_ID		8
#define DSS_FUNCTION_zmemoryGet			"zmemoryGet"
#define DSS_FUNCTION_zmemoryFree_ID		9
#define DSS_FUNCTION_zmemoryFree		"zmemoryFree"
#define DSS_FUNCTION_zreadDisk_ID		10
#define DSS_FUNCTION_zreadDisk			"zreadDisk"
#define DSS_FUNCTION_zwriteDisk_ID		11
#define DSS_FUNCTION_zwriteDisk			"zwriteDisk"
#define DSS_FUNCTION_zpermRead_ID		12
#define DSS_FUNCTION_zpermRead			"zpermRead"
#define DSS_FUNCTION_zpermCreate_ID		13
#define DSS_FUNCTION_zpermCreate		"zpermCreate"
#define DSS_FUNCTION_zpermWrite_ID		14
#define DSS_FUNCTION_zpermWrite			"zpermWrite"
#define DSS_FUNCTION_zwriteEOF_ID		15
#define DSS_FUNCTION_zwriteEOF			"zwriteEOF"
#define DSS_FUNCTION_zinit_ID			16
#define DSS_FUNCTION_zinit				"zinit"
#define DSS_FUNCTION_zinitIfltab_ID		17
#define DSS_FUNCTION_zinitIfltab		"zinitIfltab"
#define DSS_FUNCTION_zlocking_ID		18
#define DSS_FUNCTION_zlocking			"zlocking"
#define DSS_FUNCTION_znewFileSize_ID	19
#define DSS_FUNCTION_znewFileSize		"znewFileSize"
#define DSS_FUNCTION_zgetFileSpace_ID	20
#define DSS_FUNCTION_zgetFileSpace		"zgetFileSpace"
#define DSS_FUNCTION_zcheckKeys_ID		21
#define DSS_FUNCTION_zcheckKeys			"zcheckKeys"
#define DSS_FUNCTION_zflushToDisk_ID	22
#define DSS_FUNCTION_zflushToDisk		"zflushToDisk"
#define DSS_FUNCTION_zhash_ID			23
#define DSS_FUNCTION_zhash				"zhash"
#define DSS_FUNCTION_zcheck_ID			24
#define DSS_FUNCTION_zcheck				"zcheck"
#define DSS_FUNCTION_zreadInfo_ID		25
#define DSS_FUNCTION_zreadInfo			"zreadInfo"
#define DSS_FUNCTION_zcheckMultiUser_ID	26
#define DSS_FUNCTION_zcheckMultiUser	"zcheckMultiUser"
#define DSS_FUNCTION_zreadInternal_ID	27
#define DSS_FUNCTION_zreadInternal		"zreadInternal"
#define DSS_FUNCTION_zwriteInternal_ID	28
#define DSS_FUNCTION_zwriteInternal		"zwriteInternal"
#define DSS_FUNCTION_zwriteNew_ID		29
#define DSS_FUNCTION_zwriteNew			"zwriteNew"
#define DSS_FUNCTION_zwriteExisting_ID	30
#define DSS_FUNCTION_zwriteExisting		"zwriteExisting"
#define DSS_FUNCTION_zbinNew_ID			31
#define DSS_FUNCTION_zbinNew			"zbinNew"
#define DSS_FUNCTION_zbinUpdate_ID		32
#define DSS_FUNCTION_zbinUpdate			"zbinUpdate"
#define DSS_FUNCTION_zsetFile_ID		33
#define DSS_FUNCTION_zsetFile			"zsetFile"
#define DSS_FUNCTION_zquery_ID			34
#define DSS_FUNCTION_zquery				"zquery"
#define DSS_FUNCTION_zdelete_ID			35
#define DSS_FUNCTION_zdelete			"zdelete"
#define DSS_FUNCTION_zundelete_ID		36
#define DSS_FUNCTION_zundelete			"undelete"
#define DSS_FUNCTION_zcopyFile_ID		37
#define DSS_FUNCTION_zcopyFile			"zcopyFile"
#define DSS_FUNCTION_ztsStoreReg_ID		38
#define DSS_FUNCTION_ztsStoreReg		"ztsStoreReg"
#define DSS_FUNCTION_ztsRegStoreBlock_ID 39
#define DSS_FUNCTION_ztsRegStoreBlock	"ztsRegStoreBlock"
#define DSS_FUNCTION_ztsStore_ID		40
#define DSS_FUNCTION_ztsStore			"ztsStore"
#define DSS_FUNCTION_ztsRetrieve_ID		41
#define DSS_FUNCTION_ztsRetrieve		"ztsRetrieve"
#define DSS_FUNCTION_ztsRetrieveReg_ID	42
#define DSS_FUNCTION_ztsRetrieveReg		"ztsRetrieveReg"
#define DSS_FUNCTION_ztsRegReadBlock_ID	43
#define DSS_FUNCTION_ztsRegReadBlock	"ztsRegReadBlock"
#define DSS_FUNCTION_ztsWriteBlock_ID	44
#define DSS_FUNCTION_ztsWriteBlock		"ztsWriteBlock"
#define DSS_FUNCTION_ztsTrim_ID			45
#define DSS_FUNCTION_ztsTrim			"ztsTrim"
#define DSS_FUNCTION_ztsStoreIrreg_ID	46
#define DSS_FUNCTION_ztsStoreIrreg		"ztsStoreIrreg"
#define DSS_FUNCTION_ztsIrregReadBlock_ID	47
#define DSS_FUNCTION_ztsIrregReadBlock	"ztsIrregReadBlock"
#define DSS_FUNCTION_ztsIrregMergeBlocks_ID	48
#define DSS_FUNCTION_ztsIrregMergeBlocks "ztsIrregMergeBlocks"
#define DSS_FUNCTION_ztsRetrieveIrreg_ID	49
#define DSS_FUNCTION_ztsRetrieveIrreg	"ztsRetrieveIrreg"
#define DSS_FUNCTION_ztsIrregStoreBlock_ID	50
#define DSS_FUNCTION_ztsIrregStoreBlock	"ztsIrregStoreBlock"
#define DSS_FUNCTION_ztsProcessTimes_ID	51
#define DSS_FUNCTION_ztsProcessTimes	"ztsProcessTimes"
#define DSS_FUNCTION_zcatalog_ID		52
#define DSS_FUNCTION_zcatalog			"zcatalog"
#define DSS_FUNCTION_zpdStore_ID		53
#define DSS_FUNCTION_zpdStore			"zpdStore"
#define DSS_FUNCTION_zpdRetrieve_ID		54
#define DSS_FUNCTION_zpdRetrieve		"zpdRetrieve"
#define DSS_FUNCTION_zinquire_ID		55
#define DSS_FUNCTION_zinquire			"zinquire"
#define DSS_FUNCTION_zsqueeze_ID		56
#define DSS_FUNCTION_zsqueeze			"zsqueeze"
#define DSS_FUNCTION_ztextStore_ID		57
#define DSS_FUNCTION_ztextStore			"ztextStore"
#define DSS_FUNCTION_ztextRetrieve_ID	58
#define DSS_FUNCTION_ztextRetrieve		"ztextRetrieve"
#define DSS_FUNCTION_zcopyRecord_ID		59
#define DSS_FUNCTION_zcopyRecord		"zcopyRecord"
#define DSS_FUNCTION_zrename_ID			60
#define DSS_FUNCTION_zrename			"zrename"
#define DSS_FUNCTION_zduplicateRecord_ID	61
#define DSS_FUNCTION_zduplicateRecord	"zduplicateRecord"
#define DSS_FUNCTION_zarrayRetrieve_ID	62
#define DSS_FUNCTION_zarrayRetrieve		"zarrayRetrieve"
#define DSS_FUNCTION_zarrayStore_ID		63
#define DSS_FUNCTION_zarrayStore		"zarrayStore"
#define DSS_FUNCTION_zbinaryStore_ID	64
#define DSS_FUNCTION_zbinaryStore		"zbinaryStore"
#define DSS_FUNCTION_zbinaryRetrieve_ID	65
#define DSS_FUNCTION_zbinaryRetrieve	"zbinaryRetrieve"
#define DSS_FUNCTION_zcheckHashTable_ID	66
#define DSS_FUNCTION_zcheckHashTable	"zcheckHashTable"
#define DSS_FUNCTION_zcheckInfo_ID		67
#define DSS_FUNCTION_zcheckInfo			"zcheckInfo"
#define DSS_FUNCTION_zcheckLinks_ID		68
#define DSS_FUNCTION_zcheckLinks		"zcheckLinks"
#define DSS_FUNCTION_zcheckPathnameBins_ID	69
#define DSS_FUNCTION_zcheckPathnameBins	"zcheckPathnameBins"
#define DSS_FUNCTION_zcheckPathnames_ID	70
#define DSS_FUNCTION_zcheckPathnames	"zcheckPathnames"
#define DSS_FUNCTION_zspatialTinStore_ID	71
#define DSS_FUNCTION_zspatialTinStore	"zspatialTinStore"
#define DSS_FUNCTION_zspatialTinRetrieve_ID	72
#define DSS_FUNCTION_zspatialTinRetrieve	"zspatialTinRetrieve"

#define DSS_FUNCTION_zaliasAdd_ID		73
#define DSS_FUNCTION_zaliasAdd			"zaliasAdd"
#define DSS_FUNCTION_zaliasRemove_ID	74
#define DSS_FUNCTION_zaliasRemove		"zaliasRemove"
#define DSS_FUNCTION_zaliasUtil_ID		75
#define DSS_FUNCTION_zaliasUtil			"zaliasUtil"
#define DSS_FUNCTION_zlocationStore_ID	76
#define DSS_FUNCTION_zlocationStore		"zlocationStore"
#define DSS_FUNCTION_zlocationRetrieve_ID	77
#define DSS_FUNCTION_zlocationRetrieve	"zlocationRetrieve"
#define DSS_FUNCTION_zread_ID			78
#define DSS_FUNCTION_zread				"zread"
#define DSS_FUNCTION_zwrite_ID			79
#define DSS_FUNCTION_zwrite				"zwrite"
#define DSS_FUNCTION_internalUtility_ID	80
#define DSS_FUNCTION_internalUtility	"internal utility"
#define DSS_FUNCTION_ztsAggregate_ID	81
#define DSS_FUNCTION_ztsAggregate		"ztsAggregate"
#define DSS_FUNCTION_ztsDisaggregate_ID	82
#define DSS_FUNCTION_ztsDisaggregate	"ztsDisaggregate"


#define DSS_FUNCTION_zspatialGridStore_ID	83
#define DSS_FUNCTION_zspatialGridStore	"zspatialGridStore"
#define DSS_FUNCTION_zspatialGridRetrieve_ID	84
#define DSS_FUNCTION_zspatialGridRetrieve	"zspatialGridRetrieve"



#define DSS_FUNCTION_other_ID			85
#define DSS_FUNCTION_other				"other"

#define DSS_FUNCTION_javaNativeInterface_ID	86
#define DSS_FUNCTION_javaNativeInterface	"javaNativeInterface"





//  General User Log Messages

static int MESS_DEBUG_LEVEL = 5;

#define ZOPEN_EXISTING_MESS			"     -----DSS---zopen   Existing file opened,  File: "
#define ZOPEN_HANDLE_MESS			"                        Handle %d;  Process: %d;  DSS Versions - Software: %s, File:  %s"
#define ZOPEN_NEW_MESS				"     -----DSS---zopen   New file opened,  File: "
#define ZOPEN_NEW_HANDLE_MESS		"                        Handle %d;  Process: %d;  DSS Version:  %s"
#define ACCESS_READ_MESS			"                        Read access"
#define ACCESS_MULTI_USER_MESS		"                        Multi-user access"
#define ACCESS_ADVISORY_MESS		"                        Single-user advisory access mode"
#define ACCESS_EXCLUSIVE_MESS		"                        Exclusive access"
#define ZOPEN_REOPEN_MESS			"     -----DSS---zopen   Reopened for access change,  File: "
#define ZCLOSE_MESS					"     -----DSS---zclose  Handle %d;  Process: %d;  File: "
#define ZCLOSE_ERR1_MESS			"     -----DSS---zclose  File previously closed, Handle %d;  Process: %d;  File: "
#define ZCLOSE_ERR2_MESS			"     -----DSS---zclose  File not recognizable as a DSS-7 file"
#define ENTER_MULTI_USER_MESS		"-----DSS--- Entering multi-user access mode;  Handle %d"
#define GOT_MULTI_USER_MESS			"-----DSS--- Received file access;  Handle  %d"
#define WAIT_MULTI_USER_MESS		"-----DSS--- Waiting for file access (%d)...  Handle %d"
#define UNABLE_UNLOCK_MESS			"-----DSS--- Unable to unlock DSS file for multiple user access;  Handle  %d"
#define RESET_MULTI_USER_MESS		"-----DSS--- Reset from multi-user access mode to single-user advisory access mode;  Handle "
#define ZCHECK_FOUND				"-----DSS--- zcheck; Record found: "
#define ZCHECK_NOT_FOUND			"-----DSS--- zcheck; Record not found: "
#define ENTER_SINGLE_USER_MESS		"-----DSS--- Entering single-user advisory access mode;  Handle  %d"
#define DSS_PATHNAME				"-----DSS--- Pathname: "
#define ZDELETE_MESS				"-----DSS--- zdelete  Handle %d;  Pathname: "
#define ZDELETE_MESS_ERROR			"-----DSS--- zdelete  Record does not exist.  Handle %d;  Pathname: "
#define ZUNDELETE_MESS				"-----DSS--- zundelete  Record recovered.  Handle %d;  Pathname: "
#define ZUNDELETE_MESS_ERROR		"-----DSS--- zundelete  Unable to find deleted record.  Handle %d;  Pathname: "
#define ZWRITE_MESS					"-----DSS--- zwrite  Handle %d;  Version %d:  "
#define ZWRITE_MESS_ERROR			"*****DSS--- zwrite  Handle %d;  ERROR during write; status: %d:  "
#define ZREAD_MESS					"-----DSS--- zread   Handle %d;  Version %d:  "
#define ZREAD_ERROR					"*****DSS--- zread   Handle %d;  ERROR in Read; status: %d"
#define ZREAD_NOT_EXIST				"-----DSS--- zread   Record does not exist.  Handle %d;  Pathname: "
#define ZREAD_WRONG_TYPE			"-----DSS--- zread   Wrong data type.  Handle %d;  Pathname: "
#define ZCATALOG_NEW 				"-----DSS--- zcatalog  New catalog requested.  Handle %d "
#define ZRENAME_NOT_EXIST			"-----DSS--- zrename;  Record does not exist:  "
#define ZRENAME_ALREADY_EXIST		"-----DSS--- zrename;  New record already exists: "
#define ZRENAME_OLD_PATH			"-----DSS--- zrename:  Old Pathname: "
#define ZRENAME_NEW_PATH			"                      New Pathname: "


//  File info messages (printed at zclose)
#define FILE_INFO_MESS_01			"                        Number records:         %lld"
#define FILE_INFO_MESS_02			"                        File size:              %lld  64-bit words"
#define FILE_INFO_MESS_03			"                        File size:              %lld Kb;  %lld Mb"
#define FILE_INFO_MESS_04			"                        Dead space:             %lld"
#define FILE_INFO_MESS_05			"                        Hash range:             %lld"
#define FILE_INFO_MESS_06			"                        Number hash used:       %lld"
#define FILE_INFO_MESS_07			"                        Max paths for hash:     %lld"
#define FILE_INFO_MESS_08			"                        Corresponding hash:     %lld"
#define FILE_INFO_MESS_09			"                        Number non unique hash: %lld"
#define FILE_INFO_MESS_10			"                        Number bins used:       %lld"
#define FILE_INFO_MESS_11			"                        Number overflow bins:   %lld"
#define FILE_INFO_MESS_12			"                        Number physical reads:  %lld"
#define FILE_INFO_MESS_13			"                        Number physical writes: %lld"
#define FILE_INFO_MESS_14			"                        Number denied locks:    %lld"



#endif //  ZDSSMESS_H

