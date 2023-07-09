#ifndef ZERROR_CODES_H
#define ZERROR_CODES_H

#include "zdssVals.h"

//  The list of all error messages that may occurr
//  (outside of system error messages)

//  Meant to be modified for other languages

static const char *ERROR_PREAMBLE		= "*****DSS*** ERROR in function %s: ";
static const char *ERROR_AT_ADDRESS		= "Address %lld";
static const char *ERROR_IN_FILE		= "File ";
static const char *ERROR_IN_FUNCTION	= "In function ";
static const char *ERROR_CALLED_BY		= "Called by function ";
static const char *ERROR_PATHNAME		= "Pathname: ";
static const char *ERROR_LAST_PATH		= "Last pathname accessed: ";
static const char *INFORMATION_PREAMBLE	= "-----DSS--- %s  Notice: ";
static const char *WARNING_PREAMBLE		= "-----DSS--- %s  Warning: ";


#define NUMBER_ERROR_MESSAGES 67
static const char *errorMess[NUMBER_ERROR_MESSAGES] = {
	/* STATUS_OKAY */					"",
	/* INVALID_FILE_VERSION */			"Incompatible DSS file versions\nThis library is only compatible with version 7 DSS files\nThis file is version %s",
	/* INCOMPATIBLE_VERSION */			"Incompatible DSS version\nThis function is only compatible with version 7 DSS files\nThis file is version %d",
	/* INCOMPATIBLE_VERSION_6 */		"Incompatible DSS version\nThis function is only compatible with version 6 DSS files\nThis file is version %d",
	/* INVALID_FILE_NAME */				"Invalid DSS file name\nFile ",
	/* NO_EXCLUSIVE_ACCESS */			"Unable to exclusively access file\nFile ",
	/* UNABLE_TO_ACCESS_FILE */			"Unable to access file\nFile ",
	/* UNABLE_TO_WRITE_FILE */			"Unable to obtain write access to file\nFile ",
	/* UNABLE_TO_CREATE_FILE */			"Unable to create file\nFile ",
	/* NO_WRITE_PERMISSION */			"No write permission for file\nFile ",
	/* NO_PERMISSION */					"No permission to access file\nFile ",
	/* WRITE_ON_READ_ONLY */			"Write attempt on file without write access (read-only mode).\n",
	/* INVALID_DSS_FILE */				"Not a DSS file\nDSS Identifier ==>%s<==; Should be ==>ZDSS<==",
	/* INVALID_ADDRESS */				"Attempted to access an invalid address: %lld",
	/* INVALID_NUMBER_TO_READ */		"An invalid number of data has been requested to be read: %lld",
	/* INVALID_NUMBER_TO_WRITE */		"An invalid number of data has been requested to be written: %lld",
	/* WRITE_ERROR */					"An error occurred during a write attempt, status: %d",
	/* READ_ERROR */					"An error occurred during a read attempt, status: %d",
	/* READ_BEYOND_EOF */				"Attempt to read beyond end of file by %d bytes",
	/* INVALID_FILE_HEADER */			"The DSS file main header is damaged. \nEnd of perm flag does not match:  %lld is not %lld\nSqueeze the file to attempt recovery.",
	/* TRUNCATED_FILE */				"The DSS file is incomplete (truncated).\nBytes read: %d from address %lld\nSqueeze the file to attempt recovery.",
	/* INVALID_HEADER_PARAMETER */		"The DSS file main header is damaged. \nAn invalid number was read from the file: %lld\nSqueeze the file to attempt recovery.",
	/* DAMAGED_FILE */					"The DSS file is damaged.  Status: %d, detected at address: %lld\nSqueeze the file to attempt recovery.",
	/* CLOSED_FILE */					"The DSS file has been closed and must be reopened prior to use",
	/* EMPTY_FILE */					"The DSS file has no records or data in it; it is an empty file",
	/* IFLTAB_CORRUPT */				"The primary table in memory has become corrupt (probably due to a program memory error)",
	/* KEY_CORRUPT */					"The primary table keys in memory no longer match,\nindicating that the table has become corrupt (probably due to a program memory error)",
	/* KEY3_LOCATION */					"The location pointer for key 3 is outside bounds (> 500).  It is ==>%d<==",
	/* KEY_VALUE */						"The value of key %d is ==>%lld<==,  It should be ==>%lld<==",
	/* CANNOT_LOCK_FILE */				"Unable to lock file",
	/* CANNOT_LOCK_EXCLUSIVE */			"Unable to lock file for Exclusive access",
	/* CANNOT_LOCK_MULTI_USER */		"Unable to lock file for multiple user access\nUnable to obtain a lock after %d attempts.",
	/* CANNOT_SQUEEZE */				"Unable to squeeze file\n%s",
	/* INVALID_BIN_STATUS */			"Invalid status read in pathname bin: %d; possible corruption in file (squeeze to resolve).",
	/* CANNOT_ALLOCATE_MEMORY */		"Cannot allocate memory, size requested: %d",
	/* INVALID_PARAMETER */				"Invalid or unrecognized parameter: %s (ignored)",
	/* INVALID_NUMBER */				"Invalid or unrecognized number: %d, for %s (ignored)",
	/* INCOMPATIBLE_CALL */				"An invalid or incompatible call or function access has been made.  (unspecified error)",
	/* NON_EMPTY_FILE */				"Cannot set parameter for a non-empty file.  Parameter: %s, number of records in file: %d",
	/* BIN_SIZE_CONFLICT */				"Bin space used is larger than bin size, error at address %lld\nSpace: %d, allocated: %d\n %s",
	/* DIFFERENT_RECORD_TYPE */			"Cannot write to a record with a different record type.\nType being written: %d,   Type stored: %lld\n %s",
	/* WRONG_RECORD_TYPE */				"Wrong record type for this function.\nType expected: %d,   Type stored: %lld\n %s",
	/* NO_UNDELETE_WITH_RECLAIM */		"Unable to undelete in a DSS file that uses space reclamation.",
	/* ARRAY_SPACE_EXHAUSTED */			"Array space is exhausted.  Length allocated: %d, current position: %d,  %s\n",
	/* BOTH_NOTE_KINDS_USED */			"Cannot store both character notes and integer notes in same data set.",
	/* NO_DATA_GIVEN */					"No data given to store.",
	/* NO_DATA_READ */					"No data read.  Pathname: %s",
	/* NO_TIME_WINDOW */				"No time window given, or invalid date or time specified.",
	/* INVALID_DATE_TIME */				"Invalid date or time given.",
	/* INVALID_INTERVAL */				"Invalid time interval.\nInterval type: %d",
	/* TIMES_NOT_ASCENDING */			"Times are not ascending.  Irregular-interval data must have ordered ascending times.",
	/* DIFFERENT_PROFILE_NUMBER */		"Cannot store a different number of profile depths.\nNumber ask to store: %d,  Number stored:%d",
	/* RECORD_DOES_NOT_EXIST */			"Record does not exist\npathname: ",
	/* RECORD_ALREADY_EXISTS */			"Record already exists\npathname: ",
	/* INVALID_PATHNAME */				"Invalid pathname\npathname: ",
	/* INVALID_RECORD_HEADER */			"Invalid internal header for this data type; data type: %d.",
	/* ARRAY_TOO_SMALL */				"Space needed is more than that allocated.  Needed: %d,  Allocated: %d",
	/* NOT_OPENED */					"HEC-DSS file has not been opened or ifltab is corrupted.\nThis function requires the file to be opened.",
	/* FILE_DOES_NOT_EXIST */			"HEC-DSS file does not exist.  It must exist for this function.\nFile name: %s",
	/* FILE_EXISTS */					"HEC-DSS file already exists.  It should not exist for this function.\nFile name: %s",
	/* NULL_FILENAME */					"NULL File name.  No file name was provided or a null pointer was given instead of a vaild file name.",
	/* NULL_PATHNAME */					"NULL pathame.  No pathame was provided or a null pointer was given instead of a vaild pathame.",
	/* NULL_ARGUMENT */					"NULL argument passed.  A null pointer was given instead of a argument.",
	/* NULL_ARRAY */					"NULL pointer / array.  A null pointer was given instead of an expected array.",
	/* VERTICAL_DATUM_ERROR */			"Cannot perform vertical datum conversion.",
	/* INVALID_F_PART_TAGS*/            "Cannot normalize F part tags. Error is %s",



	/* UNDEFINED_ERROR */				"Undefined error",
};

static struct {
		int STATUS_OK;
		int INVALID_FILE_VERSION;
		int INCOMPATIBLE_VERSION;
		int INCOMPATIBLE_VERSION_6;
		int INVALID_FILE_NAME;
		int NO_EXCLUSIVE_ACCESS;
		int UNABLE_TO_ACCESS_FILE;
		int UNABLE_TO_WRITE_FILE;
		int UNABLE_TO_CREATE_FILE;
		int NO_WRITE_PERMISSION;
		int NO_PERMISSION;
		int WRITE_ON_READ_ONLY;
		int INVALID_DSS_FILE;
		int INVALID_ADDRESS;
		int INVALID_NUMBER_TO_READ;
		int INVALID_NUMBER_TO_WRITE;
		int WRITE_ERROR;
		int READ_ERROR;
		int READ_BEYOND_EOF;
		int INVALID_FILE_HEADER;
		int TRUNCATED_FILE;
		int INVALID_HEADER_PARAMETER;
		int DAMAGED_FILE;
		int CLOSED_FILE;
		int EMPTY_FILE;
		int IFLTAB_CORRUPT;
		int KEY_CORRUPT;
		int KEY_VALUE;
		int KEY3_LOCATION;
		int CANNOT_LOCK_FILE;
		int CANNOT_LOCK_EXCLUSIVE;
		int CANNOT_LOCK_MULTI_USER;
		int CANNOT_SQUEEZE;
		int INVALID_BIN_STATUS;
		int CANNOT_ALLOCATE_MEMORY;
		int INVALID_PARAMETER;
		int INVALID_NUMBER;
		int INCOMPATIBLE_CALL;
		int NON_EMPTY_FILE;
		int BIN_SIZE_CONFLICT;
		int DIFFERENT_RECORD_TYPE;
		int WRONG_RECORD_TYPE;
		int NO_UNDELETE_WITH_RECLAIM;
		int ARRAY_SPACE_EXHAUSTED;
		int BOTH_NOTE_KINDS_USED;
		int NO_DATA_GIVEN;
		int NO_DATA_READ;
		int NO_TIME_WINDOW;
		int INVALID_DATE_TIME;
		int INVALID_INTERVAL;
		int TIMES_NOT_ASCENDING;
		int DIFFERENT_PROFILE_NUMBER;
		int RECORD_DOES_NOT_EXIST;
		int RECORD_ALREADY_EXISTS;
		int INVALID_PATHNAME;
		int INVALID_RECORD_HEADER;
		int ARRAY_TOO_SMALL;
		int NOT_OPENED;
		int FILE_DOES_NOT_EXIST;
		int FILE_EXISTS;
		int NULL_FILENAME;
		int NULL_PATHNAME;
		int NULL_ARGUMENT;
		int NULL_ARRAY;
		int VERTICAL_DATUM_ERROR;
		int INVALID_F_PART_TAGS;
		int UNDEFINED_ERROR;
} zdssErrorCodes = {
	0,
	1,
	2,
	3,
	4,
	5,
	6,
	7,
	8,
	9,
	10,
	11,
	12,
	13,
	14,
	15,
	16,
	17,
	18,
	19,
	20,
	21,
	22,
	23,
	24,
	25,
	26,
	27,
	28,
	29,
	30,
	31,
	32,
	33,
	34,
	35,
	36,
	37,
	38,
	39,
	40,
	41,
	42,
	43,
	44,
	45,
	46,
	47,
	48,
	49,
	50,
	51,
	52,
	53,
	54,
	55,
	56,
	57,
	58,
	59,
	60,
	61,
	62,
	63,
	64,
	65,
	66
};

static struct {
		int INFORMATION;
		int WARNING;
		int INVALID_ARGUMENT;
		int WARNING_NO_WRITE_ACCESS;
		int WARNING_NO_FILE_ACCESS;
		int WRITE_ERROR;
		int READ_ERROR;
		int CORRUPT_FILE;
		int MEMORY_ERROR;  //  This and the next should stop the program or server
} zdssErrorSeverity = {
	1,
	2,
	3,
	4,
	5,
	6,
	7,
	8,
	9
};

/*	errorType indicates what kind of error occurred and what should be done about it:
		0 - None
		1 - Warning or Informational (do nothing)
		2 - Access error (file does not exist, permission problem, ...)  (User needs to fix.)
		3 - File error (broken file, junk in file)  (Squeeze file)
		4 - Memory error or memory exhausted.   Exit program without saving anything.
*/

#define ERROR_TYPE_NONE		0
#define ERROR_TYPE_WARNING	1
#define ERROR_TYPE_ACCESS	2
#define ERROR_TYPE_FILE		3
#define ERROR_TYPE_MEMORY	4


typedef struct {
	int errorCode;
	int severity;
	int errorNumber;
	int errorType;
	int systemError;
	long long lastAddress;
	int functionID;
	int calledByFunction;
	char errorMessage[MAX_LEN_ERROR_MESS];
	char systemErrorMessage[MAX_LEN_ERROR_MESS];
	char lastPathname[MAX_PATHNAME_SIZE];
	char filename[MAX_FILENAME_LENGTH];
} hec_zdssLastError;

int zerror(hec_zdssLastError *errorStruct);

#endif //  ZERROR_CODES_H

