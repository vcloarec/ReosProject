#ifndef HECLIB_DATE_H
#define HECLIB_DATE_H

#include <stddef.h>

#define UNDEFINED_TIME -2147483647
#define JULIAN_BASE_DATE 693960

#ifndef _TRUNCATE
#define _TRUNCATE ((size_t)-1)
#endif




//  Time and Date functions

long long getCurrentTimeMillis();
void getCurrentDateTime (int *julian, int *secondsPastMidnight, int *millsPastSecond);
void getCurrentTimeString (char timeString[13], size_t lenTimeString);
void getCurrentDateString (char *dateString, size_t sizeofDateString);
int getDateAndTime(int timeMinOrSec, int timeGranularitySeconds, int julianBaseDate,
				   char *dateString, int sizeOfDateString, char* hoursMins, int sizeofHoursMins);
void getDateTimeString (int julian, char *dateString, size_t sizeofDateString, int dateStyle,
					    int secondsPastMidnight, char *timeString, size_t sizeofTimeString, int timeStyle);
int julianToDate(int julianDate, int style, char *dateString, size_t sizeofDateString);
int julianToYearMonthDay (int julian, int *year, int *month, int *day);
int yearMonthDayToDate(int year, int month, int day, int style, char *dateString, size_t lenDateString);
int yearMonthDayToJulian (int year, int month, int day);
void millsToDateTime(long long mills, char *dateString, char *timeString, size_t sizeofDateString, size_t sizeofTimeString);
void minsToDateTime(int minsSince1900, char *dateString, char *timeString, size_t sizeofDateString, size_t sizeofTimeString);
void minutesToHourMin(int minutes, char *hoursMins, size_t lenHoursMins);
void secondsToTimeString(int secondsPastMidnight, int millsPastSecond, int timeStyle,
						 char *timeString, size_t sizeofTimeString);
int timeStringToSeconds(const char *timeString);
float timeStringToSecondsMills(const char *timeString);
int dayOfWeek(int julian);

int incrementTime(int intervalSeconds, int numberPeriods,
				  int julianStart, int secondsStart,
				  int *julianEnd, int *secondsEnd);
int numberPeriods(int intervalSeconds,
				  int julianStart, int secondsStart,
				  int julianEnd, int secondsEnd);
int addCentury (int year);
int isLeapYear (int year);
int cleanTime(int *julianDate, int *timeMinSec, int timeGranularitySeconds);
int dateToYearMonthDay(const char *dateString, int *year, int *month, int *day);
int dateToJulian(const char *dateString);
int isTimeDefined(int julianDate, int timeSeconds);
void printCurrentTime(int lineFeed);
int compareTimes(int julianFirst, int timeFirst, int julianBaseFirst, int timeGranularitySecondsFirst,
	int julianSecond, int timeSecond, int julianBaseSecond, int timeGranularitySecondsSecond);

int spatialDateTime(char *dateTimeString, int *julian, int *seconds);
int stringCat(char *destination, size_t sizeOfDestination, const char* source, size_t lenSource);
int strnlen_hec(const char* str, int number);

//  Misc private functions needed by date routines
void intTo2char(int i, char *c);
void intTo4char(int i, char *c);
void itoa_hec(int num, char* str, int strLength, int base);

static const char LONG_MONTH_NAMES[12][10] = {
	"January",
	"February",
	"March",
	"April",
	"May",
	"June",
	"July",
	"August",
	"September",
	"October",
	"November",
	"December"};



#endif	/* HECLIB_DATE_H */

