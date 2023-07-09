#ifndef HECLIB7_H
#define HECLIB7_H

//  Utility and DSS functions for DSS Version 7 library (only)
//  For the normal combined library, use heclib.h

#include "hecdss7.h"
#include "heclibDate.h"


//  Character manipulation and similar functions
int findInt(const char *string, int *index, int *numberDigits);
void lowerCase(char *string);
void upperCase(char *string);
int longWithCommas(char *str, size_t strSize, long long number);

char *getFileFromPath (char *filename, size_t sizeOfFilename, char *fullpath);
int stringCopy (char *destination, size_t sizeOfDestination, const char* source, size_t lenSource);
size_t trimLength(const char *string);
size_t trimLengthLen(const char *string, size_t len);





#endif //  HECLIB7_H

