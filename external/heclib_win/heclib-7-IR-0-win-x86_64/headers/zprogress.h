//  Progress info struct
//  Variables to communicate to java the progress of
//  a long operation, such as copying a file
//  To stop the operation, set interrupt to 1, handle to correct handle
//
//  This is not thread safe, however, since it is only informative
//  rare that two process would use at the same time, and the only
//  issue would be a temporary situation where the progress
//  would not be communicated to the processes, we won't worry about it.

struct {
	 int handle;
	 int interrupt;
	 int totalNumber;
	 int currentNumber;
	 int numberErrors;
	 int maxErrors;
} zprogress;

void zresetProgress(int handle, long long total);


