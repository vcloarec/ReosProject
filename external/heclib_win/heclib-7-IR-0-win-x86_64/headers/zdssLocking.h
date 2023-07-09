#ifndef ZDSSLOCKING_H
#define ZDSSLOCKING_H

static int LOCKING_LOCK_OFF = 0;
static int LOCKING_LOCK_ON  = 1;
static int LOCKING_LOCK_TEST = 2;
static int LOCKING_LOCK_NUMBER = 3;

static int LOCKING_FLUSH_OFF = 0;
static int LOCKING_FLUSH_ON = 1;

static int LOCKING_ACCESS_READ = 0;
static int LOCKING_ACCESS_WRITE = 1;

static int LOCKING_LEVEL_UNDEFINED  = 0;
static int LOCKING_LEVEL_LOW = 1;			//   zwritex
static int LOCKING_LEVEL_MID = 2;			//	 zrename, zalias
static int LOCKING_LEVEL_HIGH = 3;			//   zstoreRTS
static int LOCKING_LEVEL_SUPER = 4;			//   zcopyFile


//(long long *ifltab, int level, int lock, int flush)


#endif //  ZDSSLOCKING_H

