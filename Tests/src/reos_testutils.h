#ifndef REOS_TESTUTILS_H
#define REOS_TESTUTILS_H

#include <string>

const char *data_path();

std::string test_file( std::string basename );

std::string tmp_file( std::string basename );

#endif // REOS_TESTUTILS_H
