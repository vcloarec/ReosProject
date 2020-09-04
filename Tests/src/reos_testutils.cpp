#include "reos_testutils.h"

const char *data_path()
{
  return TESTDATA;
}

std::string test_file( std::string basename )
{
  std::string path( data_path() );
  path += basename;
  return path;
}

std::string tmp_file( std::string basename )
{
  std::string path( data_path() + std::string( "/tmp" ) );
  path += basename;
  return path;
}
