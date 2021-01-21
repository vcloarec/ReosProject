/***************************************************************************
                      reos_testutils.cpp
                     --------------------------------------
Date                 : 04-09-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reos_testutils.h"
#include "reosprocess.h"

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

ModuleProcessControler::ModuleProcessControler( ReosProcess *process ): mProcess( process )
{
  QObject::connect( process, &ReosProcess::finished, this, &ModuleProcessControler::processFinished );
  process->startOnOtherThread();
}

void ModuleProcessControler::waitForFinished()
{
  if ( mProcess && !mProcess->isFinished() )
    mEventLoop.exec();
}

void ModuleProcessControler::processFinished()
{
  mEventLoop.exit();
}
