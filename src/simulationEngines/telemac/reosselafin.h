/***************************************************************************
  reosselafin.h - ReosSelafin

 ---------------------
 begin                : 5.10.2026
 copyright            : (C) 2026 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/


#ifndef REOSSELAFIN_H
#define REOSSELAFIN_H

#include <QString>

class ReosMesh;

class ReosSelafin
{
  public:
    ReosSelafin( const QString &filePath );

    bool createMeshFrame( const ReosMesh *mesh, QList<int> vertivalPosInBoundary ) const;
    ReosMesh *loadMeshFrame( QList<int> &vertivalPosInBoundary ) const;

  private:
    QString mFilePath;
};

#endif // REOSSELAFIN_H
