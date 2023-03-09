/***************************************************************************
  reosnetcdfutils.h - ReosNetCdfUtils

 ---------------------
 begin                : 25.12.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSNETCDFUTILS_H
#define REOSNETCDFUTILS_H

#include <QString>
#include<QMap>
#include "reoscore.h"

class REOSCORE_EXPORT ReosNetCdfFile
{
  public:
    explicit ReosNetCdfFile( const QString &fileName, bool write = false );
    ~ReosNetCdfFile();

    bool isValid() const;
    bool hasVariable( const QString &variableName );

    int variableDimensionCount( const QString &variableName ) const;

    int dimensionLength( const QString &dimensionName ) const;

    QStringList variableDimensionNames( const QString &variableName ) const;

    double globalDoubleAttributeValue( const QString &attribureName ) const;
    QString globalStringAttributeValue( const QString &attribureName ) const;

    QVector<qint64> getInt64Array( const QString &variableName, int size );

    QVector<int> getIntArray(const QString &variableName, const QVector<int> &starts, const QVector<int> &count );

  private:
    bool mIsValid = false;
    int mNcId = -1;
    int mDimCount = -1;
    int mVarCount = -1;
    int mGlobalAttCOunt = -1;
    int mUnlimitDimId = -1;
    QMap<QString, int> mVarNameToVarId;
};

#endif // REOSNETCDFUTILS_H
