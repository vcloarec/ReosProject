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

#define SIP_NO_FILE

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

    bool hasVariableByLongName( const QString &variableName );

    int variableDimensionCount( const QString &variableName ) const;

    int dimensionLength( const QString &dimensionName ) const;

    QStringList variableDimensionNames( const QString &variableName ) const;

    double globalDoubleAttributeValue( const QString &attribureName ) const;
    QString globalStringAttributeValue( const QString &attribureName ) const;

    double doubleAttributeValue( const QString &variableName, const QString &attributeName ) const;
    qint16 shortAttributeValue( const QString &variableName, const QString &attributeName ) const;

    QVector<qint64> getInt64Array( const QString &variableName, int size );

    QVector<int> getIntArray( const QString &variableName, int size ) const;
    QVector<int> getIntArray( const QString &variableName, const QVector<int> &starts, const QVector<int> &counts ) const ;

    QVector<double > getDoubleArray( const QString &variableName, int size );
    QVector<double > getDoubleArray( const QString &variableName, const QVector<int> &starts, const QVector<int> &counts ) const ;


    QVector<qint16 > getShortArray( const QString &variableName, const QVector<int> &starts, const QVector<int> &counts )const;

  private:
    bool mIsValid = false;
    int mNcId = -1;
    int mDimCount = -1;
    int mVarCount = -1;
    int mGlobalAttCOunt = -1;
    int mUnlimitDimId = -1;
    QMap<QString, int> mVarNameToVarId;
    QMap<QString, int> mVarLongNameToVarId;
};

#endif // REOSNETCDFUTILS_H
