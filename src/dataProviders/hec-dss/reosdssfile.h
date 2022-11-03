/***************************************************************************
  reosdssfile.h - ReosDssFile

 ---------------------
 begin                : 11.10.2022
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
#ifndef REOSDSSFILE_H
#define REOSDSSFILE_H

#include <memory>
#include <array>
#include <QString>
#include <QDateTime>

#include <reosduration.h>
#include "reosdssutils.h"

class ReosHydrograph;

class REOSDSS_EXPORT ReosDssPath
{
  public:
    enum Part
    {
      Group,
      Location,
      Parameter,
      StartDate,
      TimeInterval,
      Version
    };

    ReosDssPath();
    explicit ReosDssPath( const QString &path );

    char *c_string( Part part );
    const char *const_c_string( Part part ) const;
    size_t size( Part part ) const;

    bool isValid() const;

    const QString string() const;
    const char *c_pathString() const;


    const QString group() const;
    void setGroup( const QString &newGroup );
    const QString location() const;
    void setLocation( const QString &newLocation );
    const QString parameter() const;
    void setParameter( const QString &newParameter );
    const QString startDate() const;
    void setStartDate( const QString &newStartDate );
    const QString timeInterval() const;
    void setTimeInterval( const ReosDuration &interval );
    void setTimeInterval( const QString &newTimeInterval );
    const QString version() const;
    void setVersion( const QString &newVersion );

    ReosDuration timeIntervalDuration() const;

  private:
    bool mIsValid = false;

    QVector<QVector<char>> mData;
    std::vector<char> mDataGroup;

    QString toQString( Part part ) const;
    void stringToData( const QString &str, Part part );

    mutable std::string mTempPathString;

};

class REOSDSS_EXPORT ReosDssFile
{
  public:

    ReosDssFile( const QString &filePath, bool create = false );
    ReosDssFile( const ReosDssFile &other ) = delete;
    ReosDssFile( ReosDssFile &&other );
    ~ReosDssFile();

    ReosDssFile &operator=( const ReosDssFile &other ) = delete;
    ReosDssFile &operator=( ReosDssFile &&other );

    bool isValid() const;
    bool isOpen() const;

    bool pathExist( const ReosDssPath &path ) const;
    bool hasData( const ReosDssPath &path ) const;

    QDateTime referenceTime( const ReosDssPath &path ) const;
    QVector<double> values( const ReosDssPath &path ) const;

    //! Create a new series with constant time interval with \a path in the file
    bool createConstantIntervalSeries( const ReosDssPath &path, QString &error );

    //! Write constant interval series on the file
    bool writeConstantIntervalSeries( const ReosDssPath &path,
                                      const QDateTime &startTime,
                                      const ReosDuration &timeStep,
                                      const QVector<double> &values,
                                      QString &error );

    QList<ReosDssPath> searchRecordsPath( const ReosDssPath &path ) const;

  private:
    std::unique_ptr<std::array<long long, 250>> mIfltab;
    QString mFileName;
    int mStatus = -1;
    bool mIsValid = false;
    bool mIsOpen = false;

    static QString getEPart( const ReosDuration &interval, bool findClosest = false );

    void removeDataset( const ReosDssPath &path );
    bool writeConstantIntervalSeriesPrivate(
      const ReosDssPath &path,
      const QDateTime &startTime,
      const ReosDuration &timeStep,
      const QVector<double> &values,
      QString &error );

    friend class ReosHecrasTesting;
};

#endif // REOSDSSFILE_H
