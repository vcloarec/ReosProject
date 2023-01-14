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
class ReosGriddedRainfall;
class ReosMapExtent;
class ReosRasterExtent;

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

    bool isEquivalent( const ReosDssPath &other ) const;

  private:
    bool mIsValid = false;
    QVector<QVector<char>> mData;

    QString toQString( Part part ) const;
    void stringToData( const QString &str, Part part );

    mutable std::string mTempPathString;

};

class REOSDSS_EXPORT ReosDssFile
{
  public:

    explicit ReosDssFile( const QString &filePath, bool create = false );
    ReosDssFile( const ReosDssFile &other ) = delete;
    ReosDssFile( ReosDssFile &&other );
    ~ReosDssFile();

    ReosDssFile &operator=( const ReosDssFile &other ) = delete;
    ReosDssFile &operator=( ReosDssFile &&other );

    bool isValid() const;
    bool isOpen() const;

    void open();
    void close();

    bool pathExist( const ReosDssPath &path, bool considerInterval ) const;

    bool getSeries( const ReosDssPath &path, QVector<double> &values, ReosDuration &timeStep, QDateTime &startTime ) const;

    virtual ReosRasterExtent gridExtent( const ReosDssPath &path ) const;

    virtual QVector<double> gridValues( const ReosDssPath &path, int &xCount, int &yCount ) const;

    //! Create a new series with constant time interval with \a path in the file
    bool createConstantIntervalSeries( const ReosDssPath &path, QString &error );

    //! Write constant interval series on the file
    bool writeConstantIntervalSeries( const ReosDssPath &path,
                                      const QDateTime &startTime,
                                      const ReosDuration &timeStep,
                                      const QVector<double> &values,
                                      QString &error );

    bool writeGriddedData( ReosGriddedRainfall *griddedrainFall,
                           const ReosDssPath &path,
                           const ReosMapExtent &destination,
                           double resolution = -1 );

    QList<ReosDssPath> searchRecordsPath( const ReosDssPath &path, bool considerInterval ) const;

    QList<ReosDssPath> allPathes() const;

    struct RecordInfo
    {
      int recordType = -1;
      int version = 0;
    };

    RecordInfo recordInformation( const ReosDssPath &path ) const;

  private:
    std::unique_ptr<std::array<long long, 250>> mIfltab;
    QString mFileName;
    int mStatus = -1;
    bool mIsValid = false;
    bool mIsOpen = false;

    static QString getEPart( const ReosDuration &interval, bool findClosest = false );
    ReosDssPath firstFullPath( const ReosDssPath &path, bool considerInterval ) const;
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
