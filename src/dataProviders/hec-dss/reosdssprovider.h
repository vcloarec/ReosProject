/***************************************************************************
  reosdssprovider.h - ReosDssProvider

 ---------------------
 begin                : 17.10.2022
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
#ifndef REOSDSSPROVIDER_H
#define REOSDSSPROVIDER_H

#include "reosdssfile.h"
#include "reostimeserieprovider.h"
#include "reosdssfile.h"


class ReosDssProviderBase
{
  public:
    ReosDssProviderBase();
    virtual ~ReosDssProviderBase();

    virtual bool createNewSerie( const ReosDssPath &path, ReosDssFile &dssFile, QString &error ) const = 0;

    static QString staticKey();

    static QString fileNameFromUri( const QString &uri );
    static ReosDssPath dssPathFromUri( const QString &uri );
    static QString uri( const QString &filePath, const ReosDssPath &dssPath );

    ReosDuration timeStepFromUri( const QString &uri );

  protected:
    std::unique_ptr<ReosDssFile> mFile;

  private:
};

class ReosDssProviderTimeSerieConstantTimeStep : public ReosTimeSerieConstantTimeStepProvider, public ReosDssProviderBase
{
  public:
    QString key() const override;

    void load() override;

    QDateTime referenceTime() const override;
    QString valueUnit() const {return QString();}
    int valueCount() const override;
    double value( int i ) const override;
    double firstValue() const override ;
    double lastValue() const override;
    double *data() override;
    const QVector<double> &constData() const override;
    ReosDuration timeStep() const override;

    bool isTimeStepCompatible( const ReosDuration &timeStep ) const override;

    void setReferenceTime( const QDateTime &referenceTime ) override;
    void setTimeStep( const ReosDuration &timeStep ) override;
    void resize( int size ) override;
    void appendValue( double value ) override;
    void prependValue( double value ) override;
    void insertValue( int pos, double value ) override;
    void setValue( int index, double value ) override;
    void removeValues( int from, int count ) override;
    void clear() override;

    ReosEncodedElement encode() const {return ReosEncodedElement();}
    void decode( const ReosEncodedElement &element ) {}

    bool persistData( QString &error ) override;
    bool createNewSerie( const ReosDssPath &path, ReosDssFile &dssFile, QString &error ) const override;

    static QString dataType();

  private:
    QDateTime mReferenceTime;
    ReosDuration mTimeStep;
    QVector<double> mValues;
    bool mDirty = false;
};

class ReosDssProviderTimeSerieVariableTimeStep : public ReosTimeSerieVariableTimeStepProvider, public ReosDssProviderBase
{
  public:

    // ReosDataProvider interface
    QString key() const override;

    // ReosTimeSerieProvider interface
    void load() override;
    QDateTime referenceTime() const override;
    QString valueUnit() const {return QString();}
    int valueCount() const override;
    double value( int i ) const override;
    double firstValue() const override;
    double lastValue() const override;
    double *data() override;
    const QVector<double> &constData() const override;
    ReosEncodedElement encode() const {return ReosEncodedElement();}
    void decode( const ReosEncodedElement &element ) {}

    // ReosDssProviderBase interface
    bool createNewSerie( const ReosDssPath &path, ReosDssFile &dssFile, QString &error ) const {return false;}

    // ReosTimeSerieVariableTimeStepProvider interface
    ReosDuration relativeTimeAt( int i ) const override;
    ReosDuration lastRelativeTime() const override;
    const QVector<ReosDuration> &constTimeData() const override;

    static QString dataType();

  private:
    QDateTime mReferenceTime;
    QVector<double> mValues;
    QVector<ReosDuration> mTimeValues;

};


class ReosDssProviderFactory: public ReosDataProviderFactory
{
  public:
    ReosTimeSerieProvider *createProvider( const QString &dataType ) const override;

    /**
     * Create a new data source from \a uri and \a dataType
     * For DSS provider the \a uri has the following form :
     * "/path/to/file/with/fileName"::/DSS/PATH/type/(void)/timeInterval/Version/
     */
    bool createNewDataSource( const QString &uri, const QString &dataType, QString &error ) override;
    QString key() const override;

};

#endif // REOSDSSPROVIDER_H
