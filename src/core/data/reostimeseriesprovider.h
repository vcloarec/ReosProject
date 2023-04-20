/***************************************************************************
  reostimeserieprovider.h - ReosTimeSerieProvider

 ---------------------
 begin                : 2.11.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSTIMESERIEPROVIDER_H
#define REOSTIMESERIEPROVIDER_H

#define SIP_NO_FILE

#include <memory>
#include <QVector>
#include <QDateTime>

#include "reosduration.h"
#include "reosdataobject.h"
#include "reosdataprovider.h"

class ReosTimeSeriesConstantInterval;
class ReosTimeSeriesVariableTimeStep;

/**
 * Abstract class for time serie provider
 */
class REOSCORE_EXPORT ReosTimeSerieProvider : public ReosDataProvider
{
    Q_OBJECT
  public:
    virtual ~ReosTimeSerieProvider();

    virtual bool isEditable() const {return false;}

    virtual QDateTime referenceTime() const = 0;
    virtual void setReferenceTime( const QDateTime &referenceTime );

    virtual QString valueUnit() const = 0;
    virtual int valueCount() const = 0;
    virtual double value( int i ) const = 0;
    virtual double firstValue() const = 0;
    virtual double lastValue() const = 0;

    virtual void setValue( int index, double value );
    virtual void removeValues( int from, int count );
    virtual void clear();

    virtual double *data() = 0;
    virtual const QVector<double> &constData() const = 0;

    virtual ReosEncodedElement encode( const ReosEncodeContext &context ) const = 0;
    virtual void decode( const ReosEncodedElement &element, const ReosEncodeContext &context ) = 0;

    virtual void load() {}
    QString dataSource() const;

    void setDataSource( const QString &dataSource, bool loadAfter = true );

    virtual QString htmlMetaData() const {return QString();}

    virtual bool persistData( QString & ) {return false;}

  private:
    QString mDataSource;
};

/**
 * Abstract class for constant time step time serie provider factory
 */
class REOSCORE_EXPORT ReosTimeSerieConstantTimeStepProvider : public ReosTimeSerieProvider
{
    Q_OBJECT
  public:
    ReosTimeSerieConstantTimeStepProvider() = default;
    ~ReosTimeSerieConstantTimeStepProvider();

    virtual void resize( int size );
    virtual void appendValue( double value );
    virtual void prependValue( double value );
    virtual void insertValue( int pos, double value );

    virtual bool isTimeStepCompatible( const ReosDuration & ) const {return true;}
    virtual ReosDuration timeStep() const = 0;
    virtual void setTimeStep( const ReosDuration &timeStep );

    virtual void copy( ReosTimeSerieConstantTimeStepProvider *other );

    virtual void setValues( const QVector<double> &vals );
};

/**
 * Abstract class for variable time step time serie provider factory
 */
class REOSCORE_EXPORT ReosTimeSerieVariableTimeStepProvider : public ReosTimeSerieProvider
{
  public:
    ReosTimeSerieVariableTimeStepProvider() = default;
    ~ReosTimeSerieVariableTimeStepProvider();

    virtual ReosDuration relativeTimeAt( int i ) const  = 0;
    virtual ReosDuration lastRelativeTime() const = 0;
    virtual const QVector<ReosDuration> &constTimeData() const = 0;

    virtual void setRelativeTimeAt( int i, const ReosDuration &relativeTime );
    virtual void appendValue( const ReosDuration &relativeTime, double v );
    virtual void prependValue( const ReosDuration &relativeTime, double v );
    virtual void insertValue( int fromPos, const ReosDuration &relativeTime, double v );

    virtual void copy( ReosTimeSerieVariableTimeStepProvider *other );

    /**
     * Writes \a series to location specified in \a uri, return false if fails.
     * Default implementation does nothing and return false.
     */
    virtual bool writeSeries( ReosTimeSeriesVariableTimeStep *series, const QString &uri );

    int timeValueIndex( const ReosDuration &time, bool &exact ) const;
    double valueAtTime( const ReosDuration &relativeTime ) const;
};


//**********************************************************************

class ReosTimeSerieConstantTimeStepMemoryProvider : public ReosTimeSerieConstantTimeStepProvider
{
  public:
    ReosTimeSerieConstantTimeStepMemoryProvider() = default;
    ReosTimeSerieConstantTimeStepMemoryProvider( const QVector<double> &values );

    QString key() const override;
    QStringList fileSuffixes() const override {return QStringList();}
    QDateTime referenceTime() const override;
    void setReferenceTime( const QDateTime &referenceTime ) override;
    ReosDuration timeStep() const override;
    void setTimeStep( const ReosDuration &timeStep ) override;
    QString valueUnit() const override;
    int valueCount() const override;
    void resize( int size ) override;
    double value( int i ) const override;
    double firstValue() const override;
    double lastValue() const override;
    void setValue( int i, double v ) override;
    void appendValue( double v ) override;
    void prependValue( double v ) override;
    void insertValue( int fromPos, double v ) override;
    bool isEditable() const override;
    double *data() override;
    const QVector<double> &constData() const override;
    void removeValues( int fromPos, int count ) override;
    void clear() override;
    ReosEncodedElement encode( const ReosEncodeContext &context ) const override;
    void decode( const ReosEncodedElement &element, const ReosEncodeContext &context ) override;
    void copy( ReosTimeSerieConstantTimeStepProvider *other ) override;
    void setValues( const QVector<double> &vals ) override;

    static QString staticType();

  private:
    QDateTime mReferenceTime;
    ReosDuration mTimeStep;
    QVector<double> mValues;
};

class ReosTimeSerieConstantTimeStepMemoryProviderFactory : public ReosDataProviderFactory
{
  public:

    ReosDataProvider *createProvider( const QString &dataType ) const override;
    QString key() const override {return QStringLiteral( "constant-time-step-memory" );}

    bool hasCapabilities( const QString &dataType, ReosDataProvider::Capabilities capabilities ) const override;

  private:
    ReosDataProvider::Capabilities mCapabilities = {ReosDataProvider::Memory};
};

//**********************************************************************

class ReosTimeSerieVariableTimeStepMemoryProvider : public ReosTimeSerieVariableTimeStepProvider
{
  public:
    ReosTimeSerieVariableTimeStepMemoryProvider() = default;
    ReosTimeSerieVariableTimeStepMemoryProvider( const QVector<double> &values, const QVector<ReosDuration> &timeValues );

    QString key() const override;
    QStringList fileSuffixes() const override {return QStringList();}
    QDateTime referenceTime() const override;
    void setReferenceTime( const QDateTime &referenceTime ) override;
    QString valueUnit() const override;
    int valueCount() const override;
    double value( int i ) const override;
    double firstValue() const override;
    double lastValue() const override;
    void setValue( int i, double v ) override;
    ReosDuration relativeTimeAt( int i ) const override;
    ReosDuration lastRelativeTime() const override;
    void setRelativeTimeAt( int i, const ReosDuration &relativeTime ) override;
    const QVector<ReosDuration> &constTimeData() const override;
    void appendValue( const ReosDuration &relativeTime, double v ) override;
    void prependValue( const ReosDuration &relativeTime, double v ) override;
    void insertValue( int fromPos, const ReosDuration &relativeTime, double v ) override;
    bool isEditable() const override {return true;}
    double *data() override {return mValues.data();}
    const QVector<double> &constData() const override {return mValues;}
    void removeValues( int fromPos, int count ) override;
    void clear() override;
    void copy( ReosTimeSerieVariableTimeStepProvider *other ) override;


  private:

    ReosEncodedElement encode( const ReosEncodeContext &context ) const override;
    void decode( const ReosEncodedElement &element, const ReosEncodeContext & ) override;

    static QString staticType();

  private:
    QDateTime mReferenceTime;
    QVector<double> mValues;
    QVector<ReosDuration> mTimeValues;


};


class ReosTimeSerieVariableTimeStepMemoryProviderFactory : public ReosDataProviderFactory
{
  public:
    ReosTimeSerieProvider *createProvider( const QString & ) const override {return new ReosTimeSerieVariableTimeStepMemoryProvider;}
    QString key() const override {return QStringLiteral( "variable-time-step-memory" );}
    bool hasCapabilities( const QString &dataType, ReosDataProvider::Capabilities capabilities ) const override;

  private:
    ReosDataProvider::Capabilities mCapabilities = {ReosDataProvider::Memory};
};


#endif // REOSTIMESERIEPROVIDER_H
